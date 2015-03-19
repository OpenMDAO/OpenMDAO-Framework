import sys

""" Some functions and objects that support the component-side derivative API.
"""
from numpy import zeros, vstack, hstack

# pylint: disable=E0611,F0401
from openmdao.main.array_helpers import flatten_slice, flattened_size
from openmdao.main.interfaces import ISystem, IAssembly
from openmdao.util.graph import list_deriv_vars

# pylint: disable=C0103


def pre_process_dicts(obj, key, arg_or_result, shape_cache, scope, is_sys):
    '''If the component supplies apply_deriv or applyMinv or their adjoint
    counterparts, it expects the contents to be shaped like the original
    variables. Also, it doesn't know how to handle array elements, so we need
    to do a fair amount of preparation on the way in.
    '''

    value = arg_or_result[key]
    if is_sys is True:
        obj = scope

    # For arrays, apply_deriv expects full arrays, not
    # indexed ones. We need to create the full array on
    # the fly, then poke in the values.
    basekey, _, index = key.partition('[')
    if index:
        if key not in shape_cache:
            var = obj.get(basekey)
            shape_cache[basekey] = var.shape
            shape_cache[key] = eval("var[%s" % index).shape

        if basekey not in arg_or_result:
            arg_or_result[basekey] = zeros(shape_cache[basekey])

        shape = shape_cache[key]
        if shape:
            value = value.reshape(shape_cache[key])

        var = arg_or_result[basekey] # This speeds up the eval
        exec("var[%s = value" % index)

    else:
        var = obj.get(key)
        if isinstance(var, float):
            return

        if hasattr(var, 'shape'):
            shape = var.shape
            arg_or_result[key] = value.reshape(shape)

def post_process_dicts(key, result):
    '''Once we've called apply_deriv or appyMinv (or their adjoint
    counterparts, we need to restore them to their expected format, so
    basically flatten and poke array elements.
    '''

    value = result[key]

    # If we have sliced arrays in our index, then we need to
    # poke the data back into the sliced keys.
    basekey, _, index = key.partition('[')
    if index:
        base = result[basekey]
        exec("var2 = base[%s" % index)
        value[:] = var2.flatten()
    else:
        if hasattr(value, 'flatten'):
            result[key] = value.flatten()

def applyJ(system, variables):
    """Multiply an input vector by the Jacobian. For an Explicit Component,
    this automatically forms the "fake" residual, and calls into the
    function hook "apply_deriv".
    """

    J = system.J
    obj = system.inner()
    scope = system.scope

    is_sys = ISystem.providedBy(obj)

    arg = {}
    for item in system.list_states():

        collapsed = scope.name2collapsed.get(item)
        if collapsed not in variables:
            continue

        key = item
        if not is_sys:
            key = item.partition('.')[-1]
        parent = system

        while True:
            if item in parent.vec['du']:
                arg[key] = parent.vec['du'][item]
                break
            parent = parent._parent_system

    for item in system.list_inputs():

        collapsed = scope.name2collapsed.get(item)
        if collapsed not in variables:
            continue

        key = item
        if not is_sys:
            key = item.partition('.')[-1]
        parent = system

        while True:
            parent = parent._parent_system
            if item in parent.vec['dp']:
                arg[key] = parent.vec['dp'][item]
                break

    result = {}
    for item in system.list_outputs():

        collapsed = scope.name2collapsed.get(item)
        if collapsed not in variables:
            continue

        key = item
        if not is_sys:
            key = item.partition('.')[-1]
        result[key] = system.rhs_vec[item]

    for item in system.list_residuals():
        key = item
        if not is_sys:
            key = item.partition('.')[-1]
        result[key] = system.rhs_vec[item]

    # Bail if this component is not connected in the graph
    if len(arg) == 0 or len(result) == 0:
        return

    # Speedhack, don't call component's derivatives if incoming vector is zero.
    nonzero = False
    for key, value in arg.iteritems():
        if any(value != 0):
            nonzero = True
            break

    if nonzero is False:
        #print 'applyJ', obj.name, arg, result
        return

    # If storage of the local Jacobian is a problem, the user can specify the
    # 'apply_deriv' function instead of provideJ.
    if J is None and hasattr(obj, 'apply_deriv'):

        # TODO - We shouldn't need to calculate the size of the full arrays,
        # so the cache shouldn't be needed. Cache is None for now.
        shape_cache = {}

        # The apply_deriv function expects the argument and result dicts for
        # each input and output to have the same shape as the input/output.
        resultkeys = sorted(result.keys())
        for key in resultkeys:
            pre_process_dicts(obj, key, result, shape_cache, scope, is_sys)

        argkeys = arg.keys()
        for key in sorted(argkeys):
            pre_process_dicts(obj, key, arg, shape_cache, scope, is_sys)

        obj.apply_deriv(arg, result)

        # Result vector needs to be flattened.
        for key in reversed(resultkeys):
            post_process_dicts(key, result)

        # Arg is still called afterwards, so flatten it back.
        for key in argkeys:
            value = arg[key]
            if hasattr(value, 'flatten'):
                arg[key] = value.flatten()

        #print 'applyJ', obj.name, arg, result
        return

    if is_sys:
        input_keys = system.list_inputs() + system.list_states()
        output_keys = system.list_outputs() + system.list_residuals()
    elif IAssembly.providedBy(obj):
        input_keys = [item.partition('.')[-1] \
                      for item in system.list_inputs()]
        output_keys = [item.partition('.')[-1] \
                       for item in system.list_outputs()]
    else:
        input_keys, output_keys = list_deriv_vars(obj)

    #print 'J', input_keys, output_keys, J

    # The Jacobian from provideJ is a 2D array containing the derivatives of
    # the flattened output_keys with respect to the flattened input keys. We
    # need to find the start and end index of each input and output.

    if obj._provideJ_bounds is None:
        obj._provideJ_bounds = get_bounds(obj, input_keys, output_keys, J)
    ibounds, obounds = obj._provideJ_bounds

    for okey in result:

        odx = None
        if okey in obounds:
            o1, o2, osh = obounds[okey]
        else:
            basekey, _, odx = okey.partition('[')
            try:
                o1, o2, osh = obounds[basekey]
            except KeyError:
                if obj.missing_deriv_policy == 'error':
                    msg = "does not provide analytical derivatives" + \
                          " for %s" % okey
                    obj.raise_exception(msg, KeyError)
                continue

        tmp = result[okey]
        used = set()
        for ikey in arg:

            idx = None
            if ikey in ibounds:
                i1, i2, ish = ibounds[ikey]
                if (i1, i2) in used:
                    continue
                used.add((i1, i2))
            else:
                basekey, _, idx = ikey.partition('[')
                try:
                    i1, i2, ish = ibounds[basekey]
                except KeyError:
                    if obj.missing_deriv_policy == 'error':
                        msg = "does not provide analytical derivatives" + \
                              " for %s" % ikey
                        obj.raise_exception(msg, KeyError)
                    continue

                if (i1, i2, idx) in used or (i1, i2) in used:
                    continue
                used.add((i1, i2, idx))

            Jsub = reduce_jacobian(J, i1, i2, idx, ish,
                                      o1, o2, odx, osh)
            #print ikey, okey, Jsub

            tmp += Jsub.dot(arg[ikey])

    #print 'applyJ', obj.name, arg, result

def applyJT(system, variables):
    """Multiply an input vector by the transposed Jacobian.
    For an Explicit Component, this automatically forms the "fake"
    residual, and calls into the function hook "apply_derivT".
    """

    J = system.J
    obj = system.inner()
    scope = system.scope
    is_sys = ISystem.providedBy(obj)

    arg = {}

    for item in system.list_outputs():

        # TODO - Linear GS needs these. Need to fix something there.
        #collapsed = system.scope.name2collapsed.get(item)
        #if collapsed not in variables:
        #    continue

        key = item
        if not is_sys:
            key = item.partition('.')[-1]
        arg[key] = system.sol_vec[item]

    for item in system.list_residuals():

        key = item
        if not is_sys:
            key = item.partition('.')[-1]
        arg[key] = system.sol_vec[item]

    result = {}
    for item in system.list_states():

        collapsed = scope.name2collapsed.get(item)
        if collapsed not in variables:
            continue

        key = item
        if not is_sys:
            key = item.partition('.')[-1]

        parent = system
        while True:
            if item in parent.vec['du']:
                result[key] = parent.vec['du'][item]
                break
            parent = parent._parent_system

    for item in system.list_inputs():

        collapsed = scope.name2collapsed.get(item)
        if collapsed not in variables:
            continue

        key = item
        if not is_sys:
            key = item.partition('.')[-1]

        parent = system
        while True:
            parent = parent._parent_system
            if item in parent.vec['dp']:
                result[key] = parent.vec['dp'][item]
                break

    # Bail if this component is not connected in the graph
    if len(arg) == 0 or len(result) == 0:
        return

    # Speedhack, don't call component's derivatives if incoming vector is zero.
    nonzero = False
    for key, value in arg.iteritems():
        if any(value != 0):
            nonzero = True
            break

    if nonzero is False:
        #print 'applyJT', obj.name, arg, result
        return

    # If storage of the local Jacobian is a problem, the user can
    # specify the 'apply_derivT' function instead of provideJ.
    if J is None and hasattr(obj, 'apply_derivT'):

        # TODO - We shouldn't need to calculate the size of the full arrays,
        # so the cache shouldn't be needed. Cache is None for now.
        shape_cache = {}

        # The apply_deriv function expects the argument and
        # result dicts for each input and output to have the
        # same shape as the input/output.
        resultkeys = sorted(result.keys())
        for key in resultkeys:
            pre_process_dicts(obj, key, result, shape_cache, scope, is_sys)

        argkeys = arg.keys()
        for key in sorted(argkeys):
            pre_process_dicts(obj, key, arg, shape_cache, scope, is_sys)

        obj.apply_derivT(arg, result)

        # Result vector needs to be flattened.
        for key in reversed(resultkeys):
            post_process_dicts(key, result)

        # Arg is still called afterwards, so flatten it back.
        for key in argkeys:
            value = arg[key]
            if hasattr(value, 'flatten'):
                arg[key] = value.flatten()

        #print 'applyJT', obj.name, arg, result
        return

    if is_sys:
        input_keys = system.list_inputs() + system.list_states()
        output_keys = system.list_outputs() + system.list_residuals()
    elif IAssembly.providedBy(obj):
        input_keys = [item.partition('.')[-1] \
                      for item in system.list_inputs()]
        output_keys = [item.partition('.')[-1] \
                       for item in system.list_outputs()]
    else:
        input_keys, output_keys = list_deriv_vars(obj)

    # The Jacobian from provideJ is a 2D array containing the derivatives of
    # the flattened output_keys with respect to the flattened input keys. We
    # need to find the start and end index of each input and output.
    if obj._provideJ_bounds is None:
        obj._provideJ_bounds = get_bounds(obj, input_keys, output_keys, J)
    obounds, ibounds = obj._provideJ_bounds

    used = set()
    for okey in result:
        odx = None
        if okey in obounds:
            o1, o2, osh = obounds[okey]
            if (o1, o2) in used:
                continue
            used.add((o1, o2))
        else:
            basekey, _, odx = okey.partition('[')
            try:
                o1, o2, osh = obounds[basekey]
            except KeyError:
                if obj.missing_deriv_policy == 'error':
                    msg = "does not provide analytical derivatives for" + \
                          "%s" % okey
                    obj.raise_exception(msg, KeyError)
                continue

            if (o1, o2, odx) in used or (o1, o2) in used:
                continue
            used.add((o1, o2, odx))

        tmp = result[okey]
        for ikey in arg:
            idx = None
            if ikey in ibounds:
                i1, i2, ish = ibounds[ikey]
            else:
                basekey, _, idx = ikey.partition('[')
                try:
                    i1, i2, ish = ibounds[basekey]
                except KeyError:
                    if obj.missing_deriv_policy == 'error':
                        msg = "does not provide analytical derivatives for" + \
                               "%s" % ikey
                        obj.raise_exception(msg, KeyError)
                    continue

            Jsub = reduce_jacobian(J, o1, o2, odx, osh,
                                      i1, i2, idx, ish).T
            #print ikey, okey, Jsub

            tmp += Jsub.dot(arg[ikey])

    #print 'applyJT', obj.name, arg, result

def applyMinv(obj, inputs, shape_cache):
    """Simple wrapper around a component's applyMinv where we can reshape the
    arrays for each input and expand any needed array elements into full arrays.
    """

    inputkeys = sorted(inputs.keys())
    for key in inputkeys:
        pre_process_dicts(obj, key, inputs, shape_cache)

    pre_inputs = inputs.copy()

    inputs = obj.applyMinv(pre_inputs, inputs)

    # Result vector needs to be flattened.
    for key in reversed(inputkeys):
        post_process_dicts(key, inputs)

    # Clean out any leftover keys we added
    for key in inputs.keys():
        if key not in inputkeys:
            inputs.pop(key)

    return inputs

def applyMinvT(obj, inputs, shape_cache):
    """Simple wrapper around a component's applyMinvT where we can reshape the
    arrays for each input and expand any needed array elements into full arrays.
    """

    inputkeys = sorted(inputs.keys())
    for key in inputkeys:
        pre_process_dicts(obj, key, inputs, shape_cache)

    pre_inputs = inputs.copy()

    inputs = obj.applyMinvT(pre_inputs, inputs)

    # Result vector needs to be flattened.
    for key in reversed(inputkeys):
        post_process_dicts(key, inputs)

    # Clean out any leftover keys we added
    for key in inputs.keys():
        if key not in inputkeys:
            inputs.pop(key)

    return inputs

def get_bounds(obj, input_keys, output_keys, J):
    """ Returns a pair of dictionaries that contain the stop and end index
    for each input and output in a pair of lists.
    """

    ibounds = {}
    nvar = 0
    scope = getattr(obj, 'parent', None)

    for key in input_keys:

        # For parameter group, all should be equal so just get first.
        if not isinstance(key, tuple):
            key = [key]

        val = obj.get(key[0])

        width = flattened_size('.'.join((obj.name, key[0])), val,
                               scope=scope)
        shape = getattr(val, 'shape', None)
        for item in key:
            ibounds[item] = (nvar, nvar+width, shape)
        nvar += width

    num_input = nvar

    obounds = {}
    nvar = 0
    for key in output_keys:
        val = obj.get(key)
        width = flattened_size('.'.join((obj.name, key)), val)
        shape = getattr(val, 'shape', None)
        obounds[key] = (nvar, nvar+width, shape)
        nvar += width

    num_output = nvar

    if num_input and num_output:
        # Give the user an intelligible error if the size of J is wrong.
        try:
            J_output, J_input = J.shape
        except ValueError as err:
            exc_type, value, traceback = sys.exc_info()
            msg = "Jacobian has the wrong dimensions. Expected 2D but got {}D."
            msg = msg.format(J.ndim)

            raise ValueError, ValueError(msg), traceback

        if num_output != J_output or num_input != J_input:
            msg = 'Jacobian is the wrong size. Expected ' + \
                '(%dx%d) but got (%dx%d)' % (num_output, num_input,
                                             J_output, J_input)
            if ISystem.providedBy(obj):
                raise RuntimeError(msg)
            else:
                obj.raise_exception(msg, RuntimeError)

    return ibounds, obounds

def reduce_jacobian(J, i1, i2, idx, ish, o1, o2, odx, osh):
    """ Return the subportion of the Jacobian that is valid for a particular
    input and output slice.

    J: 2D ndarray
        Full Jacobian

    i1, i2: int, int
        Start and end index for the input variable

    o1, o2: int, int
        Start and end index for the output variable

    idx, odx: str, str
        Index strings for the input and output, if they are arrays. These
        are None if the entries in the Jacobian have already sliced the
        array for us (this can happen with pseudoAssemblies), in which case
        we need to do no work.

    ish, osh: tuples
        Shapes of the original input and output variables before being
        flattened.
    """

    if idx or odx:
        if idx: # J inputs
            istring, ix = flatten_slice(idx, ish, offset=i1, name='ix')
        else: # The entire array, already flat
            istring = 'i1:i2'

        if odx: # J Outputs
            ostring, ox = flatten_slice(odx, osh, offset=o1, name='ox')
        else: # The entire array, already flat
            ostring = 'o1:o2'

        if ':' not in ostring and len(ox) > 1:
            ostring = 'vstack(%s)' % ostring
        if ':' not in istring and len(ix) > 1:
            istring = 'hstack(%s)' % istring

        return eval('J[%s, %s]' % (ostring, istring))
    else:
        return J[o1:o2, i1:i2]
