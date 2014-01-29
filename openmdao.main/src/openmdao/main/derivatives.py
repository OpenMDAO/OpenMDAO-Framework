""" Some functions and objects that provide the backbone to OpenMDAO's
differentiation capability.
"""

from openmdao.main.array_helpers import flatten_slice, flattened_size, \
                                        flattened_value
from openmdao.main.interfaces import IVariableTree
from openmdao.main.mp_support import has_interface
from openmdao.main.pseudocomp import PseudoComponent

try:
    from numpy import ndarray, zeros, ones, unravel_index, vstack, hstack
    # Can't solve derivatives without these
    from scipy.sparse.linalg import gmres, LinearOperator

except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import ndarray, zeros, \
                                    ones, unravel_index, vstack, hstack

# pylint: disable-msg=C0103

def calc_gradient(wflow, inputs, outputs, n_edge, shape):
    """Returns the gradient of the passed outputs with respect to
    all passed inputs.
    """

    # Size the problem
    A = LinearOperator((n_edge, n_edge),
                       matvec=wflow.matvecFWD,
                       dtype=float)

    J = zeros(shape)

    # Each comp calculates its own derivatives at the current
    # point. (i.e., linearizes)
    wflow.calc_derivatives(first=True)
    options = wflow._parent.gradient_options

    # Forward mode, solve linear system for each parameter
    j = 0
    for param in inputs:

        if isinstance(param, tuple):
            param = param[0]

        i1, i2 = wflow.get_bounds(param)

        if isinstance(i1, list):
            in_range = i1
        else:
            in_range = range(i1, i2)

        for irhs in in_range:

            RHS = zeros((n_edge, 1))
            RHS[irhs, 0] = 1.0

            # Call GMRES to solve the linear system
            dx, info = gmres(A, RHS,
                             tol=options.gmres_tolerance,
                             maxiter=options.gmres_maxiter)

            i = 0
            for item in outputs:
                k1, k2 = wflow.get_bounds(item)
                if isinstance(k1, list):
                    J[i:i+(len(k1)), j] = dx[k1]
                    i += len(k1)
                else:
                    J[i:i+(k2-k1), j] = dx[k1:k2]
                    i += k2-k1

            j += 1

    #print inputs, '\n', outputs, '\n', J
    return J

def calc_gradient_adjoint(wflow, inputs, outputs, n_edge, shape):
    """Returns the gradient of the passed outputs with respect to
    all passed inputs. Calculation is done in adjoint mode.
    """

    # Size the problem
    A = LinearOperator((n_edge, n_edge),
                       matvec=wflow.matvecREV,
                       dtype=float)
    J = zeros(shape)

    # Each comp calculates its own derivatives at the current
    # point. (i.e., linearizes)
    wflow.calc_derivatives(first=True)

    # Adjoint mode, solve linear system for each output
    j = 0
    for output in outputs:

        if isinstance(output, tuple):
            output = output[0]

        i1, i2 = wflow.get_bounds(output)

        if isinstance(i1, list):
            out_range = i1
        else:
            out_range = range(i1, i2)

        options = wflow._parent.gradient_options
        for irhs in out_range:

            RHS = zeros((n_edge, 1))
            RHS[irhs, 0] = 1.0

            # Call GMRES to solve the linear system
            dx, info = gmres(A, RHS,
                             tol=options.gmres_tolerance,
                             maxiter=options.gmres_maxiter)

            i = 0
            for param in inputs:

                if isinstance(param, tuple):
                    param = param[0]

                k1, k2 = wflow.get_bounds(param)
                if isinstance(k1, list):
                    J[j, i:i+(len(k1))] = dx[k1:k2]
                    i += len(k1)
                else:
                    J[j, i:i+(k2-k1)] = dx[k1:k2]
                    i += k2-k1

            j += 1

    #print inputs, '\n', outputs, '\n', J, dx
    return J

def pre_process_dicts(obj, key, arg_or_result, shape_cache):
    '''If the component supplies apply_deriv or applyMinv or their adjoint
    counterparts, it expects the contents to be shaped like the original
    variables. Also, it doesn't know how to handle array elements, so we need
    to do a fair amount of preparation on the way in.
    '''

    value = arg_or_result[key]

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
        else:
            meta = obj.get_metadata(key)

            # Custom data objects with data_shape in the metadata
            if 'data_shape' in meta:
                shape = meta['data_shape']
            else:
                return

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

def applyJ(obj, arg, result, residual, shape_cache, J=None):
    """Multiply an input vector by the Jacobian. For an Explicit Component,
    this automatically forms the "fake" residual, and calls into the
    function hook "apply_deriv".
    """
    for key in result:
        if key not in residual:
            result[key] = -arg[key]

    # If storage of the local Jacobian is a problem, the user can specify the
    # 'apply_deriv' function instead of provideJ.
    if J is None and hasattr(obj, 'apply_deriv'):

        # The apply_deriv function expects the argument and result dicts for
        # each input and output to have the same shape as the input/output.
        resultkeys = sorted(result.keys())
        for key in resultkeys:
            pre_process_dicts(obj, key, result, shape_cache)

        argkeys = arg.keys()
        for key in sorted(argkeys):
            pre_process_dicts(obj, key, arg, shape_cache)

        obj.apply_deriv(arg, result)

        # Result vector needs to be flattened.
        for key in reversed(resultkeys):
            post_process_dicts(key, result)

        # Arg is still called afterwards, so flatten it back.
        for key in argkeys:
            value = arg[key]
            if hasattr(value, 'flatten'):
                arg[key] = value.flatten()

        return

    input_keys, output_keys = obj.list_deriv_vars()
    # correct for the insideous one item tuple missing comma
    if isinstance(input_keys, basestring):
        input_keys = (input_keys,)
    if isinstance(output_keys, basestring):
        output_keys = (output_keys,)

    #print 'J', input_keys, output_keys, J

    # The Jacobian from provideJ is a 2D array containing the derivatives of
    # the flattened output_keys with respect to the flattened input keys. We
    # need to find the start and end index of each input and output.
    if obj._provideJ_bounds is None:
        obj._provideJ_bounds = get_bounds(obj, input_keys, output_keys)
    ibounds, obounds = obj._provideJ_bounds

    for okey in result:

        odx = None
        if okey in obounds:
            o1, o2, osh = obounds[okey]
        else:
            basekey, _, odx = okey.partition('[')
            o1, o2, osh = obounds[basekey]

        tmp = result[okey]
        used = set()
        for ikey in arg:
            if ikey in result:
                continue

            idx = None
            if ikey in ibounds:
                i1, i2, ish = ibounds[ikey]
                if (i1, i2) in used:
                    continue
                used.add((i1, i2))
            else:
                basekey, _, idx = ikey.partition('[')
                i1, i2, ish = ibounds[basekey]
                if (i1, i2, idx) in used or (i1, i2) in used:
                    continue
                used.add((i1, i2, idx))

            Jsub = reduce_jacobian(J, i1, i2, idx, ish,
                                      o1, o2, odx, osh)

            # for unit pseudocomps, just scalar multiply the args
            # by the conversion factor
            if isinstance(obj, PseudoComponent) and \
               obj._pseudo_type == 'units' and Jsub.shape == (1, 1):
                tmp += Jsub[0][0] * arg[ikey]
            else:
                tmp += Jsub.dot(arg[ikey])

    #print 'applyJ', arg, result

def applyJT(obj, arg, result, residual, shape_cache, J=None):
    """Multiply an input vector by the transposed Jacobian.
    For an Explicit Component, this automatically forms the "fake"
    residual, and calls into the function hook "apply_derivT".
    """

    for key in arg:
        if key not in residual:
            result[key] = -arg[key]

    # If storage of the local Jacobian is a problem, the user can
    # specify the 'apply_derivT' function instead of provideJ.
    if J is None and hasattr(obj, 'apply_derivT'):

        # The apply_deriv function expects the argument and
        # result dicts for each input and output to have the
        # same shape as the input/output.
        resultkeys = sorted(result.keys())
        for key in resultkeys:
            pre_process_dicts(obj, key, result, shape_cache)

        argkeys = arg.keys()
        for key in sorted(argkeys):
            pre_process_dicts(obj, key, arg, shape_cache)

        obj.apply_derivT(arg, result)

        # Result vector needs to be flattened.
        for key in reversed(resultkeys):
            post_process_dicts(key, result)

        # Arg is still called afterwards, so flatten it back.
        for key in argkeys:
            value = arg[key]
            if hasattr(value, 'flatten'):
                arg[key] = value.flatten()

        return

    input_keys, output_keys = obj.list_deriv_vars()
    # correct for the insideous one item tuple missing comma
    if isinstance(input_keys, basestring):
        input_keys = (input_keys,)
    if isinstance(output_keys, basestring):
        output_keys = (output_keys,)

    #print 'J', input_keys, output_keys, J

    # The Jacobian from provideJ is a 2D array containing the derivatives of
    # the flattened output_keys with respect to the flattened input keys. We
    # need to find the start and end index of each input and output.
    if obj._provideJ_bounds is None:
        obj._provideJ_bounds = get_bounds(obj, input_keys, output_keys)
    obounds, ibounds = obj._provideJ_bounds

    used = set()
    for okey in result:
        if okey in arg:
            continue

        odx = None
        if okey in obounds:
            o1, o2, osh = obounds[okey]
            if (o1, o2) in used:
                continue
            used.add((o1, o2))
        else:
            basekey, _, odx = okey.partition('[')
            o1, o2, osh = obounds[basekey]
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
                i1, i2, ish = ibounds[basekey]

            Jsub = reduce_jacobian(J, o1, o2, odx, osh,
                                      i1, i2, idx, ish).T

            # for unit pseudocomps, just scalar multiply the args
            # by the conversion factor
            if isinstance(obj, PseudoComponent) and \
               obj._pseudo_type == 'units' and Jsub.shape == (1, 1):
                tmp += Jsub[0][0] * arg[ikey]
            else:
                tmp += Jsub.dot(arg[ikey])

    #print 'applyJT', arg, result

def applyMinv(obj, inputs):
    """Simple wrapper around a component's applyMinv where we can reshape the
    arrays for each input and expand any needed array elements into full arrays.
    """

    inputkeys = sorted(inputs.keys())
    for key in inputkeys:
        pre_process_dicts(obj, key, inputs)

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

def get_bounds(obj, input_keys, output_keys):
    """ Returns a pair of dictionaries that contain the stop and end index
    for each input and output in a pair of lists.
    """

    ibounds = {}
    nvar = 0
    if hasattr(obj, 'parent'):
        scope = obj.parent
    else:
        scope = None  # Pseudoassys

    for key in input_keys:

        # For parameter group, all should be equal so just get first.
        if not isinstance(key, tuple):
            key = [key]

        val = obj.get(key[0])

        width = flattened_size('.'.join((obj.name, key[0])), val,
                               scope=scope)
        shape = val.shape if hasattr(val, 'shape') else None
        for item in key:
            ibounds[item] = (nvar, nvar+width, shape)
        nvar += width

    obounds = {}
    nvar = 0
    for key in output_keys:
        val = obj.get(key)
        width = flattened_size('.'.join((obj.name, key)), val)
        shape = val.shape if hasattr(val, 'shape') else None
        obounds[key] = (nvar, nvar+width, shape)
        nvar += width

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


class FiniteDifference(object):
    """ Helper object for performing finite difference on a portion of a model.
    """

    def __init__(self, pa):
        """ Performs finite difference on the components in a given
        pseudo_assembly. """

        self.inputs = pa.inputs
        self.outputs = pa.outputs
        self.in_bounds = {}
        self.out_bounds = {}
        self.pa = pa
        self.scope = pa.wflow.scope

        options = pa.wflow._parent.gradient_options

        self.fd_step = options.fd_step*ones((len(self.inputs)))
        self.form = options.fd_form
        self.form_custom = {}
        self.step_type = options.fd_step_type
        self.step_type_custom = {}
        self.relative_threshold = 1.0e-4

        driver = self.pa.wflow._parent
        driver_params = []
        driver_targets = []
        if hasattr(driver, 'get_parameters'):
            driver_params = self.pa.wflow._parent.get_parameters()
            driver_targets = driver.list_param_targets()
        in_size = 0
        for j, srcs in enumerate(self.inputs):

            # Support for parameter groups
            if isinstance(srcs, basestring):
                srcs = [srcs]

            # Local stepsize support
            meta = self.scope.get_metadata(self.scope._depgraph.base_var(srcs[0]))

            if 'fd_step' in meta:
                self.fd_step[j] = meta['fd_step']

            if srcs[0] in driver_targets:
                if srcs[0] in driver_params:
                    param = driver_params[srcs[0]]
                    if param.fd_step is not None:
                        self.fd_step[j] = param.fd_step
                else:
                    # have to check through all the param groups
                    for param_group in driver_params:
                        if not isinstance(param_group, str) and \
                           srcs[0] in param_group:
                            param = driver_params[param_group]
                            if param.fd_step is not None:
                                self.fd_step[j] = param.fd_step
                                break

            if 'fd_step_type' in meta:
                self.step_type_custom[j] = meta['fd_step_type']

            if 'fd_form' in meta:
                self.form_custom[j] = meta['fd_form']

            val = self.scope.get(srcs[0])
            width = flattened_size(srcs[0], val, self.scope)
            for src in srcs:
                self.in_bounds[src] = (in_size, in_size+width)
            in_size += width

        out_size = 0
        for src in self.outputs:
            val = self.scope.get(src)
            width = flattened_size(src, val)
            self.out_bounds[src] = (out_size, out_size+width)
            out_size += width

        self.J = zeros((out_size, in_size))
        self.y_base = zeros((out_size,))
        self.x = zeros((in_size,))
        self.y = zeros((out_size,))
        self.y2 = zeros((out_size,))

    def calculate(self):
        """Return Jacobian for all inputs and outputs."""
        self.get_inputs(self.x)
        self.get_outputs(self.y_base)

        for j, src, in enumerate(self.inputs):

            # Users can cusomtize the FD per variable
            fd_step = self.fd_step[j]
            if j in self.form_custom:
                form = self.form_custom[j]
            else:
                form = self.form
            if j in self.step_type_custom:
                step_type = self.step_type_custom[j]
            else:
                step_type = self.step_type

            if isinstance(src, basestring):
                i1, i2 = self.in_bounds[src]
            else:
                i1, i2 = self.in_bounds[src[0]]

            for i in range(i1, i2):

                # Relative stepsizing
                if step_type == 'relative':
                    current_val = self.get_value(src, i1, i2, i)
                    if current_val > self.relative_threshold:
                        fd_step = fd_step*current_val

                #--------------------
                # Forward difference
                #--------------------
                if form == 'forward':

                    # Step
                    self.set_value(src, fd_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Forward difference
                    self.J[:, i] = (self.y - self.y_base)/fd_step

                    # Undo step
                    self.set_value(src, -fd_step, i1, i2, i)

                #--------------------
                # Backward difference
                #--------------------
                elif form == 'backward':

                    # Step
                    self.set_value(src, -fd_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Backward difference
                    self.J[:, i] = (self.y_base - self.y)/fd_step

                    # Undo step
                    self.set_value(src, fd_step, i1, i2, i)

                #--------------------
                # Central difference
                #--------------------
                elif form == 'central':

                    # Forward Step
                    self.set_value(src, fd_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Backward Step
                    self.set_value(src, -2.0*fd_step, i1, i2, i)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y2)

                    # Central difference
                    self.J[:, i] = (self.y - self.y2)/(2.0*fd_step)

                    # Undo step
                    self.set_value(src, fd_step, i1, i2, i)

        # Return outputs to a clean state.
        for src in self.outputs:
            i1, i2 = self.out_bounds[src]
            old_val = self.scope.get(src)

            if isinstance(old_val, float):
                new_val = float(self.y_base[i1:i2])
            elif isinstance(old_val, ndarray):
                shape = old_val.shape
                if len(shape) > 1:
                    new_val = self.y_base[i1:i2]
                    new_val = new_val.reshape(shape)
                else:
                    new_val = self.y_base[i1:i2]
            elif has_interface(old_val, IVariableTree):
                new_val = old_val.copy()
                self.pa.wflow._update(src, new_val, self.y_base[i1:i2])

            src, _, idx = src.partition('[')
            if idx:
                old_val = self.scope.get(src)
                if isinstance(new_val, ndarray):
                    exec('old_val[%s = new_val.copy()' % idx)
                else:
                    exec('old_val[%s = new_val' % idx)
                self.scope.set(src, old_val, force=True)
            else:
                if isinstance(new_val, ndarray):
                    self.scope.set(src, new_val.copy(), force=True)
                else:
                    self.scope.set(src, new_val, force=True)

        #print 'after FD', self.pa.name, self.J
        return self.J

    def get_inputs(self, x):
        """Return matrix of flattened values from input edges."""

        for srcs in self.inputs:

            # Support for paramters groups
            if isinstance(srcs, basestring):
                srcs = [srcs]

            for src in srcs:
                src_val = self.scope.get(src)
                src_val = flattened_value(src, src_val)
                i1, i2 = self.in_bounds[src]
                if isinstance(src_val, ndarray):
                    x[i1:i2] = src_val.copy()
                else:
                    x[i1:i2] = src_val

    def get_outputs(self, x):
        """Return matrix of flattened values from output edges."""

        for src in self.outputs:

            # Speedhack: getting an indexed var in OpenMDAO is slow
            if '[' in src:
                basekey, _, index = src.partition('[')
                base = self.scope.get(basekey)
                exec("src_val = base[%s" % index)
            else:
                src_val = self.scope.get(src)

            src_val = flattened_value(src, src_val)
            i1, i2 = self.out_bounds[src]
            if isinstance(src_val, ndarray):
                x[i1:i2] = src_val.copy()
            else:
                x[i1:i2] = src_val

    def set_value(self, srcs, val, i1, i2, index):
        """Set a value in the model"""

        # Support for Parameter Groups:
        if isinstance(srcs, basestring):
            srcs = [srcs]

        for src in srcs:
            comp_name, _, var_name = src.partition('.')
            comp = self.scope.get(comp_name)

            if i2-i1 == 1:

                # Indexed array
                src, _, idx = src.partition('[')
                if idx:
                    old_val = self.scope.get(src)
                    exec('old_val[%s += val' % idx)

                    # In-place array editing doesn't activate callback, so we
                    # must do it manually.
                    if var_name:
                        base = self.scope._depgraph.base_var(src)
                        comp._input_updated(base.split('.')[-1],
                                            src.split('[')[0].partition('.')[2])
                    else:
                        self.scope._input_updated(comp_name.split('[')[0])

                # Scalar
                else:
                    old_val = self.scope.get(src)
                    self.scope.set(src, old_val+val, force=True)

            # Full vector
            else:
                index = index - i1

                # Indexed array
                if '[' in src:
                    sliced_src = self.scope.get(src)
                    sliced_shape = sliced_src.shape
                    flattened_src = sliced_src.flatten()
                    flattened_src[index] += val
                    sliced_src = flattened_src.reshape(sliced_shape)
                    exec('self.scope.%s = sliced_src') % src

                else:
                    old_val = self.scope.get(src)
                    unravelled = unravel_index(index, old_val.shape)
                    old_val[unravelled] += val

                # In-place array editing doesn't activate callback, so we must
                # do it manually.
                if var_name:
                    base = self.scope._depgraph.base_var(src)
                    comp._input_updated(base.split('.')[-1],
                                        src.split('[')[0].partition('.')[2])
                else:
                    self.scope._input_updated(comp_name.split('[', 1)[0])

            # Prevent OpenMDAO from stomping on our poked input.
            if var_name:
                self.scope.set_valid([self.scope._depgraph.base_var(src)],
                                    True)

                # Make sure we execute!
                comp._call_execute = True

            else:
                self.scope.set_valid([comp_name.split('[', 1)[0]], True)

    def get_value(self, src, i1, i2, index):
        """Get a value from the model. We only need this function for
        determining the relative stepsize to take."""

        # Parameters groups all have same value, so only take from
        # first one.
        if not isinstance(src, basestring):
            src = src[0]

        old_val = self.scope.get(src)

        # Full vector
        if i2-i1 > 1:
            index = index - i1

            # Indexed array slice
            if '[' in src:
                flattened_src = old_val.flatten()
                old_val = flattened_src[index]
            else:
                unravelled = unravel_index(index, old_val.shape)
                old_val = old_val[unravelled]

        return old_val

