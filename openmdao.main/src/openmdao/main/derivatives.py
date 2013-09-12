""" Class definition for Derivatives.
This object is used by Component to store derivative information and to
perform calculations during a Fake Finite Difference.
"""
import itertools

from openmdao.main.vartree import VariableTree
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface

try:
    from numpy import array, ndarray, zeros, inner, ones, unravel_index, \
         ravel_multi_index, arange, prod

    # Can't solve derivatives without these
    from scipy.sparse.linalg import gmres, LinearOperator

except ImportError as err:
    import logging
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import array, ndarray, zeros, inner, \
                                             ones


def flattened_size(name, val):
    """ Return size of `val` flattened to a 1D float array. """
    if isinstance(val, float):
        return 1
    elif isinstance(val, ndarray):
        return val.size
    elif isinstance(val, VariableTree):
        size = 0
        for key in sorted(val.list_vars()):  # Force repeatable order.
            value = getattr(val, key)
            size += flattened_size('.'.join((name, key)), value)
        return size
    else:
        raise TypeError('Variable %s is of type %s which is not convertable'
                        ' to a 1D float array.' % (name, type(val)))

def flattened_value(name, val):
    """ Return `val` as a 1D float array. """
    if isinstance(val, float):
        return array([val])
    elif isinstance(val, ndarray):
        return val.flatten()
    elif isinstance(val, VariableTree):
        vals = []
        for key in sorted(val.list_vars()):  # Force repeatable order.
            value = getattr(val, key)
            vals.extend(flattened_value('.'.join((name, key)), value))
        return array(vals)
    else:
        raise TypeError('Variable %s is of type %s which is not convertable'
                        ' to a 1D float array.' % (name, type(val)))

def flattened_names(name, val, names=None):
    """ Return list of names for values in `val`. """
    if names is None:
        names = []
    if isinstance(val, float):
        names.append(name)
    elif isinstance(val, ndarray):
        for i in range(len(val)):
            value = val[i]
            flattened_names('%s[%s]' % (name, i), value, names)
    elif isinstance(val, VariableTree):
        for key in sorted(val.list_vars()):  # Force repeatable order.
            value = getattr(val, key)
            flattened_names('.'.join((name, key)), value, names)
    else:
        raise TypeError('Variable %s is of type %s which is not convertable'
                        ' to a 1D float array.' % (name, type(val)))
    return names


def calc_gradient(wflow, inputs, outputs):
    """Returns the gradient of the passed outputs with respect to
    all passed inputs.
    """

    # Size the problem
    nEdge = wflow.initialize_residual()
    A = LinearOperator((nEdge, nEdge),
                       matvec=wflow.matvecFWD,
                       dtype=float)

    num_in = 0
    for item in inputs:
        
        # For parameter groups, only size the first
        if isinstance(item, tuple):
            item = item[0]
            
        val = wflow.scope.get(item)
        width = flattened_size(item, val)
        num_in += width


    num_out = 0
    for item in outputs:
        val = wflow.scope.get(item)
        width = flattened_size(item, val)
        num_out += width

    J = zeros((num_out, num_in))

    # Locate the output keys:
    obounds = {}
    interior = wflow.get_interior_edges()

    # Not necessarily efficient, but outputs can be anywhere
    for output in outputs:
        for edge in interior:
            if output == edge[0]:
                obounds[output] = wflow.bounds[edge]
                break

    # Each comp calculates its own derivatives at the current
    # point. (i.e., linearizes)
    wflow.calc_derivatives(first=True, extra_in=inputs, extra_out=outputs)

    # Forward mode, solve linear system for each parameter
    j = 0
    for param in inputs:

        if ('@in', param) in wflow.bounds:
            i1, i2 = wflow.bounds[('@in', param)]
        else:
            if isinstance(param, tuple):
                param = param[0]
            i1, i2 = wflow.bounds[('@in', param.split('[')[0])]
            
        for irhs in range(i1, i2):

            RHS = zeros((nEdge, 1))
            RHS[irhs, 0] = 1.0

            # Call GMRES to solve the linear system
            dx, info = gmres(A, RHS,
                             tol=1.0e-6,
                             maxiter=100)

            i = 0
            for item in outputs:
                if item in obounds:
                    k1, k2 = obounds[item]
                else:
                    k1, k2 = obounds[item.split('[')[0]]
                J[i:i+(k2-k1), j] = dx[k1:k2]
                i += k2-k1

            j += 1
    
    #print inputs, '\n', outputs, '\n', J
    return J

def calc_gradient_adjoint(wflow, inputs, outputs):
    """Returns the gradient of the passed outputs with respect to
    all passed inputs. Calculation is done in adjoint mode.
    """

    # Size the problem
    nEdge = wflow.initialize_residual()
    A = LinearOperator((nEdge, nEdge),
                       matvec=wflow.matvecREV,
                       dtype=float)
    num_in = 0
    for item in inputs:
        
        # For parameter groups, only size the first
        if isinstance(item, tuple):
            item = item[0]
            
        val = wflow.scope.get(item)
        width = flattened_size(item, val)
        num_in += width

    num_out = 0
    for item in outputs:
        val = wflow.scope.get(item)
        width = flattened_size(item, val)
        num_out += width

    J = zeros((num_out, num_in))
   
    # Locate the output keys:
    obounds = {}
    interior = wflow.get_interior_edges()
    # Not necessarily efficient, but outputs can be anywhere
    for item in outputs:
        for edge in interior:
            if item == edge[0]:
                obounds[item] = wflow.bounds[edge]
                break

    # Each comp calculates its own derivatives at the current
    # point. (i.e., linearizes)
    wflow.calc_derivatives(first=True, extra_in=inputs, extra_out=outputs)

    # Adjoint mode, solve linear system for each output
    j = 0
    for output in outputs:

        if output in obounds:
            i1, i2 = obounds[output]
        else:
            i1, i2 = obounds[output.split('[')[0]]
            
        for irhs in range(i1, i2):

            RHS = zeros((nEdge, 1))
            RHS[irhs, 0] = 1.0

            # Call GMRES to solve the linear system
            dx, info = gmres(A, RHS,
                             tol=1.0e-6,
                             maxiter=100)
            
            i = 0
            for param in inputs:
                if ('@in', param) in wflow.bounds:
                    k1, k2 = wflow.bounds[('@in', param)]
                else:
                    if isinstance(param, tuple):
                        param = param[0]
                    k1, k2 = wflow.bounds[('@in', param.split('[')[0])]
                    
                J[j, i:i+(k2-k1)] = dx[k1:k2]
                i += k2-k1

            j += 1
    
    #print inputs, '\n', outputs, '\n', J
    return J


def applyJ(obj, arg, result):
    """Multiply an input vector by the Jacobian. For an Explicit Component,
    this automatically forms the "fake" residual, and calls into the
    function hook "apply_deriv".
    """
    for key in result:
        result[key] = 0.0*-arg[key]

    if hasattr(obj, 'apply_deriv'):
        
        # The apply_deriv function expects the argument and result dicts for
        # each input and output to have the same shape as the input/output.

        for key, value in result.iteritems():
            if len(value) > 1:
                var = obj.get(key)
                shape = var.shape
                result[key] = value.reshape(shape)

        for key, value in arg.iteritems():
            if len(value) > 1:
                var = obj.get(key)
                shape = var.shape
                arg[key] = value.reshape(shape)

        obj.apply_deriv(arg, result)

        for key, value in result.iteritems():
            if len(value) > 1:
                result[key] = value.flatten()

        for key, value in arg.iteritems():
            if len(value) > 1:
                arg[key] = value.flatten()

        return

    # Optional specification of the Jacobian
    # (Subassemblies do this by default)
    input_keys, output_keys, J = obj.provideJ()
    
    #print 'J', input_keys, output_keys, J
    
    # The Jacobian from provideJ is a 2D array containing the derivatives of
    # the flattened output_keys with respect to the flattened input keys. We
    # need to find the start and end index of each input and output. 
    ibounds, obounds = get_bounds(obj, input_keys, output_keys)
    
    for okey in result:
        
        odx = None
        if okey in obounds:
            o1, o2, osh = obounds[okey]
        else:
            o1, o2, osh = obounds[okey.split('[')[0]]
            odx = okey.strip(']').split('[')[1]            
            
        if o2 - o1 == 1:
            oshape = 1
        else:
            oshape = result[okey].shape
            
        used = []
        for ikey in arg:
            if ikey in result:
                continue
            
            idx = None
            if ikey in ibounds:
                i1, i2, ish = ibounds[ikey]
            else:
                i1, i2, ish = ibounds[ikey.split('[')[0]]
                idx = ikey.strip(']').split('[')[1]

            # Param groups make it tricky. We only want to add the
            # piece of J once for the whole group.
            if i1 in used:
                continue
            used.append(i1)
            
            Jsub = reduce_jacobian(J, ikey, okey, i1, i2, idx, ish,
                                   o1, o2, odx, osh)
            
            tmp = Jsub.dot(arg[ikey])
                
            result[okey] += tmp.reshape(oshape)
                        
    #print 'applyJ', arg, result

def applyJT(obj, arg, result):
    """Multiply an input vector by the transposed Jacobian. For an Explicit
    Component, this automatically forms the "fake" residual, and calls into
    the function hook "apply_derivT".
    """
    
    for key in arg:
        result[key] = -arg[key]

    if hasattr(obj, 'apply_derivT'):
        
        # The apply_deriv function expects the argument and result dicts for
        # each input and output to have the same shape as the input/output.

        for key, value in result.iteritems():
            if len(value) > 1:
                var = obj.get(key)
                shape = var.shape
                result[key] = value.reshape(shape)

        for key, value in arg.iteritems():
            if len(value) > 1:
                var = obj.get(key)
                shape = var.shape
                arg[key] = value.reshape(shape)

        obj.apply_derivT(arg, result)

        for key, value in result.iteritems():
            if len(value) > 1:
                result[key] = value.flatten()

        for key, value in arg.iteritems():
            if len(value) > 1:
                arg[key] = value.flatten()

        return

    # Optional specification of the Jacobian
    # (Subassemblies do this by default)
    input_keys, output_keys, J = obj.provideJ()

    #print 'J', input_keys, output_keys, J
    
    # The Jacobian from provideJ is a 2D array containing the derivatives of
    # the flattened output_keys with respect to the flattened input keys. We
    # need to find the start and end index of each input and output. 
    obounds, ibounds = get_bounds(obj, input_keys, output_keys)
    
    for okey in result:
        if okey in arg:
            continue
        
        odx = None
        if okey in obounds:
            o1, o2, osh = obounds[okey]
        else:
            o1, o2, osh = obounds[okey.split('[')[0]]
            odx = okey.strip(']').split('[')[1]
            
        if o2 - o1 == 1:
            oshape = 1
        else:
            oshape = result[okey].shape
            
        for ikey in arg:
            
            idx = None
            if ikey in ibounds:
                i1, i2, ish = ibounds[ikey]
            else:
                i1, i2, ish = ibounds[ikey.split('[')[0]]
                idx = ikey.strip(']').split('[')[1]
            
            Jsub = reduce_jacobian(J, okey, ikey, o1, o2, odx, osh,
                                   i1, i2, idx, ish).T
            
            tmp = Jsub.dot(arg[ikey])
                
            result[okey] += tmp.reshape(oshape)

    #print 'applyJT', arg, result
    
def get_bounds(obj, input_keys, output_keys):
    """ Returns a pair of dictionaries that contain the stop and end index
    for each input and output in a pair of lists.
    """
    
    ibounds = {}
    nvar = 0
    for key in input_keys:
        
        # For parameter group, all should be equal so just get first.
        if not isinstance(key, tuple):
            key = [key]
            
        val = obj.get(key[0])
        width = flattened_size('.'.join((obj.name, key[0])), val)
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

def reduce_jacobian(J, ikey, okey, i1, i2, idx, ish, o1, o2, odx, osh):
    """ Return the subportion of the Jacobian that is valid for a particular\
    input and output slice.
    
    J: 2D ndarray
        Full Jacobian
        
    ikey: str
        Input variable for which we want the reduced Jacobian
        
    okey: str
        Output variable for which we want the reduced Jacobian
        
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
    
    # J inputs
    if idx:
        idx = idx.strip('(').strip(')')
        
        # Handle complicated slices that may have : or -1 in them
        # We just use numpy index math to convert unravelable indices into
        # index arrays so that we can ravel them to find the set of indices
        # that we need to grab from J.
        if '-' in idx or ':' in idx:
            
            idx_list = idx.split(',')
            indices = []
            for index, size in zip(idx_list, ish):
                temp = eval('arange(size)[%s]' % index)
                if not isinstance(temp, ndarray):
                    temp = array([temp]) 
                indices.append(temp)
                
            if len(indices) > 1:
                indices = zip(*itertools.product(*indices))
            rav_ind = ravel_multi_index(indices, dims=osh) + i1
            istring = 'rav_ind'
                
        # Single index into a multi-D array
        elif ',' in idx:
            idx = eval(idx)
            ix = ravel_multi_index(idx, ish) + i1
            istring = 'ix:ix+1'
            
        # Single index into a 1D array
        else:
            ix = int(idx) + i1
            istring = 'ix:ix+1'
            
    # The entire array, already flat
    else:
        istring = 'i1:i2'
        
    # J Outputs
    if odx:
        odx = odx.strip('(').strip(')')
        
        # Handle complicated slices that may have : or -1 in them
        # We just use numpy index math to convert unravelable indices into
        # index arrays so that we can ravel them to find the set of indices
        # that we need to grab from J.
        if '-' in odx or ':' in odx:
            
            idx_list = odx.split(',')
            indices = []
            for index, size in zip(idx_list, osh):
                temp = eval('arange(size)[%s]' % index)
                if not isinstance(temp, ndarray):
                    temp = array([temp]) 
                indices.append(temp)
                    
            if len(indices) > 1:
                indices = zip(*itertools.product(*indices))
                
            rav_ind = ravel_multi_index(indices, dims=osh) + o1
            ostring = 'rav_ind'
            
        # Single index into a multi-D array
        elif ',' in odx:
            odx = eval(odx)
            ox = ravel_multi_index(odx, ish) + o1
            ostring = 'ox:ox+1'
            
        # Single index into a 1D array
        else:
            ox = int(odx) + o1
            ostring = 'ox:ox+1'
            
    # The entire array, already flat
    else:
        ostring = 'o1:o2'
    
    Jsub = eval('J[%s, %s]' % (ostring, istring))
    return Jsub
    
    
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

        self.fd_step = 1.0e-6*ones((len(self.inputs)))
        self.form = 'forward'

        in_size = 0
        for srcs in self.inputs:
            
            # Support for paramters groups
            if not isinstance(srcs, tuple):
                srcs = [srcs]
                
            val = self.scope.get(srcs[0])
            width = flattened_size(srcs[0], val)
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

        for src, fd_step in zip(self.inputs, self.fd_step):
            
            if isinstance(src, tuple):
                i1, i2 = self.in_bounds[src[0]]
            else:
                i1, i2 = self.in_bounds[src]
            
            for i in range(i1, i2):

                #--------------------
                # Forward difference
                #--------------------
                if self.form == 'forward':

                    # Step
                    if i2-i1 == 1:
                        self.set_value(src, fd_step)
                    else:
                        self.set_value(src, fd_step, i-i1)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Forward difference
                    self.J[:, i] = (self.y - self.y_base)/fd_step

                    # Undo step
                    if i2-i1 == 1:
                        self.set_value(src, -fd_step)
                    else:
                        self.set_value(src, -fd_step, i-i1)

                #--------------------
                # Backward difference
                #--------------------
                elif self.form == 'backward':

                    # Step
                    if i2-i1 == 1:
                        self.set_value(src, -fd_step)
                    else:
                        self.set_value(src, -fd_step, i-i1)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Backward difference
                    self.J[:, i] = (self.y_base - self.y)/fd_step

                    # Undo step
                    if i2-i1 == 1:
                        self.set_value(src, fd_step)
                    else:
                        self.set_value(src, fd_step, i-i1)

                #--------------------
                # Central difference
                #--------------------
                elif self.form == 'central':

                    # Forward Step
                    if i2-i1 == 1:
                        self.set_value(src, fd_step)
                    else:
                        self.set_value(src, fd_step, i-i1)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y)

                    # Backward Step
                    if i2-i1 == 1:
                        self.set_value(src, -2.0*fd_step)
                    else:
                        self.set_value(src, -2.0*fd_step, i-i1)

                    self.pa.run(ffd_order=1)
                    self.get_outputs(self.y2)

                    # Central difference
                    self.J[:, i] = (self.y - self.y2)/(2.0*fd_step)

                    # Undo step
                    if i2-i1 == 1:
                        self.set_value(src, fd_step)
                    else:
                        self.set_value(src, fd_step, i-i1)

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
            elif isinstance(old_val, VariableTree):
                new_val = old_val.copy()
                self.pa.wflow._update(src, new_val, self.y_base[i1:i2])

            if '[' in src:
                src, _, idx = src.partition('[')
                idx = int(idx[:-1])
                old_val = self.scope.get(src)
                old_val[idx] = new_val
                self.scope.set(src, old_val, force=True)
            else:
                self.scope.set(src, new_val, force=True)
                
        #print self.J
        return self.J

    def get_inputs(self, x):
        """Return matrix of flattened values from input edges."""

        for srcs in self.inputs:
            
            # Support for paramters groups
            if not isinstance(srcs, tuple):
                srcs = [srcs]
                
            for src in srcs:
                src_val = self.scope.get(src)
                src_val = flattened_value(src, src_val)
                i1, i2 = self.in_bounds[src]
                x[i1:i2] = src_val

    def get_outputs(self, x):
        """Return matrix of flattened values from output edges."""

        for src in self.outputs:
            src_val = self.scope.get(src)
            src_val = flattened_value(src, src_val)
            i1, i2 = self.out_bounds[src]
            x[i1:i2] = src_val

    def set_value(self, srcs, val, index=None):
        """Set a value in the model"""

        # Support for Parameter Groups:
        if not isinstance(srcs, tuple):
            srcs = [srcs]
            
        for src in srcs:    
            i1, i2 = self.in_bounds[src]
            comp_name, dot, var_name = src.partition('.')
            comp = self.scope.get(comp_name)
            old_val = self.scope.get(src)
    
            if index is None:
                if '[' in src:
                    src, _, idx = src.partition('[')
                    idx = int(idx[:-1])
                    old_val = self.scope.get(src)
                    old_val[idx] += val
    
                    # In-place array editing doesn't activate callback, so we
                    # must do it manually.
                    if var_name:
                        comp._input_updated(var_name.split('[')[0])
                    else:
                        self.scope._input_updated(comp_name.split('[')[0])
    
                else:
                    self.scope.set(src, old_val+val, force=True)
    
            else:
                unravelled = unravel_index(index, old_val.shape)
                old_val[unravelled] += val
    
                # In-place array editing doesn't activate callback, so we must
                # do it manually.
                if var_name:
                    comp._input_updated(var_name)
                else:
                    self.scope._input_updated(comp_name)
    
            # Prevent OpenMDAO from stomping on our poked input.
            
            if var_name:
                comp._valid_dict[var_name.split('[', 1)[0]] = True
                
                # Make sure we execute!
                comp._call_execute = True
                
            else:
                self.scope._valid_dict[comp_name.split('[', 1)[0]] = True
    

def apply_linear_model(self, comp, ffd_order):
    """Returns the Fake Finite Difference output for the given output
    name using the stored baseline and derivatives along with the
    new inputs in the component.
    """

    input_keys, output_keys, J = comp.provideJ()

    # First order derivatives
    if order == 1:

        for j, out_name in enumerate(output_keys):
            y = comp.get(out_name)
            for i, in_name in enumerate(input_keys):
                y += J[i, j]*(comp.get(in_name) - comp._ffd_inputs[in_name])
                setattr(comp, name, y)

    # Second order derivatives
    #elif order == 2:
    #
    #    for in_name1, item in self.second_derivatives[out_name].iteritems():
    #        for in_name2, dx in item.iteritems():
    #            y += 0.5*dx* \
    #              (self.parent.get(in_name1) - self.inputs[in_name1])* \
    #              (self.parent.get(in_name2) - self.inputs[in_name2])
    #
    else:
        msg = 'Fake Finite Difference does not currently support an ' + \
              'order of %s.' % order
        raise NotImplementedError(msg)

    return y


def recursive_components(scope, comps):
    # Recursively find all components contained in subdrivers.

    recursed_comps = []
    for name in comps:
        recursed_comps.append(name)
        comp = scope.get(name)
        if has_interface(comp, IDriver):
            sub_comps = comp.workflow.get_names(full=True)
            recursed_comps.extend(recursive_components(scope, sub_comps))

    return recursed_comps
