""" Class definition for Derivatives.
This object is used by Component to store derivative information and to
perform calculations during a Fake Finite Difference.
"""

from openmdao.main.vartree import VariableTree
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface

try:
    from numpy import array, ndarray, zeros, inner, ones, unravel_index

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
    wflow.calc_derivatives(first=True, extra_in=inputs)

    # Forward mode, solve linear system for each parameter
    j = 0
    for param in inputs:

        i1, i2 = wflow.bounds[('@in', param)]
        for irhs in range(i1, i2):

            RHS = zeros((nEdge, 1))
            RHS[irhs, 0] = 1.0

            # Call GMRES to solve the linear system
            dx, info = gmres(A, RHS,
                             tol=1.0e-6,
                             maxiter=100)

            i = 0
            for item in outputs:
                k1, k2 = obounds[item]
                if k2-k1 > 1:
                    J[i:i+(k2-k1), j] = dx[k1:k2]
                else:
                    J[i, j] = dx[k1:k2]
                i += k2-k1

            j += 1
    
    
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
    wflow.calc_derivatives(first=True, extra_in=inputs)

    # Adjoint mode, solve linear system for each output
    j = 0
    for output in outputs:

        i1, i2 = obounds[output]
        for irhs in range(i1, i2):

            RHS = zeros((nEdge, 1))
            RHS[irhs, 0] = 1.0

            # Call GMRES to solve the linear system
            dx, info = gmres(A, RHS,
                             tol=1.0e-6,
                             maxiter=100)
            
            i = 0
            for param in inputs:
                k1, k2 = wflow.bounds[('@in', param)]
                if k2-k1 > 1:
                    J[j, i:i+(k2-k1)] = dx[k1:k2]
                else:
                    J[j, i] = dx[k1:k2]
                i += k2-k1

            j += 1
    
    return J


def applyJ(obj, arg, result):
    """Multiply an input vector by the Jacobian. For an Explicit Component,
    this automatically forms the "fake" residual, and calls into the
    function hook "apply_deriv".
    """
    for key in result:
        result[key] = -arg[key]

    if hasattr(obj, 'apply_deriv'):

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
    
    ibounds = {}
    nvar = 0
    for key in input_keys:
        
        # For parameter group, all should be equal so just get first.
        if not isinstance(key, tuple):
            key = [key]
            
        val = obj.get(key[0])
        width = flattened_size('.'.join((obj.name, key[0])), val)
        for item in key:
            ibounds[item] = (nvar, nvar+width)
        nvar += width

    obounds = {}
    nvar = 0
    for key in output_keys:
        val = obj.get(key)
        width = flattened_size('.'.join((obj.name, key)), val)
        obounds[key] = (nvar, nvar+width)
        nvar += width

    for okey in result:
        o1, o2 = obounds[okey]
        used = []
        for ikey in arg:
            if ikey not in result:
                i1, i2 = ibounds[ikey]
                
                # Param groups make it tricky. We only want to add the
                # piece of J once for the whole group.
                if i1 in used:
                    continue
                used.append(i1)
                
                if i2 - i1 == 1:
                    if o2 - o1 == 1:
                        Jsub = float(J[o1, i1])
                        result[okey] += Jsub*arg[ikey]
                    else:
                        Jsub = J[o1:o2, i1:i2]
                        tmp = Jsub*arg[ikey]
                        result[okey] += tmp.reshape(result[okey].shape)
                else:
                    tmp = flattened_value('.'.join((obj.name, ikey)),
                                          arg[ikey]).reshape(1, -1)
                    Jsub = J[o1:o2, i1:i2]
                    tmp = inner(Jsub, tmp)
                    if o2 - o1 == 1:
                        result[okey] += float(tmp)
                    else:
                        result[okey] += tmp.reshape(result[okey].shape)
                        
    #print 'applyJ', arg, result

def applyJT(obj, arg, result):
    """Multiply an input vector by the transposed Jacobian. For an Explicit
    Component, this automatically forms the "fake" residual, and calls into
    the function hook "apply_derivT".
    """
    
    for key in arg:
        result[key] = -arg[key]

    if hasattr(obj, 'apply_derivT'):
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

    ibounds = {}
    nvar = 0
    for key in output_keys:
        val = obj.get(key)
        width = flattened_size('.'.join((obj.name, key)), val)
        ibounds[key] = (nvar, nvar+width)
        nvar += width

    obounds = {}
    nvar = 0
    for key in input_keys:
        
        # For parameter group, all should be equal so just get first.
        if not isinstance(key, tuple):
            key = [key]
            
        val = obj.get(key[0])
        width = flattened_size('.'.join((obj.name, key[0])), val)
        for item in key:
            obounds[item] = (nvar, nvar+width)
        nvar += width

    for okey in result:
        if okey not in arg:
            o1, o2 = obounds[okey]
            
            for ikey in arg:
                i1, i2 = ibounds[ikey]
                if i2 - i1 == 1:
                    if o2 - o1 == 1:
                        Jsub = float(J[i1, o1])
                        result[okey] += Jsub*arg[ikey]
                    else:
                        Jsub = J[i1:i2, o1:o2].T
                        tmp = Jsub*arg[ikey]
                        result[okey] += tmp.reshape(result[okey].shape)
                else:
                    tmp = flattened_value('.'.join((obj.name, ikey)),
                                          arg[ikey]).reshape(1, -1)
                    Jsub = J[i1:i2, o1:o2].T
                    tmp = inner(Jsub, tmp)
                    if o2 - o1 == 1:
                        result[okey] += float(tmp)
                    else:
                        result[okey] += tmp.reshape(result[okey].shape)


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
            
            # Cached OpenMDAO's valid state
            #comp_name, dot, var_name = src.partition('.')
            #var_name = var_name.split('[')[0]
            #comp = self.scope.get(comp_name)
            #saved_state = comp._valid_dict
            

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

            # Reset OpenMDAO's valid state.
            #comp._valid_dict = saved_state

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
                    comp._input_updated(var_name.split('[')[0])
    
                else:
                    self.scope.set(src, old_val+val, force=True)
    
            else:
                unravelled = unravel_index(index, old_val.shape)
                old_val[unravelled] += val
    
                # In-place array editing doesn't activate callback, so we must
                # do it manually.
                comp._input_updated(var_name)
    
            # Prevent OpenMDAO from stomping on our poked input.
            comp._valid_dict[var_name.split('[',1)[0]] = True
    
            # Make sure we execute!
            comp._call_execute = True

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
