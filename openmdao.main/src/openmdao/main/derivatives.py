""" Class definition for Derivatives.
This object is used by Component to store derivative information and to
perform calculations during a Fake Finite Difference.
"""

from openmdao.main.vartree import VariableTree

try:
    from numpy import array, ndarray, zeros, inner, ones
    
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


def calc_gradient(wflow, inputs, outputs):
    """Returns the gradient of the passed outputs with respect to
    all passed inputs.
    """    
            
    # Size the problem
    nEdge = wflow.initialize_residual()
    A = LinearOperator((nEdge, nEdge),
                       matvec=wflow.matvecFWD,
                       dtype=float)
    J = zeros((len(outputs), len(inputs)))
    
    # Locate the output keys:
    obounds = {}
    # Not necessarily efficient, but outputs can be anywhere
    for item in outputs:
        for edge in wflow.get_interior_edges():
            if item == edge[0]:
                obounds[item] = wflow.bounds[edge]
                break
            
    # Each comp calculates its own derivatives at the current
    # point. (i.e., linearizes)
    wflow.calc_derivatives(first=True)
    
    # Forward mode, solve linear system for each parameter
    for j, param in enumerate(inputs):
        RHS = zeros((nEdge, 1))
        i1, i2 = wflow.bounds[('@in', param)]
        for i in range(i1, i2):
            RHS[i, 0] = 1.0
    
        # Call GMRES to solve the linear system
        dx, info = gmres(A, RHS,
                         tol=1.0e-6,
                         maxiter=100)

        i = 0
        for item in outputs:
            k1, k2 = obounds[item]
            J[i:i+(k2-k1), j] = dx[k1:k2]
            i += k2-k1
        
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
    J = zeros((len(outputs), len(inputs)))
    
    # Locate the output keys:
    obounds = {}
    # Not necessarily efficient, but outputs can be anywhere
    for item in outputs:
        for edge in wflow.get_interior_edges():
            if item == edge[0]:
                obounds[item] = wflow.bounds[edge]
                break
            
    # Each comp calculates its own derivatives at the current
    # point. (i.e., linearizes)
    wflow.calc_derivatives(first=True)
    
    # Adjoint mode, solve linear system for each output
    for j, output in enumerate(outputs):
        RHS = zeros((nEdge, 1))
        i1, i2 = obounds[output]
        for i in range(i1, i2):
            RHS[i, 0] = 1.0
    
        # Call GMRES to solve the linear system
        dx, info = gmres(A, RHS,
                         tol=1.0e-6,
                         maxiter=100)

        i = 0
        for param in inputs:
            k1, k2 = wflow.bounds[('@in', param)]
            J[j, i:i+(k2-k1)] = dx[k1:k2]
            i += k2-k1
        
    return J


def applyJ(obj, arg, result):
    """Multiply an input vector by the Jacobian. For an Explicit Component,
    this automatically forms the "fake" residual, and calls into the
    function hook "apply_deriv".
    """
    for key in result:
        result[key] = -arg[key]

    if hasattr(obj, 'apply_deriv'):
        obj.apply_deriv(arg, result)
        return

    # Optional specification of the Jacobian
    # (Subassemblies do this by default)
    input_keys, output_keys, J = obj.provideJ()

    ibounds = {}
    nvar = 0
    for key in input_keys:
        val = obj.get(key)
        width = flattened_size('.'.join((obj.name, key)), val)
        ibounds[key] = (nvar, nvar+width)
        nvar += width

    obounds = {}
    nvar = 0
    for key in output_keys:
        val = obj.get(key)
        width = flattened_size('.'.join((obj.name, key)), val)
        obounds[key] = (nvar, nvar+width)
        nvar += width

    for okey in result:
        for ikey in arg:
            if ikey not in result:
                i1, i2 = ibounds[ikey]
                o1, o2 = obounds[okey]
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

def applyJT(obj, arg, result):
    """Multiply an input vector by the transposed Jacobian. For an Explicit
    Component, this automatically forms the "fake" residual, and calls into
    the function hook "apply_derivT".
    """
    for key in arg:
        result[key] = -arg[key]

    if hasattr(obj, 'apply_derivT'):
        obj.apply_derivT(arg, result)
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
        val = obj.get(key)
        width = flattened_size('.'.join((obj.name, key)), val)
        obounds[key] = (nvar, nvar+width)
        nvar += width

    for okey in result:
        for ikey in arg:
            if okey not in arg:
                i1, i2 = ibounds[ikey]
                o1, o2 = obounds[okey]
                if i2 - i1 == 1:
                    if o2 - o1 == 1:
                        Jsub = float(J[i1, o1])
                        result[okey] += Jsub*arg[ikey]
                    else:
                        Jsub = J[i1:i2, o1:o2]
                        tmp = Jsub*arg[ikey]
                        result[okey] += tmp.reshape(result[okey].shape)
                else:
                    tmp = flattened_value('.'.join((obj.name, ikey)),
                                          arg[ikey]).reshape(1, -1)
                    Jsub = J[i1:i2, o1:o2]
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
        for src in self.inputs:
            val = self.scope.get(src)
            width = flattened_size(src, val)
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

                # Reset OpenMDAO's valid state.
                comp_name, dot, var_name = src.partition('.')
                #var_name = var_name.split('[')[0]
                comp = self.scope.get(comp_name)
                comp._valid_dict[var_name] = False
                    
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
       
        for src in self.inputs:
            src_val = self.scope.get(src)
            src_val = flattened_value(src, src_val).reshape(-1, 1)
            i1, i2 = self.in_bounds[src]
            x[i1:i2] = src_val

    def get_outputs(self, x):
        """Return matrix of flattened values from output edges."""
       
        for src in self.outputs:
            src_val = self.scope.get(src)
            src_val = flattened_value(src, src_val).reshape(-1, 1)
            i1, i2 = self.out_bounds[src]
            x[i1:i2] = src_val
            
    def set_value(self, src, val, index=None):
        """Set a value in the model"""
        
        i1, i2 = self.in_bounds[src]
        old_val = self.scope.get(src)
        comp_name, dot, var_name = src.partition('.')
        
        if index==None:
            if '[' in src:
                src, _, idx = src.partition('[')
                idx = int(idx[:-1])
                old_val = self.scope.get(src)
                old_val[idx] += val
                self.scope.set(src, old_val, force=True)
            else:
                self.scope.set(src, old_val+val, force=True)
        else:
            old_val[index] += val
            self.scope.set(src, old_val, force=True)
            
        # Prevent OpenMDAO from stomping on our poked input.
        comp = self.scope.get(comp_name)
        comp._valid_dict[var_name] = True
        
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


                      
#-------------------------------------------
# Everything below here will be deprecated.
#-------------------------------------------


def _check_var(comp, var_name, iotype):
    """ Checks a variable to make sure it's the proper type and iotype."""
    
    if iotype == 'input':
        conns = comp.list_inputs()
    else:
        conns = comp.list_outputs()
        
    if var_name not in conns:
        msg = 'Variable %s ' % var_name + \
              'should be an %s. ' % iotype +\
              'Derivatives need to be declared for outputs with respect' + \
              ' to inputs.'
        raise RuntimeError(msg)
    
    value = comp.get(var_name)
    if not isinstance(value, float):
        msg = 'At present, derivatives can only be declared for float-' + \
              'valued variables. Variable %s ' % var_name + \
              'is of type %s.' % type(var_name)
        raise RuntimeError(msg)

    
def derivative_name(input_name, output_name):
    """ Assemble the name string for a derivative output based on its input
    and output name. This name string is used in several places and is 
    considered part of the API."""
    
    # Sometimes a parameter is connected to multiple inputs.
    if isinstance(input_name, tuple):
        input_name = input_name[0]
    
    return "d__%s__%s" % (output_name.replace('.', '_'),
                          input_name.replace('.', '_'))


class Derivatives(object):
    """Class for storing derivatives between the inputs and outputs of a
    component at specified orders.
    """
    
    def __init__(self, parent):
        
        self.parent = parent
        
        # Multi-dimensional dicts contain first and second derivative for all
        # requested input-output pairs
        self.first_derivatives = {}
        self.second_derivatives = {}
        
        # Baseline variables are saved in a dict.
        self.inputs = {}
        self.outputs = {}
        
        # Keep track of these in a list, so we know which vars to save.
        self.in_names = []
        self.out_names = []


    def declare_first_derivative(self, out_name, in_name):
        """ Declares that a component can calculate a first derivative
        between the given input and output.
        
        out_name: str
            Name of component's output variable.
            
        in_name: str
            Name of component's first input variable for derivative.
        """
        
        _check_var(self.parent, in_name, "input")
        _check_var(self.parent, out_name, "output")
        
        if out_name not in self.first_derivatives:
            self.first_derivatives[out_name] = {}
            
        self.first_derivatives[out_name][in_name] = 0.0
        
        if in_name not in self.in_names:
            self.in_names.append(in_name)
            
        if out_name not in self.out_names:
            self.out_names.append(out_name)

            
    def set_first_derivative(self, out_name, in_name, value):
        """
        Stores a single first derivative value.
        
        out_name: str
            Name of component's output variable.
            
        in_name: str
            Name of component's input variable.
            
        value: float
            Value of derivative.
        """
        
        try:
            if in_name not in self.first_derivatives[out_name]:
                raise KeyError()
            self.first_derivatives[out_name][in_name] = value
        except KeyError:
            msg = "Derivative of %s " % out_name + \
                  "with repect to %s " % in_name + \
                  "must be declared before being set."
            raise KeyError(msg)
        

    def declare_second_derivative(self, out_name, in_name1, in_name2):
        """ Declares that a component can calculate a second derivative
        between the given input and output.
        
        out_name: str
            Name of component's output variable.
            
        in_name1: str
            Name of component's first input variable for derivative.
            
        in_name2: str
            Name of component's second input variable for derivative.
        """
        
        _check_var(self.parent, in_name1, "input")
        _check_var(self.parent, in_name2, "input")
        _check_var(self.parent, out_name, "output")
        
        if out_name not in self.second_derivatives:
            self.second_derivatives[out_name] = {}
            
        if in_name1 not in self.second_derivatives[out_name]:
            self.second_derivatives[out_name][in_name1] = {}
        
        self.second_derivatives[out_name][in_name1][in_name2] = 0.0
        
        # For cross terms, we also have a symmetric derivative
        if in_name1 != in_name2:
            
            if in_name2 not in self.second_derivatives[out_name]:
                self.second_derivatives[out_name][in_name2] = {}
                
            self.second_derivatives[out_name][in_name2][in_name1] = 0.0
        
        if in_name1 not in self.in_names:
            self.in_names.append(in_name1)
            
        if in_name2 not in self.in_names:
            self.in_names.append(in_name2)
            
        if out_name not in self.out_names:
            self.out_names.append(out_name)

            
    def set_second_derivative(self, out_name, in_name1, in_name2, value):
        """
        Stores a single second derivative value.
        
        Note cross terms (i.e., df_dxdy) are assumed symmetric so you only
        have to specify them once.
        
        out_name: str
            Name of component's output variable.
            
        in_name1: str
            Name of component's first input variable for derivative.
            
        in_name2: str
            Name of component's second input variable for derivative.
            
        value: float
            Value of derivative.
        """
        
        try:
            if in_name2 not in self.second_derivatives[out_name][in_name1]:
                raise KeyError()
            self.second_derivatives[out_name][in_name1][in_name2] = value
        except KeyError:
            msg = "Derivative of %s " % out_name + \
                  "with repect to %s " % in_name1 + \
                  "and %s " % in_name2 + \
                  "must be declared before being set."
            raise KeyError(msg)
        
        # For cross terms, populate the symmetric derivative
        if in_name1 != in_name2:
            self.second_derivatives[out_name][in_name2][in_name1] = value


    def save_baseline(self, comp):
        """Saves the baseline of all inputs and outputs for which derivatives
        have been specified.
        """
        
        for name in self.in_names:
            self.inputs[name] = self.parent.get(name)

        for name in self.out_names:
            self.outputs[name] = self.parent.get(name)


    def calculate_output(self, out_name, order):
        """Returns the Fake Finite Difference output for the given output
        name using the stored baseline and derivatives along with the
        new inputs in the component.
        """
        
        y = self.outputs[out_name]
            
        # First order derivatives
        if order == 1:
            
            for in_name, dx in self.first_derivatives[out_name].iteritems():
                y += dx*(self.parent.get(in_name) - self.inputs[in_name])
        
        # Second order derivatives
        elif order == 2:
            
            for in_name1, item in self.second_derivatives[out_name].iteritems():
                for in_name2, dx in item.iteritems():
                    y += 0.5*dx* \
                      (self.parent.get(in_name1) - self.inputs[in_name1])* \
                      (self.parent.get(in_name2) - self.inputs[in_name2])
        
        else:
            msg = 'Fake Finite Difference does not currently support an ' + \
                  'order of %s.' % order
            raise NotImplementedError(msg)
        
        return y

    
    def validate(self, order, driver_inputs, driver_outputs):
        """Check the component's inputs and output and warn about any input-
        output combinations that are missing a derivative."""
        
        input_list = driver_inputs
        output_list = driver_outputs
        
        # only check float-valued outputs that are connected in the framework,
        # and have not been excluded from checking using 'no_deriv_check'
        for outvar in self.parent.list_outputs(connected=True):
            if isinstance(self.parent.get(outvar), float) and \
               'no_deriv_check' not in self.parent.get_metadata(outvar):
                output_list.append(outvar)
            
        # only check float-valued inputs that are connected in the framework,
        # and have not been excluded from checking using 'no_deriv_check'
        for invar in self.parent.list_inputs(connected=True):
            if isinstance(self.parent.get(invar), float) and \
               'no_deriv_check' not in self.parent.get_metadata(invar):
                input_list.append(invar)
                
        if order == 1 and self.first_derivatives:
            for outvar in output_list:
                
                if outvar not in self.first_derivatives:
                    no_outvar = True
                else:
                    no_outvar = False
                    
                for invar in input_list:
                    if no_outvar or \
                       invar not in self.first_derivatives[outvar]:
                        
                        msg = 'Warning: no first derivative defined for ' \
                               'output %s and ' % outvar + \
                               'input %s ' % invar + \
                               'in %s.' % self.parent.name
                        self.parent._logger.warning(msg)
                        
                        # TODO - This should be removed when logging is
                        # finalized.
                        print msg
                        
        elif order == 2 and self.second_derivatives:
            for outvar in output_list:
                
                if outvar not in self.second_derivatives:
                    no_outvar = True
                else:
                    no_outvar = False
                    
                input_list2 = []
                for invar in input_list:
                    input_list2.append(invar)
                    for invar2 in input_list2:
                        if no_outvar or \
                           invar not in self.second_derivatives[outvar][invar2]:
                            
                            msg = 'Warning: no second derivative defined ' \
                                   'for output %s and ' % outvar + \
                                   'input1 %s and' % invar + \
                                   'input2 %s ' % invar2 + \
                                   'in %s.' % self.parent.name
                            self.parent._logger.warning(msg)
                            
                            # TODO - This should be removed when logging is
                            # finalized.
                            print msg
            
        
