""" Class definition for Derivatives.
This object is used by Component to store derivative information and to
perform calculations during a Fake Finite Difference.
"""


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

    
class Derivatives(object):
    """Class for storing derivatives between the inputs and outputs of a
    component at specified orders.
    """
    
    def __init__(self):
        
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


    def declare_first_derivative(self, comp, out_name, in_name):
        """ Declares that a component can calculate a first derivative
        between the given input and output.
        
        comp: Component
            Component that contains the variables out_name and in_name.
            
        out_name: str
            Name of component's output variable.
            
        in_name: str
            Name of component's first input variable for derivative.
        """
        
        _check_var(comp, in_name, "input")
        _check_var(comp, out_name, "output")
        
        if out_name not in self.first_derivatives.keys():
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
        

    def declare_second_derivative(self, comp, out_name, in_name1, in_name2):
        """ Declares that a component can calculate a second derivative
        between the given input and output.
        
        comp: Component
            Component that contains the variables out_name and in_name(1,2)
            
        out_name: str
            Name of component's output variable.
            
        in_name1: str
            Name of component's first input variable for derivative.
            
        in_name2: str
            Name of component's second input variable for derivative.
        """
        
        _check_var(comp, in_name1, "input")
        _check_var(comp, in_name2, "input")
        _check_var(comp, out_name, "output")
        
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
            self.inputs[name] = comp.get(name)

        for name in self.out_names:
            self.outputs[name] = comp.get(name)


    def calculate_output(self, comp, out_name, order):
        """Returns the Fake Finite Difference output for the given output
        name using the stored baseline and derivatives along with the
        new inputs in comp.
        """
        
        y = self.outputs[out_name]
            
        # First order derivatives
        if order == 1:
            
            for in_name, dx in self.first_derivatives[out_name].iteritems():
                y += dx*(comp.get(in_name) - self.inputs[in_name])
        
        # Second order derivatives
        elif order == 2:
            
            for in_name1, item in self.second_derivatives[out_name].iteritems():
                for in_name2, dx in item.iteritems():
                    y += 0.5*dx* \
                      (comp.get(in_name1) - self.inputs[in_name1])* \
                      (comp.get(in_name2) - self.inputs[in_name2])
        
        else:
            msg = 'Fake Finite Difference does not currently support an ' + \
                  'order of %s.' % order
            raise NotImplementedError(msg)
        
        return y

    
    def validate(self, comp, order, driver_inputs, driver_outputs):
        """Check the component's inputs and output and warn about any input-
        output combinations that are missing a derivative."""
        
        input_list = driver_inputs
        output_list = driver_outputs
        
        # only check float-valued outputs that are connected in the framework,
        # and have not been excluded from checking using 'no_deriv_check'
        for outvar in comp.list_outputs(connected=True):
            if isinstance(comp.get(outvar), float) and \
               'no_deriv_check' not in comp.get_metadata(outvar):
                output_list.append(outvar)
            
        # only check float-valued inputs that are connected in the framework,
        # and have not been excluded from checking using 'no_deriv_check'
        for invar in comp.list_inputs(connected=True):
            if isinstance(comp.get(invar), float) and \
               'no_deriv_check' not in comp.get_metadata(invar):
                input_list.append(invar)
                
        if order==1 and self.first_derivatives:
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
                               'in %s.' % comp.name
                        comp._logger.warning(msg)
                        
                        # TODO - This should be removed when logging is
                        # finalized.
                        print msg
                        
        elif order==2 and self.second_derivatives:
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
                                   'in %s.' % comp.name
                            comp._logger.warning(msg)
                            
                            # TODO - This should be removed when logging is
                            # finalized.
                            print msg
            
        
