""" Class definition for Derivatives.
This object is used by Component to store derivative information and to
perform calculations during a Fake Finite Difference.
"""


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
        self.input_names = []
        self.output_names = []


    def declare_first_derivative(self, output_name, input_name):
        """ Declares that a component can calculate a first derivative
        between the given input and output.
        """
        
        if output_name not in self.first_derivatives.keys():
            self.first_derivatives[output_name] = {}
            
        self.first_derivatives[output_name][input_name] = 0.0
        
        if input_name not in self.input_names:
            self.input_names.append(input_name)
            
        if output_name not in self.output_names:
            self.output_names.append(output_name)

            
    def set_first_derivative(self, output_name, input_name, value):
        """
        Stores a single first derivative value.
        
        output_name: string
            name of component's output variable
            
        input_name: string
            name of component's input variable
            
        value: float
            value of derivative 
        """
        
        if input_name not in self.first_derivatives[output_name]:
            msg = "Derivative of %s " % output_name + \
                  "with repect to %s " % input_name + \
                  "must be declared before being set."
            raise KeyError(msg)
            
        try:
            self.first_derivatives[output_name][input_name] = value
        except KeyError:
            msg = "Derivative of %s " % output_name + \
                  "with repect to %s " % input_name + \
                  "must be declared before being set."
            raise KeyError(msg)


    def declare_second_derivative(self, output_name, input_name1, input_name2):
        """ Declares that a component can calculate a second derivative
        between the given input and output.
        """
        
        if output_name not in self.second_derivatives:
            self.second_derivatives[output_name] = {}
            
        if input_name1 not in self.second_derivatives[output_name]:
            self.second_derivatives[output_name][input_name1] = {}
        
        self.second_derivatives[output_name][input_name1][input_name2] = 0.0
        
        if input_name1 not in self.input_names:
            self.input_names.append(input_name)
            
        if input_name2 not in self.input_names:
            self.input_names.append(input_name)
            
        if output_name not in self.output_names:
            self.output_names.append(output_name)

            
    def set_second_derivative(self, output_name, input_name1, input_name2, value):
        """
        Stores a single second derivative value.
        
        Note cross terms (i.e., df_dxdy) are assumed symmetric, so you onnly
        have to specify them once.
        
        output_name: string
            name of component's output variable
            
        input_name: string
            name of component's first input variable for derivative
            
        input_name: string
            name of component's second input variable for derivative
            
        value: float
            value of derivative
        """
        
        if input_name2 not in self.second_derivatives[output_name][input_name1]:
            msg = "Derivative of %s " % output_name + \
                  "with repect to %s " % input_name1 + \
                  "and %s " % input_name2 + \
                  "must be declared before being set."
            raise KeyError(msg)
            
        try:
            self.second_derivatives[output_name][input_name1][input_name2] = value
        except KeyError:
            msg = "Derivative of %s " % output_name + \
                  "with repect to %s " % input_name1 + \
                  "and %s " % input_name2 + \
                  "must be declared before being set."
            raise KeyError(msg)
        
        # For cross terms, populate the symmetric derivative
        if input_name1 != input_name2:
            
            if input_name2 not in self.second_derivatives[output_name]:
                self.second_derivatives[output_name][input_name2] = {}
        
            self.second_derivatives[output_name][input_name2][input_name1] = value


    def save_baseline(self, comp):
        """Saves the baseline of all inputs and outputs for which derivatives
        have been specified.
        """
        
        for name in self.input_names:
            self.inputs[name] = comp.get(name)

        for name in self.output_names:
            self.outputs[name] = comp.get(name)


    def calculate_output(self, comp, output_name, order):
        """Returns the Fake Finite Difference output for the given output
        name using the stored baseline and derivatives along with the
        new inputs in comp.
        """
        
        y = self.outputs[output_name]
            
        # First order derivatives
        if order == 1:
            
            for input_name, dx in self.first_derivatives[output_name].iteritems():
                y += dx*(comp.get(input_name) - self.inputs[input_name])
        
        # Second order derivatives
        elif order == 2:
            
            for input_name1, item in self.second_derivatives[output_name].iteritems():
                for input_name2, dx in item.iteritems():
                    y += 0.5*dx* \
                      (comp.get(input_name1) - self.inputs[input_name1])* \
                      (comp.get(input_name2) - self.inputs[input_name2])
        
        else:
            msg = 'Fake Finite Difference does not currently support an ' + \
                  'order of %n.' % order
            raise NotImplementedError(msg)
        
        return y

    
    def validate(self, comp, driver_inputs, driver_outputs):
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
                
        if self.first_derivatives:
            for outvar in output_list:
                
                if outvar not in self.first_derivatives:
                    no_outvar = True
                else:
                    no_outvar = False
                    
                for invar in invar_list:
                    if no_outvar or \
                       invar not in self.first_derivatives[outvar]:
                        
                        msg = 'Warning: no first derivative defined for ' \
                               'output %s and ' % outvar + \
                               'input %s.' % invar
                        comp._logger.warning(msg)
                        
                        # TODO - This should be removed when logging is
                        # finalized.
                        print msg
                        
        if self.second_derivatives:
            for outvar in output_list:
                
                if outvar not in self.second_derivatives:
                    no_outvar = True
                else:
                    no_outvar = False
                    
                invar_list2 = []
                for invar in invar_list:
                    invar_list2.append(invar)
                    for invar2 in invar_list2:
                        if no_outvar or \
                           invar not in self.second_derivatives[outvar][invar2]:
                            
                            msg = 'Warning: no second derivative defined ' \
                                   'for output %s and ' % outvar + \
                                   'input1 %s and' % invar + \
                                   'input2 %s.' % invar2
                            comp._logger.warning(msg)
                            
                            # TODO - This should be removed when logging is
                            # finalized.
                            print msg
            
        