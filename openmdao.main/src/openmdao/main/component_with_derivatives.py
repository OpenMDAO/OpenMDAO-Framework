""" A component with derivatives. Derived from Component """

from openmdao.main.component import Component
from openmdao.main.derivatives import Derivatives

class ComponentWithDerivatives (Component):
    """This is the base class for all objects containing Traits that are \
    accessible to the OpenMDAO framework and are "runnable."
    """
    
    def __init__(self, *args, **kwargs):
        """ Only one thing to do: create Derivatives object."""
        
        self.derivatives = Derivatives(self)
        super(ComponentWithDerivatives, self).__init__(*args, **kwargs)
        
        
    def _execute_ffd(self, ffd_order):
        """During Fake Finite Difference, instead of executing, a component
        can use the available derivatives to calculate the output efficiently.
        Before FFD can execute, calc_derivatives must be called to save the
        baseline state and the derivatives at that baseline point.
        
        This method approximates the output using a Taylor series expansion
        about the saved baseline point.
        
        ffd_order: int
            Order of the derivatives to be used (typically 1 or 2).
        """
        
        for name in self.derivatives.out_names:
            setattr(self, name,
                     self.derivatives.calculate_output(name, ffd_order))

            
    def calc_derivatives(self, first=False, second=False):
        """Prepare for Fake Finite Difference runs by calculating all needed
        derivatives, and saving the current state as the baseline. The user
        must supply calculate_first_derivatives() and/or
        calculate_second_derivatives() in the component.
        
        This function should not be overriden.
        
        first: Bool
            Set to True to calculate first derivatives.
        
        second: Bool
            Set to True to calculate second derivatives.
        """
        
        savebase = False
        
        # Calculate first derivatives in user-defined function
        if first and hasattr(self, 'calculate_first_derivatives'):
            self.calculate_first_derivatives()
            savebase = True
            
        # Calculate second derivatives in user-defined function
        if second and hasattr(self, 'calculate_second_derivatives'):
            self.calculate_second_derivatives()
            savebase = True
            
        # Save baseline state
        if savebase:
            self.derivatives.save_baseline(self)
            
            
    def check_derivatives(self, order, driver_inputs, driver_outputs):
        """Calls the validate method of the derivatives object, in order to
        warn the user about all missing derivatives."""
        
        local_inputs = []
        for item in driver_inputs:
            if isinstance(item, tuple):
                for linked_item in item:
                    paths = linked_item.split('.',1)
                    if paths[0] == self.name:
                        local_inputs.append(paths[1])
            else:
                paths = item.split('.',1)
                if paths[0] == self.name:
                    local_inputs.append(paths[1])
        
        local_outputs = []
        for item in driver_outputs:
            if isinstance(item, tuple):
                for linked_item in item:
                    paths = linked_item.split('.',1)
                    if paths[0] == self.name:
                        local_outputs.append(paths[1])
            else:
                paths = item.split('.',1)
                if paths[0] == self.name:
                    local_outputs.append(paths[1])
        
        self.derivatives.validate(order, local_inputs, local_outputs)