""" A component with derivatives. Derived from Component """

from openmdao.main.component import Component
from openmdao.main.derivatives import Derivatives

class ComponentWithDerivatives (Component):
    """This is the base class for all objects containing Traits that are \
    accessible to the OpenMDAO framework and are "runnable."
    """
    
    def __init__(self, doc=None, directory=''):
        """ Only one thing to do: create Derivatives object."""
        
        self.derivatives = Derivatives()
        super(ComponentWithDerivatives, self).__init__(doc)
        
    def check_derivatives(self, order, driver_inputs, driver_outputs):
        """Calls the validate method of the derivatives object, in order to
        warn the user about all missing derivatives."""
        
        local_inputs = []
        for item in driver_inputs:
            paths = item.split('.',1)
            if paths[0] == self.name:
                local_inputs.append(paths[1])
        
        local_outputs = []
        for item in driver_outputs:
            paths = item.split('.',1)
            if paths[0] == self.name:
                local_outputs.append(paths[1])
        
        self.derivatives.validate(self, order, local_inputs, local_outputs)