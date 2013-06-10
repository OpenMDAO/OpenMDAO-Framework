""" A component with derivatives. Derived from Component. This is presently
DEPRECATED, although it still contains some check_derivatives code which hasn't
been moved to Component."""

import logging

from openmdao.main.component import Component
from openmdao.main.derivatives import Derivatives

class ComponentWithDerivatives(Component):
    """This is the base class for all objects containing Traits that are \
    accessible to the OpenMDAO framework and are "runnable."
    """
    
    def __init__(self, *args, **kwargs):
        """ Deprecated."""
        
        logging.warning('ComponentwithDerivatives is deprecated. You can ' + \
                        'use Component instead.')
        
        super(ComponentWithDerivatives, self).__init__(*args, **kwargs)
        
        
    def check_derivatives(self, order, driver_inputs, driver_outputs):
        """Calls the validate method of the derivatives object in order to
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
