""" A component with derivatives. Derived from Component. This is presently
DEPRECATED."""

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
        
        