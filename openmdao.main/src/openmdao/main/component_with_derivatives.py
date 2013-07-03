""" A component with derivatives. Derived from Component. This is presently
DEPRECATED."""

from openmdao.main.component import Component
from openmdao.util.log import logger

class ComponentWithDerivatives(Component):
    """This is the base class for all objects containing Traits that are \
    accessible to the OpenMDAO framework and are "runnable."
    """
    
    def __init__(self, *args, **kwargs):
        """ Deprecated."""
        
        logger.warning('ComponentwithDerivatives is deprecated. You can ' + 
                        'use Component instead.')
        
        super(ComponentWithDerivatives, self).__init__(*args, **kwargs)
