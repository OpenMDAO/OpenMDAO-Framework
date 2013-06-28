""" A driver with derivatives. Derived from Driver. Inherit from this
    if your driver requires gradients or Hessians.
    
    From a driver's perspective, derivatives are needed from its parameters
    to its objective(s) and constraints.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.datatypes.api import Slot
from openmdao.main.interfaces import IDifferentiator
from openmdao.main.driver import Driver

class DriverUsesDerivatives(Driver): 
    """This class provides an implementation of the derivatives delegates."""

    differentiator = Slot(IDifferentiator, 
                          desc = "Slot for a differentiator")
    
    def _differentiator_changed(self, old, new):
        """When a new differentiator is slotted, give it a handle to the
        parent."""
        
        if self.differentiator is not None:
            self.differentiator._parent = self
    
        
        
        
