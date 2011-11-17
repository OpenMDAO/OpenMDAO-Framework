""" A driver with derivatives. Derived from Driver. Inherit from this
    if your driver requires gradients or Hessians.
    
    From a driver's perspective, derivatives are needed from its parameters
    to its objective(s) and constraints.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.datatypes.slot import Slot
from openmdao.main.interfaces import IDifferentiator
from openmdao.main.driver import Driver

class DriverUsesDerivatives(Driver): 
    """This class provides an implementation of the derivatives delegates."""

    def __init__(self, *args, **kwargs):
        
        super(DriverUsesDerivatives, self).__init__(*args, **kwargs)
        
        self.add("differentiator", 
                   Slot(IDifferentiator, iotype='in',
                        desc = "Slot for a differentiator"))
        
        # These flags tell whether to check for missing derivatives during
        # check_config. Default as stated.
        self.uses_gradients = True
        self.uses_Hessians = False
        
                                                         
    def _differentiator_changed(self, old, new):
        """When a new differentiator is slotted, give it a handle to the
        parent."""
        
        self.differentiator._parent = self
        
    def _list_driver_connections(self):
        """Return a list of inputs and a list of outputs that are referenced by
        any existing driver Expreval."""
        
        # for collecting a unique set of referenced varnames from objectives
        # and constraints
        varset = set()

        # Objective expressions can contain inputs. These do not need to
        # be checked, because:
        # -- if they are physically connected to another comp, they will be
        #    checked regardless (through fake finite difference or alternative
        #    means)
        # -- if they are not connected, their derivative is always 0
        for delegate in ['_hasobjective', '_hasobjectives']:
            if hasattr(self, delegate):
                obj = getattr(self, delegate)
                varset.update(obj.get_referenced_varpaths())
                
        # Constraints can also introduce additional connections.
        for delegate in ['_hasineqconstraints', '_haseqconstraints', '_hasconstraints']:
            if hasattr(self, delegate):
                constraints = getattr(self, delegate)
                varset.update(constraints.get_referenced_varpaths())

        get_metadata = self.parent.get_metadata
        
        # Parameters are all inputs.
        driver_inputs = self.get_parameters().keys()
        
        driver_outputs = [vname for vname in varset 
                          if get_metadata(vname, 'iotype')=='out']
                        
        return driver_inputs, list(driver_outputs)
    
        
    def check_config (self):
        """Verify that our workflow is able to resolve all of its components."""
        
        super(DriverUsesDerivatives, self).check_config()
        
        if self.uses_gradients:
            self.check_gradients()
        if self.uses_Hessians:
            self.check_hessians()
        
    def check_gradients(self):
        """Run check_derivatives on our workflow."""
    
        driver_inputs, driver_outputs = self._list_driver_connections()
        self.workflow.check_derivatives(1, driver_inputs, driver_outputs)     
        
        
    def check_hessians(self):
        """Run check_derivatives on our workflow."""
        
        driver_inputs, driver_outputs = self._list_driver_connections()
        self.workflow.check_derivatives(2, driver_inputs, driver_outputs)  
        
        
