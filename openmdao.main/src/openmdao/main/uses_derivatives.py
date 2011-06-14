""" Decorator for drivers that need to take derivatives (either gradients or
    Hessians) of their workflow. The following delegates are available:
    
    UsesGradients - if you need first derivatives
    
    UsesHessians  - if you need second derivatives
    
    From a driver's perspective, derivatives are needed from its parameters
    to its objective(s) and constraints.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.main.slot import Slot
from openmdao.main.interfaces import IDifferentiator


class UsesDerivatives_Base(object): 
    """This class provides an implementation of the derivatives delegates."""

    def __init__(self, parent):
        self._parent = parent
        
        parent.add_trait("differentiator", 
                         Slot(IDifferentiator, iotype='in',
                                desc = "Slot for a differentiator"))
                                                         

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
            if hasattr(self._parent, delegate):
                obj = getattr(self._parent, delegate)
                varset.update(obj.get_referenced_varpaths())
                
        # Constraints can also introduce additional connections.
        for delegate in ['_hasineqconstraints', '_haseqconstraints', '_hasconstraints']:
            if hasattr(self._parent, delegate):
                constraints = getattr(self._parent, delegate)
                varset.update(constraints.get_referenced_varpaths())

        get_metadata = self._parent.parent.get_metadata
        
        # Parameters are all inputs.
        driver_inputs = self._parent.get_parameters().keys()
        
        driver_outputs = [vname for vname in varset 
                          if get_metadata(vname, 'iotype')=='out']
                        
        return driver_inputs, list(driver_outputs)
    
        
class UsesGradients(UsesDerivatives_Base): 
    """This class provides an implementation of the UsesGradients interface."""

    def check_gradients(self):
        """Run check_derivatives on our workflow."""
        
        driver_inputs, driver_outputs = self._list_driver_connections()
        self._parent.workflow.check_derivatives(1, driver_inputs, driver_outputs)     
        
        
class UsesHessians(UsesDerivatives_Base): 
    """This class provides an implementation of the UsesHessians interface."""

    def check_hessians(self):
        """Run check_derivatives on our workflow."""
        
        driver_inputs, driver_outputs = self._list_driver_connections()
        self._parent.workflow.check_derivatives(2, driver_inputs, driver_outputs)  
        
        
