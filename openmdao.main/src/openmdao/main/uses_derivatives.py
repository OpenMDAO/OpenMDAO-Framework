""" Decorator for drivers that need to take derivatives (either gradients or
    Hessians) of their workflow. The following delegates are available:
    
    UsesGradients - if you need first derivatives
    UsesHessians  - if you need second derivatives
    
    From a driver's perspective, derivatives are needed from its parameters
    to it's objective(s) and constraints.
"""


class UsesDerivatives_Base(object): 
    """This class provides an implementation of the derivatives delegates."""

    def __init__(self, parent):
        self._parent = parent

    def _list_driver_connections(self):
        """Return a list of inputs and a list of outputs that are referenced by
        any existing driver Expreval."""
        
        driver_inputs = []
        driver_outputs = []
        
        # Parameters are all inputs.
        for name in self._parent.get_parameters().keys():
            driver_inputs.append(name)

        # Objective expressions can contain inputs. These do not need to
        # be checked, because:
        # -- if they are physically connected to another comp, they will be
        #    checked regardless (through fake finite difference or alternative
        #    means)
        # -- if they are not connected, their derivative is always 0
        if hasattr(self._parent, '_hasobjective'):
            obj = getattr(self._parent, '_hasobjective')
            
            if obj._objective:
                for varpath in obj._objective.get_referenced_varpaths():
                    metadata = self._parent.parent.get_metadata(varpath)
                    if metadata['iotype'] == 'out':
                        driver_outputs.append(varpath)
                
        if hasattr(self._parent, '_hasobjectives'):
            obj = getattr(self._parent, '_hasobjectives')
            
            for item in obj._objectives.values():
                for varpath in item.get_referenced_varpaths():
                    metadata = self._parent.parent.get_metadata(varpath)
                    if metadata['iotype'] == 'out':
                        driver_outputs.append(varpath)
                
        # Constraints can also introduce additional connections.
        for delegate in ['_hasineqconstraints', '_haseqconstraints', 
                         '_hasconstraints']:
            
            if hasattr(self._parent, delegate):
                constraints = getattr(self._parent, delegate)
                for item in constraints._constraints.values():
                    
                    for varpath in item.lhs.get_referenced_varpaths():
                        metadata = self._parent.parent.get_metadata(varpath)
                        if metadata['iotype'] == 'out' and \
                           varpath not in driver_outputs:
                            driver_outputs.append(varpath)

                    for varpath in item.rhs.get_referenced_varpaths():
                        metadata = self._parent.parent.get_metadata(varpath)
                        if metadata['iotype'] == 'out' and \
                           varpath not in driver_outputs:
                            driver_outputs.append(varpath)
                            
        return driver_inputs, driver_outputs
    
        
class UsesGradients(UsesDerivatives_Base): 
    """This class provides an implementation of the UsesGradients interface."""

    def check_gradients(self):
        """Run check_derivatives on our workflow."""
        
        driver_inputs, driver_outputs = self._list_driver_connections()
        
        wf = self._parent.workflow
        wf.check_derivatives(1, driver_inputs, driver_outputs)     
        
        
class UsesHessians(UsesDerivatives_Base): 
    """This class provides an implementation of the UsesHessians interface."""

    def check_hessians(self):
        """Run check_derivatives on our workflow."""
        
        driver_inputs, driver_outputs = self._list_driver_connections()
        
        wf = self._parent.workflow
        wf.check_derivatives(2, driver_inputs, driver_outputs)  
        
        