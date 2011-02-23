""" Decorator for drivers that need to take derivatives (either gradients or
    Hessians) of their workflow.
"""

class UsesGradients(object): 
    """This class provides an implementation of the IUsesGradients interface."""

    def __init__(self, parent):
        self._parent = parent

    def check_gradients(self):
        """Run check_derivatives on our workflow."""
        
        driver_inputs = []
        driver_outputs = []
        
        # Parameters
        for name in self._parent.get_parameters().keys():
            driver_inputs.append(name)

        # Objective expressions can contain inputs. These do not need to
        # be checked, because:
        # -- if they are physically connected to another comp, they will be
        #    checked regardless
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
                            
        wf = self._parent.workflow
        wf.check_derivatives(1, driver_inputs, driver_outputs)     
        
        