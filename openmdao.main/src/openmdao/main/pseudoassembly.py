'''The PseudoAssembly is used to aggregate blocks of components that cannot
provide derivatives, and thus must be finite differenced.'''

from openmdao.main.derivatives import FiniteDifference

class PseudoAssembly(object):
    """The PseudoAssembly is used to aggregate blocks of components that cannot
    provide derivatives, and thus must be finite differenced. It is not a real
    assembly, and should never be used in an OpenMDAO model."""
    
    def __init__(self, name, comps, inputs, outputs, wflow):
        """Initialized with list of components, and the parent workflow."""
        
        self.name = name
        self.comps = comps
        self.wflow = wflow
        self.inputs = list(inputs)
        self.outputs = list(outputs)
        self.itername = ''
        
        self.fd = FiniteDifference(self)
        self.J = None
        
    def set_itername(self, name):
        """Comp API compatibility; allows iteration coord to be set in 
        components."""
        self.itername = name
    
    def run(self, ffd_order=0, case_id=''):
        """Run all components contained in this assy. Used by finite 
        difference."""
        
        for comp in self.comps:
            comp.set_itername(self.itername+'-fd')
            comp.run(ffd_order=ffd_order, case_id=case_id)
            
    def calc_derivatives(self, first=False, second=False, savebase=False):
        """Calculate the derivatives for this non-differentiable block using
        Finite Difference."""
        
        # First, linearize about operating point.
        # Note: Only needed for differentiable islands, which are handled
        # with Fake Finite Difference.
        if first:
            for comp in self.comps:
                if hasattr(comp, 'apply_deriv') or hasattr(comp, 'provideJ'):
                    comp.calc_derivatives(first, second, savebase)
                
        self.J = self.fd.calculate()
            
    def provideJ(self):
        """Jacobian for this block"""
        return self.inputs, self.outputs, self.J
    
    def get(self, varname):
        """ Return the value of a variable in the Pseudoassembly"""
        
        return self.wflow.scope.get(varname)
            