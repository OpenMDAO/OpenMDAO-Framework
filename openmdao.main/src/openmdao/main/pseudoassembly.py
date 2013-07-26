'''The PseudoAssembly is used to aggregate blocks of components that cannot
provide derivatives, and thus must be finite differenced.'''

from openmdao.main.derivatives import FiniteDifference
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface

class PseudoAssembly(object):
    """The PseudoAssembly is used to aggregate blocks of components that cannot
    provide derivatives, and thus must be finite differenced. It is not a real
    assembly, and should never be used in an OpenMDAO model."""
    
    def __init__(self, name, comps, inputs, outputs, wflow):
        """Initialized with list of components, and the parent workflow."""
        
        if '~' not in name:
            name = "~" + name
            
        self.name = name
        self.comps = comps
        self.wflow = wflow
        self.inputs = list(inputs)
        self.outputs = list(outputs)
        self.itername = ''
        self.recursed_comp_names = []
        
        self.fd = None
        self.J = None
        
        # By default, use fake finite-difference on components if they have
        # derivatives.
        self.ffd_order = 1
        
        # Figure out our set of all recursed component names.
        for comp in self.comps:
            if has_interface(comp, IDriver):
                names = comp.workflow.get_names(full=True)
                self.recursed_comp_names.extend(names)
        
    def set_itername(self, name):
        """Comp API compatibility; allows iteration coord to be set in 
        components."""
        self.itername = name
    
    def run(self, ffd_order=0, case_id=''):
        """Run all components contained in this assy. Used by finite 
        difference."""
        
        # Override fake finite difference if requested. This enables a pure
        # finite-differences for check_derivatives.
        if self.ffd_order == 0:
            ffd_order = 0
        
        for comp in self.comps:
            comp.set_itername(self.itername+'-fd')
            comp.run(ffd_order=ffd_order, case_id=case_id)
            
    def calc_derivatives(self, first=False, second=False, savebase=False,
                         extra_in=None, extra_out=None):
        """Calculate the derivatives for this non-differentiable block using
        Finite Difference."""
        
        # We don't do this in __init__ because some inputs and outputs
        # are added after creation (for nested driver support).
        if not self.fd:
            self.fd = FiniteDifference(self)
            
        # First, linearize about operating point.
        # Note: Only needed for differentiable islands, which are handled
        # with Fake Finite Difference.
        if first:
            for comp in self.comps:
                if hasattr(comp, 'apply_deriv') or hasattr(comp, 'provideJ'):
                    comp.calc_derivatives(first, second, savebase=True)
                
        self.J = self.fd.calculate()
            
    def provideJ(self):
        """Jacobian for this block"""
        return self.inputs, self.outputs, self.J
    
    def get(self, varname):
        """ Return the value of a variable in the Pseudoassembly"""
        
        return self.wflow.scope.get(varname)
    
    def list_all_comps(self):
        """list all components, including any sub-driver components that
        interact with the outer driver"""
        return [item.name for item in self.comps] + \
               self.recursed_comp_names
    
    #def 
