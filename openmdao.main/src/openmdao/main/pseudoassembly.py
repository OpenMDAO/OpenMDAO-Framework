'''The PseudoAssembly is used to aggregate blocks of components that cannot
provide derivatives, and thus must be finite differenced.'''

class PseudoAssembly(object):
    """The PseudoAssembly is used to aggregate blocks of components that cannot
    provide derivatives, and thus must be finite differenced. It is not a real
    assembly, and should never be used in an OpenMDAO model."""
    
    def __init__(self, comps, wflow):
        """Initialized with list of components, and the parent workflow."""
        
        self.comps = comps
        self.wflow = wflow
        self.itername = ''
        
    def set_itername(self, name):
        """Comp API compatibility; allows iteration coord to be set in 
        components."""
        self.itername = name
    
    def run(self, ffd_order=0, case_id=''):
        """Run all components contained in this assy. Used by finite difference."""
        
        for comp in self.comps:
            comp.set_itername(self.itername+'-fd')
            comp.run(ffd_order=ffd_order, case_id=case_id)