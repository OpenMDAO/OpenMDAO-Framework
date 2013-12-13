'''The PseudoAssembly is used to aggregate blocks of components that cannot
provide derivatives, and thus must be finite differenced.'''

from openmdao.main.derivatives import FiniteDifference
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import ISolver

def to_PA_var(name, pa_name):
    ''' Converts an input to a unique input name on a pseudoassembly.'''
    
    return pa_name + '.' + name.replace('.', '|')
        
def from_PA_var(name):
    ''' Converts a pseudoassembly input name back to the real input.'''
    
    if '~' in name:
        name = name.partition('.')[2].replace('|', '.')
        
    return name
        
class PseudoAssembly(object):
    """The PseudoAssembly is used to aggregate blocks of components that cannot
    provide derivatives, and thus must be finite differenced. It is not a real
    assembly, and should never be used in an OpenMDAO model."""

    def __init__(self, name, comps, inputs, outputs, wflow):
        """Initialized with list of components, and the parent workflow."""

        scope = wflow.scope
        
        if '~' not in name:
            name = "~" + name

        self.name = name
        self.comps = comps
        self.wflow = wflow
        self.inputs = list(inputs)
        self.outputs = list(outputs)
        self.mapped_inputs = []
        for varpath in self.inputs:
            if isinstance(varpath, basestring):
                val = to_PA_var(varpath, name).partition('.')[2]
            else:
                val = tuple([to_PA_var(vp, name).partition('.')[2]
                             for vp in varpath])
            self.mapped_inputs.append(val)
        #self.mapped_inputs = [to_PA_var(varpath, name).partition('.')[2]
        #                       for varpath in self.inputs]
        self.mapped_outputs = [to_PA_var(varpath, name).partition('.')[2]
                               for varpath in self.outputs]
        self.itername = ''

        self.fd = None
        self.J = None

        # By default, use fake finite-difference on components if they have
        # derivatives.
        self.ffd_order = 1
        
        #print [comp.name for comp in comps], inputs, outputs
        
        # if a solver in our parent workflow has an iteration set that
        # is completely contained within this PA, then replace all of
        # our components from its iterset with the solver
        solvers = []
        cset = set(comps)
        for cname in wflow.get_names(full=True):
            comp = getattr(scope, cname)
            if has_interface(comp, ISolver):
                iset = [c.name for c in comp.iteration_set()]
                if not cset.difference(iset): # all solver comps are contained in this PA
                    solvers.append((comp.name, iset))
                    
        for solver, iset in solvers:
            cset = cset.difference(iset)
            cset.add(solver)
            
        self.itercomps = list(cset)
                
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

        for name in self.itercomps:
            comp = self.wflow.scope.get(name)
            comp.set_itername(self.itername+'-fd')
            comp.run(ffd_order=ffd_order, case_id=case_id)

    def calc_derivatives(self, first=False, second=False, savebase=True,
                         required_inputs=None, required_outputs=None):
        """Calculate the derivatives for this non-differentiable block using
        Finite Difference."""
        # We don't do this in __init__ because some inputs and outputs
        # are added after creation (for nested driver support).
        if self.fd is None:
            self.fd = FiniteDifference(self)

        if hasattr(self.wflow, '_severed_edges'):
            self.wflow.sever_edges(self.wflow._severed_edges)

        try:
            # First, linearize about operating point.
            # Note: Only needed for differentiable islands, which are handled
            # with Fake Finite Difference.
            # Don't do this for full-model finite difference.
            if first and self.ffd_order>0:
                for name in self.comps:
                    comp = self.wflow.scope.get(name)
                    comp.calc_derivatives(first, second, True)

            self.J = self.fd.calculate()
        finally:
            if hasattr(self.wflow, '_severed_edges'):
                self.wflow.unsever_edges()
        
    def provideJ(self):
        """Jacobian for this block"""
        return self.mapped_inputs, self.mapped_outputs, self.J

    def get(self, varname):
        """ Return the value of a variable in the Pseudoassembly. Used
        when sizing variables in the Jacobian."""

        return self.wflow.scope.get(from_PA_var(self.name+'.'+varname))

