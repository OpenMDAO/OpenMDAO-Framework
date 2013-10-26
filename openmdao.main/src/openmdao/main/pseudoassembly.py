'''The PseudoAssembly is used to aggregate blocks of components that cannot
provide derivatives, and thus must be finite differenced.'''

from openmdao.main.derivatives import FiniteDifference

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

        self.fd = None
        self.J = None

        # By default, use fake finite-difference on components if they have
        # derivatives.
        self.ffd_order = 1
        
        #print [comp.name for comp in comps], inputs, outputs

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

    def calc_derivatives(self, first=False, second=False, savebase=True,
                         required_inputs=None, required_outputs=None):
        """Calculate the derivatives for this non-differentiable block using
        Finite Difference."""
        # We don't do this in __init__ because some inputs and outputs
        # are added after creation (for nested driver support).
        if self.fd is None:
            self.fd = FiniteDifference(self)

        self.wflow.sever_edges(self.wflow._severed_edges)

        try:
            # First, linearize about operating point.
            # Note: Only needed for differentiable islands, which are handled
            # with Fake Finite Difference.
            # Don't do this for full-model finite difference.
            if first and self.ffd_order>0:
                for comp in self.comps:
                    comp.calc_derivatives(first, second, True)

            self.J = self.fd.calculate()
        finally:
            self.wflow.unsever_edges()
        
    def provideJ(self):
        """Jacobian for this block"""
        return self.inputs, self.outputs, self.J

    # TODO: Maybe this is never used.
    def get(self, varname):
        """ Return the value of a variable in the Pseudoassembly"""

        return self.wflow.scope.get(varname)

