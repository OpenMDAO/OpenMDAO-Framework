'''The PseudoAssembly is used to aggregate blocks of components that cannot
provide derivatives, and thus must be finite differenced.'''

from openmdao.main.derivatives import FiniteDifference

class PseudoAssembly(object):
    """The PseudoAssembly is used to aggregate blocks of components that cannot
    provide derivatives, and thus must be finite differenced. It is not a real
    assembly, and should never be used in an OpenMDAO model."""

    def __init__(self, name, comps, inputs, outputs, wflow,
                 recursed_components=None, no_fake_fd=False):
        """Initialized with list of components, and the parent workflow."""

        if '~' not in name:
            name = "~" + name

        self.name = name
        self.comps = comps
        self.wflow = wflow
        self.no_fake_fd = no_fake_fd
        self.inputs = list(inputs)
        self.outputs = list(outputs)
        self.itername = ''

        self.recursed_comp_names = []
        if recursed_components is not None:
            self.recursed_comp_names = recursed_components

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

        print 'running %s' % self.name
        # Override fake finite difference if requested. This enables a pure
        # finite-differences for check_derivatives.
        if self.ffd_order == 0:
            ffd_order = 0

        for comp in self.comps:
            comp.set_itername(self.itername+'-fd')
            print 'pa running %s, ffd_order=%s' % (comp.name, ffd_order)
            comp.run(ffd_order=ffd_order, case_id=case_id)
        print 'pa done'

    def calc_derivatives(self, first=False, second=False, savebase=True,
                         extra_in=None, extra_out=None):
        """Calculate the derivatives for this non-differentiable block using
        Finite Difference."""
        print "pa calc_derivatives"
        # We don't do this in __init__ because some inputs and outputs
        # are added after creation (for nested driver support).
        if self.fd is None:
            self.fd = FiniteDifference(self)

        # The only reason not to turn on fake is if we are in a global
        # finite-difference.
        if self.no_fake_fd:
            savebase = False
        else:
            savebase = True

        # First, linearize about operating point.
        # Note: Only needed for differentiable islands, which are handled
        # with Fake Finite Difference.
        if first:
            for comp in self.comps:
                comp.calc_derivatives(first, second, savebase)

        self.J = self.fd.calculate()
        print "pa calc_derivatives done"
        
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

