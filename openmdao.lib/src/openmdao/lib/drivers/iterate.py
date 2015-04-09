"""
This is a simple iteration driver that basically runs a workflow, passing the
output to the input for the next iteration. Relative change and number of
iterations are used as termination criteria.
"""

# pylint: disable=E0611,F0401

from openmdao.main.mpiwrap import MPI, get_norm
if not MPI:
    from numpy.linalg import norm

from openmdao.main.datatypes.api import Float, Int, Bool, Enum
from openmdao.util.decorators import add_delegate
from openmdao.main.driver import Driver
from openmdao.main.hasstopcond import HasStopConditions
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.main.interfaces import IHasParameters, IHasEqConstraints, \
                                     ISolver, implements
from networkx.algorithms.components import strongly_connected_components

@add_delegate(HasParameters, HasEqConstraints, HasStopConditions)
class FixedPointIterator(Driver):
    """ A simple fixed point iteration driver, which runs a workflow and passes
    the value from the output to the input for the next iteration. Relative
    change and number of iterations are used as termination criteria. This type
    of iteration is also known as Gauss-Seidel."""

    implements(IHasParameters, IHasEqConstraints, ISolver)

    # pylint: disable=E1101
    max_iteration = Int(25, iotype='in', desc='Maximum number of '
                                         'iterations before termination.')

    tolerance = Float(1.0e-6, iotype='in', desc='Absolute convergence '
                                            'tolerance between iterations.')

    norm_order = Enum('Infinity', ['Infinity', 'Euclidean'],
                       desc='For multivariable iteration, type of norm '
                                   'to use to test convergence.')

    iprint = Enum(0, [0, 1], iotype='in', desc='set to 1 to print '
                  'residual during convergence.')

    def __init__(self):
        super(FixedPointIterator, self).__init__()
        self.current_iteration = 0
        self.normval = 1.e99
        self.norm0 = 1.e99

    def execute(self):
        """ Executes an iterative solver """

        # print 'Executes an iterative solver', 
        # if MPI:
        #     print 'driver', self
        #     #print 'size',self.workflow._system.mpi.size
        #     print 'dir(comm)',dir(self.workflow._system.mpi.comm)
        #     print 'comm',self.workflow._system.mpi.comm
        #     print 'comm.name',self.workflow._system.mpi.comm.name
        #     print 'comm.group',self.workflow._system.mpi.comm.group
        #     print 'rank',self.workflow._system.mpi.rank
        #     print 'requested_cpus',self.workflow._system.mpi.requested_cpus
        #     #print dir(self.workflow._system.mpi)
        #     print 'MPI not null'

        self.current_iteration = 0
        if MPI:
            self._norm_order = None # norm order not used in MPI
            if self.workflow._system.mpi.comm == MPI.COMM_NULL:
                return
        else:
            if self.norm_order == 'Infinity':
                self._norm_order = float('inf')
            else:
                self._norm_order = self.norm_order

        super(FixedPointIterator, self).execute()

    def start_iteration(self):
        """ Commands run before any iterations """
        self.current_iteration = 0
        self.normval = 1.e99
        self.norm0 = 1.e99
        self.run_iteration()
        self.normval = get_norm(self.workflow._system.vec['f'],
                                self._norm_order)
        self.norm0 = self.normval if self.normval != 0.0 else 1.0

        if self.iprint > 0:
            self.print_norm('NLN_GS', 0, self.normval, self.norm0)

    def pre_iteration(self):
        """Runs an iteration."""
        self.current_iteration += 1
        system = self.workflow._system
        uvec = system.vec['u']
        fvec = system.vec['f']

        cycle_vars = self.workflow._cycle_vars
        for name in uvec.keys():
            if name not in cycle_vars:
                uvec[name] -= fvec[name]

    def continue_iteration(self):
        """Convergence check."""
        return not self.should_stop() and \
               self.current_iteration < self.max_iteration and \
               self.normval > self.tolerance
              # and self.normval/self.norm0 > self.rtol:

    def post_iteration(self):
        """Runs after each iteration"""
        self.normval = get_norm(self.workflow._system.vec['f'],
                                self._norm_order)
        if self.iprint > 0:
            self.print_norm('NLN_GS', self.current_iteration-1, self.normval,
                            self.norm0)

    def end_iteration(self):
        """Print convergence."""
        if self.iprint > 0:
            self.print_norm('NLN_GS', self.current_iteration-1, self.normval,
                            self.norm0, msg='Converged')

    def check_config(self, strict=False):
        """Make sure the problem is set up right."""

        super(FixedPointIterator, self).check_config(strict=strict)

        # We need to figure our severed edges before querying.
        eqcons = self.get_constraints().values()
        n_dep = len(eqcons)
        n_indep = len(self.get_parameters())

        if n_dep != n_indep:
            msg = "The number of input parameters must equal the number of" \
                  " output constraint equations in FixedPointIterator."
            self.raise_exception(msg, RuntimeError)

        # Check to make sure we don't have a null problem.
        if n_dep == 0:
            cgraph = self.parent._depgraph.component_graph().subgraph([c.name for c in self.workflow])
            strong = list(strongly_connected_components(cgraph))
            if not ((strong and len(strong[0]) > 1) or self._get_param_constraint_pairs()):
                msg = "FixedPointIterator requires a cyclic workflow, or a " \
                      "parameter/constraint pair."
                self.raise_exception(msg, RuntimeError)

        # Check the eq constraints to make sure they look ok.
        for eqcon in eqcons:

            if eqcon.rhs.text == '0' or eqcon.lhs.text == '0':
                msg = "Please specify constraints in the form 'A=B'"
                msg += ': %s = %s' % (eqcon.lhs.text, eqcon.rhs.text)
                self.raise_exception(msg, RuntimeError)

            if len(eqcon.get_referenced_varpaths()) > 2:
                msg = "Please specify constraints in the form 'A=B'"
                msg += ': %s = %s' % (eqcon.lhs.text, eqcon.rhs.text)
                self.raise_exception(msg, RuntimeError)


@add_delegate(HasStopConditions)
class IterateUntil(Driver):
    """ A simple driver to run a workflow until some stop condition is met. """

    max_iterations = Int(10, iotype="in", desc="Maximum number of iterations.")
    iteration = Int(0, iotype="out", desc="Current iteration counter.")
    run_at_least_once = Bool(True, iotype="in", desc="If True, driver will"
                             " ignore stop conditions for the first iteration"
                             " and run at least one iteration.")

    def start_iteration(self):
        """ Code executed before the iteration. """
        self.iteration = 0

    def continue_iteration(self):
        """ Convergence check."""
        if self.iteration < 1 and self.run_at_least_once:
            self.iteration += 1
            return True

        if self.should_stop():
            return False

        if self.iteration < self.max_iterations:
            self.iteration += 1
            return True

        return False
