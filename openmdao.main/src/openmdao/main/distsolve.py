"""
This is a simple iteration driver that basically runs a workflow, passing the
output to the input for the next iteration. Relative change and number of
iterations are used as termination criteria.
"""
from numpy.linalg import norm

from openmdao.main.datatypes.api import Float, Int, Bool, Enum
from openmdao.main.api import Driver, CyclicWorkflow
from openmdao.util.decorators import add_delegate, stub_if_missing_deps
from openmdao.main.hasstopcond import HasStopConditions
from openmdao.main.exceptions import RunStopped
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.main.interfaces import IHasParameters, IHasEqConstraints, \
                                     ISolver, implements

class MPIDriver(Driver):
    """ Base Driver class for Drivers iterating over a workflow
    of distributed components using MPI.
    """
    pass

@add_delegate(HasParameters, HasEqConstraints)
class MPIFixedPointIterator(MPIDriver):
    """ A simple fixed point iteration driver, which runs a workflow and passes
    the value from the output to the input for the next iteration. Relative
    change and number of iterations are used as termination criterea. This type
    of iteration is also known as Gauss-Seidel."""

    implements(IHasParameters, IHasEqConstraints, ISolver)

    # pylint: disable-msg=E1101
    max_iteration = Int(25, iotype='in', desc='Maximum number of '
                                         'iterations before termination.')

    tolerance = Float(1.0e-3, iotype='in', desc='Absolute convergence '
                                            'tolerance between iterations.')

    norm_order = Enum('Infinity', ['Infinity', 'Euclidean'],
                       desc='For multivariable iteration, type of norm '
                                   'to use to test convergence.')


    def __init__(self):
        super(MPIFixedPointIterator, self).__init__()

        self.current_iteration = 0

        self.workflow = CyclicWorkflow()

    def execute(self):
        """Perform the iteration."""

        # perform an initial run
        self.pre_iteration()
        self.run_iteration()
        self.post_iteration()
        self.current_iteration = 0

        # Find dimension of our problem.
        self.workflow.initialize_residual()

        # Get and save the intial value of the input parameters
        val0 = self.workflow.get_independents()

        nvar = len(val0)
        delta = zeros(nvar)

        res = self.workflow.get_dependents(fixed_point=True)

        if self.norm_order == 'Infinity':
            order = float('inf')
        else:
            order = self.norm_order

        unconverged = True
        while unconverged:

            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            # check max iteration
            if self.current_iteration >= self.max_iteration-1:

                self._logger.warning('Max iterations exceeded without '
                                     'convergence.')
                self.record_case()
                return

            # Pass output to input
            val0 += res
            self.workflow.set_independents(val0)

            # run the workflow
            self.pre_iteration()
            self.run_iteration()
            self.post_iteration()

            self.record_case()

            self.current_iteration += 1

            # check convergence
            delta[:] = self.workflow.get_dependents(fixed_point=True)
            res = delta

            if norm(delta, order) < self.tolerance:
                break
            # relative tolerance -- problematic around 0
            #if abs( (val1-val0)/val0 ) < self.tolerance:
            #    break

    def check_config(self):
        """Make sure the problem is set up right."""

        # We need to figure our severed edges before querying.
        eqcons = self.get_constraints().values()
        n_dep = len(eqcons)
        n_indep = len(self.get_parameters())

        if n_dep != n_indep:
            msg = "The number of input parameters must equal the number of" \
                  " output constraint equations in FixedPointIterator."
            self.raise_exception(msg, RuntimeError)

        # Check to make sure we don't have a null problem.
        if n_dep==0:
            self.workflow._get_topsort()
            if len(self.workflow._severed_edges) == 0:
                msg = "FixedPointIterator requires a cyclic workflow, or a " + \
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


