"""
This is a simple iteration driver that basically runs a workflow, passing the output
to the input for the next iteration. Relative change and number of iterations
are used as termination criteria.
"""

import logging
# pylint: disable-msg=E0611,F0401
try:
    from numpy import zeros
    from numpy.linalg import norm
except ImportError as err:
    logging.warn("In %s: %r", __file__, err)

from openmdao.main.datatypes.api import Float, Int, Bool, Enum
from openmdao.main.api import Driver
from openmdao.util.decorators import add_delegate, stub_if_missing_deps
from openmdao.main.hasstopcond import HasStopConditions
from openmdao.main.exceptions import RunStopped
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.main.interfaces import IHasParameters, IHasEqConstraints, \
                                     ISolver, implements

@stub_if_missing_deps('numpy')
@add_delegate(HasParameters, HasEqConstraints)
class FixedPointIterator(Driver):
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
        super(FixedPointIterator, self).__init__()

        self.history = zeros(0)
        self.current_iteration = 0

    def execute(self):
        """Perform the iteration."""

        nvar = self.total_parameters()
        history = zeros([self.max_iteration, nvar])
        delta = zeros(nvar)

        # Get and save the intial value of the input parameters
        val0 = self.eval_parameters(self.parent)

        # perform an initial run
        self.run_iteration()
        self.current_iteration = 0

        history[0, :] = self.eval_eq_constraints(self.parent)

        if self.norm_order == 'Infinity':
            order = float('inf')
        else:
            order = 2

        unconverged = True
        while unconverged:

            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            # check max iteration
            if self.current_iteration >= self.max_iteration-1:
                self.history = history[:self.current_iteration+1, :]

                self._logger.warning('Max iterations exceeded without '
                                     'convergence.')
                return

            # Pass output to input
            val0 += history[self.current_iteration, :]
            self.set_parameters(val0)

            # run the workflow
            self.run_iteration()

            self.record_case()

            self.current_iteration += 1

            # check convergence
            delta[:] = self.eval_eq_constraints(self.parent)
            history[self.current_iteration] = delta

            if norm(delta, order) < self.tolerance:
                break
            # relative tolerance -- problematic around 0
            #if abs( (val1-val0)/val0 ) < self.tolerance:
            #    break
        self.history = history[:self.current_iteration+1, :]

    def check_config(self):
        """Make sure the problem is set up right."""

        ncon = self.total_eq_constraints()

        if ncon == 0:
            msg = "FixedPointIterator requires a constraint equation."
            self.raise_exception(msg, RuntimeError)

        nparm = self.total_parameters()

        if nparm == 0:
            msg = "FixedPointIterator requires an input parameter."
            self.raise_exception(msg, RuntimeError)

        if ncon != nparm:
            msg = "The number of input parameters must equal the number of" \
                  " output constraint equations in FixedPointIterator."
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

        self.record_case()

        if self.iteration < 1 and self.run_at_least_once:
            self.iteration += 1
            return True

        if self.should_stop():
            return False

        if self.iteration < self.max_iterations:
            self.iteration += 1
            return True

        return False


# End iterate.py
