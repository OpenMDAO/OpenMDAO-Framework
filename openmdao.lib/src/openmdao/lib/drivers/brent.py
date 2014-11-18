"""
Solver based on scipy.optimize.brentq
"""

from scipy.optimize import brentq

# pylint: disable-msg=E0611,F0401
from openmdao.main.driver import Driver
from openmdao.main.interfaces import IHasParameters, IHasEqConstraints, \
                                     ISolver, implements
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.util.decorators import add_delegate

from openmdao.lib.datatypes.api import Float, Int, Slot


@add_delegate(HasParameters, HasEqConstraints)
class Brent(Driver):
    """Root finding using Brent's method."""

    implements(IHasParameters, IHasEqConstraints, ISolver)

    # pylint: disable-msg=E1101
    lower_bound = Float(0., iotype="in", 
                        desc="lower bound for the root search")
    upper_bound = Float(100., iotype="in", 
                        desc="upper bound for the root search")


    xtol = Float(0.0, iotype="in",
                 desc='The routine converges when a root is known to lie within xtol of the value return. Should be >= 0. '
                 'The routine modifies this to take into account the relative precision of doubles.')

    rtol = Float(0.0, iotype="in",
                 desc='The routine converges when a root is known to lie within rtol times the value returned of '
                 'the value returned. Should be >= 0. Defaults to np.finfo(float).eps * 2.')

    maxiter = Int(100, iotype="in",
                  desc='if convergence is not achieved in maxiter iterations, and error is raised. Must be >= 0.')

    iprint = Int(0, iotype='in',
                 desc='Set to 1 to print out itercount and residual.')

    f_resize_bracket = Slot(object,
                           desc='user supplied function to handle resizing bracket.  Form of function is: \
                           lower_new, upper_new, continue = f_resize_bracket(lower, upper, iteration) \
                           inputs include the current lower and upper bracket and the current iteration \
                           count starting from 1.  Outputs include a new lower and upper bracket, and a \
                           boolean flag on whether or not to terminate calling resize bracket')

    invalid_bracket_return = Float(-1, iotype='in',
                                   desc='user supplied value to handle what value should be returned \
                                         when a suitable bracket cannot be found. sets the "zero" as a \
                                         linear combination of the lower and upper brackets. \
                                         Must be between 0 and 1 or an error will be thrown. \
                                         root = lower + invalid_bracket_return*(upper-lower)')

    def __init__(self):
        super(Brent, self).__init__()
        self.xstar = None

    def _eval(self, x):
        """Callback function for evaluating f(x)"""
        self.set_parameters([x])
        self.run_iteration()
        return self.eval_eq_constraints(self.parent)[0]

    def execute(self):

        bracket_invalid = self._eval(self.lower_bound)*self._eval(self.upper_bound) > 0

        # check if user supplied function to handle resizing bracket
        if bracket_invalid and self.f_resize_bracket:

            # try to resize bracket to find a valid bracket.
            iteration = 1
            should_continue = True
            bracket_invalid = True

            while bracket_invalid and should_continue:
                self.lower_bound, self.upper_bound, should_continue = \
                    self.f_resize_bracket(self.lower_bound, self.upper_bound, iteration)

                bracket_invalid = self._eval(self.lower_bound)*self._eval(self.upper_bound) > 0
                iteration += 1

        if bracket_invalid:  # if bracket is still invalid, see if user has specified what to return

            if self.invalid_bracket_return >= 0.0 and self.invalid_bracket_return <= 1.0:
                xstar = self.lower_bound + self.invalid_bracket_return*(self.upper_bound-self.lower_bound)
                brent_iterations = 'valid bracket not found.  returning user specified value'

            else:
                self.raise_exception('bounds (low=%s, high=%s) do not bracket a root' %
                                 (self.lower_bound, self.upper_bound))

        else:

            kwargs = {'maxiter': self.maxiter, 'a': self.lower_bound,
                      'b': self.upper_bound, 'full_output': True}

            if self.xtol > 0:
                kwargs['xtol'] = self.xtol
            if self.rtol > 0:
                kwargs['rtol'] = self.rtol

            # Brent's method
            xstar, r = brentq(self._eval, **kwargs)
            brent_iterations = r.iterations


        # Propagate solution back into the model
        self.set_parameters([xstar])
        self.run_iteration()
  
        if self.iprint == 1:
            print 'iterations:', brent_iterations
            print 'residual:', self.eval_eq_constraints()

    def check_config(self, strict=False):
        '''Make sure we have 1 parameter and 1 constraint'''

        super(Brent, self).check_config(strict=strict)

        nparam = len(self.get_parameters())
        if nparam != 1:
            self.raise_exception("Brent driver must have 1 parameter, "
                                 "but instead it has %d" % nparam)

        ncnst = len(self.get_eq_constraints())
        if ncnst != 1:
            self.raise_exception("Brent driver must have 1 equality constraint, "
                                 "but instead it has %d" % ncnst)

