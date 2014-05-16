"""
cobyladriver.py - Contains a driver that wraps the cobyla
optimizer as used in pyOpt:

Minimize a function using the Constrained Optimization BY Linear
Approximation (COBYLA) method.

COBYLA is gradient-free and can handle inequality constraints.
"""

from math import isnan

from numpy import zeros, array, hstack

from cobyla.cobyla import cobyla, closeunit

from openmdao.main.datatypes.api import Enum, Float, Int, Str
from openmdao.main.driver import Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasIneqConstraints
from openmdao.main.hasobjective import HasObjective
from openmdao.main.interfaces import IHasParameters, IHasIneqConstraints, \
                                     IHasObjective, implements, IOptimizer
from openmdao.util.decorators import add_delegate


@add_delegate(HasParameters, HasIneqConstraints, HasObjective)
class COBYLAdriver(Driver):
    """Minimize a function using the Constrained Optimization BY Linear
    Approximation (COBYLA) method.

    COBYLA is gradient-free and can handle inequality constraints.

    Note: Constraints should be added using the OpenMDAO convention
    (positive = violated).
    """

    implements(IHasParameters, IHasIneqConstraints, IHasObjective, IOptimizer)

    # pylint: disable-msg=E1101
    rhobeg = Float(1.0, iotype='in',
                   desc='Reasonable initial changes to the variables.')

    rhoend = Float(1e-4, iotype='in',
                   desc='Final accuracy in the optimization'
                        ' (not precisely guaranteed).')

    iprint = Enum(1, [0, 1, 2, 3], iotype='in',
                  desc='Controls the frequency of output: 0 (no output),1,2,3.')

    maxfun = Int(1000, iotype='in',
                 desc='Maximum number of function evaluations.')

    iout = Int(6, iotype='in',
               desc='Fortran output unit. Leave this at 6 for STDOUT.')

    output_filename = Str('cobyla.out', iotype='in',
                          desc='Name of output file (if iout not 6).')

    error_code = Int(0, iotype='out',
                     desc='Error code returned from COBYLA.')


    def __init__(self):

        super(COBYLAdriver, self).__init__()

        self.error_messages = {
            1 : 'Max. number of function evaluations reached',
            2 : 'Rounding errors are becoming damaging'
        }

        self.x = zeros(0, 'd')
        self.work_vector = zeros(0, 'd')
        self.gg = zeros(0, 'd')
        self.iact = zeros(0, 'd')
        self.g = zeros(0, 'd')

        self.ff = 0
        self.nfvals = 0
        self.nparam = 0
        self.ncon = 0

        self.upper = None
        self.lower = None
        self._continue = None

    def start_iteration(self):
        """Perform initial setup before iteration loop begins."""

        # Inital run to make sure the workflow executes
        super(COBYLAdriver, self).run_iteration()

        self.nparam = self.total_parameters()
        self.ncon = self.total_ineq_constraints()
        self.ncon += 2*self.nparam
        self.g = zeros(self.ncon, 'd')
        self.work_vector = zeros(self.ncon, 'd')

        # get the initial values of the parameters
        self.x = self.eval_parameters(self.parent)

        self.upper = self.get_upper_bounds()
        self.lower = self.get_lower_bounds()

        n = self.nparam
        m = self.ncon
        self.work_vector = zeros([n*(3*n+2*m+11)+4*m+6], 'd')
        self.iact = zeros([m+1], 'i')
        self.gg = zeros([m], 'd')

        self._continue = True

    def run_iteration(self):
        """ Note: cobyla controls the looping."""

        try:
            self.iact, self.error_code, self.nfvals = \
              cobyla(self._func, self.nparam, self.ncon, self.x,
                   self.rhobeg, self.rhoend, self.iprint, self.maxfun,
                   self.work_vector, self.iact, self.error_code, self.nfvals,
                   self.iout, self.output_filename, self.ff, self.gg)

        except Exception as err:
            self._logger.error(str(err))
            raise

        if self.iprint > 0:
            closeunit(self.iout)

        # Log any errors
        if self.error_code != 0:
            self._logger.warning(self.error_messages[self.error_code])

        # Iteration is complete
        self._continue = False

    def _func(self, n, m, xnew, f, g):
        """ Return ndarrays containing the function and constraint
        evaluations.

        Note: n, m, f, and g are unused inputs."""

        self.set_parameters(xnew)
        super(COBYLAdriver, self).run_iteration()
        f = self.eval_objective()

        if isnan(f):
            msg = "Numerical overflow in the objective"
            self.raise_exception(msg, RuntimeError)

        # Constraints (COBYLA defines positive as satisfied)
        cons = -1. * array(self.eval_ineq_constraints())

        # Side Constraints
        vals = self.eval_parameters(self.parent)
        g = hstack([cons, (vals - self.lower), (self.upper - vals)])

        return f, g

