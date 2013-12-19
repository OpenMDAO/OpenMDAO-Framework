from scipy.optimize import brentq

from openmdao.main.driver import Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints

from openmdao.util.decorators import add_delegate

from openmdao.lib.datatypes.api import Float, Int


@add_delegate(HasParameters, HasEqConstraints)
class Brent(Driver):
    """Root finding using Brent's method."""

    lower_bound = Float(0., iotype="in", desc="lower bound for the root search")
    upper_bound = Float(100., iotype="in", desc="upper bound for the root search")

    xtol = Float(0.0, iotype="in", desc='The routine converges when a root is known to lie within xtol of the value return. Should be >= 0. '
        'The routine modifies this to take into account the relative precision of doubles.')

    rtol = Float(0.0, iotype="in", desc='The routine converges when a root is known to lie within rtol times the value returned of '
        'the value returned. Should be >= 0. Defaults to np.finfo(float).eps * 2.')

    maxiter = Int(100, iotype="in", desc='if convergence is not achieved in maxiter iterations, and error is raised. Must be >= 0.')

    def _eval(self, x):
        """evaluate f(x)"""

        self._param.set(x)
        self.run_iteration()

        f = self.eval_eq_constraints(self.parent)[0]

        return f


    def execute(self):

        # TODO: better error handling.  ideally, if this failed we would attempt to find
        # bounds ourselves rather than throwing an error or returning a value at the bounds
        if self._eval(self.lower_bound)*self._eval(self.upper_bound) > 0:
            self.raise_exception('bounds (low=%s, high=%s) do not bracket a root' %
                                 (self.lower_bound, self.upper_bound))

        kwargs = {'maxiter':self.maxiter, 'a':self.lower_bound, 'b':self.upper_bound}
        if self.xtol > 0: 
            kwargs['xtol'] = self.xtol
        if self.rtol > 0:
            kwargs['rtol'] = self.rtol

        # Brent's method
        xstar = brentq(self._eval,**kwargs)

        # set result
        #param.set(xstar)
        self.xstar = xstar


    def check_config(self):
        params = self.get_parameters().values()
        if len(params) != 1:
            self.raise_exception("Brent driver must have 1 parameter, "
                                 "but instead it has %d" % len(params))

        constraints = self.get_eq_constraints()
        if len(constraints) != 1:
            self.raise_exception("Brent driver must have 1 equality constraint, "
                                 "but instead it has %d" % len(constraints))
        self._param = params[0]



