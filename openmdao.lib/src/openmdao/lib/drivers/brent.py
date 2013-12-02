from scipy.optimize import brentq

from openmdao.main.api import Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints

from openmdao.util.decorators import add_delegate

from openmdao.lib.datatypes.api import Float, Int


@add_delegate(HasParameters, HasEqConstraints)
class Brent(Driver):
    """root finding using Brent's method."""


    lower_bound = Float(0,iotype="in", desc="lower bound for the root search")
    upper_bound = Float(100,iotype="in", desc="upper bound for the root search")

    xtol = Float(0.0, iotype="in", desc='The routine converges when a root is known to lie within xtol of the value return. Should be >= 0. '
        'The routine modifies this to take into account the relative precision of doubles.')

    rtol = Float(0.0, iotype="in", desc='The routine converges when a root is known to lie within rtol times the value returned of '
        'the value returned. Should be >= 0. Defaults to np.finfo(float).eps * 2.')

    maxiter = Int(100, iotype="in", desc='if convergence is not achieved in maxiter iterations, and error is raised. Must be >= 0.')

    def _eval(self, x):
        """evaluate f(x)"""

        self._param.set(x)
        self.run_iteration()
        #obj = self.get_objectives()
        #f = obj['f'].evaluate(self.parent)

        f = self.eval_eq_constraints(self.parent)[0]

        return f


    def execute(self):

        # bounds
        self._param_name, self._param = self.get_parameters().popitem()
        xlow = self._param.low
        xhigh = self._param.high

        # TODO: better error handling.  ideally, if this failed we would attempt to find
        # bounds ourselves rather than throwing an error or returning a value at the bounds
        if self._eval(xlow)*self._eval(xhigh) > 0:
            raise Exception('bounds do not bracket a root')

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






