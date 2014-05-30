"""Expected Improvement calculation for single objective."""

from numpy import exp, abs, pi, seterr

try:
    from math import erfc   # py27 and later has erfc in the math module
except ImportError as err:
    from scipy.special import erfc

from openmdao.main.datatypes.api import Float, Instance
from openmdao.main.api import Component
from openmdao.main.uncertain_distributions import NormalDistribution

class ExpectedImprovement(Component):
    """Expected Improvement calculation for single objective."""

    target = Float(0, iotype="in", desc="Current objective minimum.")

    current = Instance(NormalDistribution, iotype="in",
                       desc="The Normal Distribution of the predicted value "
                            "for some function at some point where you wish to"
                            " calculate the EI.")

    EI = Float(0.0, iotype="out",
               desc="The expected improvement of the predicted_value " + \
                    "in current.")

    PI = Float(0.0, iotype="out",
               desc="The probability of improvement of the predicted_value" + \
                    " in current.")

    def execute(self):
        """ Calculates the expected improvement of the model at a given point.
        """

        mu = self.current.mu
        sigma = self.current.sigma
        target = self.target

        try:
            seterr(divide='raise')
            self.PI = 0.5*erfc(-(1./2.**.5)*((target-mu)/sigma))

            T1 = (target-mu)*.5*(erfc(-(target-mu)/(sigma*2.**.5)))
            T2 = sigma*((1./((2.*pi)**.5))*exp(-0.5*((target-mu)/sigma)**2.))
            self.EI = abs(T1+T2)

        except (ValueError, ZeroDivisionError, FloatingPointError):
            self.EI = 0
            self.PI = 0



