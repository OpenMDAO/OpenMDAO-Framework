"""Expected Improvement calculation for multiple objectives."""

from numpy import exp, pi, array, isnan, diag, random

try:
    from math import erfc
except ImportError as err:
    from scipy.special import erfc

from openmdao.main.datatypes.api import Enum, Float, Array, Int
from openmdao.main.component import Component
from openmdao.main.uncertain_distributions import NormalDistribution


class MultiObjExpectedImprovement(Component):
    """Expected Improvement calculation for multiple objectives."""

    # best_cases
    target = Array(iotype="in", desc="Array of Pareto-optimal cases.")

    # predicted_values
    current = Array(iotype="in", dtype=NormalDistribution,
                        desc="The NormalDistributions for each response " + \
                        "at a location where you wish to calculate EI.")

    n = Int(1000, iotype="in", desc="Number of Monte Carlo Samples with \
                        which to calculate probability of improvement.")

    calc_switch = Enum("PI", ["PI", "EI"], iotype="in", desc="Switch to use either \
                        probability (PI) or expected (EI) improvement.")

    PI = Float(0.0, iotype="out", desc="The probability of improvement of " + \
                                       "the next_case.")

    EI = Float(0.0, iotype="out", desc="The expected improvement of the " + \
                                       "next_case.")

    def __init__(self):
        super(MultiObjExpectedImprovement, self).__init__()
        self.y_star = None

    def _2obj_PI(self, mu, sigma):
        """Calculates the multi-objective probability of improvement
        for a new point with two responses. Takes as input a
        pareto frontier, mean and sigma of new point."""

        y_star = self.y_star

        PI1 = (0.5*erfc(-(1/(2**0.5))*((y_star[0][0]-mu[0])/sigma[0])))
        PI3 = (1-(0.5*erfc(-(1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0]))))\
        *(0.5*erfc(-(1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))

        PI2 = 0
        if len(y_star) > 1:
            for i in range(len(y_star) - 1):
                PI2 = PI2+((0.5*erfc(-(1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))\
                -(0.5*erfc(-(1/(2**0.5))*((y_star[i][0]-mu[0])/sigma[0]))))\
                *(0.5*erfc(-(1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))
        mcpi = PI1 + PI2 + PI3
        return mcpi

    def _2obj_EI(self, mu, sigma):
        """Calculates the multi-criteria expected improvement
        for a new point with two responses. Takes as input a
        pareto frontier, mean and sigma of new point."""

        y_star = self.y_star
        ybar11 = mu[0]*(0.5*erfc(-(1/(2**0.5))*((y_star[0][0]-mu[0])/sigma[0])))\
        -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[0][0]-mu[0])**2/sigma[0]**2))
        ybar13 = (mu[0]*(0.5*erfc(-(1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0])))\
        -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[-1][0]-mu[0])**2/sigma[0]**2)))\
        *(0.5*erfc(-(1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))

        ybar12 = 0
        if len(y_star) > 1:
            for i in range(len(y_star) - 1):
                ybar12 = ybar12+((mu[0]*(0.5*erfc(-(1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))\
                -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i+1][0]-mu[0])**2/sigma[0]**2)))\
                -(mu[0]*(0.5*erfc(-(1/(2**0.5))*((y_star[i][0]-mu[0])/sigma[0])))\
                -sigma[0]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i][0]-mu[0])**2/sigma[0]**2))))\
                *(0.5*erfc(-(1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))

        ybar1 = (ybar11+ybar12+ybar13)/self.PI

        ybar21 = mu[1]*(0.5*erfc(-(1/(2**0.5))*((y_star[0][1]-mu[1])/sigma[1])))\
        -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[0][1]-mu[1])**2/sigma[1]**2))
        ybar23 = (mu[1]*(0.5*erfc(-(1/(2**0.5))*((y_star[-1][1]-mu[1])/sigma[1])))\
        -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[-1][1]-mu[1])**2/sigma[1]**2)))\
        *(0.5*erfc(-(1/(2**0.5))*((y_star[-1][0]-mu[0])/sigma[0])))

        ybar22 = 0
        if len(y_star) > 1:
            for i in range(len(y_star) - 1):
                ybar22 = ybar22+((mu[1]*(0.5*erfc(-(1/(2**0.5))*((y_star[i+1][1]-mu[1])/sigma[1])))\
                -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i+1][1]-mu[1])**2/sigma[1]**2)))\
                -(mu[1]*(0.5*erfc(-(1/(2**0.5))*((y_star[i][1]-mu[1])/sigma[1])))\
                -sigma[1]*(1/((2*pi)**0.5))*exp(-0.5*((y_star[i][1]-mu[1])**2/sigma[1]**2))))\
                *(0.5*erfc(-(1/(2**0.5))*((y_star[i+1][0]-mu[0])/sigma[0])))

        ybar2 = (ybar21+ybar22+ybar23)/self.PI
        dists = [((ybar1-point[0])**2+(ybar2-point[1])**2)**0.5 for point in y_star]
        mcei = self.PI*min(dists)
        if isnan(mcei):
            mcei = 0
        return mcei

    def _dom(self, a, b):
        """determines if a completely dominates b
       returns True is if does
    """
        comp = [c1 < c2 for c1, c2 in zip(a, b)]
        if sum(comp) == self.target.shape[1]:
            return True
        return False

    def _nobj_PI(self, mu, sigma):
        ''' n-objective probability of improvement.'''

        cov = diag(array(sigma)**2)
        rands = random.multivariate_normal(mu, cov, self.n)
        num = 0  # number of cases that dominate the current Pareto set

        for random_sample in rands:
            for par_point in self.y_star:
                #par_point = [p[2] for p in par_point.outputs]
                if self._dom(par_point, random_sample):
                    num = num + 1
                    break
        pi = (self.n - num)/float(self.n)
        return pi

    def execute(self):
        """ Calculates the expected improvement or probability of improvement
        of a candidate point given by a normal distribution.
        """
        mu = [objective.mu for objective in self.current]
        sig = [objective.sigma for objective in self.current]

        target = self.target
        self.y_star = target[array([i[0] for i in target]).argsort()]

        n_objs = target.shape[1]

        if n_objs == 2:
            # biobjective optimization
            self.PI = self._2obj_PI(mu, sig)
            if self.calc_switch == 'EI':
                # execute EI calculations
                self.EI = self._2obj_EI(mu, sig)

        if n_objs > 2:
            # n objective optimization
            self.PI = self._nobj_PI(mu, sig)
            if self.calc_switch == 'EI':
                # execute EI calculations
                self.raise_exception("EI calculations not supported"
                                        " for more than 2 objectives", ValueError)
