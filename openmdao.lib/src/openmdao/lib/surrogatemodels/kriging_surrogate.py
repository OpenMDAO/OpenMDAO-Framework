""" Surrogate model based on Kriging. """
from math import log, e, sqrt

# pylint: disable-msg=E0611,F0401
from numpy import array, zeros, dot, ones, eye, abs, vstack, exp, \
                  sum, log10
from numpy.linalg import det, linalg, lstsq
from scipy.linalg import cho_factor, cho_solve
from scipy.optimize import minimize

from openmdao.main.api import Container
from openmdao.main.interfaces import implements, ISurrogate
from openmdao.main.uncertain_distributions import NormalDistribution


class KrigingSurrogate(Container):
    """Surrogate Modeling method based on the simple Kriging interpolation.
    Predictions are returned as a NormalDistribution instance."""

    implements(ISurrogate)

    def __init__(self):
        super(KrigingSurrogate, self).__init__()

        self.m = None       # number of independent
        self.n = None       # number of training points
        self.thetas = None
        self.nugget = 0     # nugget smoothing parameter from [Sasena, 2002]

        self.R = None
        self.R_fact = None
        self.mu = None
        self.log_likelihood = None

    def get_uncertain_value(self, value):
        """Returns a NormalDistribution centered around the value, with a
        standard deviation of 0."""
        return NormalDistribution(value, 0.)

    def predict(self, new_x):
        """Calculates a predicted value of the response based on the current
        trained model for the supplied list of inputs.
        """
        if self.m is None:  # untrained surrogate
            raise RuntimeError("KrigingSurrogate has not been trained, so no "
                               "prediction can be made")
        r = zeros(self.n)
        X, Y = self.X, self.Y
        thetas = 10.**self.thetas
        XX = array(X)
        new_x = array(new_x)
        for i in range(self.n):
            r[i] = sum(thetas*(XX[i] - new_x)**2.)
        r = exp(-r)

        one = ones(self.n)
        if self.R_fact is not None:
            #---CHOLESKY DECOMPOSTION ---
            #f = self.mu+dot(r,cho_solve(self.R_fact,Y-dot(one,self.mu)))
            #term1 = dot(r,cho_solve(self.R_fact,r))
            #term2 = (1.0-dot(one,cho_solve(self.R_fact,r)))**2./dot(one,cho_solve(self.R_fact,one))

            rhs = vstack([(Y-dot(one, self.mu)), r, one]).T
            R_fact = (self.R_fact[0].T, not self.R_fact[1])
            cho = cho_solve(R_fact, rhs).T

            f = self.mu + dot(r, cho[0])
            term1 = dot(r, cho[1])
            term2 = (1.0 - dot(one, cho[1]))**2./dot(one, cho[2])

        else:
            #-----LSTSQ-------
            rhs = vstack([(Y-dot(one, self.mu)), r, one]).T
            lsq = lstsq(self.R.T, rhs)[0].T

            f = self.mu + dot(r, lsq[0])
            term1 = dot(r, lsq[1])
            term2 = (1.0 - dot(one, lsq[1]))**2./dot(one, lsq[2])
            """
      #-----LSTSQ-------
      rhs = vstack([(Y-dot(one, self.mu)), r, one]).T
      lsq = lstsq(self.R.T, rhs)[0].T

      f = self.mu + dot(r, lsq[0])
      term1 = dot(r, lsq[1])
      term2 = (1.0 - dot(one, lsq[1]))**2./dot(one, lsq[2])
      """
        MSE = self.sig2*(1.0 - term1 + term2)
        RMSE = sqrt(abs(MSE))

        dist = NormalDistribution(f, RMSE)
        return dist

    def train(self, X, Y):
        """Train the surrogate model with the given set of inputs and outputs."""

        #TODO: Check if one training point will work... if not raise error
        """self.X = []
        self.Y = []
        for ins,out in zip(X,Y):
            if ins not in self.X:
                self.X.append(ins)
                self.Y.append(out)
            else: "duplicate training point" """

        self.X = X
        self.Y = Y
        self.m = len(X[0])
        self.n = len(X)

        thetas = zeros(self.m)
        #print "initial guess", thetas

        def _calcll(thetas):
            ''' Callback function'''
            self.thetas = thetas
            self._calculate_log_likelihood()
            return -self.log_likelihood

        #if self.thetas == None:
        #self.thetas = fmin(_calcll, thetas, disp=False, ftol=0.0001)
        def lowerBound(log10t):
            return log10t - log10(self.thetas)

        def upperBound(log10t):
            return log10(self.thetas) - log10t

        cons = []
        for i in xrange(self.m):
            cons.append({'type': 'ineq', 'fun': lambda log10t: log10t[i] - log10(1e-2)})  # min
            cons.append({'type': 'ineq', 'fun': lambda log10t: log10(3) - log10t[i]})     # max

        self.thetas = minimize(_calcll, thetas, method='COBYLA', constraints=cons, tol=1e-8).x
        #print self.thetas
        self._calculate_log_likelihood()

    def _calculate_log_likelihood(self):
        #if self.m == None:
        #    Give error message
        R = zeros((self.n, self.n))
        X, Y = array(self.X), array(self.Y)
        thetas = 10.**self.thetas

        #weighted distance formula
        for i in range(self.n):
            R[i, i+1:self.n] = e**(-sum(thetas*(X[i] - X[i+1:self.n])**2., 1))

        R = R*(1.0 - self.nugget)
        R = R + R.T + eye(self.n)
        self.R = R

        one = ones(self.n)
        try:
            self.R_fact = cho_factor(R)
            rhs = vstack([Y, one]).T
            R_fact = (self.R_fact[0].T, not self.R_fact[1])
            cho = cho_solve(R_fact, rhs).T

            self.mu = dot(one, cho[0])/dot(one, cho[1])
            ymdotone = Y - dot(one, self.mu)
            self.sig2 = dot(ymdotone, cho_solve(self.R_fact,
                                                (ymdotone)))/self.n
            #self.log_likelihood = -self.n/2.*log(self.sig2)-1./2.*log(abs(det(self.R)+1.e-16))-sum(thetas)
            self.log_likelihood = -self.n/2.*log(self.sig2) - \
                                  1./2.*log(abs(det(self.R) + 1.e-16))

        except (linalg.LinAlgError, ValueError):
            #------LSTSQ---------
            self.R_fact = None  # reset this to none, so we know not to use cholesky
            # self.R = self.R+diag([10e-6]*self.n)  # improve conditioning[Booker et al., 1999]
            rhs = vstack([Y, one]).T
            lsq = lstsq(self.R.T, rhs)[0].T
            self.mu = dot(one, lsq[0])/dot(one, lsq[1])
            ymdotone = Y - dot(one, self.mu)
            self.sig2 = dot(ymdotone, lstsq(self.R, ymdotone)[0])/self.n
            self.log_likelihood = -self.n/2.*log(self.sig2) - \
                                   1./2.*log(abs(det(self.R) + 1.e-16))
            #print self.log_likelihood


class FloatKrigingSurrogate(KrigingSurrogate):
    """Surrogate model based on the simple Kriging interpolation. Predictions are returned as floats,
    which are the mean of the NormalDistribution predicted by the model."""

    def predict(self, new_x):
        dist = super(FloatKrigingSurrogate, self).predict(new_x)
        return dist.mu

    def get_uncertain_value(self, value):
        """Returns a float"""
        return float(value)
