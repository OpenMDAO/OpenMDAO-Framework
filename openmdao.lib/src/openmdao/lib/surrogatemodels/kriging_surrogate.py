""" Surrogate model based on Kriging """

from math import log, e, sqrt

# pylint: disable-msg=E0611,F0401
from numpy import array, zeros, dot, ones, arange, eye, abs, vstack, exp
from numpy.linalg import det, linalg, lstsq
from scipy.linalg import cho_factor, cho_solve
from scipy.optimize import fmin

from enthought.traits.api import HasTraits, implements

from openmdao.main.interfaces import ISurrogate
from openmdao.main.uncertain_distributions import NormalDistribution


class KrigingSurrogate(HasTraits): 
    implements(ISurrogate)
    
    def __init__(self,X=None,Y=None):
        
        # must call HasTraits init to set up Traits stuff
        super(KrigingSurrogate, self).__init__() 

        self.m = None #number of independent
        self.n = None #number of training points
        self.thetas = None
        
        self.R = None
        self.R_fact = None
        self.mu = None
        self.sig2 = None
        self.log_likelihood = None
        
        self.X = X
        self.Y = Y

        if X is not None and Y is not None: 
            self.train(X,Y)
            
    def get_uncertain_value(self,value): 
        """Returns a NormalDistribution centered around the value, with a 
        standard deviation of 0."""
        return NormalDistribution(value,0.)

    def predict(self,new_x):
        """Calculates a predicted value of the response based on the current
        trained model for the supplied list of inputs.
        """
        if self.m == None: #untrained surrogate
            raise RuntimeError("KrigingSurrogate has not been trained, so no "
                               "prediction can be made")
        r = zeros(self.n)
        X, Y = self.X, self.Y
        thetas = 10.**self.thetas
        XX = array(X)
        for i in range(self.n):
            r[i] = sum(thetas*(XX[i]-new_x)**2.)
        r = exp(-r)
            
        one = ones(self.n)
        if self.R_fact is not None: 
            #---CHOLESKY DECOMPOSTION ---
            #f = self.mu+dot(r,cho_solve(self.R_fact,Y-dot(one,self.mu)))
            #term1 = dot(r,cho_solve(self.R_fact,r))
            #term2 = (1.0-dot(one,cho_solve(self.R_fact,r)))**2./dot(one,cho_solve(self.R_fact,one))
            
            rhs = vstack([(Y-dot(one, self.mu)), r, one]).T
            R_fact = (self.R_fact[0].T,not self.R_fact[1])
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
        MSE = self.sig2*(1.0-term1+term2)
        RMSE = sqrt(abs(MSE))
        
        return NormalDistribution(f, RMSE)
        

    def train(self,X,Y):
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
        def _calcll(thetas):
            self.thetas = thetas
            self._calculate_log_likelihood()
            return -self.log_likelihood
        self.thetas = fmin(_calcll, thetas, disp=False, ftol = 0.0001)
        self._calculate_log_likelihood()
        
    def _calculate_log_likelihood(self):
        #if self.m == None:
        #    Give error message
        R = zeros((self.n, self.n))
        X,Y = array(self.X), array(self.Y)
        thetas = 10.**self.thetas
        for i in range(self.n):
            for j in arange(i+1,self.n):
                R[i,j] = e**(-sum(thetas*(X[i]-X[j])**2.)) #weighted distance formula
        R = R + R.T + eye(self.n)
        self.R = R
        one = ones(self.n)
        try:
            
            self.R_fact = cho_factor(R)
            rhs = vstack([Y, one]).T
            R_fact = (self.R_fact[0].T,not self.R_fact[1])
            cho = cho_solve(R_fact, rhs).T
            
            self.mu = dot(one,cho[0])/dot(one,cho[1])
            #self.mu = dot(one,cho_solve(self.R_fact,Y))/dot(one,cho_solve(self.R_fact,one))
            self.sig2 = dot(Y-dot(one,self.mu),cho_solve(self.R_fact,(Y-dot(one,self.mu))))/self.n
            self.log_likelihood = -self.n/2.*log(self.sig2)-1./2.*log(abs(det(self.R)))-sum(self.thetas)-sum(abs(self.thetas))
        except (linalg.LinAlgError,ValueError):
            #------LSTSQ---------
            self.R_fact = None #reset this to none, so we know not to use cholesky
            rhs = vstack([Y, one]).T
            lsq = lstsq(self.R.T,rhs)[0].T
            self.mu = dot(one,lsq[0])/dot(one,lsq[1])
            self.sig2 = dot(Y-dot(one,self.mu),lstsq(self.R,Y-dot(one,self.mu))[0])/self.n
            self.log_likelihood = -self.n/2.*log(self.sig2)-1./2.*log(abs(det(self.R)+1.e-16))-sum(self.thetas)
