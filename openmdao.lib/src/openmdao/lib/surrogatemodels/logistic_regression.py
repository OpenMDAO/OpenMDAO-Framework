"""Surrogate Model based on a logistic regression model, with regularization to 
adjust for overfitting. Based on work from 
http://blog.smellthedata.com/2009/06/python-logistic-regression-with-l2.html"""
from random import seed
import numpy as np
from numpy import log
from scipy.optimize.optimize import fmin_bfgs

from enthought.traits.api import HasTraits

from openmdao.lib.datatypes.api import Float, Bool
from openmdao.main.interfaces import implements, ISurrogate

def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-x))

class LogisticRegression(HasTraits): 
    implements(ISurrogate)
    
    alpha = Float(.1,low=0,iotype='in',desc='L2 regularization strength')
    
    def __init__(self,X=None,Y=None,alpha=.1):
        
        # must call HasTraits init to set up Traits stuff
        super(LogisticRegression, self).__init__()          
        
        self.m = None #number of independents
        self.n = None #number of training points

        self.alpha = alpha
        
        self.degenerate = False
        
        if X is not None and Y is not None: 
            self.train(X,Y)
            
    def lik(self, betas):
        """ Likelihood of the data under the current settings of parameters. """
        
        # Data likelihood
        l = 0
        for i in range(self.n):
            l += log(sigmoid(self.Y[i] * \
                             np.dot(betas, self.X[i,:])))
        
        # Prior likelihood
        for k in range(1, self.X.shape[1]):
            l -= (self.alpha / 2.0) * self.betas[k]**2
        
        #multiply by -1 so the optimizer will maxize    
        return -1*l   
    
    def get_uncertain_value(self,value): 
        """Returns the value iself. Logistic regressions don't have uncertainty"""
        return value
            
    def train(self,X,Y):
        """ Define the gradient and hand it off to a scipy gradient-based
        optimizer. """
        
        #normalize all Y data to be between -1 and 1
        low = min(Y)
        high = max(Y)
        
        #there was no data to predict on, so just degenerate to predicting True all the time
        self.degenerate = False
        if high == low: 
            self.degenerate = high
            return 
        
        self.m = 2.0/(high-low)
        self.b = (2.0*low/(low-high))-1
        
        #constants for unscaling the output
        self.z = high-low
        self.w = low 
        
        self.X = np.array(X)
        self.Y = self.m*np.array(Y)+self.b
        self.n = len(X)
        self.betas = np.zeros(len(X[0]))
        
        # Define the derivative of the likelihood with respect to beta_k.
        # Need to multiply by -1 because we will be minimizing.
        dB_k = lambda B, k : (k > 0) * self.alpha * B[k] - np.sum([ \
                                     self.Y[i] * self.X[i, k] * \
                                     sigmoid(-self.Y[i] *\
                                             np.dot(B, self.X[i,:])) \
                                     for i in range(self.n)])
        
        # The full gradient is just an array of componentwise derivatives
        dB = lambda B : np.array([dB_k(B, k) \
                                  for k in range(self.X.shape[1])])
        
        # Optimize
        self.betas = fmin_bfgs(self.lik, self.betas, fprime=dB, disp=False)
        
        
    def predict(self,new_x):
        """Calculates a predicted value of the response based on the current
        trained model for the supplied list of inputs.
        """ 
        if self.degenerate: return self.degenerate
        
        return self.z*sigmoid(np.dot(self.betas,np.array(new_x)))+self.w

    
    
    
    