#import sqlite3
from random import gauss, weibullvariate, uniform
try:
    from scipy.special import gamma
except ImportError:
    # as of python2.7, gamma is in the math module (even though docs say it's new as of 3.2)
    import math
    if hasattr(math, 'gamma'):
        gamma = math.gamma

class UncertainDistribution(object):
    """Base class for uncertain variables."""
    
    default_val_method = 'expected'
    
    def __init__(self, valmethod=None):
        self.valmethod = valmethod

    def getvalue(self):
        if self.valmethod:
            return getattr(self, self.valmethod)()
        return getattr(self, self.default_val_method)()
    
    def sample(self): 
        raise NotImplementedError('The %s class has no sample() method' % self.__class__.__name__)
    
    def expected(self): 
        raise NotImplementedError('The %s class has no expected() method' % self.__class__.__name__)


class NormalDistribution(UncertainDistribution): 
    """An UncertainDistribution which represents a quantity with a 
    normal distribution of uncertainty.
    
    mu: float
       mean value
       
    sigma: float
       standard deviation
    """
    
    def __init__(self,mu=0., sigma=1.): 
        super(NormalDistribution,self).__init__()
        
        self.mu = mu
        self.sigma = sigma
        
    def sample(self): 
        return gauss(self.mu,self.sigma)
    
    def expected(self): 
        return self.mu
    
    def __add__(self,other): 
        return NormalDistribution(mu = self.mu + other, sigma = self.sigma)
    
    def __sub__(self,other): 
        return NormalDistribution(mu = self.mu - other, sigma = self.sigma)
    
    def __str__(self): 
        return "NormalDistribution(mu=%s,sigma=%s)"%(self.mu,self.sigma)
    
#def _adapt_norm_dist(nd): 
    #return "%f;%f"%(nd.mu,nd.sigma)

#def _convert_norm_dist(nd):
    #mu,sigma = map(float, nd.split(';'))
    #return NormalDistribution(mu,sigma)

##register the adapter
#sqlite3.register_adapter(NormalDistribution, _adapt_norm_dist)
##register the converter
#sqlite3.register_converter("NormalDistribution", _convert_norm_dist)
    
class UniformDistribution(UncertainDistribution):
    """An UncertainDistribution which represents a quantity with a 
    uniform distribution of uncertainty.
    
    min: float
       minimum value
       
    max: float
       maximum value
    """
 
    def __init__(self,max=0.,min=1.,*args,**kwargs):
        super(UniformDistribution,self).__init__(*args,**kwargs)
        
        self.max = max
        self.min = min

    def sample(self):
        return uniform(self.min,self.max)
        
    def expected(self):
        return (self.max+self.min)/2.

class TriangularDistribution(UncertainDistribution):
    """An UncertainDistribution which represents a quantity with a 
    triangular distribution of uncertainty.
    
    min: float
       minimum value
       
    max: float
       maximum value
       
    mode: float
       mode
    """
 
    def __init__(self,max=0.,min=1.,mode=0.5,*args,**kwargs):
        super(TriangularDistribution,self).__init__(*args,**kwargs)
        
        self.max = max
        self.min = min
        self.mode = mode
 
    def sample(self):
        return triangular(self.min,self.max,self.mode)
        
    def expected(self):
        return (self.max+self.mode+self.min)/3.
        
        
if 'gamma' in globals():
    class WeibullDistribution(UncertainDistribution):
        """An UncertainDistribution which represents a quantity with a 
        weibull distribution of uncertainty.
        
        alpha: float
           scale parameter
           
        beta: float
           shape parameter
        """
    
        def __init__(self,alpha=1.,beta=2.,*args,**kwargs):
            super(UniformDistribution,self).__init__(*args,**kwargs)
            
            self.alpha = alpha
            self.beta = beta
    
        def sample(self):
            return weibullvariate(self.alpha,self.beta)
            
        def expected(self):
            return self.alpha*gamma(1.+1./self.beta)
            
