
from random import normalvariate

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
        raise NotImplemented('The %s class has no sample() method' % self.__class__.__name__)
    
    def expected(self): 
        raise NotImplemented('The %s class has no expected() method' % self.__class__.__name__)

        
class NormalDistribution(UncertainDistribution): 
    """An UncertainDistribution which represents a quantity with a 
    normal distribution of uncertainty.
    
    mu : float
       mean value
       
    sigma : float
       standard deviation
    """
    
    def __init__(self,mu=0, sigma=1): 
        super(NormalDistribution,self).__init__()
        
        self.mu = mu
        self.sigma = sigma
        
    def sample(self): 
        return normalvariate(self.mu,self.sigma)
    
    def expected(self): 
        return self.mu
        
    
