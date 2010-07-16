from enthought.traits.api import implements, HasTraits

from openmdao.lib.traits.float import Float
from openmdao.main.interfaces import IUncertainVariable

from random import normalvariate,uniform

class NormalDistribution(HasTraits): 
    """IUncertainVariable which represents a quantity with a normal distribution of uncertainty"""
    
    mu = Float(0, desc="mean value")
    sigma = Float(1, desc="standard deviation")
    
    implements(IUncertainVariable)
    
    def __init__(self,mu=0,sigma=1,*args,**kwargs): 
        super(NormalDistribution,self).__init__(*args,**kwargs)
        
        self.mu = mu
        self.sigma = sigma
        
    def getvalue(self):
        return self.expected()
        
    def sample(self): 
        return normalvariate(self.mu,self.sigma)
    
    def expected(self): 
        return self.mu
        
class UniformDistribution(HasTraits):
    """IUncertainVariable which represents a quantity with a uniform distribution of uncertainty"""
    
    max = Float(0,desc="maximum value of random variable")
    min = Float(1,desc="minimum value of random vatiable")
    
    implements(IUncertainVariable)

    def __init__(self,max=0,min=1,*args,**kwargs):
        super(UniformDistribution,self).__init__(*args,**kwargs)
        
        self.max = max
        self.min = min
        
    def sample(self):
        return uniform(self.min,self.max)
        
    def expected(self):
        return (self.max+self.min)/2.
            
class TriangularDistribution(hasTraits):
    """IUncertainVariable which represents a quantity with a triangular distribution of uncertainty"""

    max = Float(0,desc="maximum value of random variable")
    min = Float(1,desc="maximum value of random variable")
    mode = Float(0.5,desc="location of the most frequent value of the distribution")

    implements(IUncertainVariable)
    
    def __init__(self,max=0,min=1,mode=0.5,*args,**kwargs):
        super(TriangularDistribution,self).__init__(*args,**kwargs)
        
        self.max = max
        self.min = min
        self.mode = mode
        
    def sample(self):
        return triangular(self.min,self.max,self.mode)
        
    def expected(self):
        return (self.max+self.mode+self.min)/3.