from enthought.traits.api import implements, HasTraits

from openmdao.lib.traits.float import Float
from openmdao.main.interfaces import IUncertainVaraible

from random import normalvariate

class NormalDistribution(HasTraits): 
    """IUncertainVariable which represents a quantity with a normal distribution of uncertainty"""
    
    mu = Float(0,iotype="in",desc="mean value")
    sigma = Float(1,iotype="in",desc="standard deviation")
    
    implements(IUncertainVariable)
    
    def __init__(self,mu=0,sigma=1,*args,**kwargs): 
        super(NormalDistribution,self).__init__(*args,**kwargs)
        
        self.mu = mu
        self.sigma = sigma
        
    def sample(self): 
        return normalvariate(self.mu,self.sigma)
    
    def expected(self): 
        return self.mu
        
    
