from itertools import product

from numpy import linspace
from enthought.traits.api import implements, HasTraits

from openmdao.main.interfaces import IDOEgenerator
from openmdao.lib.datatypes.int import Int


class FullFactorial(HasTraits): 
    implements(IDOEgenerator)
    
    num_parameters = Int(0,iotype="in",desc="number of independent parameters in the DOE")
    num_levels = Int(0,iotype="in",desc="number of levels of values for each parameter")
    
    def __init__(self,num_levels=None,num_parameters=None,*args,**kwargs):
        super(FullFactorial,self).__init__(*args,**kwargs)
        if num_parameters is not None: 
            self.num_parameters = num_parameters
        if num_levels is not None: 
            self.num_levels = num_levels
    
    def __iter__(self):
        """Return an iterator over our sets of input values"""
        return product(*[linspace(0.,1.,self.num_levels) for i in range(self.num_parameters)])
        
