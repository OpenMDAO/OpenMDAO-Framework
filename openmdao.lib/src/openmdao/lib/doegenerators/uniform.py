""" DOEgenerator that performs a uniform space-filling Design of Experiments. Plugs
into the DOEgenerator socket on a DOEdriver."""

# pylint: disable-msg=E0611,F0401
from numpy import linspace,random
from openmdao.lib.datatypes.api import Int
from openmdao.lib.casehandlers.api import ListCaseIterator
from openmdao.main.interfaces import implements, IDOEgenerator
from openmdao.main.api import Container

class Uniform(Container):
    """ DOEgenerator that performs a space-filling Design of Experiments with uniform
    distributions on all design variables. Plugs into the DOEgenerator socket on a 
    DOEdriver."""
    
    implements(IDOEgenerator)
    
    # pylint: disable-msg=E1101
    num_parameters = Int(0, iotype="in", desc="Number of independent "
                                              "parameters in the DOE.")
    num_samples = Int(0, iotype="in", desc="Number of total samples in "
                                              "the DOE.")
    
    def __init__(self, num_samples=None, *args, **kwargs):
    
        super(Uniform, self).__init__(*args, **kwargs)
        
        self.num = 0
        
        if num_samples is not None: 
            self.num_samples = num_samples
        
    def __iter__(self):
        """Return an iterator over our sets of input values"""
        if self.num_samples < 2: 
            raise ValueError("Uniform distributions must have at least 2 samples. num_samples is set to less than 2.")
        return self
                                           
    def next(self):
        if self.num < self.num_samples:
            self.num = self.num+1
            return random.uniform(0,1,self.num_parameters)
        else:
            raise StopIteration()
            
