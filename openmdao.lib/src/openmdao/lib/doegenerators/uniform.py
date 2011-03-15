""" DOEgenerator that performs a uniform space-filling Design of Experiments. Plugs
into the DOEgenerator socket on a DOEdriver."""

# pylint: disable-msg=E0611,F0401
from numpy import linspace,random
from enthought.traits.api import HasTraits
from openmdao.lib.datatypes.api import implements, Int
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator
from openmdao.main.interfaces import IDOEgenerator

class Uniform(HasTraits):
    """ DOEgenerator that performs a space-filling Design of Experiments with uniform
    distributions on all design variables. Plugs into the DOEgenerator socket on a 
    DOEdriver."""
    
    implements(IDOEgenerator)
    
    # pylint: disable-msg=E1101
    num_parameters = Int(0, iotype="in", desc="number of independent "
                                              "parameters in the DOE")
    num_samples = Int(0, iotype="in", desc="number of total samples in "
                                              "the DOE")
    
    def __init__(self, num_samples=None, *args, **kwargs):
    
        super(Uniform, self).__init__(*args, **kwargs)
        
        self.num = 0
        
        if num_samples is not None: 
            self.num_samples = num_samples
        
    def __iter__(self):
        """Return an iterator over our sets of input values"""
        return self
                                           
    def next(self):
        if self.num < self.num_samples:
            self.num = self.num+1
            return random.uniform(0,1,self.num_parameters)
        else:
            raise StopIteration()
            