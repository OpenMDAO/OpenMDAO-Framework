""" DOEgenerator that performs a full-factorial Design of Experiments. Plugs
into the DOEgenerator socket on a DOEdriver."""

from itertools import product

# pylint: disable-msg=E0611,F0401
from numpy import linspace
from enthought.traits.api import HasTraits
from openmdao.main.interfaces import implements
from openmdao.lib.datatypes.api import Int

from openmdao.main.interfaces import IDOEgenerator


class FullFactorial(HasTraits):
    """ DOEgenerator that performs a full-factorial Design of Experiments. Plugs
    into the DOEgenerator socket on a DOEdriver."""
    
    implements(IDOEgenerator)
    
    # pylint: disable-msg=E1101
    num_parameters = Int(0, iotype="in", desc="Number of independent "
                                              "parameters in the DOE.")
    num_levels = Int(0, iotype="in", desc="Number of levels of values for "
                                          "each parameter.")
    
    def __init__(self, num_levels=None, *args, **kwargs):
        
        super(FullFactorial, self).__init__(*args, **kwargs)
        
        if num_levels is not None: 
            self.num_levels = num_levels
    
    def __iter__(self):
        """Return an iterator over our sets of input values."""
        
        return product(*[linspace(0., 1., self.num_levels) \
                         for i in range(self.num_parameters)])
        
