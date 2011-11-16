""" The FullFactorial DOEgenerator implements a full factorial Design of Experiments; that is, it
generates a set of design points that fully span the range of the parameters at the requested
resolution. It plugs into the DOEgenerator socket on a DOEdriver."""

from itertools import product

# pylint: disable-msg=E0611,F0401
# FIXME: adding a numpy dependency just for linspace doesn't seem worth it
try:
    from numpy import linspace
except ImportError:
    pass

from enthought.traits.api import HasTraits
from openmdao.main.interfaces import implements, IDOEgenerator
from openmdao.lib.datatypes.api import Int
from openmdao.util.decorators import stub_if_missing_deps

@stub_if_missing_deps('numpy')
class FullFactorial(HasTraits):
    """ DOEgenerator that performs a full-factorial Design of Experiments. Plugs
    into the DOEgenerator socket on a DOEdriver."""
    
    implements(IDOEgenerator)
    
    # pylint: disable-msg=E1101
    num_parameters = Int(0, iotype="in", desc="Number of independent "
                                              "parameters in the DOE.")
    num_levels = Int(0, iotype="in", desc="Number of levels of values for "
                                          "each parameter.")
    
    def __init__(self, num_levels=0, *args, **kwargs):
        
        super(FullFactorial, self).__init__(*args, **kwargs)
        self.num_levels = num_levels
    
    def __iter__(self):
        """Return an iterator over our sets of input values."""
        
        return product(*[linspace(0., 1., self.num_levels)
                         for i in range(self.num_parameters)])
        
