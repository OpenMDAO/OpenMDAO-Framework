""" DOEgenerator that performs a central composite Design of Experiments. Plugs
into the DOEgenerator socket on a DOEdriver."""

from itertools import product, permutations, chain

# pylint: disable-msg=E0611,F0401
from numpy import linspace
from enthought.traits.api import HasTraits
from openmdao.lib.datatypes.api import implements, Int, Float, Enum

from openmdao.main.interfaces import IDOEgenerator


class CentralComposite(HasTraits):
    """ DOEgenerator that performs a central composite Design of Experiments. Plugs
    into the DOEgenerator socket on a DOEdriver."""
    
    implements(IDOEgenerator)
    
    # pylint: disable-msg=E1101
    num_parameters = Int(0, iotype="in", desc="Number of independent parameters in the DOE.")
    type = Enum("Face-Centered", ["Face-Centered", "Inscribed"], iotype="in", desc="Type of central composite design")
    alpha = Float(0.0, iotype="in", desc="Scaling factor which sets the location of the axial (star) points")
    center_points = Int(1, iotype="in", desc="Number of center points to be added.")
    
    def __init__(self, type="Face-Centered", *args, **kwargs):
        
        super(CentralComposite, self).__init__(*args, **kwargs)
        
        self.type = type
        
    def __iter__(self):
        """Return an iterator over our sets of input values."""

        if self.type == "Face-Centered":
            self.alpha = 1.0
        if self.type == "Inscribed":
            self.alpha = self.num_parameters**0.5

        return chain(product(*[[0.5-0.5/max(self.alpha,1.),0.5+0.5/max(self.alpha,1.)] for i in range(self.num_parameters)]), \
                     set(permutations([0.5-0.5*min(self.alpha,1.)]+(self.num_parameters-1)*[0.5])), \
                     set(permutations([0.5+0.5*min(self.alpha,1.)]+(self.num_parameters-1)*[0.5])), \
                     self.center_points*[self.num_parameters*[0.5]])