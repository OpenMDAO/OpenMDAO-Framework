""" DOEgenerator that performs a central composite Design of Experiments. Plugs
into the DOEgenerator socket on a DOEdriver."""

from itertools import product, permutations, chain

# pylint: disable-msg=E0611,F0401
from numpy import linspace
from enthought.traits.api import HasTraits
from openmdao.lib.datatypes.api import Int, Float, Enum

from openmdao.main.api import implements
from openmdao.main.interfaces import IDOEgenerator


class CentralComposite(HasTraits):
    """ DOEgenerator that performs a central composite Design of Experiments. Plugs
    into the DOEgenerator socket on a DOEdriver."""
    
    implements(IDOEgenerator)
    
    # pylint: disable-msg=E1101
    num_parameters = Int(0, iotype="in", desc="Number of independent parameters in the DOE.")
    type = Enum("Face-Centered", ["Face-Centered", "Inscribed"], iotype="in", desc="Type of central composite design")
    
    def __init__(self, type="Face-Centered", *args, **kwargs):
        
        super(CentralComposite, self).__init__(*args, **kwargs)
        
        self.type = type
        
    def __iter__(self):
        """Return an iterator over our sets of input values."""

        # Set the number of center points and the alpha parameter based on the type of central composite design
        num_center_points = 1
        if self.type == "Face-Centered":
            alpha = 1.0
        if self.type == "Inscribed":
            alpha = self.num_parameters**0.5  # Based on recommendation from "Response Surface Methodology" by R.H. Myers and D.C. Montgomery
            
        # Form the iterator for the corner points using a 2 level full factorial
        low_corner_val = 0.5-0.5/alpha
        high_corner_val = 0.5+0.5/alpha
        corner_points = product(*[[low_corner_val,high_corner_val] for i in range(self.num_parameters)])
        
        # Form iterators for the face centered points (one for low value faces, one for high value faces)
        center_list = (self.num_parameters-1)*[0.5]
        low_face_points = set(permutations([0.0]+center_list))
        high_face_points = set(permutations([1.0]+center_list))
        
        # Form iterator for center point(s)
        center_points = num_center_points*[self.num_parameters*[0.5]]
        
        # Chain case lists together to get complete iterator
        return chain(corner_points,low_face_points,high_face_points,center_points)