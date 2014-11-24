"""
Variable class for passing geometry data.
"""

#public symbols
__all__ = ["GeomData"]

import numpy as np

from openmdao.main.datatypes.array import Array
from openmdao.main.vartree import VariableTree


class GeomData(VariableTree):
    
    points = Array([], desc='nx3 array of point (x,y,z) locations.')
    
    facets = Array([], desc='nx3 (or nx4) integer array of triangle or quad' + 
                   'connectivities.', dtype='int')
    
    def __init__(self, n_point, n_facet, facet_size=3):
        super(GeomData, self).__init__()
        
        if facet_size not in [3, 4]:
            msg = "facet size must be either 3 or 4"
            raise ValueError(msg)
        
        self.points = np.zeros((n_point, 3))
        self.facets = np.zeros((n_facet, facet_size), dtype='int')
