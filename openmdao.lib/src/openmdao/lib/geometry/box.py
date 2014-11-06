from numpy import array, float32, int32

from openmdao.main.api import Component
from openmdao.lib.geometry.geom_data import GeomData
from openmdao.lib.datatypes.api import Float, VarTree


class BoxParametricGeometry(Component):
    """A simple parametric geometry (a box)"""

    height = Float(2, iotype="in")
    volume = Float(8, iotype="in")

    geom_data = VarTree(GeomData(n_point=8, n_facet=6, facet_size=4), iotype="out")

    def __init__(self):
        super(BoxParametricGeometry, self).__init__()

        # color array.  one color per face in this case
        self.colors = array([
               [0, 0, 255],    # v0-v1-v2-v3
               [255, 0, 0],    # v0-v3-v4-v5
               [0, 255, 0],    # v0-v5-v6-v1
               [255, 255, 0],  # v1-v6-v7-v2
               [255, 0, 255],  # v7-v4-v3-v2
               [0, 255, 255],  # v4-v7-v6-v5
        ], dtype=float32)

        # index array
        # each face has 2 triangles
        self.triangles = array([0, 1, 2, 0, 2, 3], dtype=int32)

        self.bbox = [-1.,-1.,-1.,1.,1.,1.]
              
        self.geom_data.facets = array([
          [0,1,2,3], #front
          [0,3,4,5], #right
          [0,5,6,1], #top
          [1,6,7,2], #left
          [7,4,3,2], #bottom
          [4,7,6,5], #back
          ], dtype="int")

   

    def execute(self):
        h = self.height
        x = h/2.

        self.bbox = [-1, -1, -x, 1, 1, x]
        self.volume = 2 * 2 * h

        self.geom_data.points = array([
          [ 1, 1, x], [-1, 1, x], [-1,-1, x], [1,-1, x],   # v0, v1, v2, v3
          [ 1,-1,-x], [ 1, 1,-x], [-1, 1,-x], [-1,-1,-x],  # v4, v5, v6, v7
        ], dtype=float32)


if __name__ == "__main__": 

  box = BoxParametricGeometry()
  box.height = 10
  box.run()
  print box.geom_data.points

