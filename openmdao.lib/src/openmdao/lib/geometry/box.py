

from numpy import array, float32, int32

from openmdao.main.geom import ParametricGeometry
from openmdao.main.interfaces import IStaticGeometry, implements

from pyV3D.sender import WV_Sender


class BoxParametricGeometry(ParametricGeometry):
    """A simple parametric geometry (a box)"""

    def __init__(self):
        super(BoxParametricGeometry, self).__init__()
        self.meta = {
           'height': { 'iotype': 'in', 'value': 2. }, 
           'volume': { 'iotype': 'out', 'value': 2.*2.*2. },
        }

        self.vertices = self.get_vertices()

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
        self.triangles = array([0, 1, 2,   0, 2, 3], dtype=int32)

        self.bbox = [-1.,-1.,-1.,1.,1.,1.]
              
        self.regen_model()


    def get_vertices(self):
        h = self.meta['height']['value']
        x = h/2.
        self.meta['volume']['value'] = 2 * 2 * h
        self.bbox = [-1, -1, -x, 1, 1, x]

        # box  (2 x 2 x height)
        #    v6----- v5
        #   /|      /|
        #  v1------v0|
        #  | |     | |
        #  | |v7---|-|v4
        #  |/      |/
        #  v2------v3
        #
        # vertex coords arrays
        return array([
          [ 1, 1, x,  -1, 1, x,  -1,-1, x,   1,-1, x],  # v0-v1-v2-v3 front
          [ 1, 1, x,   1,-1, x,   1,-1,-x,   1, 1,-x],  # v0-v3-v4-v5 right
          [ 1, 1, x,   1, 1,-x,  -1, 1,-x,  -1, 1, x],  # v0-v5-v6-v1 top
          [-1, 1, x,  -1, 1,-x,  -1,-1,-x,  -1,-1, x],  # v1-v6-v7-v2 left
          [-1,-1,-x,   1,-1,-x,   1,-1, x,  -1,-1, x],  # v7-v4-v3-v2 bottom
          [ 1,-1,-x,  -1,-1,-x,  -1, 1,-x,   1, 1,-x],  # v4-v7-v6-v5 back
        ], dtype=float32)

    def regen_model(self):
        self.vertices = self.get_vertices()

    def list_parameters(self):
        """Return a list of parameters (inputs and outputs) for this model.
        """
        return self.meta.items()

    def set_parameter(self, name, val):
        """Set new value for a driving parameter.

        """
        if name in self.meta and self.meta[name]['iotype'] == 'in':
            self.meta[name]['value'] = val
        else:
            raise RuntimeError("%s is not an input parameter" % name)

    def get_parameters(self, names):
        """Get parameter values"""
        return [self.meta[n]['value'] for n in names if n in self.meta]

    def get_static_geometry(self):
        return BoxGeometry(self)


class BoxGeometry(object):
    '''A static (non-parametric) box geometry.'''

    implements(IStaticGeometry)
    
    def __init__(self, parametric_geom):
        self.parametric_geom = parametric_geom

    def get_visualization_data(self, wv):
        '''Fills the given WV_Wrapper object with data for faces,
        edges, colors, etc.
        
        wv: WV_Wrapper object 
        '''
        pgeom = self.parametric_geom

        for i in range(6):  # 6 faces
            wv.set_face_data(points=pgeom.vertices[i], 
                             tris=pgeom.triangles, 
                             colors=pgeom.colors[i],
                             #normals=pgeom.normals[i],
                             bbox=pgeom.bbox,
                             name="Face %d"%(i+1))

            wv.set_edge_data(points=pgeom.vertices[i],
                             bbox=pgeom.bbox,
                             name="Edge %d"%(i+1))


class BoxSender(WV_Sender):
    """This is just here for demo purposes so that something can be viewed even
    if no real binpub plugins have been installed.
    """

    def initialize(self, **kwargs):
        eye    = array([0.0, 0.0, 7.0], dtype=float32)
        center = array([0.0, 0.0, 0.0], dtype=float32)
        up     = array([0.0, 1.0, 0.0], dtype=float32)
        fov   = 30.0
        zNear = 1.0
        zFar  = 10.0

        bias = 0
        self.wv.createContext(bias, fov, zNear, zFar, eye, center, up)

    @staticmethod
    def supports(obj):
        return isinstance(obj, BoxParametricGeometry) or isinstance(obj, BoxGeometry)

    def geom_from_obj(self, obj):
        if isinstance(obj, BoxParametricGeometry):
            obj = obj.get_static_geometry()
        if not isinstance(obj, BoxGeometry):
            raise TypeError("geometry object must be of type BoxParametricGeometry or BoxGeometry, not %s" % type(obj))
        obj.get_visualization_data(self.wv)



