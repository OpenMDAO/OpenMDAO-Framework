
from numpy import array, float32

import sys
import numpy as np

from pyV3D.sender import WV_Sender

from openmdao.main.interfaces import IParametricGeometry, implements


class GeoMACHParametricGeometry(object):
    """A wrapper for a GeoMACH object with modifiable parameters.  This object
    implements the IParametricGeometry interface.
    """

    implements(IParametricGeometry)

    def __init__(self, modpath):
        parts = modpath.split('.')
        i = len(parts)-1
        mod = None
        while(i>0):
            mpath = '.'.join(parts[:i])
            __import__(mpath)
            mod = sys.modules.get(mpath)
            if mod:
                break
            i -= 1
        if mod is None:
            raise RuntimeError("couldn't locate class %s" % modpath)
        obj = mod
        for j in range(i, len(parts)):
            obj = getattr(obj, parts[j])
        self._model = obj()

    def regenModel(self):
        if self._model is not None:
            try:
                return self._model.regenerate()
            except Exception as err:
                raise RuntimeError("Error regenerating model: %s" % str(err))

    def listParameters(self):
        """Return a list of parameters (inputs and outputs) for this model.
        """
        if self._model is not None:
            return self._model.listParams()
        else:
            return []

    def setParameter(self, name, val):
        """Set new value for a driving parameter.

        """
        if self._model is not None:
            try:
                self._model.setParam(name, val)
            except Exception as err:
                raise RuntimeError("Error setting parameter '%s': %s" % (name, str(err)))
        else:
            raise RuntimeError("Error setting parameter: no model")

    def getParameter(self, name):
        """Get info about a Parameter in a Model"""
        if self._model is not None:
            return self._model.getParam(name)
        else:
            raise RuntimeError("Error getting parameter: no model")

    def get_attributes(self, io_only=True):
        """Return an attribute dict for use by the openmdao GUI.
        """
        
        return {
            'type': type(self).__name__,
            # 'Inputs': [
            #     {
            #         'name': 'model_file',
            #         'id': 'model_file',
            #         'type': type(self._model_file).__name__,
            #         'value': self._model_file,
            #         'connected': '',
            #     }
            # ]
        }

    def get_geometry(self):
        if self._model is not None:
            geom = GeoMACHGeometry()
            geom._model = self._model
            return geom
        return None


class GeoMACHGeometry(object):
    '''A wrapper for a GeoMACH object that respresents a specific instance of a
    geometry at a designated set of parameters. Parameters are not modifiable.
    This object is able to provide data for visualization.
    '''  
        
    def get_visualization_data(self, wv):
        '''Fills the given WV_Wrapper object with data for faces,
        edges, colors, etc.
        
        wv: WV_Wrapper object
 
        '''
        if self._model is None:
            return []

        xyzs = self._model.oml0.P0[:,:3]
        tris = self._model.oml0.exportPtri()

        mins = np.min(xyzs, axis=0)
        maxs = np.max(xyzs, axis=0)

        box = [mins[0], mins[1], mins[2], maxs[0], maxs[1], maxs[2]]

        print 'xyz shape = %s' % list(xyzs.shape)
        #xyzs = xyzs.astype(np.float32).flatten(order='C')

        print 'len(tris) = ',len(tris)

        for i,tri in enumerate(tris):
            min_idx = int(np.min(tri))
            max_idx = int(np.max(tri))
            #print "type = %s" % type(tri[0,0])
            #print 'i: %d    len(tri): %d' % (i,len(tri))
            new_a = xyzs[min_idx:max_idx+1]
            new_tri = tri - min_idx

            wv.set_face_data(new_a.astype(np.float32).flatten(order='C'), 
                             new_tri.astype(np.int32).flatten(order='C'), bbox=box, name="oml_surf%d" % i)

            # for j in range(1, nedge+1):
            #     points = drep.getDiscrete(i+1, j)
            #     if len(points) < 2:
            #         continue
            #     wv.set_edge_data(points.astype(np.float32).flatten(),
            #                     bbox=box,
            #                     name="Body %d Edge %d"%(i+1,j))


class GeoMACHSender(WV_Sender):

    def initialize(self, **kwargs):
        eye    = array([0.0, 0.0, 7.0], dtype=float32)
        center = array([0.0, 0.0, 0.0], dtype=float32)
        up     = array([0.0, 1.0, 0.0], dtype=float32)
        fov   = 30.0
        zNear = 1.0
        zFar  = 10.0

        bias  = 0
        self.wv.createContext(bias, fov, zNear, zFar, eye, center, up)

    @staticmethod
    def supports(obj):
        return isinstance(obj, GeoMACHGeometry) or isinstance(obj, GeoMACHParametricGeometry)

    def geom_from_obj(self, obj):
        if isinstance(obj, GeoMACHParametricGeometry):
            obj = obj.get_geometry()
            if obj is None:
                raise RuntimeError("can't get Geometry object from GeoMACHParametricGeometry")
        elif not isinstance(obj, GeoMACHGeometry):
            raise TypeError("object must be a GeoMACHParametricGeometry or GeoMACHGeometry but is a '%s' instead" %
                str(type(obj)))
        obj.get_visualization_data(self.wv)

