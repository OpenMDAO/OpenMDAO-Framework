import os
import struct
import logging

from numpy import array, float32
import numpy as np

from openmdao.main.interfaces import IStaticGeometry, implements


def dbg(*args):
    for arg in args:
        logging.error(arg)
    

class STLGeometryObject(object):
    '''This is an object that follows the IStaticGeometry interface.
    '''

    implements(IStaticGeometry)
    
    def __init__(self, filename):
    
        self.filename = filename
        self.geom_name = os.path.basename(filename)[:-4]
        
    def get_visualization_data(self, wv, *args, **kwargs):
        '''Load a tesselation from a geometry model.
        
        wv: WV_Wrapper instance
            The pyV3D WV_Wrapper object
        '''
        
        dbg(' reading %r', self.filename)
        
        # Determine if we are binary or ascii
        binary = False
        with open(self.filename, 'rU') as stl:
            for line in stl:
        
                line = line.strip()
                if not line:
                    continue
                    
                fields = line.split()
                if fields[0] not in ('solid'):
                    binary = True
                
                break
                
        if binary:
            with open(self.filename, 'rb') as stl:
                dbg(' reading binary STL file %r', self.filename)
                self._load_binary(wv, stl)
        else:
            with open(self.filename, 'rU') as stl:
                dbg(' reading ascii STL file %r', self.filename)
                self._load_ascii(wv, stl)
            
    def _load_ascii(self, wv, stl):
        '''Load from ascii STL file.'''
        
        vertices = []
        normals = []
        nsolid = 0
        
        for line in stl:
        
            line = line.strip()
            if not line:
                continue
            fields = line.split()
            
            if fields[0] in ('solid', 'outer',
                             'endloop', 'endfacet'):
                continue
                
            elif fields[0] == 'facet':
            
                # Replicate normal for each vertex.
                normal = [float(xyz) for xyz in fields[2:]]
                normals.extend(normal)
                normals.extend(normal)
                normals.extend(normal)
                
            elif fields[0] == 'vertex':
                vertices.extend([float(xyz) for xyz in fields[1:]])
            
            # Finish with this solid and prepare for next one.
            elif fields[0] == 'endsolid':
            
                nver = len(vertices)
                ntri = nver/3
                
                # Determine bounding box.
                min_x = max_x = vertices[0]
                min_y = max_y = vertices[1]
                min_z = max_z = vertices[2]
                for i in range(ntri):
                    min_x = min(min_x, vertices[i*3])
                    max_x = max(max_x, vertices[i*3])
                    min_y = min(min_y, vertices[i*3+1])
                    max_y = max(max_y, vertices[i*3+1])
                    min_z = min(min_y, vertices[i*3+2])
                    max_z = max(max_y, vertices[i*3+2])
                    
                box = [max_x, max_y, max_z, min_x, min_y, min_z]
                    
                nsolid += 1
                wv.set_face_data(np.array(vertices, dtype=np.float32),
                                 np.array(range(1, ntri+1), dtype=np.int32),
                                 None,
                                 np.array(normals, dtype=np.float32), 
                                 bbox=box,
                                 name="%s_solid%d"%(self.geom_name, nsolid))
                    
                dbg(' added gprim with %d vertices' % (len(vertices)/3))
                             
                normals = []
                vertices = []
        
            else:
                dbg(' ignoring %r', line)        
                    
    def _load_binary(self, wv, stl):
        '''Load from binary STL file.'''
        
        BINARY_HEADER ="80sI"
        BINARY_FACET = "12fH"
        
        vertices = []
        normals = []
        
        header, ntri = struct.unpack(BINARY_HEADER, stl.read(84))

        def remove_non_ascii(s): 
            return "".join(i for i in s if ord(i)<128)

        header = remove_non_ascii(header)
        dbg(header)
        dbg(ntri)
        
        for i in xrange(0, ntri):
            facet = struct.unpack(BINARY_FACET, stl.read(50))

            normal = [float(xyz) for xyz in facet[0:3]]
            normals.extend(normal)
            normals.extend(normal)
            normals.extend(normal)
            
            vertices.extend([float(xyz) for xyz in facet[3:12]])

        # Determine bounding box.
        min_x = max_x = vertices[0]
        min_y = max_y = vertices[1]
        min_z = max_z = vertices[2]
        for i in range(ntri*3):
            min_x = min(min_x, vertices[i*3])
            max_x = max(max_x, vertices[i*3])
            min_y = min(min_y, vertices[i*3+1])
            max_y = max(max_y, vertices[i*3+1])
            min_z = min(min_y, vertices[i*3+2])
            max_z = max(max_y, vertices[i*3+2])
            
        box = [max_x, max_y, max_z, min_x, min_y, min_z]
            
        wv.set_face_data(np.array(vertices, dtype=np.float32),
                         np.array(range(1, 3*ntri+1), dtype=np.int32),
                         None,
                         np.array(normals, dtype=np.float32), 
                         bbox=box,
                         name=header)
            
        dbg(' added gprim with %d vertices' % (len(vertices)/3))
                             

try:
    from pyV3D.handlers import WV_Sender
except ImportError:
    pass
else:
    class STLSender(WV_Sender):
        def initialize(self, **kwargs):
            eye    = array([0.0, 0.0, 7.0], dtype=float32)
            center = array([0.0, 0.0, 0.0], dtype=float32)
            up     = array([0.0, 1.0, 0.0], dtype=float32)
            fov   = 30.0
            zNear = 1.0
            zFar  = 10.0

            bias  = 1
            self.wv.createContext(bias, fov, zNear, zFar, eye, center, up)

        @staticmethod
        def supports(obj):
            return isinstance(obj, basestring) and obj.lower().endswith('.stl')

        def geom_from_file(self, fname):
            geom = STLGeometryObject(fname)
            geom.get_visualization_data(self.wv, angle=15., relSide=.02, relSag=.001)

