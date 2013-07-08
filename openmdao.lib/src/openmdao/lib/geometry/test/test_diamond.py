
import os
import tempfile
import shutil

import unittest
import nose

try:
    from openmdao.lib.geometry.diamond import GEMParametricGeometry
except ImportError:
    GEMParametricGeometry = None

class GEMParametricGeometryTestCase(unittest.TestCase):

    def setUp(self):
        if GEMParametricGeometry is None:
            raise nose.SkipTest("pygem_diamond is not installed")

        self.csm_input = """
# bottle2 (from OpenCASCADE tutorial)
# written by John Dannenhoffer

# default design parameters
despmtr   width               10.00
despmtr   depth                4.00
despmtr   height              15.00
despmtr   neckDiam             2.50
despmtr   neckHeight           3.00
despmtr   wall                 0.20     wall thickness (in neck)
despmtr   filRad1              0.25     fillet radius on body of bottle
despmtr   filRad2              0.10     fillet radius between bottle and neck

# basic bottle shape (filletted)

set       baseHt    height-neckHeight

skbeg     -width/2  -depth/4  0
   cirarc 0         -depth/2  0         +width/2  -depth/4  0
   linseg +width/2  +depth/4  0
   cirarc 0         +depth/2  0         -width/2  +depth/4  0
   linseg -width/2  -depth/4  0
skend
extrude   0         0         baseHt
fillet    filRad1

# neck
cylinder  0         0         baseHt    0         0         height      neckDiam/2

# join the neck to the bottle and apply a fillet at the union
union
fillet    filRad2

# hollow out bottle
hollow    wall      18

end
        """
        self.tdir = tempfile.mkdtemp()
        self.model_file = os.path.join(self.tdir, 'bottle.csm')
        with open(self.model_file, 'w') as f:
            f.write(self.csm_input)

    def tearDown(self):
        shutil.rmtree(self.tdir)

    def test_GEMParametricGeometry(self):
        geom = GEMParametricGeometry()
        geom.model_file = self.model_file
        params = geom.list_parameters()
        expected_inputs = set(['width', 'depth', 'height', 
            'neckDiam', 'neckHeight', 'wall',
            'filRad1', 'filRad2'])
        self.assertEqual(expected_inputs, 
            set([k for k,v in params if v['iotype']=='in']))
        expected_outs = set(['zcg', 'zmax', 'xcg', 'zmin', 'Ixz', 'Izx', 'Ixx', 'Ixy', 
                'baseHt', 'xmin', 'Izy', 'Izz', 'ymin', 'ibody1', 'ibody2', 'ymax', 'nnode', 
                'ycg', 'nface', 'volume', 'Iyy', 'Iyx', 'Iyz', 'area', 'nedge', 'xmax', 'iedge', 
                'length', 'iface', 'inode', 'nbody'])
        self.assertEqual(expected_outs, set([k for k,v in params if v['iotype']=='out']))

        vals = geom.get_parameters(['baseHt'])
        baseHt = vals[0]
        self.assertEqual(baseHt, 12.0)
        geom.set_parameter('height', 20.0)
        geom.regen_model()
        vals = geom.get_parameters(['baseHt'])
        baseHt = vals[0]
        self.assertEqual(baseHt, 17.0)
        geom.terminate()

if __name__ == "__main__":
    unittest.main()
