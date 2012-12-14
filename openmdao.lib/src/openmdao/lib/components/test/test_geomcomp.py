# pylint: disable-msg=C0111,C0103

import unittest
from nose import SkipTest

import shutil
import os
import tempfile

from openmdao.lib.components.geomcomp import GeomComponent
from openmdao.main.api import Component

class GeomCompTestCase(unittest.TestCase):

    def setUp(self):
        self.geomcomp = GeomComponent()
        self.tdir = tempfile.mkdtemp()
        comp = Component()
        self.base_inputs = set(comp.list_inputs())
        self.base_outputs = set(comp.list_outputs())

    def tearDown(self):
        shutil.rmtree(self.tdir)

    def test_with_pygem_diamond(self):
        try:
            from pygem_diamond.pygem import GEMParametricGeometry
        except ImportError:
            raise SkipTest("pygem_diamond is not installed")
        self.geomcomp.add('parametric_geometry', GEMParametricGeometry())

        csm_input = """
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

        model_file = os.path.join(self.tdir, 'bottle.csm')
        with open(model_file, 'w') as f:
            f.write(csm_input)
        self.geomcomp.parametric_geometry.model_file = model_file
        self._exercise_geom()

    def test_with_pygem_quartz(self):
        try:
            from pygem_quartz.pygem import GEMParametricGeometry
        except ImportError:
            raise SkipTest("pygem_quartz is not installed")
        self.geomcomp.add('parametric_geometry', GEMParametricGeometry())
        self.geomcomp.parametric_geometry.model_file = self.model_file
        self._exercise_geom()

    def _exercise_geom(self):
        inputs = set(self.geomcomp.list_inputs())
        outputs = set(self.geomcomp.list_outputs())
        self.assertEquals(inputs - self.base_inputs, 
            set(['width','depth','height','neckDiam',
                 'neckHeight','wall','filRad1','filRad2']))
        self.assertEquals(outputs - self.base_outputs, set([]))
        
        # # now put a different model in with the same inputs/outputs
        # self.geomcomp.parametric_geometry.model_file = ???
        # inputs = set(self.geomcomp.list_inputs())
        # outputs = set(self.geomcomp.list_outputs())
        # self.assertEquals(inputs, set([]))
        # self.assertEquals(outputs, set([]))

        # # now put a different model in
        # self.geomcomp.parametric_geometry.model_file = ???
        # inputs = set(self.geomcomp.list_inputs())
        # outputs = set(self.geomcomp.list_outputs())
        # self.assertEquals(inputs, set([]))
        # self.assertEquals(outputs, set([]))

       
if __name__ == "__main__":
    unittest.main()


