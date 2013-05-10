# pylint: disable-msg=C0111,C0103

import math
import shutil
import os
import tempfile

import unittest
from nose import SkipTest

from openmdao.main.vartree import VariableTree
from openmdao.main.datatypes.api import Enum, Python
from openmdao.lib.components.geomcomp import GeomComponent
from openmdao.lib.geometry.box import BoxParametricGeometry
from openmdao.util.fileutil import onerror

import numpy

class GeomCompTestCase(unittest.TestCase):

    def setUp(self):
        self.geomcomp = GeomComponent()
        self.tdir = tempfile.mkdtemp()
        self.base_inputs = set(self.geomcomp.list_inputs())
        self.base_outputs = set(self.geomcomp.list_outputs())

    def tearDown(self):
        shutil.rmtree(self.tdir, onerror=onerror)

    def test_auto_run_with_box(self): 
    
        self.geomcomp.add('parametric_geometry',BoxParametricGeometry())  

        self.geomcomp.height = 2
        self.geomcomp.auto_run = False
        self.geomcomp.run()

        self.assertEquals(self.geomcomp.volume,8)

        self.geomcomp.height = 10  

        #check that is has not run yet, and that volume has not changed
        self.assertFalse(self.geomcomp.is_valid())
        self.assertEquals(self.geomcomp.volume,8)

        self.geomcomp.run()
        self.assertTrue(self.geomcomp.is_valid())
        self.assertEquals(self.geomcomp.volume,40)


        self.geomcomp.auto_run = True
        self.geomcomp.height = 2
        self.assertTrue(self.geomcomp.is_valid())
        self.assertEquals(self.geomcomp.volume,8)

        #make sure setting back to false works
        self.geomcomp.auto_run = False
        self.geomcomp.height = 10 
        self.assertFalse(self.geomcomp.is_valid())
        self.assertEquals(self.geomcomp.volume,8)

    def test_with_pygem_diamond(self):
        try:
            from openmdao.lib.geometry.diamond import GEMParametricGeometry
        except ImportError:
            raise SkipTest("pygem_diamond is not installed")
        self.geomcomp.add('parametric_geometry', GEMParametricGeometry())
        base_ins = set(self.geomcomp.list_inputs())
        base_outs = set(self.geomcomp.list_outputs())

        gem_outs = ['zcg', 'zmax', 'xcg', 'zmin', 'Ixz',
            'Izx', 'Ixx', 'Ixy', 'xmin', 'Izy', 'Izz', 'ymin', 'ibody',
            'ymax', 'nnode', 'ycg', 'nface', 'volume', 'Iyy', 'Iyx', 'Iyz',
            'area', 'nedge', 'xmax']

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
        # default outputs that OpenCSM calculates for you
        expected_outs = set(['baseHt']+gem_outs)
        expected_ins = set(['width', 'depth', 'height', 'neckDiam',
                          'neckHeight', 'wall', 'filRad1', 'filRad2'])
        self.assertEquals(set(self.geomcomp.list_inputs()) - base_ins, expected_ins)
        self.assertEquals(set(self.geomcomp.list_outputs()) - base_outs, expected_outs)

        # now do a simple box solid with a sphere subtracted out of it
        csm_input = """
despmtr   radius     0.5
despmtr   side       2.0

set   sph_dist   side/2.0

box            0.0  0.0  0.0    side      side      side
sphere        sph_dist     sph_dist      sph_dist      radius
subtract

end
        """
        model_file = os.path.join(self.tdir, 'bool.csm')
        with open(model_file, 'w') as f:
            f.write(csm_input)
        self.geomcomp.parametric_geometry.model_file = model_file

        expected_outs = set(['sph_dist']+gem_outs)
        expected_ins = set(['radius', 'side'])
        self.assertEquals(set(self.geomcomp.list_inputs())-base_ins, expected_ins)
        self.assertEquals(set(self.geomcomp.list_outputs())-base_outs, expected_outs)

        comp = self.geomcomp

        self.assertEqual(comp.side, 2.0)
        self.assertEqual(comp.radius, 0.5)
        self.assertEqual(comp.sph_dist, self.geomcomp.side/2.0)

        # volume should be box volume - sphere volume
        self.assertAlmostEqual(comp.volume, comp.side**3.0 - 4./3.*math.pi*comp.radius**3.0)

        # test a set/rebuild
        comp.side = 3.0
        comp.radius = 0.25
        comp.run()
        self.assertEqual(comp.side, 3.0)
        self.assertEqual(comp.radius, 0.25)
        self.assertEqual(comp.sph_dist, self.geomcomp.side/2.0)
        self.assertAlmostEqual(comp.volume, comp.side**3.0 - 4./3.*math.pi*comp.radius**3.0)

    # def test_with_pygem_quartz(self):
    #     try:
    #         from pygem_quartz.pygem import GEMParametricGeometry
    #     except ImportError:
    #         raise SkipTest("pygem_quartz is not installed")
    #     self.geomcomp.add('parametric_geometry', GEMParametricGeometry())
    #     self.geomcomp.parametric_geometry.model_file = self.model_file
    #     self.def_outs = []
    #     # add test here...

    def test_with_box(self):
        self.geomcomp.add('parametric_geometry', 
                          BoxParametricGeometry())
        ins = set(self.geomcomp.list_inputs()) - self.base_inputs
        outs = set(self.geomcomp.list_outputs()) - self.base_outputs
        self.assertEqual(ins, set(['height']))
        self.assertEqual(outs, set(['volume']))
        height = self.geomcomp.height
        volume = self.geomcomp.volume
        self.assertEqual(volume, 2*2*height)
        self.geomcomp.height = 5
        self.geomcomp.run()
        self.assertEqual(self.geomcomp.volume, 2*2*5.)

    def test_with_vartrees(self):
        class VTBoxParametricGeometry(BoxParametricGeometry):
            def _update_meta(self):
                newmeta = {
                    'myvt.f': {
                        'iotype': 'in',
                        'value': 3.14,
                    },
                    'myvt.subvt.f': {
                        'iotype': 'in',
                        'value': 11.1,
                    },
                    'myvt.subvt.i': {
                        'iotype': 'in',
                        'value': 4,
                    },
                    'myvt.subvt.s': {
                        'iotype': 'in',
                        'value': "foo",
                    },
                    'myvt.subvt.arr': {
                        'iotype': 'in',
                        'value': numpy.array([1., 2., 3., 4.]),
                    },
                    'myvt.subvt.en': {
                        'iotype': 'in',
                        'type': 'enum',
                        'values': ["yes", "no"],
                        'value': 'yes',
                    },
                    'fproduct': {
                        'iotype': 'out',
                        'value': 3.14 * 11.1,
                    },
                    'concat': {
                        'iotype': 'out',
                        'value': 'fooyes'
                    },
                    'bogus': {
                        'iotype': 'out',
                        'type': 'flarn',  # this should cause a warning and revert trait to a Python trait
                        'value': object(),
                    }
                }
                self.meta.update(newmeta)                

            def regen_model(self):
                super(VTBoxParametricGeometry, self).regen_model()
                if 'myvt.f' not in self.meta:
                    self._update_meta()

                self.meta['fproduct']['value'] = self.meta['myvt.f']['value']*self.meta['myvt.subvt.f']['value']
                self.meta['concat']['value'] = self.meta['myvt.subvt.s']['value']+self.meta['myvt.subvt.en']['value']

        self.geomcomp.add('parametric_geometry', VTBoxParametricGeometry())
        ins = set(self.geomcomp.list_inputs()) - self.base_inputs
        outs = set(self.geomcomp.list_outputs()) - self.base_outputs

        # are expected inputs and outputs here?
        self.assertEqual(ins, set(["height", "myvt"]))
        self.assertEqual(outs, set(["volume", "fproduct", "concat", "bogus"]))

        # check types of stuff
        self.assertTrue(isinstance(self.geomcomp.myvt, VariableTree))
        self.assertTrue(isinstance(self.geomcomp.myvt.subvt, VariableTree))
        self.assertTrue(isinstance(self.geomcomp.myvt.subvt.arr, numpy.ndarray))
        self.assertTrue(isinstance(self.geomcomp.myvt.subvt.en, basestring))
        self.assertTrue(isinstance(self.geomcomp.myvt.subvt.trait('en').trait_type, Enum))
        self.assertTrue(isinstance(self.geomcomp.trait('bogus').trait_type, Python))

        # set some vartree values and see if they update in the geometry
        self.geomcomp.myvt.f = 6.
        self.geomcomp.myvt.subvt.f = 7.

        self.geomcomp.run()

        self.assertEqual(self.geomcomp.fproduct, 42.)

        # see if enum is limited to allowed values
        try:
            self.geomcomp.myvt.subvt.en = "foo"
        except Exception as err:
            self.assertEqual("myvt.subvt: Variable 'en' must be in ['yes', 'no'], but a value of foo <type 'str'> was specified.", 
                             str(err))

if __name__ == "__main__":
    unittest.main()
