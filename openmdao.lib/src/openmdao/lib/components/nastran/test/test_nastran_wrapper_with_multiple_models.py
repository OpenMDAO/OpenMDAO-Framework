"""
  These tests are regression tests for the Nastran
  parser. They use real world Nastran input BDF files.
  These tests actually use Nastran to do the tests. If Nastran
  is not available, the tests are skipped.

  Two of the tests make use of proprietary files that are
  not part of the OpenMDAO distribution. These tests will
  also be skipped if the files are not available.
"""

import os
import sys
import unittest
import pkg_resources

from nose.plugins.skip import SkipTest

from openmdao.main.api import SimulationRoot
from openmdao.lib.components.nastran.test.nastran_models.bar3_static_nastran import Bar3Static
from openmdao.lib.components.nastran.test.nastran_models.bar10_static_nastran import Bar10Static
from openmdao.lib.components.nastran.test.nastran_models.bar25_static_nastran import Bar25Static
from openmdao.lib.components.nastran.test.nastran_models.ring_25dv_static_nastran import RingTruss
from openmdao.lib.components.nastran.test.nastran_models.dome_static_nastran import DomeStatic
from openmdao.lib.components.nastran.test.nastran_models.comp_plate_static_nastran import Comp_Plate
from openmdao.lib.components.nastran.test.nastran_models.blade_2dv_static_nastran import BladeStatic
from openmdao.lib.components.nastran.test.nastran_models.comp_module_static_nastran import Comp_Module

ORIG_DIR = os.getcwd()
DIRECTORY = pkg_resources.resource_filename('openmdao.lib.components.nastran', 'test')

def find_executable(executable, path=None):
    """Try to find 'executable' in the directories listed in 'path' (a
    string listing directories separated by 'os.pathsep'; defaults to
    os.environ['PATH']).  Returns the complete filename or None if not
    found
    """
    if path is None:
        path = os.environ['PATH']
    paths = path.split(os.pathsep)
    extlist = ['']
    if os.name == 'os2':
        (base, ext) = os.path.splitext(executable)
        # executable files on OS/2 can have an arbitrary extension, but
        # .exe is automatically appended if no dot is present in the name
        if not ext:
            executable = executable + ".exe"
    elif sys.platform == 'win32':
        pathext = os.environ['PATHEXT'].lower().split(os.pathsep)
        (base, ext) = os.path.splitext(executable)
        if ext.lower() not in pathext:
            extlist = pathext
    for ext in extlist:
        execname = executable + ext
        if os.path.isfile(execname):
            return execname
        else:
            for p in paths:
                f = os.path.join(p, execname)
                if os.path.isfile(f):
                    return f
    else:
        return None

class TestNastranMultipleModels(unittest.TestCase):


    def setUp(self):
        SimulationRoot.chroot(DIRECTORY)

        self.model = None

    def tearDown(self):
        SimulationRoot.chroot(ORIG_DIR)

    def basic_setup(self,bdf_filepath):

        self.model.nastran_command = "nastran"
        if not find_executable( self.model.nastran_command ) :
            raise SkipTest( "Nastran executable, %s, not found" % \
                            self.model.nastran_command )
        self.model.delete_tmp_files = True
        self.model.stdout = os.devnull
        self.model.stderr = os.devnull
        if os.path.isfile(bdf_filepath):
            self.model.nastran_filename = bdf_filepath
        else:
            raise SkipTest( "BDF file, '%s' not found" % bdf_filepath )

    def test_bar3_static(self):

        self.model = Bar3Static()

        self.basic_setup( "bdf_files/bar3.bdf" )

        # set some variables.
        self.model.bar1_area = 1.0
        self.model.bar2_area = 1.0
        self.model.bar3_area = 1.0

        self.model.run()

        self.assertAlmostEqual(self.model.bar1_stress, 64644.66, places=2)
        self.assertAlmostEqual(self.model.bar2_stress, 58578.64, places=2)
        self.assertAlmostEqual(self.model.bar3_stress, 6066.017,places=3)
        self.assertAlmostEqual(self.model.displacement_x_dir, 0.2357,places=4)
        self.assertAlmostEqual(self.model.displacement_y_dir, -0.1953,places=4)
        self.assertAlmostEqual(self.model.weight, 108.7273,places=4)

    def test_bar10_static(self):

        self.model = Bar10Static()
        self.basic_setup( "bdf_files/bar10.bdf" )

        self.model.run()

        self.assertAlmostEqual(self.model.weight, 4196.4680,places=4)

        self.assertAlmostEqual(self.model.bar1_stress, 19073.0, places=2)
        self.assertAlmostEqual(self.model.bar2_stress, 3024.927, places=2)
        self.assertAlmostEqual(self.model.bar3_stress, 8024.926, places=2)
        self.assertAlmostEqual(self.model.bar4_stress, 6975.074, places=2)
        self.assertAlmostEqual(self.model.bar5_stress, 20927.0, places=2)
        self.assertAlmostEqual(self.model.bar6_stress, 7097.924, places=2)
        self.assertAlmostEqual(self.model.bar7_stress, 15453.12, places=2)
        self.assertAlmostEqual(self.model.bar8_stress, 12831.16, places=2)
        self.assertAlmostEqual(self.model.bar9_stress, 9864.243, places=2)
        self.assertAlmostEqual(self.model.bar10_stress, 4277.892, places=2)
        
        self.assertAlmostEqual(self.model.displacement1_y_dir, 3.7229,places=4)
        self.assertAlmostEqual(self.model.displacement2_y_dir, 4.0118,places=4)

    def test_bar25_static(self):

        self.model = Bar25Static()
        self.basic_setup( "bdf_files/bar25.bdf" )

        self.model.run()

        self.assertAlmostEqual(self.model.weight, 3307.2070,places=4)

        self.assertAlmostEqual(self.model.bar1_stress, .0, places = 2 )
        self.assertAlmostEqual(self.model.bar2_stress,  69.37414, places = 2 )
        self.assertAlmostEqual(self.model.bar3_stress,  260.6976, places = 2 )
        self.assertAlmostEqual(self.model.bar4_stress,  1337.964, places = 2 )
        self.assertAlmostEqual(self.model.bar5_stress,  139.9831, places = 2 )
        self.assertAlmostEqual(self.model.bar6_stress,  1458.678, places = 2 )
        self.assertAlmostEqual(self.model.bar7_stress,  1486.055, places = 2 )
        self.assertAlmostEqual(self.model.bar8_stress,  1921.267, places = 2 )
        self.assertAlmostEqual(self.model.bar9_stress,  213.3464, places = 2 )
        self.assertAlmostEqual(self.model.bar10_stress,  114.5576, places = 2 )
        self.assertAlmostEqual(self.model.bar11_stress,  768.671, places = 2 )
        self.assertAlmostEqual(self.model.bar12_stress,  61.35138, places = 2 )
        self.assertAlmostEqual(self.model.bar13_stress,  253.6606, places = 2 )
        self.assertAlmostEqual(self.model.bar14_stress,  66.10903, places = 2 )
        self.assertAlmostEqual(self.model.bar15_stress,  1109.566, places = 2 )
        self.assertAlmostEqual(self.model.bar16_stress,  973.566, places = 2 )
        self.assertAlmostEqual(self.model.bar17_stress,  214.2616, places = 2 )
        self.assertAlmostEqual(self.model.bar18_stress,  438.6087, places = 2 )
        self.assertAlmostEqual(self.model.bar19_stress,  1040.009, places = 2 )
        self.assertAlmostEqual(self.model.bar20_stress,  509.1378, places = 2 )
        self.assertAlmostEqual(self.model.bar21_stress,  1090.343, places = 2 )
        self.assertAlmostEqual(self.model.bar22_stress,  5.948054, places = 2 )
        self.assertAlmostEqual(self.model.bar23_stress,  921.9872, places = 2 )
        self.assertAlmostEqual(self.model.bar24_stress,  525.2112, places = 2 )
        self.assertAlmostEqual(self.model.bar25_stress,  666.3184, places = 2 )

        self.assertAlmostEqual(self.model.displacement1_x_dir, 0.0021,places=4)
        self.assertAlmostEqual(self.model.displacement1_y_dir, -0.0287,places=4)
        self.assertAlmostEqual(self.model.displacement2_x_dir, 0.0026,places=4)
        self.assertAlmostEqual(self.model.displacement2_y_dir, -0.1085,places=4)

    def test_ring_truss(self):

        self.model = RingTruss()
        self.basic_setup( "bdf_files/ring_25dv.bdf" )

        self.model.run()

        self.assertAlmostEqual(self.model.weight, 250.2978,places=4)

        max_stress = 0.0
        for i in range(1,61):
            max_cmd = "max_stress = max(max_stress, self.model.bar%d_stress)" %i
            exec(max_cmd)


        self.assertAlmostEqual(max_stress, 18194.14, places = 2 )

        self.assertAlmostEqual(self.model.displacement1_x_dir, 0.0, places=4)
        self.assertAlmostEqual(self.model.displacement1_y_dir, 0.0, places=4)
        self.assertAlmostEqual(self.model.displacement2_x_dir, -0.0049,places=4)
        self.assertAlmostEqual(self.model.displacement2_y_dir, 2.7275,places=4)
        self.assertAlmostEqual(self.model.displacement3_x_dir, 1.5735,places=4)
        self.assertAlmostEqual(self.model.displacement3_y_dir, 1.3559,places=4)
        self.assertAlmostEqual(self.model.displacement4_x_dir, -1.6284,places=4)
        self.assertAlmostEqual(self.model.displacement4_y_dir, 1.3717,places=4)

    def test_geodesic_dome(self):

        self.model = DomeStatic()
        self.basic_setup( "bdf_files/dome.bdf" )

        self.model.run()

        self.assertAlmostEqual(self.model.weight, 11833.9400,places=4)

        max_stress_bar = 0.0
        max_stress_tria = 0.0
        for i in range(1,157):
            max_cmd = "max_stress_bar = max(max_stress_bar, self.model.bar%d_stress)" %i
            exec(max_cmd)
            
        for i in range(157,253):
            max_cmd = "max_stress_tria = max(max_stress_tria, self.model.tria%d_stress)" %i
            exec(max_cmd)

        self.assertAlmostEqual(max_stress_bar, 1018.785, places = 2 )
        self.assertAlmostEqual(max_stress_tria, 0.0, places = 2 )

    def test_composite_plate(self):

        self.model = Comp_Plate()
        self.basic_setup( "bdf_files/comp_plate.bdf" )

        self.model.run()

        self.assertAlmostEqual(self.model.weight, 0.1343, places=4)

        self.assertAlmostEqual(self.model.property1_max_major_strain, 0.0071, places = 2 )
        self.assertAlmostEqual(self.model.property2_max_major_strain, 0.0046, places = 2 )
        self.assertAlmostEqual(self.model.property3_max_major_strain, 0.0062, places = 2 )
        self.assertAlmostEqual(self.model.property1_max_minor_strain, 0.0071, places = 2 )
        self.assertAlmostEqual(self.model.property2_max_minor_strain, 0.0046, places = 2 )
        self.assertAlmostEqual(self.model.property3_max_minor_strain, 0.0062, places = 2 )

        self.assertAlmostEqual(self.model.displacement_18_z_dir, -0.0418, places = 2 )

    def test_composite_blade(self):

        self.model = BladeStatic()
        self.basic_setup( "bdf_files/blade_2dv.bdf" )

        self.model.run()

        self.assertAlmostEqual(self.model.weight, 0.1221, places=4)
        
        self.assertAlmostEqual(self.model.group1_stress, 431315.7000, places = 2 )
        self.assertAlmostEqual(self.model.group2_stress, 793400.2000, places = 2 )
        
        self.assertAlmostEqual(self.model.displacement_z_dir, 0.1633, places = 2 )

    def test_composite_module(self):

        self.model = Comp_Module()
        self.basic_setup( "bdf_files/comp_module.bdf" )

        self.model.run()

        self.assertAlmostEqual(self.model.weight, 23.4387, places=4)

if __name__ == "__main__":
    unittest.main()
