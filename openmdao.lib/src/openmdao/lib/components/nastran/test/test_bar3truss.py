import os
import unittest
import pkg_resources

from openmdao.main.api import SimulationRoot
from openmdao.lib.components.nastran.test.bar3truss.bar3_static_nastran import Bar3Static

ORIG_DIR = os.getcwd()
DIRECTORY = pkg_resources.resource_filename('openmdao.lib.components.nastran', 'test')

class TestBar3Truss(unittest.TestCase):

    def setUp(self):
        SimulationRoot.chroot(DIRECTORY)

    def tearDown(self):
        SimulationRoot.chroot(ORIG_DIR)

    def test_one_iteration(self):

        static = Bar3Static()
        static.delete_tmp_files = False
        static.stdout = "/dev/null"
        static.stderr = "/dev/null"
        static.nastran_filename = "bar3truss/vared_bar3.bdf"
        #static.nastran_command = "/msc/nastran/bin/nastran"
        static.nastran_command = "python fake_nastran.py " + \
           "test_bar3truss_correct_input.bdf test_bar3truss_correct_output.out"

        # set some variables.
        static.bar1_area = 18.88
        static.bar2_area = 45
        static.bar3_area = 2902.55950333333333
        static.load_x_dir = 50000
        static.load_y_dir = 100000
        static.loadmag = 20
        static.Youngs_Modulus = 70000000
        static.weight_density = 0.289

        static.run()

        # these values were gotten by running it with real nastran
        self.assertAlmostEqual(static.bar1_stress, 13585.68)
        self.assertAlmostEqual(static.bar2_stress, 14161.28)
        self.assertAlmostEqual(static.bar3_stress, 575.5993)
        self.assertAlmostEqual(static.displacement_x_dir, -0.01858583)
        self.assertAlmostEqual(static.displacement_y_dir, 0.0202304)
        self.assertAlmostEqual(static.weight, 120702)


if __name__ == "__main__":
    unittest.main()
