#
# Test valid CONMIN optimization after saving and loading.
#

import logging
import os
import shutil
import unittest

from openmdao.main import Component
from openmdao.examples.engine_design.engine_optimization \
    import Engine_Optimization


class EngineOptimizationTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Engine_Optimization("Test_Vehicle")

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        self.model.vehicle_sim.bore = 100
        self.model.vehicle_sim.sparkAngle = -35.368341874
        self.model.driver.maxiters = 1

        egg_name = self.model.save_to_egg()
        self.model.pre_delete()
        self.model = None

        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            self.model = Component.load_from_egg(os.path.join('..', egg_name))

            self.model.run()
        
            self.assertAlmostEqual(self.model.vehicle_sim.AccelTime, 
                                   5.9, places=6)
            self.assertAlmostEqual(self.model.vehicle_sim.EPACity, 
                                   25.18837, places=4)
            self.assertAlmostEqual(self.model.vehicle_sim.EPAHighway, 
                                   30.91469, places=4)
        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)
            if egg_name and os.path.exists(egg_name):
                os.remove(egg_name)


if __name__ == '__main__':
    unittest.main()
 
