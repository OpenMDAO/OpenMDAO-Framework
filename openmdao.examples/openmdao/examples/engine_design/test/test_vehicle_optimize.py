#
# Test for Engine_Optimization.py undergoing optimization with CONMIN
#

import unittest

from openmdao.examples.engine_design.engine_optimization import Engine_Optimization


class EngineOptimizationTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Engine_Optimization("Test_Vehicle")

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_runvehicle(self):
        
        # We are just testing the mechanics of the Engine_Optimization component, so
        # set the design variables at the global minimum.
        
        self.model.vehicle_sim.bore = 100
        self.model.vehicle_sim.sparkAngle = -35.368341874

        self.model.driver.maxiters = 1
        
        self.model.execute()
        
        self.assertAlmostEqual(self.model.vehicle_sim.AccelTime, 
                               5.9, places=6)
        self.assertAlmostEqual(self.model.vehicle_sim.EPACity, 
                               25.18837, places=4)
        self.assertAlmostEqual(self.model.vehicle_sim.EPAHighway, 
                               30.91469, places=4)
        