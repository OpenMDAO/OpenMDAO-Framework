#
# Test for vehicle.py and its components
#

import unittest

from openmdao.main.api import Assembly
from openmdao.examples.engine_design.vehicle import Vehicle


class VehicleTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Assembly()
        self.model.add_container('test_vehicle', Vehicle())

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_runvehicle(self):
        
        self.model.test_vehicle.current_gear = 3
        self.model.test_vehicle.velocity = 60.0
        self.model.test_vehicle.throttle = .2
        self.model.test_vehicle.Cf = .01
        self.model.run()
        
        self.assertAlmostEqual(self.model.test_vehicle.acceleration, 
                               0.450554819193, places=5)
        self.assertAlmostEqual(self.model.test_vehicle.fuel_burn, 
                               0.00236333261766, places=5)        
        self.assertAlmostEqual(self.model.test_vehicle.engine.torque, 
                               81.7322022986, places=5)        
        self.assertAlmostEqual(self.model.test_vehicle.engine.RPM, 
                               3216.9984, places=5)        

        
if __name__ == "__main__":
    unittest.main()
        