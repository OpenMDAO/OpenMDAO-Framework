#
# Test for vehicle.py and its components
#

import unittest

from openmdao.main import Assembly
from openmdao.examples.engine_design.vehicle import Vehicle


class VehicleTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Assembly('top')
        Vehicle("Test_Vehicle", parent=self.model)

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_runvehicle(self):
        
        self.model.set('Test_Vehicle.CurrentGear', 3)
        self.model.set('Test_Vehicle.Velocity', 60.0)
        self.model.set('Test_Vehicle.Throttle', .2)
        self.model.set('Test_Vehicle.Cf', .01)
        self.model.run()
        
        self.assertAlmostEqual(self.model.get('Test_Vehicle.Acceleration'), 
                               0.450554819193, places=5)
        self.assertAlmostEqual(self.model.get('Test_Vehicle.FuelBurn'), 
                               0.00236333261766, places=5)        
        self.assertAlmostEqual(self.model.get('Test_Vehicle.Engine.Torque'), 
                               81.7322022986, places=5)        
        self.assertAlmostEqual(self.model.get('Test_Vehicle.Engine.RPM'), 
                               3216.9984, places=5)        

        
if __name__ == "__main__":
    unittest.main()
        