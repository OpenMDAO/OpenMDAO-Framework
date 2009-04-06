#
# Test for vehicle.py and its components
#

import unittest

from openmdao.examples.engine_design.vehicle import Vehicle


class VehicleTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Vehicle("Test_Vehicle")

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_runvehicle(self):
        
        self.model.CurrentGear = 3
        self.model.Velocity = 60.0*(26.8224/60.0)
        self.model.Throttle = .2
        self.model.execute()
        
        self.assertAlmostEqual(self.model.Acceleration, 
                               0.414431695282, places=8)
        self.assertAlmostEqual(self.model.FuelBurn, 
                               0.00236333261766, places=8)        
        self.assertAlmostEqual(self.model.Engine.Torque, 
                               81.7322022986, places=8)        
        self.assertAlmostEqual(self.model.Engine.RPM, 
                               3216.9984, places=8)        
        