#
# Test for driving_sim.py and its components
#

import unittest

from openmdao.main.api import Assembly, set_as_top
from openmdao.examples.enginedesign.vehicle import Vehicle
from openmdao.examples.enginedesign.driving_sim import SimAcceleration, SimEconomy


class VehicleTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        
        self.top = set_as_top(Assembly())

        self.top.add('sim_acc', SimAcceleration())
        self.top.add('sim_EPA_city', SimEconomy())
        self.top.add('vehicle', Vehicle())
        
        self.top.driver.workflow.add('sim_acc')
        self.top.driver.workflow.add('sim_EPA_city')
        self.top.driver.workflow.add('sim_EPA_highway')
        
        self.top.sim_acc.workflow.add('vehicle')
        self.top.sim_acc.velocity_str = 'vehicle.velocity'
        self.top.sim_acc.throttle_str = 'vehicle.throttle'
        self.top.sim_acc.gear_str = 'vehicle.current_gear'
        self.top.sim_acc.acceleration_str = 'vehicle.acceleration'
        self.top.sim_acc.overspeed_str = 'vehicle.overspeed'
        
        self.top.sim_EPA_city.workflow.add('vehicle')
        self.top.sim_EPA_city.velocity_str = 'vehicle.velocity'
        self.top.sim_EPA_city.throttle_str = 'vehicle.throttle'
        self.top.sim_EPA_city.gear_str = 'vehicle.current_gear'
        self.top.sim_EPA_city.acceleration_str = 'vehicle.acceleration'
        self.top.sim_EPA_city.fuel_burn_str = 'vehicle.fuel_burn'
        self.top.sim_EPA_city.overspeed_str = 'vehicle.overspeed'
        self.top.sim_EPA_city.underspeed_str = 'vehicle.underspeed'
        self.top.sim_EPA_city.profilename = 'EPA-city.csv'
        
    def tearDown(self):
        pass
        
    def test_errors(self):
        
        self.top.vehicle.ratio1 = 3.54
        self.top.vehicle.ratio2 = 3.54
        self.top.vehicle.ratio3 = 3.54
        self.top.vehicle.ratio4 = 3.54
        self.top.vehicle.ratio5 = 3.54
        try:
            self.top.sim_EPA_city.run()
        except RuntimeError, err:
            msg = "sim_EPA_city: Transmission gearing cannot " \
                  "achieve acceleration and speed required by EPA " \
                  "test."
            
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected.')
        
        self.top.sim_acc.end_speed = 12.0
        self.top.vehicle.ratio1 = 18.0
        self.top.vehicle.ratio2 = 18.0
        self.top.vehicle.ratio3 = 18.0
        self.top.vehicle.ratio4 = 18.0
        self.top.vehicle.ratio5 = 18.0
        
        try:
            self.top.sim_acc.run()
        except RuntimeError, err:
            msg = "sim_acc: Gearing problem in Accel test."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected.')
        
        self.top.vehicle.ratio1 = 1.0
        self.top.vehicle.ratio2 = 1.0
        self.top.vehicle.ratio3 = 1.0
        self.top.vehicle.ratio4 = 1.0
        self.top.vehicle.ratio5 = 1.0
        
        try:
            self.top.sim_EPA_city.run()
        except RuntimeError, err:
            msg = "sim_EPA_city: Vehicle is unable to achieve " \
                  "acceleration required to match EPA driving profile."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected.')
        
if __name__ == "__main__":
    unittest.main()
        