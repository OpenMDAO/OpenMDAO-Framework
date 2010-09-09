#
# Test for driving_sim.py and its components
#

import unittest

from openmdao.main.api import Assembly, set_as_top
from openmdao.examples.enginedesign.vehicle import Vehicle
from openmdao.examples.enginedesign.driving_sim import DrivingSim


class VehicleTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.sim = DrivingSim()
        self.sim.add('vehicle', Vehicle())

    def tearDown(self):
        pass
        
    def test_errors(self):
        
        self.sim.end_speed = 6.0
        self.timestep = 0.5
        self.sim.ratio1 = 3.54
        self.sim.ratio2 = 3.54
        self.sim.ratio3 = 3.54
        self.sim.ratio4 = 3.54
        self.sim.ratio5 = 3.54
        try:
            self.sim.run()
        except RuntimeError, err:
            msg = ": Transmission gearing cannot " \
                  "achieve acceleration and speed required by EPA " \
                  "test."
            
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected.')
        
        self.sim.end_speed = 12.0
        self.sim.ratio1 = 18.0
        self.sim.ratio2 = 18.0
        self.sim.ratio3 = 18.0
        self.sim.ratio4 = 18.0
        self.sim.ratio5 = 18.0
        
        try:
            self.sim.run()
        except RuntimeError, err:
            msg = ": Gearing problem in Accel test."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected.')
        
        self.sim.end_speed = 6.0
        self.timestep = 5.0
        self.sim.ratio1 = 1.0
        self.sim.ratio2 = 1.0
        self.sim.ratio3 = 1.0
        self.sim.ratio4 = 1.0
        self.sim.ratio5 = 1.0
        
        try:
            self.sim.run()
        except RuntimeError, err:
            msg = ": Vehicle is unable to achieve " \
                  "acceleration required to match EPA driving profile."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected.')
        
if __name__ == "__main__":
    unittest.main()
        