#
# Test for EngineOptimization.py undergoing optimization with CONMIN
#

import unittest

from openmdao.main.api import set_as_top
from openmdao.examples.enginedesign.engine_optimization_smarter import EngineOptimization
from openmdao.examples.enginedesign.vehicle_singlesim import VehicleSim
from openmdao.examples.enginedesign.vehicle_threesim import VehicleSim2


class EngineOptimizationSmarterTestCase(unittest.TestCase):
    """ Test """

    def setUp(self):
        self.model = set_as_top(EngineOptimization())

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_runvehicle(self):
        
        # We are just testing the mechanics of the EngineOptimization component, so
        # set the design variables at the global minimum.
        
        self.model.vehicle.bore = 100
        self.model.vehicle.spark_angle = -35.368341874

        self.model.driver.itmax = 1
        
        self.model.run()
        
        self.assertAlmostEqual(self.model.sim_acc.accel_time, 
                               5.5999999999999961, places=6)
        self.assertAlmostEqual(self.model.sim_EPA_city.fuel_economy, 
                               25.203, places=3)
        self.assertAlmostEqual(self.model.sim_EPA_highway.fuel_economy, 
                               32.8139, places=4)

class SimVehicleScriptsTestCase(unittest.TestCase):
    """ Test """

    def setUp(self):
        pass
    
    def tearDown(self):
        pass
    
    def test_simvehicle_one(self):
        my_sim = VehicleSim()
        set_as_top(my_sim)

        my_sim.run()
        
        self.assertAlmostEqual(my_sim.driver.accel_time, 7.5, places=2)

    def test_simvehicle_three(self):
        my_sim = VehicleSim2()
        set_as_top(my_sim)

        my_sim.run()
        
        self.assertAlmostEqual(my_sim.sim_acc.accel_time, 7.5, places=2)
        self.assertAlmostEqual(my_sim.sim_EPA_city.fuel_economy, 24.8079694553, places=2)
        self.assertAlmostEqual(my_sim.sim_EPA_highway.fuel_economy, 33.4540583989, places=2)
        

if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1 and sys.argv[1] == '-prof':
        import cProfile
        import pstats
        
        sys.argv.remove('-prof') #unittest doesn't like -prof
        cProfile.run('unittest.main()', 'profout')
        p = pstats.Stats('profout')
        p.strip_dirs()
        p.sort_stats('cum', 'time')
        p.print_stats()
        print '\n\n---------------------\n\n'
        p.print_callers()
        print '\n\n---------------------\n\n'
        p.print_callees()
    else:
        unittest.main()
