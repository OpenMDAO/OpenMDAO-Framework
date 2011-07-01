#
# Test for EngineOptimization.py undergoing optimization with CONMIN
#

import unittest

from openmdao.examples.enginedesign.engine_optimization import EngineOptimization


class EngineOptimizationTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = EngineOptimization()

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_runvehicle(self):
        
        # We are just testing the mechanics of the EngineOptimization component, so
        # set the design variables at the global minimum.
        
        self.model.vehicle.bore = 95
        self.model.vehicle.spark_angle = -35.368341874

        self.model.driver.itmax = 1
        
        self.model.run()
        
        self.assertAlmostEqual(self.model.sim_acc.accel_time, 
                               5.5999999999999961, places=6)
        self.assertAlmostEqual(self.model.sim_EPA_city.fuel_economy, 
                               25.203, places=3)
        self.assertAlmostEqual(self.model.sim_EPA_highway.fuel_economy, 
                               32.8139, places=4)

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
