#
# Test for bar3_optimization.py and its components
#

import unittest

from openmdao.examples.bar3_simulation.bar3_optimization import Bar3Optimization


class Bar3OptimizationTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Bar3Optimization()

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_runvehicle(self):
        
        self.model.run()
        
        self.assertAlmostEqual(self.model.bar3_truss.weight, 
                               83.3852245385, places=5)

if __name__ == "__main__":
    
    unittest.main()
