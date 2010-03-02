#
# Test for bar3_optimization.py and its components
#

import unittest

from openmdao.examples.bar3simulation.bar3_optimization import Bar3Optimization


class Bar3OptimizationTestCase(unittest.TestCase):
    """ Test Vehicle """

    def setUp(self):
        self.model = Bar3Optimization()

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_bar3(self):
        
        self.model.run()
        
        from platform import architecture
        
        if architecture()[0] == '32bit':
            weight_expected = 83.4010088
        else:
            weight_expected = 83.3852245385
                    
        self.assertAlmostEqual(self.model.bar3_truss.weight, 
                               weight_expected, places=5)

if __name__ == "__main__":
    
    unittest.main()
