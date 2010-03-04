#
# Test for bar3_optimization.py and its components
#

import unittest

from openmdao.util.testutil import assert_rel_error

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
        
        assert_rel_error(self, self.model.bar3_truss.weight, 83.3852245385, 0.001)

if __name__ == "__main__":
    import nose, sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
