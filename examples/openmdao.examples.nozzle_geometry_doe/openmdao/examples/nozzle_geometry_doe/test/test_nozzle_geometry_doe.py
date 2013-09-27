#
# Test for NozzleGeometryDOE.py 
#

import unittest

from openmdao.examples.nozzle_geometry_doe.test.nozzle_geometry_doe import NozzleGeometryDOE
from openmdao.main.api import set_as_top

class NozzleGeometryDOETestCase(unittest.TestCase):
    """ NozzleGeometryDOE test case """

    def setUp(self):
        self.model = set_as_top(NozzleGeometryDOE())

    def tearDown(self):
        self.model.pre_delete()
        self.model = None
        
    def test_run_nozzle_geometry_doe(self):
        
        self.model.run()

        # just to test something
        self.assertEqual(self.model.sc.exec_count, self.model.driver.DOEgenerator.num_samples)

if __name__ == "__main__":
    import nose, sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

