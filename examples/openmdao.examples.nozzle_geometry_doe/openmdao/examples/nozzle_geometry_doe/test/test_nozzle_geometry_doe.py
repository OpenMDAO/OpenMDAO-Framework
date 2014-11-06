#
# Test for NozzleGeometryDOE.py
#

import unittest
import os
import shutil

from openmdao.main.api import set_as_top
from openmdao.examples.nozzle_geometry_doe.test.nozzle_geometry_doe \
     import NozzleGeometryDOE


class NozzleGeometryDOETestCase(unittest.TestCase):
    """ NozzleGeometryDOE test case """

    def setUp(self):
        self.model = set_as_top(NozzleGeometryDOE())

    def tearDown(self):
        self.model.pre_delete()
        self.model = None

        outfile = 'pyBspline_pkl'
        if os.path.exists(outfile):
            shutil.rmtree(outfile)

        outfile = 'driver.csv'
        if os.path.exists(outfile):
            os.remove(outfile)

    def test_run_nozzle_geometry_doe(self):
        self.model.run()

        # just to test something
        self.assertEqual(self.model.plug_noz.exec_count,
                         self.model.driver.DOEgenerator.num_samples)


if __name__ == "__main__":
    import nose, sys
    sys.argv.append('--cover-package=openmdao.examples.nozzle_geometry_doe')
    sys.argv.append('--cover-erase')
    nose.runmodule()

