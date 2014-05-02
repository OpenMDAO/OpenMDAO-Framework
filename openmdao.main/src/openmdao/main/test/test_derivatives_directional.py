"""
Unit testing for directional derivatives.
"""

import unittest

from openmdao.main.api import Assembly, set_as_top
from openmdao.test.execcomp import ExecComp, ExecCompWithDerivatives
from openmdao.util.testutil import assert_rel_error

class TestDirectionalFD(unittest.TestCase):

    def test_simplest(self):

        top = set_as_top(Assembly())
        top.add('comp', ExecComp(['y=4.0*x']))
        top.driver.workflow.add('comp')

        top.run()
        top.driver.gradient_options.directional_fd = True

        J = top.driver.workflow.calc_gradient(inputs=['comp.x'],
                                              outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.0, 0.0001)

    def test_baseline_case(self):

        top = set_as_top(Assembly())
        top.add('comp1', ExecCompWithDerivatives(['y1=2.0*x',
                                                  'y2=3.0*x',
                                                  'y3=4.0*x',
                                                  'y4=5.0*x'],
                                                 ['dy1_dx = 2.0',
                                                  'dy2_dx = 3.0',
                                                  'dy3_dx = 4.0',
                                                  'dy4_dx = 5.0']))

        top.add('comp2', ExecComp(['y = 2.0*x1 + x2 + x3 + x4']))
        top.driver.workflow.add(['comp1', 'comp2'])
        top.connect('comp1.y1', 'comp2.x1')
        top.connect('comp1.y2', 'comp2.x2')
        top.connect('comp1.y3', 'comp2.x3')
        top.connect('comp1.y4', 'comp2.x4')

        top.run()
        self.assertEqual(top.comp2.exec_count, 1)
        top.driver.gradient_options.directional_fd = True

        J = top.driver.workflow.calc_gradient(inputs=['comp1.x'],
                                              outputs=['comp2.y'])

        # Ran 3 times, not 4.
        self.assertEqual(top.comp2.exec_count, 4)
        assert_rel_error(self, J[0, 0], 16.0, 0.0001)

        top.driver.gradient_options.directional_fd = False

        top.driver.workflow.config_changed()
        J = top.driver.workflow.calc_gradient(inputs=['comp1.x'],
                                              outputs=['comp2.y'])

        # Ran 4 times (4 fd steps).
        self.assertEqual(top.comp2.exec_count, 8)
        assert_rel_error(self, J[0, 0], 16.0, 0.0001)

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()