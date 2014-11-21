"""
Unit testing for directional derivatives.
"""

import unittest

from openmdao.main.api import Assembly, set_as_top
from openmdao.main.test.test_derivatives import SimpleDriver, GComp_noD, \
                                                ArrayComp1, ArrayComp2D
from openmdao.test.execcomp import ExecComp, ExecCompWithDerivatives
from openmdao.util.testutil import assert_rel_error

class TestDirectionalFD(unittest.TestCase):

    def test_simplest(self):

        top = set_as_top(Assembly())
        top.add('comp', ExecComp(['y=4.0*x']))
        top.driver.workflow.add('comp')

        top.run()
        top.driver.gradient_options.directional_fd = True

        J = top.driver.calc_gradient(inputs=['comp.x'],
                                     outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.0, 0.0001)

        top.driver.gradient_options.fd_form = 'backward'

        J = top.driver.calc_gradient(inputs=['comp.x'],
                                     outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.0, 0.0001)

        top.driver.gradient_options.fd_form = 'central'

        J = top.driver.calc_gradient(inputs=['comp.x'],
                                     outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.0, 0.0001)

        top.driver.gradient_options.fd_form = 'complex_step'

        J = top.driver.calc_gradient(inputs=['comp.x'],
                                     outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.0, 0.0001)

        top.driver.gradient_options.directional_fd = True
        try:
            J = top.driver.calc_gradient(inputs=['comp.x'],
                                         outputs=['comp.y'],
                                         mode = 'adjoint')
        except RuntimeError, err:
            msg = "Directional derivatives can only be used with forward "
            msg += "mode."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')


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

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'])

        # Ran 1 times, not 4.
        self.assertEqual(top.comp2.exec_count, 2)
        assert_rel_error(self, J[0, 0], 16.0, 0.0001)

        top.driver.gradient_options.directional_fd = False

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'])

        # Ran 4 times (4 fd steps).
        self.assertEqual(top.comp2.exec_count, 6)
        assert_rel_error(self, J[0, 0], 16.0, 0.0001)

        # Next, test param / obj

        top.add('driver', SimpleDriver())
        top.driver.add_parameter('comp1.x', low=-1000, high=1000)
        top.driver.add_objective('comp2.y')
        top.run()
        self.assertEqual(top.comp2.exec_count, 7)

        top.driver.gradient_options.directional_fd = True

        J = top.driver.calc_gradient()

        # Ran 1 more times.
        self.assertEqual(top.comp2.exec_count, 8)
        assert_rel_error(self, J[0, 0], 16.0, 0.0001)

        J = top.driver.calc_gradient(mode='fd')

        # Ran 1 more times (full model fd, 2 edges).
        self.assertEqual(top.comp2.exec_count, 9)
        assert_rel_error(self, J[0, 0], 16.0, 0.0001)

    def test_paramgroup(self):

        top = set_as_top(Assembly())
        top.add('comp1', GComp_noD())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1'])
        top.driver.gradient_options.directional_fd = True

        top.run()

        J = top.driver.calc_gradient(inputs=[('comp1.x1', 'comp1.x2')],
                                     outputs=['comp1.y1'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 12.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x1', 'comp1.x2')],
                                     outputs=['comp1.y1'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 12.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x1', ('comp1.x2')],
                                     outputs=['comp1.y1'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], 7.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x1', 'comp1.x2', 'comp1.x3')],
                                     outputs=['comp1.y1'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 9.0, .001)

    def test_arrays(self):

        top = set_as_top(Assembly())
        top.add('comp1', ArrayComp1())
        top.add('comp2', ArrayComp1())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.connect('comp1.y', 'comp2.x')

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 39.0, .001)
        assert_rel_error(self, J[0, 1], -7.0, .001)
        assert_rel_error(self, J[1, 0], -5.0, .001)
        assert_rel_error(self, J[1, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[0]'],
                                     outputs=['comp2.y[0]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 39.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp2.y[1]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp2.y[-1]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 44.0, .001)

        # this tests the finite difference code.
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 39.0, .001)
        assert_rel_error(self, J[0, 1], -7.0, .001)
        assert_rel_error(self, J[1, 0], -5.0, .001)
        assert_rel_error(self, J[1, 1], 44.0, .001)

        # this tests a simultaneous full and indexed array conn
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[1]', 'comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[1]', 'comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[-1]', 'comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[-1]', 'comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

    def test_nested_2Darray(self):

        top = Assembly()
        top.add('nest', Assembly())
        top.nest.add('comp', ArrayComp2D())

        top.driver.workflow.add(['nest'])
        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')
        top.run()

        J = top.driver.calc_gradient(inputs=['nest.x',],
                                     outputs=['nest.y'],
                                     mode='forward')

        diff = J - top.nest.comp.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 0]',],
                                     outputs=['nest.y[0, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[0, 0]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 1]',],
                                     outputs=['nest.y[1, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 1]',],
                                     outputs=['nest.y[1, 0]'],
                                     mode='fd')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, -1]',],
                                     outputs=['nest.y[-1, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, -1]',],
                                     outputs=['nest.y[-1, 0]'],
                                     mode='fd')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        Jsub = top.nest.comp.J[2:3, 2:3]
        J = top.driver.calc_gradient(inputs=['nest.x[1][:]',],
                                     outputs=['nest.y[1][:]'],
                                     mode='forward')

        diff = J - Jsub

        top.run()
        J = top.driver.calc_gradient(inputs=['nest.x[1][:]',],
                                     outputs=['nest.y[1][:]'],
                                     mode='fd')
        diff = J - Jsub


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()