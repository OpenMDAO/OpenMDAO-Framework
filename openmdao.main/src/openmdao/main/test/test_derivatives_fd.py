"""
Specific unit testing for finite difference.
"""

import unittest

import numpy as np

from openmdao.main.api import Component, VariableTree, Driver, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, Array, File
from openmdao.main.depgraph import simple_node_iter
from openmdao.main.test.test_derivatives import SimpleDriver, ArrayComp2D
from openmdao.test.execcomp import ExecCompWithDerivatives, ExecComp
from openmdao.util.testutil import assert_rel_error

class MyComp(Component):

    x1 = Float(1.0, iotype='in') # default is 1.0e-06
    x2 = Float(1.0, iotype='in', fd_step = .1)
    x3 = Float(1.0, iotype='in', fd_step = .1, fd_form='central')
    x4 = Float(0.001, iotype='in', fd_step = .1, fd_step_type='relative')
    x5 = Float(1.0, iotype='in', fd_step = .01,
               low=0.0, high=10.0,
               fd_step_type='bounds_scaled')
    # Same as x5 but fd_step and low/high bounds scaled equally
    #   We will test this by setting the
    #     gradient_options.fd_step_type to 'bounds_scaled'
    x6 = Float(1.0, iotype='in', fd_step = 0.001,
               low=0.0, high=100.0
               )
    # So we can test the check on needing low and high when using bounds_scaled
    x7 = Float(1.0, iotype='in', fd_step = 0.01,
               fd_step_type='bounds_scaled')

    y = Float(3.3, iotype='out')

    def execute(self):
        ''' Simple eq '''

        self.y = 2.0*self.x1*self.x1 + 2.0*self.x2*self.x2 + \
                 2.0*self.x3*self.x3 + 2.0*self.x4*self.x4 + \
                 2.0*self.x5*self.x5 + 2.0*self.x6*self.x6 + \
                 2.0*self.x7*self.x7

class MyCompDerivs(Component):

    x1 = Float(1.0, iotype='in')
    x2 = Float(1.0, iotype='in')

    y = Float(3.3, iotype='out')

    def execute(self):
        ''' Simple eq '''

        self.y = 2.0*self.x1*self.x1 + 2.0*self.x2*self.x2

    def provideJ(self):
        ''' Simple eq '''

        self.J = np.array([[4.0*self.x1, 4.0*self.x2]])
        return self.J

    def list_deriv_vars(self):

        input_keys = ('x1', 'x2')
        output_keys = ('y', )
        return input_keys, output_keys

class ArrayParaboloid(Component):

    x = Array([[0., 0.]], iotype='in', desc='The variable x')
    f_x = Float(iotype='out', desc='F(x)')

    def execute(self):
        """f(x) = (x[0][0]-3)^2 + x[0][0]x[0][1] + (x[0][1]+4)^2 - 3
        Optimal solution (minimum): x[0][0] = 6.6667; x[0][1] = -7.3333
        """
        x = self.x
        self.f_x = (x[0][0]-3.0)**2 + x[0][0]*x[0][1] + (x[0][1]+4.0)**2 - 3.0

class TestFiniteDifference(unittest.TestCase):

    def test_fd_step(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp())
        model.driver.workflow.add(['comp'])

        model.run()

        J = model.driver.calc_gradient(inputs=['comp.x1', 'comp.x2', 'comp.x3', 'comp.x4'],
                                       outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.0, 0.0001)
        assert_rel_error(self, J[0, 1], 4.2, 0.0001)
        assert_rel_error(self, J[0, 2], 4.0, 0.0001)
        assert_rel_error(self, J[0, 3], 0.0042, 0.0001)

        # test add_parameter's fdstep

        model.add('driver', SimpleDriver())
        model.driver.workflow.add(['comp'])
        model.driver.add_parameter('comp.x1', low=-100, high=100, fd_step=.1)
        J = model.driver.calc_gradient(outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.2, 0.0001)

        # Parameter groups

        model.driver.add_parameter(['comp.x2', 'comp.x3'], low=-100, high=100, fd_step=.001)
        J = model.driver.calc_gradient(outputs=['comp.y'])

        assert_rel_error(self, J[0, 1], 8.004, 0.0001)

        # More Parameter Groups with pseudocomps in them.
        model.driver.add_parameter('comp.x4', low=-100, high=100, fd_step=1.001)
        model.driver.add_objective('comp.x1 + comp.x2 + comp.x3 + comp.x4 + comp.y')


        model.run()
        J = model.driver.calc_gradient(inputs=['comp.x4'], mode='fd')
        assert_rel_error(self, J[0, 0], 1.006, 0.0001)


    def test_central(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp())
        model.driver.workflow.add(['comp'])
        model.driver.gradient_options.fd_form = 'central'

        model.run()

        J = model.driver.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                       outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.0, 0.0001)
        # Central gets this right even with a bad step
        assert_rel_error(self, J[0, 1], 4.0, 0.0001)

    def test_fd_step_type_relative(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())
        model.add('comp', MyComp())
        model.driver.workflow.add(['comp'])
        model.comp.x1 = 1e12
        model.run()

        J = model.driver.calc_gradient(inputs=['comp.x1'],
                                       outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 0.0, 0.0001)

        model.driver.gradient_options.fd_step_type = 'relative'
        J = model.driver.calc_gradient(inputs=['comp.x1'],
                                       outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 4.0e12, 0.0001)

    def test_fd_step_type_bounds_scaled(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp())
        model.driver.workflow.add(['comp'])

        model.run()
        J = model.driver.calc_gradient(inputs=['comp.x5'],
                                       outputs=['comp.y'])
        assert_rel_error(self, J[0, 0], 4.2, 0.0001)


        model.driver.gradient_options.fd_step_type = 'bounds_scaled'
        model.run()
        J = model.driver.calc_gradient(inputs=['comp.x6'],
                                       outputs=['comp.y'])
        assert_rel_error(self, J[0, 0], 4.2, 0.0001)


        model.run()
        try:
            J = model.driver.calc_gradient(inputs=['comp.x7'],
                                           outputs=['comp.y'])
        except RuntimeError as err:
            self.assertEqual(str(err),
               "For variable 'comp.x7', a finite "
               "difference step type of bounds_scaled "
               "is used but required low and high "
               "values are not set" )
        else:
            self.fail("Exception expected because low "
                      "and high not set for comp.x7")

        # test add_parameter's fdstep
        model.add('driver', SimpleDriver())
        model.driver.workflow.add(['comp'])
        model.driver.gradient_options.fd_step_type = 'bounds_scaled'
        model.driver.add_parameter('comp.x2', low=0.0, high=1000.0,
                                   fd_step=.0001)
        J = model.driver.calc_gradient(outputs=['comp.y'])
        assert_rel_error(self, J[0, 0], 4.2, 0.0001)

    def test_force_fd(self):

        model = set_as_top(Assembly())
        model.add('comp', MyCompDerivs())
        model.driver.workflow.add(['comp'])

        model.x1 = 1.0
        model.x2 = 1.0
        model.run()

        J = model.driver.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                       outputs=['comp.y'])
        self.assertEqual(model.comp.exec_count, 1)
        self.assertEqual(model.comp.derivative_exec_count, 1)

        # Component-wise force FD
        model.comp.force_fd = True
        J = model.driver.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                       outputs=['comp.y'])
        self.assertEqual(model.comp.exec_count, 3)
        self.assertEqual(model.comp.derivative_exec_count, 1)

        model.check_gradient(inputs=['comp.x1', 'comp.x2'],
                             outputs=['comp.y'], stream=None)
        model.check_gradient(inputs=['comp.x1', 'comp.x2'],
                             outputs=['comp.y'], fd_form='central', stream=None)
        model.check_gradient(inputs=['comp.x1', 'comp.x2'],
                             outputs=['comp.y'], fd_step_type='relative', stream=None)

        # Full model force FD
        model.comp.force_fd = False
        model.driver.gradient_options.force_fd = True
        old_count = model.comp.exec_count
        J = model.driver.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                       outputs=['comp.y'])
        self.assertEqual(model.comp.exec_count - old_count, 2)
        self.assertEqual(model.comp.derivative_exec_count, 1)

    def test_smarter_nondifferentiable_blocks(self):

        top = set_as_top(Assembly())
        top.add('comp1', ExecCompWithDerivatives(['y=2.0*x + 3.0*x2'],
                                                 ['dy_dx = 2.0', 'dy_dx2 = 3.0']))
        top.add('comp2', ExecComp(['y=2.0*x + 3.0*x2', 'y2=4.0*x + 5.0*x2']))
        top.add('comp3', ExecCompWithDerivatives(['y=2.0*x + 3.0*x2'],
                                                 ['dy_dx = 2.0', 'dy_dx2 = 3.0']))
        top.add('comp4', ExecComp(['y=2.0*x + 3.0*x2']))
        top.add('comp5', ExecCompWithDerivatives(['y=2.0*x + 3.0*x2'],
                                                 ['dy_dx = 2.0', 'dy_dx2 = 3.0']))
        top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        top.connect('comp1.y', 'comp2.x')
        top.connect('comp2.y', 'comp3.x')
        top.connect('comp2.y2', 'comp4.x')
        top.connect('comp3.y', 'comp4.x2')
        top.connect('comp4.y', 'comp5.x')

        top.run()

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp5.y'],
                                     mode='forward')

        self.assertTrue(len(top.driver.workflow._system.subsystems()) == 4)
        comp_list = simple_node_iter(top.driver.workflow._system.subsystems()[2]._nodes)
        self.assertTrue(len(comp_list) == 3)
        self.assertTrue('comp2' in comp_list)
        self.assertTrue('comp3' in comp_list)
        self.assertTrue('comp4' in comp_list)

        top.replace('comp4', ExecCompWithDerivatives(['y=2.0*x + 3.0*x2'],
                    ['dy_dx = 2.0', 'dy_dx2 = 3.0']))

        top.run()

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp5.y'],
                                     mode='forward')

        self.assertTrue(len(top.driver.workflow._system.subsystems()) == 6)
        comp_list = simple_node_iter(top.driver.workflow._system.subsystems()[2]._nodes)
        self.assertTrue(len(comp_list) == 1)
        self.assertTrue('comp2' in comp_list)

    def test_smart_low_high_array_param(self):

        top = Assembly()
        top.add('paraboloid', ArrayParaboloid())
        driver = top.add('driver', SimpleDriver())
        driver.add_objective('paraboloid.f_x')
        driver.add_parameter('paraboloid.x', low=[-100, -99], high=[100, 99])
        driver.workflow.add('paraboloid')
        top.run()
        J = top.driver.calc_gradient()

    def test_smart_low_high(self):

        top = Assembly()
        top.add('comp', MyComp())
        driver = top.add('driver', SimpleDriver())
        top.comp.add('x1', Float(1.0, iotype='in', low=-1.0, high=1.0))
        driver.add_objective('comp.y')
        driver.add_parameter('comp.x1', low=-1.0, high=1.0)
        driver.workflow.add('comp')

        top.driver.gradient_options.fd_form = 'central'
        top.driver.gradient_options.fd_step = 0.1

        top.comp.x1 = -0.95
        top.run()
        J = top.driver.calc_gradient()
        assert_rel_error(self, J[0, 0], -3.6, 0.001)

        top.comp.x1 = 0.95
        top.run()
        J = top.driver.calc_gradient()
        assert_rel_error(self, J[0, 0], 3.6, 0.001)

    def test_PA_slices(self):

        top = set_as_top(Assembly())
        top.add('comp', ArrayComp2D())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add('comp')
        top.comp.force_fd = True
        top.driver.gradient_options.directional_fd = True

        top.driver.add_parameter('comp.x', low=-100, high=100)
        top.driver.add_constraint('comp.y[0][-1] < 1.0')
        top.driver.add_constraint('numpy.sum(comp.y) < 4.0')

        top.run()
        J = top.driver.calc_gradient(mode='forward')
        print J
        assert_rel_error(self, J[0, 0], 4.0, 0.001)
        assert_rel_error(self, J[0, 1], 2.0, 0.001)
        assert_rel_error(self, J[0, 2], 6.0, 0.001)
        assert_rel_error(self, J[0, 3], 5.0, 0.001)

    def test_broadcast_input_to_opaque_system(self):

        # This test replicates a bug when finite differencing a boundary variable that
        # broadcasts to multiple places.

        top = set_as_top(Assembly())
        top.add('comp1', ExecComp(['y=7.0*x1']))
        top.add('comp2', ExecComp(['y=5.0*x1 + 2.0*x2']))
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])

        top.add('x1', Float(3.0, iotype='in'))
        top.connect('comp1.y', 'comp2.x2')
        top.connect('x1', 'comp1.x1')
        top.connect('x1', 'comp2.x1')

        top.run()

        J = top.driver.calc_gradient(inputs=['x1'],
                                     outputs=['comp2.y'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 19.0, .001)

    def test_branch_output_in_opaque_system(self):

        # This test replicates a bug where an interior output was missing in
        # an opaque system.

        top = set_as_top(Assembly())
        top.add('nest', Assembly())
        top.nest.add('comp1', ExecComp(['y=7.0*x1']))
        top.nest.add('comp2', ExecComp(['y=5.0*x1 + 2.0*x2']))
        top.driver.workflow.add(['nest'])
        top.nest.driver.workflow.add(['comp1', 'comp2'])

        top.nest.add('x1', Float(3.0, iotype='in'))
        top.nest.add('y2', Float(3.0, iotype='out'))
        top.nest.connect('comp1.y', 'comp2.x2')
        top.nest.connect('x1', 'comp1.x1')
        top.nest.connect('x1', 'comp2.x1')
        top.nest.create_passthrough('comp1.y')
        top.nest.connect('comp2.y', 'y2')

        top.run()

        J = top.driver.calc_gradient(inputs=['nest.x1'],
                                     outputs=['nest.y', 'nest.y2'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 7.0, .001)
        assert_rel_error(self, J[1, 0], 19.0, .001)

    def test_deriv_ignore_nondiff_groups(self):

        # This test makes sure that we don't group comps in a nondiff block
        # if we have 1 differentiable connection and 1 non-differentiable one
        # with deriv_ignore.

        top = set_as_top(Assembly())
        top.add('src', MyCompDerivs())
        top.add('target', MyCompDerivs())

        top.src.add('mesh_file', File(iotype='out', deriv_ignore=True))
        top.target.add('mesh_file', File(iotype='in', deriv_ignore=True))

        top.connect('src.mesh_file', 'target.mesh_file')
        top.connect('src.y', 'target.x1')

        top.driver.workflow.add(['src', 'target'])

        top.run()

        J = top.driver.calc_gradient(inputs=['src.x1'], outputs=['target.y'])
        assert_rel_error(self, J[0, 0], 64.0, .001)
        systems = top._system.dump(stream=None)
        self.assertTrue('FD_' not in systems)

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
