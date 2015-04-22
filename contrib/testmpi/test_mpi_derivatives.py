
import numpy as np
import sys

from openmdao.main.mpiwrap import MPIContext
from openmdao.test.mpiunittest import MPITestCase
from openmdao.util.testutil import assert_rel_error
from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecCompWithDerivatives, ExecComp

class Paraboloid(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')

    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

    def provideJ(self):
        """Analytical first derivatives"""

        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x

        self.J = np.array([[df_dx, df_dy]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        return input_keys, output_keys


class MPITests_2Proc(MPITestCase):

    N_PROCS = 2

    def setUp(self):
        # this model mimics the one in test_derivatives, test_single_comp
        self.top = top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.comp.x = 3
        top.comp.y = 5

    def test_run(self):

        self.top.run()

        if self.comm.rank == 0:
            self.assertEqual(self.top.comp.f_xy, 93.)
            self.assertEqual(self.top._pseudo_0.out0, 93.)

    def test_calc_gradient_fwd(self):
        self.top.run()

        J = self.top.driver.calc_gradient(mode='forward',
                                          return_format='dict')

        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0],
                                        5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0],
                                        21.0, 0.0001)

    def test_calc_gradient_adjoint(self):
        self.top.run()

        J = self.top.driver.calc_gradient(mode='adjoint',
                                          return_format='dict')

        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0],
                                    5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0],
                                    21.0, 0.0001)

    def test_calc_gradient_fd(self):
        self.top.run()

        J = self.top.driver.calc_gradient(mode='fd',
                                          return_format='dict')

        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0],
                                        5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0],
                                        21.0, 0.0001)

    def test_calc_gradient_fwd_linGS(self):

        self.top.driver.gradient_options.lin_solver = 'linear_gs'
        self.top.driver.gradient_options.maxiter = 1
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp.x','comp.y'], mode='forward',
                                          return_format='dict')

        J = self.top.driver.workflow._system.get_combined_J(J)

        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0],
                                        5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0],
                                        21.0, 0.0001)

    def test_two_to_one_forward(self):

        top = set_as_top(Assembly())

        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]

        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())

        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_parameter('comp2.x', low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.calc_gradient(mode='forward',
                                     return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)
        assert_rel_error(self, J['_pseudo_0.out0']['comp1.x'][0][0],
                                    15.0, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['comp2.x'][0][0],
                                    -8.0, 0.0001)

    def test_two_to_one_adjoint(self):

        top = set_as_top(Assembly())

        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]

        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())

        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_parameter('comp2.x', low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.calc_gradient(mode='adjoint',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)
        assert_rel_error(self, J['_pseudo_0.out0']['comp1.x'][0][0],
                                    15.0, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['comp2.x'][0][0],
                                    -8.0, 0.0001)

    def test_two_to_one_fd(self):

        top = set_as_top(Assembly())

        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]

        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())

        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_parameter('comp2.x', low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.calc_gradient(mode='fd',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)
        assert_rel_error(self, J['_pseudo_0.out0']['comp1.x'][0][0],
                                    15.0, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['comp2.x'][0][0],
                                    -8.0, 0.0001)

    def test_two_to_one_forward_bcast(self):

        top = set_as_top(Assembly())

        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]

        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())

        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter(('comp1.x', 'comp2.x'),
                                 low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.calc_gradient(mode='forward',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)
        #print J
        assert_rel_error(self, J['_pseudo_0.out0']['comp1.x'][0][0],
                                    7.0, 0.0001)

    def test_two_to_one_adjoint_bcast(self):

        top = set_as_top(Assembly())

        exp1 = ["y = 3.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x1 + 4.0*x2"]

        deriv1 = ["dy_dx = 3.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx1 = 5.0", "dy_dx2 = 4.0"]

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())

        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y', 'comp3.x1')
        top.connect('comp2.y', 'comp3.x2')
        top.driver.add_parameter(('comp1.x', 'comp2.x'),
                                 low=-100, high=100)
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.calc_gradient(mode='adjoint',
                                              return_format='dict')

        J = top.driver.workflow._system.get_combined_J(J)

        #from openmdao.util.dotgraph import plot_system_tree
        #plot_system_tree(top._system)

        assert_rel_error(self, J['_pseudo_0.out0']['comp1.x'][0][0],
                                    7.0, 0.0001)

    def test_one_to_two_forward(self):

        top = set_as_top(Assembly())

        exp1 = ["y1 = 3.0*x", "y2 = 4.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x"]

        deriv1 = ["dy1_dx = 3.0", "dy2_dx = 4.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx = 5.0"]

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())

        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y1', 'comp2.x')
        top.connect('comp1.y2', 'comp3.x')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_constraint('comp2.y < 1000')
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.calc_gradient(mode='forward',
                                              return_format='dict')
        J = top.driver.workflow._system.get_combined_J(J)

        assert_rel_error(self, J['_pseudo_0.out0']['comp1.x'][0][0],
                                    -6.0, 0.0001)
        assert_rel_error(self, J['_pseudo_1.out0']['comp1.x'][0][0],
                                    20.0, 0.0001)

    def test_one_to_two_adjoint(self):

        top = set_as_top(Assembly())

        exp1 = ["y1 = 3.0*x", "y2 = 4.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x"]

        deriv1 = ["dy1_dx = 3.0", "dy2_dx = 4.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx = 5.0"]

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())

        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y1', 'comp2.x')
        top.connect('comp1.y2', 'comp3.x')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_constraint('comp2.y < 1000')
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.calc_gradient(mode='adjoint',
                                              return_format='dict')
        J = top.driver.workflow._system.get_combined_J(J)

        assert_rel_error(self, J['_pseudo_0.out0']['comp1.x'][0][0],
                                    -6.0, 0.0001)
        assert_rel_error(self, J['_pseudo_1.out0']['comp1.x'][0][0],
                                    20.0, 0.0001)

    def test_one_to_two_fd(self):

        top = set_as_top(Assembly())

        exp1 = ["y1 = 3.0*x", "y2 = 4.0*x"]
        exp2 = ["y = -2.0*x"]
        exp3 = ["y = 5.0*x"]

        deriv1 = ["dy1_dx = 3.0", "dy2_dx = 4.0"]
        deriv2 = ["dy_dx = -2.0"]
        deriv3 = ["dy_dx = 5.0"]

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('driver', SimpleDriver())

        top.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.connect('comp1.y1', 'comp2.x')
        top.connect('comp1.y2', 'comp3.x')
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_constraint('comp2.y < 1000')
        top.driver.add_constraint('comp3.y < 1000')
        top.run()

        J = top.driver.calc_gradient(mode='fd',
                                              return_format='dict')
        J = top.driver.workflow._system.get_combined_J(J)

        assert_rel_error(self, J['_pseudo_0.out0']['comp1.x'][0][0],
                                    -6.0, 0.0001)
        assert_rel_error(self, J['_pseudo_1.out0']['comp1.x'][0][0],
                                    20.0, 0.0001)

    def test_three_comp_diamond_forward(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 50.0*x1',
                'y2 = 1.0*x1']
        deriv1 = ['dy1_dx1 = 50.0',
                  'dy2_dx1 = 1.0']

        exp2 = ['y1 = 1.2*x1']
        deriv2 = ['dy1_dx1 = 1.2']

        exp3 = ['y1 = 100.0*x1*x2 + 30*x1 + 0.3*x2']
        deriv3 = ['dy1_dx1 = 100.0*x2 + 30',
                  'dy1_dx2 = 100.0*x1 + 0.3']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('driver', SimpleDriver())

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp3.x2')

        self.top.driver.add_parameter('comp1.x1', low=-100, high=100)
        self.top.driver.add_objective('comp3.y1')
        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp3.y1'],
                                          mode='forward',
                                          return_format='dict')
        if self.comm.rank == 0:
            assert_rel_error(self, J['comp3.y1']['comp1.x1'][0][0],
                             24048.0, 0.0001)

    def test_diverge_converge_forward(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')

        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward',
                                          return_format='dict')

        assert_rel_error(self, J['comp5.y1']['comp1.x1'][0][0],
                                    313.0, 0.0001)

    def test_diverge_converge_LinGS_forward(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')

        self.top.comp1.x1 = 2.0
        self.top.driver.gradient_options.lin_solver = 'linear_gs'
        self.top.driver.gradient_options.maxiter = 1
        self.top.run()

        # from openmdao.util.dotgraph import plot_system_tree
        # plot_system_tree(self.top._system)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward',
                                          return_format='dict')

        assert_rel_error(self, J['comp5.y1']['comp1.x1'][0][0],
                                    313.0, 0.0001)

    def test_diverge_converge_LinGS_adjoint(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')

        self.top.comp1.x1 = 2.0
        self.top.driver.gradient_options.lin_solver = 'linear_gs'
        self.top.driver.gradient_options.maxiter = 1
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='adjoint',
                                          return_format='dict')

        assert_rel_error(self, J['comp5.y1']['comp1.x1'][0][0],
                                    313.0, 0.0001)

    def test_diverge_converge_adjoint(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')

        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='adjoint',
                                          return_format='dict')
        #from openmdao.util.dotgraph import plot_system_tree
        #plot_system_tree(self.top._system)
        #print J
        assert_rel_error(self, J['comp5.y1']['comp1.x1'][0][0],
                                    313.0, 0.0001)

    def test_diverge_converge_extended_adjoint(self):

        top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp2b', ExecCompWithDerivatives(exp3, deriv3))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('comp3b', ExecCompWithDerivatives(exp3, deriv3))
        top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        top.driver.workflow.add(['comp1', 'comp2', 'comp2b', 'comp3', 'comp3b', 'comp4', 'comp5'])

        top.connect('comp1.y1', 'comp2.x1')
        top.connect('comp1.y2', 'comp3.x1')
        top.connect('comp2.y1', 'comp2b.x1')
        top.connect('comp3.y1', 'comp3b.x1')
        top.connect('comp2b.y1', 'comp4.x1')
        top.connect('comp3b.y1', 'comp4.x2')
        top.connect('comp4.y1', 'comp5.x1')
        top.connect('comp4.y2', 'comp5.x2')
        top.connect('comp4.y3', 'comp5.x3')

        top.comp1.x1 = 2.0
        #top.driver.gradient_options.lin_solver = 'linear_gs'
        #top.driver.gradient_options.maxiter = 1
        top.run()

        J = top.driver.calc_gradient(inputs=['comp1.x1'],
                                     outputs=['comp5.y1'],
                                     mode='adjoint',
                                     return_format='dict')

        #from openmdao.util.dotgraph import plot_system_tree
        #plot_system_tree(self.top._system)

        #print J
        assert_rel_error(self, J['comp5.y1']['comp1.x1'][0][0],
                                    3300.5, 0.0001)

    def test_diverge_converge_extended_adjoint_under_driver(self):

        top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        top.add('comp2b', ExecCompWithDerivatives(exp3, deriv3))
        top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        top.add('comp3b', ExecCompWithDerivatives(exp3, deriv3))
        top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))
        top.add('subdrv1', Driver())
        top.add('subdrv2', Driver())

        top.driver.workflow.add(['comp1', 'subdrv1', 'subdrv2', 'comp4', 'comp5'])
        top.subdrv1.workflow.add(['comp2', 'comp3'])
        top.subdrv2.workflow.add(['comp2b', 'comp3b'])

        top.connect('comp1.y1', 'comp2.x1')
        top.connect('comp1.y2', 'comp3.x1')
        top.connect('comp2.y1', 'comp2b.x1')
        top.connect('comp3.y1', 'comp3b.x1')
        top.connect('comp2b.y1', 'comp4.x1')
        top.connect('comp3b.y1', 'comp4.x2')
        top.connect('comp4.y1', 'comp5.x1')
        top.connect('comp4.y2', 'comp5.x2')
        top.connect('comp4.y3', 'comp5.x3')

        top.comp1.x1 = 2.0
        top.run()

        J = top.driver.calc_gradient(inputs=['comp1.x1'],
                                     outputs=['comp5.y1'],
                                     mode='adjoint',
                                     return_format='dict')

        #from openmdao.util.dotgraph import plot_system_tree
        #plot_system_tree(self.top._system)

        #print J
        assert_rel_error(self, J['comp5.y1']['comp1.x1'][0][0],
                                    3300.5, 0.0001)

    def test_diverge_converge_nondiff_comp3_forward(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('driver', SimpleDriver())
        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecComp(exp3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')

        self.top.driver.add_parameter('comp1.x1', low=-100, high=100)
        self.top.driver.add_objective('comp5.y1')

        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward',
                                          return_format='dict')

        assert_rel_error(self, J['comp5.y1']['comp1.x1'][0][0],
                                    313.0, 0.0001)

    def test_one_two_one_two_one_forward(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1 - 5.0*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = -5.0',]

        exp5 = ['y1 = 0.8*x1']
        deriv5 = ['dy1_dx1 = 0.8']

        exp6 = ['y1 = 0.5*x1']
        deriv6 = ['dy1_dx1 = 0.5']

        exp7 = ['y1 = x1 + 3.0*x2']
        deriv7 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))
        self.top.add('comp6', ExecCompWithDerivatives(exp6, deriv6))
        self.top.add('comp7', ExecCompWithDerivatives(exp7, deriv7))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5', 'comp6', 'comp7'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp6.x1')
        self.top.connect('comp5.y1', 'comp7.x1')
        self.top.connect('comp6.y1', 'comp7.x2')

        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp7.y1'],
                                          mode='forward',
                                          return_format='dict')

        assert_rel_error(self, J['comp7.y1']['comp1.x1'][0][0],
                                    -40.75, 0.0001)

    def test_one_two_one_two_one_adjoint(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1 - 5.0*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = -5.0',]

        exp5 = ['y1 = 0.8*x1']
        deriv5 = ['dy1_dx1 = 0.8']

        exp6 = ['y1 = 0.5*x1']
        deriv6 = ['dy1_dx1 = 0.5']

        exp7 = ['y1 = x1 + 3.0*x2']
        deriv7 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))
        self.top.add('comp6', ExecCompWithDerivatives(exp6, deriv6))
        self.top.add('comp7', ExecCompWithDerivatives(exp7, deriv7))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5', 'comp6', 'comp7'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp6.x1')
        self.top.connect('comp5.y1', 'comp7.x1')
        self.top.connect('comp6.y1', 'comp7.x2')

        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp7.y1'],
                                          mode='adjoint',
                                          return_format='dict')

        assert_rel_error(self, J['comp7.y1']['comp1.x1'][0][0],
                                    -40.75, 0.0001)

    def test_lin_GS_subassy(self):

        class Sub(Assembly):

            def configure(self):
                exp1 = ['y1 = 2.0*x1**2',
                        'y2 = 3.0*x1']
                deriv1 = ['dy1_dx1 = 4.0*x1',
                          'dy2_dx1 = 3.0']

                exp2 = ['y1 = 0.5*x1']
                deriv2 = ['dy1_dx1 = 0.5']

                exp3 = ['y1 = 3.5*x1']
                deriv3 = ['dy1_dx1 = 3.5']

                exp4 = ['y1 = x1 + 2.0*x2',
                        'y2 = 3.0*x1',
                        'y3 = x1*x2']
                deriv4 = ['dy1_dx1 = 1.0',
                          'dy1_dx2 = 2.0',
                          'dy2_dx1 = 3.0',
                          'dy2_dx2 = 0.0',
                          'dy3_dx1 = x2',
                          'dy3_dx2 = x1']

                exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
                deriv5 = ['dy1_dx1 = 1.0',
                          'dy1_dx2 = 3.0',
                          'dy1_dx3 = 2.0']

                self.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
                self.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
                self.add('comp2b', ExecCompWithDerivatives(exp3, deriv3))
                self.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
                self.add('comp3b', ExecCompWithDerivatives(exp3, deriv3))
                self.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
                self.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

                self.driver.workflow.add(['comp1', 'comp2', 'comp2b', 'comp3', 'comp3b', 'comp4', 'comp5'])

                self.connect('comp1.y1', 'comp2.x1')
                self.connect('comp1.y2', 'comp3.x1')
                self.connect('comp2.y1', 'comp2b.x1')
                self.connect('comp3.y1', 'comp3b.x1')
                self.connect('comp2b.y1', 'comp4.x1')
                self.connect('comp3b.y1', 'comp4.x2')
                self.connect('comp4.y1', 'comp5.x1')
                self.connect('comp4.y2', 'comp5.x2')
                self.connect('comp4.y3', 'comp5.x3')

                self.comp1.x1 = 2.0

                self.create_passthrough('comp1.x1')
                self.create_passthrough('comp5.y1')
                self.create_passthrough('comp1.y2')

                self.driver.system_type = 'serial'


        top = set_as_top(Assembly())
        top.add('sub1', Sub())
        top.add('sub2', Sub())

        top.replace('driver', SimpleDriver())
        top.driver.workflow.add(['sub1', 'sub2'])

        top.driver.add_parameter('sub1.x1', low=-10, high=10)
        top.driver.add_parameter('sub2.x1', low=-10, high=10)
        top.driver.add_objective('sub1.y1 + sub2.y1')

        # These make it lock up
        top.driver.add_constraint('sub1.y2 < 100')
        #top.driver.add_constraint('sub2.y2 < 100')

        top.sub1.x1 = 2.0
        top.sub2.x1 = 3.0

        top.driver.gradient_options.lin_solver = 'linear_gs'
        top.driver.gradient_options.maxiter = 1
        top.run()

        # from openmdao.util.dotgraph import plot_system_tree
        # plot_system_tree(top._system)

        J = top.driver.calc_gradient(mode='adjoint',
                                     return_format='dict')

        assert_rel_error(self, J['_pseudo_0.out0']['sub1.x1'][0][0],
                                    3300.5, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['sub2.x1'][0][0],
                                    7229.25, 0.0001)
        assert_rel_error(self, J['_pseudo_1.out0']['sub1.x1'][0][0],
                                    3.0, 0.0001)

    def test_parsys_transdriver(self):

        class Sub(Assembly):

            def __init__(self, factor):
                super(Sub, self).__init__()

                self.factor = factor

            def configure(self):

                exp = ['y = %f*x' % self.factor]
                deriv = ['dy_dx = %f' % self.factor]

                self.add('comp', ExecCompWithDerivatives(exp, deriv))

                self.driver.workflow.add(['comp'])

                self.create_passthrough('comp.x')
                self.create_passthrough('comp.y')

                self.driver.system_type = 'serial'


        top = set_as_top(Assembly())
        top.add('sub1', Sub(factor=1.0))
        top.add('sub2', Sub(factor=3.0))
        top.add('stuff', Driver())
        top.stuff.system_type = "serial"

        exp = ['y = 3.0*x1 + 5.0*x2']
        deriv = ['dy_dx1 = 3.0', 'dy_dx2 = 5.0']

        top.add('post', ExecCompWithDerivatives(exp, deriv))
        top.connect('sub1.y', 'post.x1')
        top.connect('sub2.y', 'post.x2')

        top.replace('driver', SimpleDriver())
        top.driver.workflow.add(['sub1', 'sub2', 'stuff'])
        top.stuff.workflow.add(['post'])

        top.driver.add_parameter('sub1.x', low=-10, high=10)
        top.driver.add_parameter('sub2.x', low=-10, high=10)
        top.driver.add_objective('post.y')

        top.sub1.x = 2.0
        top.sub2.x = 3.0

        top.run()

        with MPIContext():
            assert_rel_error(self, top.post.y, 51.0, 0.0001)

        # from openmdao.util.dotgraph import plot_system_tree
        # plot_system_tree(top._system)

        J = top.driver.calc_gradient(mode='forward', return_format='dict')

        with MPIContext():
            assert_rel_error(self, J['_pseudo_0.out0']['sub1.x'][0][0],
                             3.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['sub2.x'][0][0],
                             15.0, 0.0001)

        J = top.driver.calc_gradient(mode='adjoint', return_format='dict')

        with MPIContext():
            assert_rel_error(self, J['_pseudo_0.out0']['sub1.x'][0][0],
                             3.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['sub2.x'][0][0],
                             15.0, 0.0001)

        J = top.driver.calc_gradient(mode='fd', return_format='dict')

        with MPIContext():
            assert_rel_error(self, J['_pseudo_0.out0']['sub1.x'][0][0],
                             3.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['sub2.x'][0][0],
                             15.0, 0.0001)

    def test_parsys_in_transdriver_and_transdriver(self):

        class Sub(Assembly):

            def __init__(self, factor):
                super(Sub, self).__init__()

                self.factor = factor

            def configure(self):

                exp = ['y = %f*x' % self.factor]
                deriv = ['dy_dx = %f' % self.factor]

                self.add('comp', ExecCompWithDerivatives(exp, deriv))

                self.driver.workflow.add(['comp'])

                self.create_passthrough('comp.x')
                self.create_passthrough('comp.y')

                self.driver.system_type = 'serial'


        top = set_as_top(Assembly())
        top.add('sub1', Sub(factor=1.0))
        top.add('sub2', Sub(factor=3.0))
        top.add('stuff', Driver())
        top.add('missions', Driver())
        top.stuff.system_type = "serial"

        exp = ['y = 3.0*x1 + 5.0*x2']
        deriv = ['dy_dx1 = 3.0', 'dy_dx2 = 5.0']

        top.add('post', ExecCompWithDerivatives(exp, deriv))
        top.connect('sub1.y', 'post.x1')
        top.connect('sub2.y', 'post.x2')

        top.replace('driver', SimpleDriver())
        top.driver.workflow.add(['missions', 'stuff'])
        top.missions.workflow.add(['sub1', 'sub2'])
        top.stuff.workflow.add(['post'])

        top.driver.add_parameter('sub1.x', low=-10, high=10)
        top.driver.add_parameter('sub2.x', low=-10, high=10)
        top.driver.add_objective('post.y')

        top.sub1.x = 2.0
        top.sub2.x = 3.0

        top.run()

        with MPIContext():
            assert_rel_error(self, top.post.y, 51.0, 0.0001)

        # from openmdao.util.dotgraph import plot_system_tree
        # plot_system_tree(top._system)

        J = top.driver.calc_gradient(mode='forward', return_format='dict')

        with MPIContext():
            assert_rel_error(self, J['_pseudo_0.out0']['sub1.x'][0][0],
                             3.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['sub2.x'][0][0],
                             15.0, 0.0001)

        J = top.driver.calc_gradient(mode='adjoint', return_format='dict')

        with MPIContext():
            assert_rel_error(self, J['_pseudo_0.out0']['sub1.x'][0][0],
                             3.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['sub2.x'][0][0],
                             15.0, 0.0001)

        J = top.driver.calc_gradient(mode='fd', return_format='dict')

        with MPIContext():
            assert_rel_error(self, J['_pseudo_0.out0']['sub1.x'][0][0],
                             3.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['sub2.x'][0][0],
                             15.0, 0.0001)


    def test_parallel_gather_for_objective(self):

        class SpecialDriver(SimpleDriver):

            def execute(self):

                self.run_iteration()

                self.func_dict = {}
                for key, obj in self.get_objectives().iteritems():
                    name = '%s.out0' % obj.pcomp_name
                    self.func_dict[name] = np.array(obj.evaluate())

        class Sub(Assembly):

            def configure(self):
                exp1 = ['y1 = 2.0*x1**2',
                        'y2 = 3.0*x1']
                deriv1 = ['dy1_dx1 = 4.0*x1',
                          'dy2_dx1 = 3.0']

                exp2 = ['y1 = 0.5*x1']
                deriv2 = ['dy1_dx1 = 0.5']

                exp3 = ['y1 = 3.5*x1']
                deriv3 = ['dy1_dx1 = 3.5']

                exp4 = ['y1 = x1 + 2.0*x2',
                        'y2 = 3.0*x1',
                        'y3 = x1*x2']
                deriv4 = ['dy1_dx1 = 1.0',
                          'dy1_dx2 = 2.0',
                          'dy2_dx1 = 3.0',
                          'dy2_dx2 = 0.0',
                          'dy3_dx1 = x2',
                          'dy3_dx2 = x1']

                exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
                deriv5 = ['dy1_dx1 = 1.0',
                          'dy1_dx2 = 3.0',
                          'dy1_dx3 = 2.0']

                self.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
                self.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
                self.add('comp2b', ExecCompWithDerivatives(exp3, deriv3))
                self.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
                self.add('comp3b', ExecCompWithDerivatives(exp3, deriv3))
                self.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
                self.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

                self.driver.workflow.add(['comp1', 'comp2', 'comp2b', 'comp3', 'comp3b', 'comp4', 'comp5'])

                self.connect('comp1.y1', 'comp2.x1')
                self.connect('comp1.y2', 'comp3.x1')
                self.connect('comp2.y1', 'comp2b.x1')
                self.connect('comp3.y1', 'comp3b.x1')
                self.connect('comp2b.y1', 'comp4.x1')
                self.connect('comp3b.y1', 'comp4.x2')
                self.connect('comp4.y1', 'comp5.x1')
                self.connect('comp4.y2', 'comp5.x2')
                self.connect('comp4.y3', 'comp5.x3')

                self.comp1.x1 = 2.0

                self.create_passthrough('comp1.x1')
                self.create_passthrough('comp5.y1')
                self.create_passthrough('comp1.y2')

                self.driver.system_type = 'serial'


        top = set_as_top(Assembly())
        top.add('sub1', Sub())
        top.add('sub2', Sub())

        top.replace('driver', SpecialDriver())
        top.driver.workflow.add(['sub1', 'sub2'])

        top.driver.add_parameter('sub1.x1', low=-10, high=10)
        top.driver.add_parameter('sub2.x1', low=-10, high=10)
        top.driver.add_objective('sub1.y1 + sub2.y1')

        top.sub1.x1 = 2.0
        top.sub2.x1 = 3.0

        top.run()

        #from openmdao.util.dotgraph import plot_system_tree
        #plot_system_tree(top._system)

        assert_rel_error(self, 9826.25, top._pseudo_0.out0, 0.0001)
        assert_rel_error(self, 9826.25, top.driver.func_dict['_pseudo_0.out0'], 0.0001)


        # Piggyback testing of the is_variable_local function.
        system = top.driver.workflow._system

        # Exclusive or - you either got sub1 or sub2 on a given process.
        self.assertTrue(system.is_variable_local('sub1.comp3.y1') != system.is_variable_local('sub2.comp3.y1'))

    def test_CADRE_bug1(self):

        class AComp(Component):

            x = Array(np.array([7.0]), iotype='in')
            v = Array(np.array([5.0, 3.0]), iotype='in')

            y = Float(1.0, iotype='out')

            def execute(self):

                self.y = self.x[0] * (self.v[0] + self.v[1])

            def list_deriv_vars(self):

                return ('x', 'v'), ('y', )

            def provideJ(self):

                self.J = np.array([self.v[0] + self.v[1], self.x[0], self.x[0]])
                return None

            def apply_deriv(self, arg, result):

                if 'x' in arg:
                    result['y'] += self.J[0] * arg['x']

                if 'v' in arg:
                    result['y'] += self.J[1:].dot(arg['v'])

            def apply_derivT(self, arg, result):

                if 'x' in result:
                    result['x'] += self.J[0] * arg['y']

                if 'v' in result:
                    result['v'] += self.J[1:] * arg['y']


        top = set_as_top(Assembly())
        top.add('nest1', Assembly())
        top.add('nest2', Assembly())
        top.add('driver', SimpleDriver())
        top.nest1.add('comp', AComp())

        top.nest2.add('comp', AComp())

        top.nest1.create_passthrough('comp.x')
        top.nest1.create_passthrough('comp.v')
        top.nest1.create_passthrough('comp.y')
        top.nest2.create_passthrough('comp.x')
        top.nest2.create_passthrough('comp.v')
        top.nest2.create_passthrough('comp.y')

        top.nest1.driver.workflow.add('comp')
        top.nest2.driver.workflow.add('comp')
        top.driver.workflow.add(['nest1', 'nest2'])
        top.driver.add_parameter('nest1.x[0]', low=-10, high=10)
        top.driver.add_parameter('nest1.v', low=-10, high=10)
        top.driver.add_parameter('nest2.x[0]', low=-10, high=10)
        top.driver.add_parameter('nest2.v', low=-10, high=10)
        top.driver.add_objective('nest1.y + nest2.y')

        # Answers: 8, 7, 7
        top.run()
        J = top.driver.calc_gradient(mode='forward', return_format='dict')
        print J

        # Check for Bret (Note, need better way to figure out rank that contains an assembly.)
        if self.comm.rank == 1:
            asys = top.nest1._system
        else:
            asys = top.nest2._system

        # Slice should be there
        self.assertTrue(('x[0]', ('comp.x[0]', 'x[0]')) in asys.variables.keys())

        # Full vec shoulld not
        self.assertTrue(('x', ('comp.x', 'x')) not in asys.variables.keys())


        with MPIContext():
            assert_rel_error(self,
                             J['_pseudo_0.out0']['nest1.x[0]'][0][0],
                             8.0, 0.0001)

if __name__ == '__main__':
    from openmdao.test.mpiunittest import mpirun_tests
    mpirun_tests()
