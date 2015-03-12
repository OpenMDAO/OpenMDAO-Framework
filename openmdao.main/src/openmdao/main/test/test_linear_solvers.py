"""
Basic unit testing of the linear solvers.
"""

import unittest
import numpy as np

from openmdao.examples.simple.paraboloid import Paraboloid
from openmdao.lib.drivers.api import NewtonSolver
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives
from openmdao.main.api import Component, Assembly, set_as_top, Driver
from openmdao.main.datatypes.api import Float
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.main.test.test_derivatives import ArrayComp2D
from openmdao.util.testutil import assert_rel_error

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


class Sellar_MDA_subbed(Assembly):

    def configure(self):

        self.add('d1', Discipline1_WithDerivatives())
        self.d1.x1 = 1.0
        self.d1.y1 = 1.0
        self.d1.y2 = 1.0
        self.d1.z1 = 5.0
        self.d1.z2 = 2.0

        self.add('d2', Discipline2_WithDerivatives())
        self.d2.y1 = 1.0
        self.d2.y2 = 1.0
        self.d2.z1 = 5.0
        self.d2.z2 = 2.0

        self.connect('d1.y1', 'd2.y1')
        #self.connect('d2.y2', 'd1.y2')

        self.add('driver', SimpleDriver())
        self.add('driver2', Driver())
        self.add('subdriver', NewtonSolver())
        self.driver.workflow.add(['driver2'])
        self.driver2.workflow.add(['subdriver'])
        self.subdriver.workflow.add(['d1', 'd2'])

        self.subdriver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.subdriver.add_constraint('d1.y2 = d2.y2')

        self.driver.add_parameter('d1.x1', low=-1e99, high=1e99)
        self.driver.add_constraint('d1.y1 < 0')
        self.driver.add_constraint('d2.y2 < 0')

class Sellar_MDA_subbed_connected(Assembly):

    def configure(self):

        self.add('d1', Discipline1_WithDerivatives())
        self.d1.x1 = 1.0
        self.d1.y1 = 1.0
        self.d1.y2 = 1.0
        self.d1.z1 = 5.0
        self.d1.z2 = 2.0

        self.add('d2', Discipline2_WithDerivatives())
        self.d2.y1 = 1.0
        self.d2.y2 = 1.0
        self.d2.z1 = 5.0
        self.d2.z2 = 2.0

        self.add('P1', Paraboloid())
        self.add('P2', Paraboloid())

        self.connect('d1.y1', 'd2.y1')
        self.connect('P1.f_xy', 'd1.x1')
        self.connect('d1.y1', 'P2.x')
        self.connect('d2.y2', 'P2.y')

        self.add('driver', SimpleDriver())
        self.add('driver2', Driver())
        self.add('subdriver', NewtonSolver())
        self.driver.workflow.add(['P1', 'subdriver', 'P2'])
        self.subdriver.workflow.add(['d1', 'd2'])

        self.subdriver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.subdriver.add_constraint('d1.y2 = d2.y2')

        self.driver.add_parameter('P1.x', low=-1e99, high=1e99)
        self.driver.add_constraint('P2.f_xy < 0')


class Testcase_Scipy_Gmres(unittest.TestCase):
    """ Test gmres linear solver. """

    def test_scipy_gmres_single_comp(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.driver.gradient_options.lin_solver = 'scipy_gmres'

        top.comp.x = 3
        top.comp.y = 5
        top.run()

        self.assertEqual(top.comp.f_xy, 93.)
        self.assertEqual(top._pseudo_0.out0, 93.)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        # Make sure we aren't add-scattering out p vector

        top.run()
        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)


class Testcase_Linear_GS(unittest.TestCase):
    """ Test Linear Gauss Siedel linear solver. """

    def test_linearGS_single_comp(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.driver.gradient_options.lin_solver = 'linear_gs'
        top.driver.gradient_options.maxiter = 1

        top.comp.x = 3
        top.comp.y = 5
        top.run()

        self.assertEqual(top.comp.f_xy, 93.)
        self.assertEqual(top._pseudo_0.out0, 93.)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

    def test_linearGS_Sellar_subbed(self):

        old_diff = Driver.is_differentiable
        def is_differentiable(self):
            return True
        Driver.is_differentiable = is_differentiable

        top = set_as_top(Sellar_MDA_subbed())
        top.driver.gradient_options.lin_solver = 'linear_gs'
        top.driver.gradient_options.maxiter = 1
        top.run()
        J = top.driver.calc_gradient(mode='forward')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

        J = top.driver.calc_gradient(mode='adjoint')

        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

        Driver.is_differentiable = old_diff

    def test_linearGS_Sellar_subbed_connected(self):

        top = set_as_top(Sellar_MDA_subbed_connected())
        top.driver.gradient_options.lin_solver = 'linear_gs'
        top.driver.gradient_options.maxiter = 1
        top.run()
        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], -628.543, 0.01)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], -628.543, 0.01)

    def test_linearGS_simul_element_and_full_connection(self):
        # Added because of a bug with array slices for Linear GS

        top = Assembly()
        top.add('comp1', ArrayComp2D())
        top.add('comp2', ArrayComp2D())

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.connect('comp1.y', 'comp2.x')
        top.driver.add_parameter('comp1.x[0][0]', low=-10, high=10)
        top.driver.add_objective('comp1.y[0][0]')
        top.driver.add_constraint('comp2.y[0][1] < 0')
        top.driver.gradient_options.lin_solver = 'linear_gs'
        top.driver.gradient_options.maxiter = 1

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 2.0, .000001)
        assert_rel_error(self, J[1, 0], 39.0, .000001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 2.0, .000001)
        assert_rel_error(self, J[1, 0], 39.0, .000001)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
