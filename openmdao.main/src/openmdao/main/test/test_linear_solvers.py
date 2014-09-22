"""
Basic unit testing of the linear solvers.
"""

import unittest

import numpy as np

from openmdao.lib.drivers.api import NewtonSolver
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives
from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.main.test.simpledriver import SimpleDriver
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
        self.add('subdriver', NewtonSolver())
        self.driver.workflow.add(['subdriver'])
        self.subdriver.workflow.add(['d1', 'd2'])

        self.subdriver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.subdriver.add_constraint('d1.y2 = d2.y2')

        self.driver.add_parameter('d1.x1', low=-1e99, high=1e99)
        self.driver.add_constraint('d1.y1 < 0')
        self.driver.add_constraint('d2.y2 < 0')


class Testcase_derivatives(unittest.TestCase):
    """ Test derivative aspects of a simple workflow. """

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

        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)


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

        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.workflow.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

    def test_linearGS_Sellar_subbed(self):

        top = set_as_top(Sellar_MDA_subbed())
        top.driver.gradient_options.lin_solver = 'linear_gs'
        top.driver.gradient_options.maxiter = 1
        #top.subdriver.gradient_options.lin_solver = 'linear_gs'
        #top.subdriver.gradient_options.maxiter = 1
        top.run()
        J = top.driver.workflow.calc_gradient(mode='forward')

        print J
        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)

        J = top.driver.workflow.calc_gradient(mode='adjoint')

        print J
        assert_rel_error(self, J[0, 0], 0.9806145, 0.0001)
        assert_rel_error(self, J[1, 0], 0.0969276, 0.0001)




if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()