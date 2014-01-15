"""
Test the Newton solver
"""

import unittest
from nose import SkipTest
import numpy

# pylint: disable-msg=F0401,E0611
from openmdao.lib.drivers.newton_solver import NewtonKrylov
from openmdao.lib.optproblems.scalable import Discipline
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives, \
                                            Discipline1, Discipline2
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.test.execcomp import ExecComp
from openmdao.util.testutil import assert_rel_error


class Sellar_MDA(Assembly):

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
        self.connect('d2.y2', 'd1.y2')

        self.add('driver', NewtonKrylov())
        self.driver.workflow.add(['d1', 'd2'])


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
        self.connect('d2.y2', 'd1.y2')

        self.add('subdriver', NewtonKrylov())
        self.driver.workflow.add(['subdriver'])
        self.subdriver.workflow.add(['d1', 'd2'])


class Sellar_MDA_Mixed(Assembly):

    def configure(self):

        self.add('d1', Discipline1())
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
        self.connect('d2.y2', 'd1.y2')

        self.add('driver', NewtonKrylov())
        self.driver.workflow.add(['d1', 'd2'])

class Sellar_MDA_Mixed_Flipped(Assembly):

    def configure(self):

        self.add('d1', Discipline1_WithDerivatives())
        self.d1.x1 = 1.0
        self.d1.y1 = 1.0
        self.d1.y2 = 1.0
        self.d1.z1 = 5.0
        self.d1.z2 = 2.0

        self.add('d2', Discipline2())
        self.d2.y1 = 1.0
        self.d2.y2 = 1.0
        self.d2.z1 = 5.0
        self.d2.z2 = 2.0

        self.connect('d1.y1', 'd2.y1')
        self.connect('d2.y2', 'd1.y2')

        self.add('driver', NewtonKrylov())
        self.driver.workflow.add(['d1', 'd2'])

class Sellar_MDA_None(Assembly):

    def configure(self):

        self.add('d1', Discipline1())
        self.d1.x1 = 1.0
        self.d1.y1 = 1.0
        self.d1.y2 = 1.0
        self.d1.z1 = 5.0
        self.d1.z2 = 2.0

        self.add('d2', Discipline2())
        self.d2.y1 = 1.0
        self.d2.y2 = 1.0
        self.d2.z1 = 5.0
        self.d2.z2 = 2.0

        self.connect('d1.y1', 'd2.y1')
        self.connect('d2.y2', 'd1.y2')

        self.add('driver', NewtonKrylov())
        self.driver.workflow.add(['d1', 'd2'])



class Scalable_MDA(Assembly):

    def configure(self):

        self.add('d1', Discipline(prob_size=2))
        self.add('d2', Discipline(prob_size=2))

        self.connect('d1.y_out', 'd2.y_in')
        self.connect('d2.y_out', 'd1.y_in')

        self.add('driver', NewtonKrylov())
        self.driver.workflow.add(['d1', 'd2'])
        self.driver.newton = True


class MDA_SolverTestCase(unittest.TestCase):
    """test the MDA Solver component"""

    def setUp(self):
        self.top = set_as_top(Sellar_MDA())

    def tearDown(self):
        self.top = None

    def test_newton(self):

        self.top.driver.newton = True
        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_param_con(self):

        self.top.disconnect('d2.y2')
        self.top.driver.add_parameter('d1.y2', low=-100, high=100)
        self.top.driver.add_constraint('d1.y2 = d2.y2')
        self.top.driver.newton = True
        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_mixed(self):

        self.top = set_as_top(Sellar_MDA_Mixed())
        self.top.driver.newton = True

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_mixed_flipped(self):

        self.top = set_as_top(Sellar_MDA_Mixed_Flipped())
        self.top.driver.newton = True

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_none(self):

        self.top = set_as_top(Sellar_MDA_None())
        self.top.driver.newton = True

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_none_param_con(self):

        self.top.disconnect('d2.y2')
        self.top.driver.add_parameter('d1.y2', low=-100, high=100)
        self.top.driver.add_constraint('d1.y2 = d2.y2')
        self.top = set_as_top(Sellar_MDA_None())
        self.top.driver.newton = True
        self.top.driver.gradient_options.fd_step_size = 0.01
        self.top.driver.gradient_options.fd_step_type = 'relative'

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_scalable_newton(self):

        # This verifies that it works for arrays

        self.top = set_as_top(Scalable_MDA())

        self.top.d1.x = self.top.d2.x = numpy.array([[3.0], [-1.5]])
        self.top.d1.z = self.top.d2.z = numpy.array([[-1.3], [2.45]])
        self.top.d1.C_y = numpy.array([[1.1, 1.3], [1.05, 1.13]])
        self.top.d2.C_y = numpy.array([[0.95, 0.98], [0.97, 0.95]])

        self.top.run()

        assert_rel_error(self, self.top.d1.y_out[0],
                               self.top.d2.y_in[0],
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y_out[1],
                               self.top.d2.y_in[1],
                               1.0e-4)
        assert_rel_error(self, self.top.d2.y_out[0],
                               self.top.d1.y_in[0],
                               1.0e-4)
        assert_rel_error(self, self.top.d2.y_out[1],
                               self.top.d1.y_in[1],
                               1.0e-4)

    def test_general_solver(self):

        a = set_as_top(Assembly())
        comp = a.add('comp', ExecComp(exprs=["f=a * x**n + b * x - c"]))
        comp.n = 77.0/27.0
        comp.a = 1.0
        comp.b = 1.0
        comp.c = 10.0
        comp.x = 0.0

        driver = a.add('driver', NewtonKrylov())

        driver.add_parameter('comp.x', 0, 100)
        driver.add_constraint('comp.f=0')
        self.top.driver.newton = True
        self.top.driver.gradient_options.fd_step_size = 0.01
        self.top.driver.gradient_options.fd_step_type = 'relative'

        a.run()

        assert_rel_error(self, a.comp.x, 2.06720359226, .0001)
        assert_rel_error(self, a.comp.f, 0, .0001)

if __name__ == "__main__":
    unittest.main()
