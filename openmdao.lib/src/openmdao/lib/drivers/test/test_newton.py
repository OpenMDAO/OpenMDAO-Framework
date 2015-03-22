"""
Test the Newton solver
"""

import unittest
import numpy

# pylint: disable=F0401,E0611
from openmdao.lib.drivers.newton_solver import NewtonSolver
from openmdao.lib.optproblems.scalable import Discipline
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives, \
                                            Discipline1, Discipline2
from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.main.datatypes.api import Float
from openmdao.test.execcomp import ExecComp, ExecCompWithDerivatives
from openmdao.util.testutil import assert_rel_error
from openmdao.util.decorators import add_delegate


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
        #self.connect('d2.y2', 'd1.y2')

        self.add('driver', NewtonSolver())
        self.driver.workflow.add(['d1', 'd2'])
        self.driver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.driver.add_constraint('d1.y2 = d2.y2')


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

        self.add('subdriver', NewtonSolver())
        self.driver.workflow.add(['subdriver'])
        self.subdriver.workflow.add(['d1', 'd2'])
        self.driver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.driver.add_constraint('d1.y2 = d2.y2')


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
        #self.connect('d2.y2', 'd1.y2')

        self.add('driver', NewtonSolver())
        self.driver.workflow.add(['d1', 'd2'])
        self.driver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.driver.add_constraint('d1.y2 = d2.y2')

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
        #self.connect('d2.y2', 'd1.y2')

        self.add('driver', NewtonSolver())
        self.driver.workflow.add(['d1', 'd2'])
        self.driver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.driver.add_constraint('d1.y2 = d2.y2')

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
        #self.connect('d2.y2', 'd1.y2')

        self.add('driver', NewtonSolver())
        self.driver.workflow.add(['d1', 'd2'])
        self.driver.add_parameter('d1.y2', low=-1e99, high=1e99)
        self.driver.add_constraint('d1.y2 = d2.y2')


class Scalable_MDA(Assembly):

    def configure(self):

        self.add('d1', Discipline(prob_size=2))
        self.add('d2', Discipline(prob_size=2))

        self.connect('d1.y_out', 'd2.y_in')
        #self.connect('d2.y_out', 'd1.y_in')

        self.add('driver', NewtonSolver())
        self.driver.workflow.add(['d1', 'd2'])
        self.driver.add_parameter('d1.y_in', low=-1e99, high=1e99)
        self.driver.add_constraint('d2.y_out = d1.y_in')
        ##self.driver.add_constraint('d1.y_in = d2.y_out')


class Newton_SolverTestCase(unittest.TestCase):
    """test the Newton Solver component"""

    def setUp(self):
        self.top = set_as_top(Sellar_MDA())

    def tearDown(self):
        self.top = None

    def test_newton(self):

        print self.top.d1.y1, self.top.d2.y1, self.top.d1.y2, self.top.d2.y2
        self.top.run()
        print self.top.d1.y1, self.top.d2.y1, self.top.d1.y2, self.top.d2.y2

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_flip_constraint(self):

        self.top.driver.clear_constraints()
        self.top.driver.add_constraint('d2.y2 = d1.y2')

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_mixed(self):

        self.top = set_as_top(Sellar_MDA_Mixed())

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_mixed_flipped(self):

        self.top = set_as_top(Sellar_MDA_Mixed_Flipped())

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_none(self):

        self.top = set_as_top(Sellar_MDA_None())

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

        driver = a.add('driver', NewtonSolver())
        driver.workflow.add('comp')

        driver.add_parameter('comp.x', 0, 100)
        driver.add_constraint('comp.f=0')
        self.top.driver.gradient_options.fd_step = 0.01
        self.top.driver.gradient_options.fd_step_type = 'relative'

        a.run()

        assert_rel_error(self, a.comp.x, 2.06720359226, .0001)
        assert_rel_error(self, a.comp.f, 0, .0001)

    # The following test generates warnings due to nans and infs in u and df
    # vectors in the newton backtracking.  The test doesn't actually check
    # anything except apparently that we don't raise an exception, so it's
    # not really a good test.
    #def test_initial_run(self):

        #class MyComp(Component):

            #x = Float(0.0, iotype='in')
            #xx = Float(0.0, iotype='in', low=-100000, high=100000)
            #f_x = Float(iotype='out')
            #y = Float(iotype='out')

            #def execute(self):
                #if self.xx != 1.0:
                    #self.raise_exception("Lazy", RuntimeError)
                #self.f_x = 2.0*self.x
                #self.y = self.x

        #@add_delegate(HasParameters)
        #class SpecialDriver(Driver):

            #implements(IHasParameters)

            #def execute(self):
                #self.set_parameters([1.0])

        #top = set_as_top(Assembly())
        #top.add('comp', MyComp())
        #top.add('driver', NewtonSolver())
        #top.add('subdriver', SpecialDriver())
        #top.driver.workflow.add('subdriver')
        #top.subdriver.workflow.add('comp')

        #top.subdriver.add_parameter('comp.xx')
        #top.driver.add_parameter('comp.x')
        #top.driver.add_constraint('comp.y = 1.0')
        #top.driver.max_iteration = 2

        #top.run()

    def test_newton_nested(self):
        # Make sure derivatives across the newton-solved system are correct.

        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())

        top.add('d1', Discipline1_WithDerivatives())
        top.d1.x1 = 1.0
        top.d1.y1 = 1.0
        top.d1.y2 = 1.0
        top.d1.z1 = 5.0
        top.d1.z2 = 2.0

        top.add('d2', Discipline2_WithDerivatives())
        top.d2.y1 = 1.0
        top.d2.y2 = 1.0
        top.d2.z1 = 5.0
        top.d2.z2 = 2.0

        top.connect('d1.y1', 'd2.y1')

        top.add('solver', NewtonSolver())
        top.solver.atol = 1e-9
        top.solver.workflow.add(['d1', 'd2'])
        top.solver.add_parameter('d1.y2', low=-1e99, high=1e99)
        top.solver.add_constraint('d1.y2 = d2.y2')

        top.driver.workflow.add(['solver'])
        top.driver.add_parameter('d1.z1', low=-100, high=100)
        top.driver.add_objective('d1.y1 + d1.y2')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        print J
        assert_rel_error(self, J[0][0], 10.77542099, 1e-5)

        J = top.driver.calc_gradient(mode='adjoint')
        print J
        assert_rel_error(self, J[0][0], 10.77542099, 1e-5)

        top.driver.gradient_options.fd_step = 1e-7
        top.driver.gradient_options.fd_form = 'central'
        J = top.driver.calc_gradient(mode='fd')
        print J
        assert_rel_error(self, J[0][0], 10.77542099, 1e-5)

    def test_equation(self):

        top = set_as_top(Assembly())

        top.add('precomp', ExecCompWithDerivatives(['y=x'],
                                                   ['dy_dx = 1']))
        top.precomp.x = 1.0

        expr = ['y = 3.0*x*x -4.0*x']
        deriv = ['dy_dx = 6.0*x -4.0']

        top.add('comp', ExecCompWithDerivatives(expr, deriv))
        top.driver.workflow.add(['comp'])

        top.add('driver', NewtonSolver())
        top.driver.add_parameter('comp.x')
        top.driver.add_constraint('precomp.y - comp.y = 1.0 - 2.0')

        top.run()

        print top.comp.x, top.comp.y
        assert_rel_error(self, top.comp.x, -0.38742588, 1e-4)


class Sellar_MDA_Cycles(Assembly):

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

        self.add('driver', NewtonSolver())
        self.driver.workflow.add(['d1', 'd2'])

class SysX(Component):

    z = Float(1.0, iotype='in')
    x = Float(1.0, iotype='out')

    def execute(self):
        self.x = 0.8*self.z + 2

    def provideJ(self):
        return numpy.array([[0.8]])

    def list_deriv_vars(self):
        return ['z'], ['x']

class SysY(Component):

    x = Float(1.0, iotype='in')
    z = Float(1.0, iotype='in')
    y = Float(1.0, iotype='out')

    def execute(self):
        self.y = -0.2*self.z + 4.0*self.x + 3

    def provideJ(self):
        return numpy.array([[4.0, -0.2]])

    def list_deriv_vars(self):
        return ['x', 'z'], ['y']

class SysZ(Component):

    x = Float(1.0, iotype='in')
    y = Float(1.0, iotype='in')
    z = Float(1.0, iotype='out')

    def execute(self):
        self.z = 1.0*self.x - 1.0*self.y - 1.0

    def provideJ(self):
        return numpy.array([[1.0, -1.0]])

    def list_deriv_vars(self):
        return ['x', 'y'], ['z']


class DoubleCycle(Assembly):

    def configure(self):

        self.add('SysX', SysX())
        self.add('SysY', SysY())
        self.add('SysZ', SysZ())

        self.connect('SysX.x', 'SysY.x')
        self.connect('SysX.x', 'SysZ.x')
        self.connect('SysY.y', 'SysZ.y')
        self.connect('SysZ.z', 'SysX.z')
        self.connect('SysZ.z', 'SysY.z')

        self.add('driver', NewtonSolver())
        self.driver.workflow.add(['SysX', 'SysY', 'SysZ'])
        #self.driver.add_parameter('SysX.z')
        #self.driver.add_constraint('SysX.z = SysZ.z')


class DoubleCycle_1PC(Assembly):

    def configure(self):

        self.add('SysX', SysX())
        self.add('SysY', SysY())
        self.add('SysZ', SysZ())

        self.connect('SysX.x', 'SysY.x')
        self.connect('SysX.x', 'SysZ.x')
        self.connect('SysY.y', 'SysZ.y')
        self.connect('SysZ.z', 'SysY.z')

        self.add('driver', NewtonSolver())
        self.driver.workflow.add(['SysX', 'SysY', 'SysZ'])
        self.driver.add_parameter('SysX.z')
        self.driver.add_constraint('SysX.z = SysZ.z')



class Newton_SolverTestCase_with_Cycles(unittest.TestCase):
    """test the Newton Solver component with cycles"""

    def setUp(self):
        self.top = set_as_top(Sellar_MDA_Cycles())

    def tearDown(self):
        self.top = None

    def test_newton(self):

        self.top.run()

        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)

    def test_newton_nested(self):
        # Make sure derivatives across the newton-solved system are correct.

        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())

        top.add('d1', Discipline1_WithDerivatives())
        top.d1.x1 = 1.0
        top.d1.y1 = 1.0
        top.d1.y2 = 1.0
        top.d1.z1 = 5.0
        top.d1.z2 = 2.0

        top.add('d2', Discipline2_WithDerivatives())
        top.d2.y1 = 1.0
        top.d2.y2 = 1.0
        top.d2.z1 = 5.0
        top.d2.z2 = 2.0

        top.connect('d1.y1', 'd2.y1')
        top.connect('d2.y2', 'd1.y2')

        top.add('solver', NewtonSolver())
        top.solver.atol = 1e-9
        top.solver.workflow.add(['d1', 'd2'])

        top.driver.workflow.add(['solver'])
        top.driver.add_parameter('d1.z1', low=-100, high=100)
        top.driver.add_objective('d1.y1 + d1.y2')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0][0], 10.77542099, 1e-5)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0][0], 10.77542099, 1e-5)

        top.driver.gradient_options.fd_step = 1e-7
        top.driver.gradient_options.fd_form = 'central'
        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0][0], 10.77542099, 1e-5)

    def test_for_push_scatters(self):
        # This test will fail when we go to push scatters. The problem is
        # that Newton (and linear Gauss Siedel) both need to poke values
        # directly into the u vector and expects those values to be
        # scattered. To make it work, we need to manually scatter them during
        # execution of those solvers.

        top = set_as_top(DoubleCycle_1PC())
        top.run()

        assert_rel_error(self, top.SysX.x, -0.5, .0001)
        assert_rel_error(self, top.SysY.y, 1.625, .0001)
        assert_rel_error(self, top.SysZ.z, -3.125, .0001)

    def test_for_push_scatters__cycle_only(self):
        # This test will fail when we go to push scatters. See Above.
        # However, this flavor of the test was added because of a key error
        # in the code that finds the smallest edges to break.

        top = set_as_top(DoubleCycle())
        top.run()

        assert_rel_error(self, top.SysX.x, -0.5, .0001)
        assert_rel_error(self, top.SysY.y, 1.625, .0001)
        assert_rel_error(self, top.SysZ.z, -3.125, .0001)



if __name__ == "__main__":
    unittest.main()
