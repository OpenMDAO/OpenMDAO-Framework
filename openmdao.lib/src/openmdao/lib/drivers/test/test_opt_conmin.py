"""
Test the CONMIN optimizer component
"""

import unittest
import numpy

# pylint: disable=F0401,E0611
from openmdao.main.api import Assembly, Component, VariableTree, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array, Str, VarTree
from openmdao.lib.casehandlers.api import ListCaseRecorder
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.lib.drivers.conmindriver import CONMINdriver
from openmdao.util.testutil import assert_rel_error


class OptRosenSuzukiComponent(Component):
    """ From the CONMIN User's Manual:
    EXAMPLE 1 - CONSTRAINED ROSEN-SUZUKI FUNCTION. NO GRADIENT INFORMATION.

         MINIMIZE OBJ = X(1)**2 - 5*X(1) + X(2)**2 - 5*X(2) +
                        2*X(3)**2 - 21*X(3) + X(4)**2 + 7*X(4) + 50

         Subject to:

              G(1) = X(1)**2 + X(1) + X(2)**2 - X(2) +
                     X(3)**2 + X(3) + X(4)**2 - X(4) - 8   .LE.0

              G(2) = X(1)**2 - X(1) + 2*X(2)**2 + X(3)**2 +
                     2*X(4)**2 - X(4) - 10                  .LE.0

              G(3) = 2*X(1)**2 + 2*X(1) + X(2)**2 - X(2) +
                     X(3)**2 - X(4) - 5                     .LE.0

    This problem is solved beginning with an initial X-vector of
         X = (1.0, 1.0, 1.0, 1.0)
    The optimum design is known to be
         OBJ = 6.000
    and the corresponding X-vector is
         X = (0.0, 1.0, 2.0, -1.0)
    """

    x = Array(iotype='in', low=-10, high=99)
    g = Array([1., 1., 1.], iotype='out')
    result = Float(iotype='out')
    obj_string = Str(iotype='out')
    opt_objective = Float(iotype='out')

    # pylint: disable=C0103
    def __init__(self):
        super(OptRosenSuzukiComponent, self).__init__()
        self.x = numpy.array([1., 1., 1., 1.], dtype=float)
        self.result = 0.

        self.opt_objective = 6.*10.0
        self.opt_design_vars = [0., 1., 2., -1.]

    def execute(self):
        """calculate the new objective value"""
        x = self.x

        self.result = (x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] +
                       2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50)

        self.obj_string = "Bad"
        #print "rosen", self.x

        self.g[0] = (x[0]**2 + x[0] + x[1]**2 - x[1] +
                     x[2]**2 + x[2] + x[3]**2 - x[3] - 8)
        self.g[1] = (x[0]**2 - x[0] + 2*x[1]**2 + x[2]**2 +
                     2*x[3]**2 - x[3] - 10)
        self.g[2] = (2*x[0]**2 + 2*x[0] + x[1]**2 - x[1] +
                     x[2]**2 - x[3] - 5)
        #print self.x, self.g


class RosenSuzuki2D(Component):
    """ RosenSuzuki with 2D input. """

    x = Array(iotype='in', low=-10, high=99)
    result = Float(iotype='out')
    opt_objective = Float(iotype='out')

    # pylint: disable=C0103
    def __init__(self):
        super(RosenSuzuki2D, self).__init__()
        self.x = numpy.array([[1., 1.], [1., 1.]], dtype=float)
        self.result = 0.

        self.opt_objective = 6.*10.0
        self.opt_design_vars = [0., 1., 2., -1.]

    def execute(self):
        """calculate the new objective value"""
        self.result = (self.x[0][0]**2 - 5.*self.x[0][0] +
                       self.x[0][1]**2 - 5.*self.x[0][1] +
                       2.*self.x[1][0]**2 - 21.*self.x[1][0] +
                       self.x[1][1]**2 + 7.*self.x[1][1] + 50)


class RosenSuzukiMixed(Component):
    """ RosenSuzuki with mixed scalar and 1D inputs. """

    x0 = Float(iotype='in', low=-10, high=99)
    x12 = Array(iotype='in', low=-10, high=99)
    x3 = Float(iotype='in', low=-10, high=99)
    result = Float(iotype='out')
    opt_objective = Float(iotype='out')

    # pylint: disable=C0103
    def __init__(self):
        super(RosenSuzukiMixed, self).__init__()
        self.x0 = 1.
        self.x12 = numpy.array([1., 1.], dtype=float)
        self.x3 = 1.
        self.result = 0.

        self.opt_objective = 6.*10.0
        self.opt_design_vars = [0., 1., 2., -1.]

    def execute(self):
        """calculate the new objective value"""
        self.result = (self.x0**2 - 5.*self.x0 +
                       self.x12[0]**2 - 5.*self.x12[0] +
                       2.*self.x12[1]**2 - 21.*self.x12[1] +
                       self.x3**2 + 7.*self.x3 + 50)


class CONMINdriverTestCase(unittest.TestCase):
    """test CONMIN optimizer component"""

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', CONMINdriver())
        self.top.add('comp', OptRosenSuzukiComponent())
        self.top.driver.workflow.add('comp')
        self.top.driver.iprint = 0
        self.top.driver.itmax = 30

    def test_opt1(self):
        # Run with scalar parameters, scalar constraints, and OpenMDAO gradient.
        self.top.driver.add_objective('10*comp.result')
        # pylint: disable=C0301
        map(self.top.driver.add_parameter,
            ['comp.x[0]', 'comp.x[1]', 'comp.x[2]', 'comp.x[3]'])

        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])
        self.top.recorders = [ListCaseRecorder()]
        self.top.driver.iprint = 0
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

        cases = self.top.recorders[0].get_iterator()
        end_case = cases[-1]

        self.assertEqual(self.top.comp.x[1],
                         end_case.get_input('comp.x[1]'))
        self.assertEqual(10*self.top.comp.result,
                         end_case.get_output('_pseudo_0.out0'))

    def test_opt1_a(self):
        # Run with scalar parameters, 1D constraint, and OpenMDAO gradient.
        self.top.driver.add_objective('10*comp.result')
        # pylint: disable=C0301
        map(self.top.driver.add_parameter,
            ['comp.x[0]', 'comp.x[1]', 'comp.x[2]', 'comp.x[3]'])

        self.top.driver.add_constraint('comp.g <= 0')
        self.top.driver.iprint = 0
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

    def test_opt1_with_CONMIN_gradient(self):
        # Note: all other tests use OpenMDAO gradient
        self.top.driver.add_objective('10*comp.result')
        self.top.driver.add_parameter('comp.x[0]', fd_step=.00001)
        self.top.driver.add_parameter('comp.x[1]', fd_step=.00001)
        self.top.driver.add_parameter('comp.x[2]', fd_step=.00001)
        self.top.driver.add_parameter('comp.x[3]', fd_step=.00001)

        # pylint: disable=C0301
        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])

        self.top.driver.conmin_diff = True
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

    def test_opt1_with_CONMIN_gradient_a(self):
        # Scalar parameters, array constraint, CONMIN gradient.
        # Note: all other tests use OpenMDAO gradient
        self.top.driver.add_objective('10*comp.result')
        self.top.driver.add_parameter('comp.x[0]', fd_step=.00001)
        self.top.driver.add_parameter('comp.x[1]', fd_step=.00001)
        self.top.driver.add_parameter('comp.x[2]', fd_step=.00001)
        self.top.driver.add_parameter('comp.x[3]', fd_step=.00001)

        self.top.driver.add_constraint('comp.g <= 0')

        self.top.driver.conmin_diff = True
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

    def test_opt1_flippedconstraints(self):
        self.top.driver.add_objective('10*comp.result')
        map(self.top.driver.add_parameter,
            ['comp.x[0]', 'comp.x[1]', 'comp.x[2]', 'comp.x[3]'])

        # pylint: disable=C0301
        map(self.top.driver.add_constraint, [
            '8 > comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3]',
            '10 > comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3]',
            '5 > 2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3]'])
        self.top.run()
        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

    def test_gradient_step_size_large(self):
        # Test that a larger value of fd step-size is less acurate

        self.top.driver.add_objective('10*comp.result')
        map(self.top.driver.add_parameter, ['comp.x[0]', 'comp.x[1]',
                                            'comp.x[2]', 'comp.x[3]'])

        # pylint: disable=C0301
        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8.',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10.',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5.'])

        self.top.driver.conmin_diff = True
        self.top.driver.fdch = 1.0e-6
        self.top.driver.fdchm = 1.0e-6
        self.top.run()
        baseerror = abs(self.top.comp.opt_objective - self.top.driver.eval_objective())

        self.top.driver.fdch = .3
        self.top.driver.fdchm = .3
        self.top.comp.x = numpy.array([1., 1., 1., 1.], dtype=float)
        self.top.run()
        newerror = abs(self.top.comp.opt_objective - self.top.driver.eval_objective())

        # pylint: disable=E1101
        if baseerror > newerror:
            self.fail("Coarsening CONMIN gradient step size did not make the objective worse.")

    def test_linear_constraint_specification(self):
        # Note, just testing problem specification and setup

        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter,
            ['comp.x[0]', 'comp.x[1]', 'comp.x[2]', 'comp.x[3]'])

        self.top.driver.add_constraint('comp.x[1] + 3.0*comp.x[2] > 3.0', linear=True)
        self.top.driver.add_constraint('comp.x[2] + comp.x[3] > 13.0', linear=True)
        self.top.driver.add_constraint('comp.x[1] - 0.73*comp.x[3]*comp.x[2] > -12.0', linear=False)
        self.top.driver.itmax = 1

        self.top.run()
        self.assertEqual(self.top.driver._cons_is_linear[0], 1, 1e-6)
        self.assertEqual(self.top.driver._cons_is_linear[1], 1, 1e-6)
        self.assertEqual(self.top.driver._cons_is_linear[2], 0, 1e-6)

        lcons = self.top.driver.get_constraints(linear=True)
        self.assertTrue(len(lcons) == 2)
        self.assertTrue('comp.x[2]+comp.x[3]>13.0' in lcons)
        self.assertTrue('comp.x[1]-0.73*comp.x[3]*comp.x[2]>-12.0' not in lcons)

        lcons = self.top.driver.get_constraints(linear=False)
        self.assertTrue(len(lcons) == 1)
        self.assertTrue('comp.x[2]+comp.x[3]>13.0' not in lcons)
        self.assertTrue('comp.x[1]-0.73*comp.x[3]*comp.x[2]>-12.0' in lcons)

    def test_max_iteration(self):

        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter, ['comp.x[0]', 'comp.x[1]',
                                            'comp.x[2]', 'comp.x[3]'])
        self.top.driver.nscal = -1

        self.top.driver.itmax = 2

        # pylint: disable=C0301
        self.top.run()

        # pylint: disable=E1101
        self.assertEqual(self.top.driver.iter_count, 2)

    def test_remove(self):
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter,
            ['comp.x[0]', 'comp.x[1]', 'comp.x[2]', 'comp.x[3]'])

        # pylint: disable=C0301
        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])

        self.top.remove('comp')
        self.assertEqual(self.top.driver.list_param_targets(), [])
        self.assertEqual(self.top.driver.list_constraints(), [])
        self.assertEqual(self.top.driver.get_objectives(), {})

    def test_initial_run(self):
        # Test the fix that put run_iteration at the top
        #   of the start_iteration method
        class MyComp(Component):

            x = Float(0.0, iotype='in', low=-10, high=10)
            xx = Float(0.0, iotype='in', low=-10, high=10)
            f_x = Float(iotype='out')
            y = Float(iotype='out')

            def execute(self):
                if self.xx != 1.0:
                    self.raise_exception("Lazy", RuntimeError)
                self.f_x = 2.0*self.x
                self.y = self.x

        @add_delegate(HasParameters)
        class SpecialDriver(Driver):

            implements(IHasParameters)

            def execute(self):
                self.set_parameters([1.0])

        top = set_as_top(Assembly())
        top.add('comp', MyComp())
        top.add('driver', CONMINdriver())
        top.add('subdriver', SpecialDriver())
        top.driver.workflow.add('subdriver')
        top.subdriver.workflow.add('comp')

        top.subdriver.add_parameter('comp.xx')
        top.driver.add_parameter('comp.x')
        top.driver.add_constraint('comp.y > 1.0')
        top.driver.add_objective('comp.f_x')

        top.run()


class TestContainer(VariableTree):

    dummy1 = Float(desc='default value of 0.0') #this value is being grabbed by the optimizer
    dummy2 = Float(11.0)


class TestComponent(Component):

    dummy_data = VarTree(TestContainer(), iotype='in')
    x = Float(iotype='out')

    def execute(self):
        self.x = (self.dummy_data.dummy1-3)**2 - self.dummy_data.dummy2


class TestAssembly(Assembly):

    def configure(self):
        self.add('dummy_top', TestContainer())
        self.add('comp', TestComponent())
        self.add('driver', CONMINdriver())

        self.driver.workflow.add(['comp'])
        #self.driver.iprint = 4 #debug verbosity
        self.driver.add_objective('comp.x')
        self.driver.add_parameter('comp.dummy_data.dummy1', low=-10.0, high=10.0)

class CONMINdriverTestCase2(unittest.TestCase):

    def test_vartree_opt(self):
        blah = set_as_top(TestAssembly())
        blah.run()
        self.assertAlmostEqual(blah.comp.dummy_data.dummy1, 3.0, 1) #3.0 should be minimum


class TestCase1D(unittest.TestCase):
    """Test using 1D array connections and 1D array constraint."""

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('comp', OptRosenSuzukiComponent())
        driver = self.top.add('driver', CONMINdriver())
        driver.workflow.add('comp')
        driver.iprint = 0
        driver.itmax = 30

        driver.add_objective('10*comp.result')
        driver.add_parameter('comp.x')

    def test_conmin_gradient_a(self):
        # Run with 1D parameter, 1D constraint, and CONMIN gradient.

        self.top.driver.add_constraint('comp.g <= 0')
        self.top.driver.conmin_diff = True
        self.top.driver.fdch = .000001
        self.top.driver.fdchm = .000001
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

    def test_conmin_gradient_s(self):
        # Run with 1D parameter, scalar constraints, and CONMIN gradient.
        # pylint: disable=C0301
        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])

        self.top.driver.conmin_diff = True
        self.top.driver.fdch = .000001
        self.top.driver.fdchm = .000001
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.driver.eval_objective(),
                         self.top.comp.opt_objective,
                         0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

    def test_openmdao_gradient_a(self):
        # Run with 1D parameter, 1D constraint, and OpenMDAO gradient.
        self.top.driver.add_constraint('comp.g <= 0')
        self.top.driver.conmin_diff = False
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

    def test_openmdao_gradient_s(self):
        # Run with 1D parameter, scalar constraints, and OpenMDAO gradient.
        # pylint: disable=C0301
        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])

        self.top.driver.conmin_diff = False
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[2], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[3], 0.05)

class TestCase2D(unittest.TestCase):
    """Test using 2D array connections."""

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('comp', RosenSuzuki2D())
        driver = self.top.add('driver', CONMINdriver())
        driver.workflow.add('comp')
        driver.iprint = 0
        driver.itmax = 30

        driver.add_objective('10*comp.result')
        driver.add_parameter('comp.x')

        # pylint: disable=C0301
        map(driver.add_constraint, [
            'comp.x[0][0]**2+comp.x[0][0]+comp.x[0][1]**2-comp.x[0][1]+comp.x[1][0]**2+comp.x[1][0]+comp.x[1][1]**2-comp.x[1][1] < 8',
            'comp.x[0][0]**2-comp.x[0][0]+2*comp.x[0][1]**2+comp.x[1][0]**2+2*comp.x[1][1]**2-comp.x[1][1] < 10',
            '2*comp.x[0][0]**2+2*comp.x[0][0]+comp.x[0][1]**2-comp.x[0][1]+comp.x[1][0]**2-comp.x[1][1] < 5'])

    def test_conmin_gradient(self):
        # Run with 2D parameter and CONMIN gradient.
        self.top.driver.conmin_diff = True
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0][0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[0][1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[1][0], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[1][1], 0.05)

    def test_openmdao_gradient(self):
        # Run with 2D parameter and OpenMDAO gradient.
        self.top.driver.conmin_diff = False
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x[0][0], 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x[0][1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x[1][0], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x[1][1], 0.05)


class TestCaseMixed(unittest.TestCase):
    """Test using mixed scalar and 1D connections."""

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('comp', RosenSuzukiMixed())
        driver = self.top.add('driver', CONMINdriver())
        driver.workflow.add('comp')
        driver.iprint = 0
        driver.itmax = 30

        driver.add_objective('10*comp.result')
        map(driver.add_parameter, ['comp.x0', 'comp.x12', 'comp.x3'])

        # pylint: disable=C0301
        map(driver.add_constraint, [
            'comp.x0**2+comp.x0+comp.x12[0]**2-comp.x12[0]+comp.x12[1]**2+comp.x12[1]+comp.x3**2-comp.x3 < 8',
            'comp.x0**2-comp.x0+2*comp.x12[0]**2+comp.x12[1]**2+2*comp.x3**2-comp.x3 < 10',
            '2*comp.x0**2+2*comp.x0+comp.x12[0]**2-comp.x12[0]+comp.x12[1]**2-comp.x3 < 5'])

    def test_conmin_gradient(self):
        # Run with mixed parameters and CONMIN gradient.
        self.top.driver.conmin_diff = True
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x0, 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x12[0], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x12[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x3, 0.05)

    def test_openmdao_gradient(self):
        # Run with mixed parameters and OpenMDAO gradient.
        self.top.driver.conmin_diff = False
        self.top.run()

        # pylint: disable=E1101
        assert_rel_error(self, self.top.comp.opt_objective,
                         self.top.driver.eval_objective(), 0.01)
        assert_rel_error(self, 1 + self.top.comp.opt_design_vars[0],
                         1 + self.top.comp.x0, 0.05)
        assert_rel_error(self, self.top.comp.opt_design_vars[1],
                         self.top.comp.x12[0], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[2],
                         self.top.comp.x12[1], 0.06)
        assert_rel_error(self, self.top.comp.opt_design_vars[3],
                         self.top.comp.x3, 0.05)


if __name__ == "__main__":
    unittest.main()

