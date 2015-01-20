"""
Test the SLSQP optimizer driver
"""

import unittest
import numpy

# pylint: disable=F0401,E0611
from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array, Str
from openmdao.lib.casehandlers.api import ListCaseRecorder
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.lib.drivers.slsqpdriver import SLSQPdriver
from openmdao.util.testutil import assert_raises, assert_rel_error


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

        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]

    def execute(self):
        """calculate the new objective value"""
        x = self.x

        self.result = (x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] +
                       2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50)

        self.obj_string = "Bad"

        self.g[0] = (x[0]**2 + x[0] + x[1]**2 - x[1] +
                     x[2]**2 + x[2] + x[3]**2 - x[3] - 8)
        self.g[1] = (x[0]**2 - x[0] + 2*x[1]**2 + x[2]**2 +
                     2*x[3]**2 - x[3] - 10)
        self.g[2] = (2*x[0]**2 + 2*x[0] + x[1]**2 - x[1] +
                     x[2]**2 - x[3] - 5)


class SLSPQdriverTestCase(unittest.TestCase):
    """test SLSQP optimizer component"""

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('driver', SLSQPdriver())
        self.top.add('comp', OptRosenSuzukiComponent())
        self.top.driver.workflow.add('comp')
        self.top.driver.iprint = 0

    def tearDown(self):
        self.top = None

    def test_opt1(self):
        # Run with scalar parameters, scalar constraints.
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter,
            ['comp.x[0]', 'comp.x[1]', 'comp.x[2]', 'comp.x[3]'])

        # pylint: disable=C0301
        map(self.top.driver.add_constraint, [
            'comp.x[0]**2+comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2+comp.x[2]+comp.x[3]**2-comp.x[3] < 8',
            'comp.x[0]**2-comp.x[0]+2*comp.x[1]**2+comp.x[2]**2+2*comp.x[3]**2-comp.x[3] < 10',
            '2*comp.x[0]**2+2*comp.x[0]+comp.x[1]**2-comp.x[1]+comp.x[2]**2-comp.x[3] < 5'])
        self.top.recorders = [ListCaseRecorder()]

        self.top.run()

        # pylint: disable=E1101
        self.assertAlmostEqual(self.top.comp.opt_objective,
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0],
                               self.top.comp.x[0], places=1)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1],
                               self.top.comp.x[1], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[2],
                               self.top.comp.x[2], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[3],
                               self.top.comp.x[3], places=1)

        cases = self.top.recorders[0].get_iterator()
        end_case = cases[-1]

        self.assertEqual(self.top.comp.x[1],
                         end_case.get_input('comp.x[1]'))
        self.assertEqual(self.top.comp.result,
                         end_case.get_output('_pseudo_0.out0'))

    def test_max_iter(self):
        self.top.driver.add_objective('comp.result')
        map(self.top.driver.add_parameter,
            ['comp.x[0]', 'comp.x[1]', 'comp.x[2]', 'comp.x[3]'])
        self.top.driver.maxiter = 2

        self.top.run()

        self.assertEqual(self.top.driver.error_code, 9)

    def test_array(self):
        # Run with array parameter, array constraint.
        self.top.driver.add_objective('comp.result')
        self.top.driver.add_parameter('comp.x')
        self.top.driver.add_constraint('comp.g < 0')
        self.top.recorders = [ListCaseRecorder()]

        self.top.run()

        # pylint: disable=E1101
        self.assertAlmostEqual(self.top.comp.opt_objective,
                               self.top.driver.eval_objective(), places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[0],
                               self.top.comp.x[0], places=1)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[1],
                               self.top.comp.x[1], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[2],
                               self.top.comp.x[2], places=2)
        self.assertAlmostEqual(self.top.comp.opt_design_vars[3],
                               self.top.comp.x[3], places=1)

        cases = self.top.recorders[0].get_iterator()
        end_case = cases[-1]

        self.assertEqual(self.top.comp.x[1],
                         end_case.get_input('comp.x')[1])
        self.assertEqual(self.top.comp.result,
                         end_case.get_output('_pseudo_0.out0'))

    def test_reconfig(self):
        # Test that ArrayParameters can be configured at run() time.

        class MyComp(Component):

            x = Array([0, 0], iotype="in", dtype=float)
            y = Array([0, 0], iotype="out", dtype=float)

            def execute(self):
                self.y = self.x**2

        class MyAssembly(Assembly):

            def configure(self):
                self.add('comp1', MyComp())
                self.add('comp2', MyComp())
                driver = self.add('driver', SLSQPdriver())
                driver.add_parameter('comp1.x', low=-10, high=10)
                driver.add_parameter('comp2.x', low=-10, high=10)
                driver.add_objective('comp1.y[0] + comp2.y[1]')

        asm = set_as_top(MyAssembly())
        asm.comp1.x = [1, 2, 3]
        asm.comp2.x = [2.781828, 3.14159]
        asm.run()

        assert_rel_error(self, asm.comp1.x, [0, 2, 3], 1e-6)
        assert_rel_error(self, asm.comp1.y, [0, 4, 9], 1e-6)
        assert_rel_error(self, asm.comp2.x, [2.781828, 0], 1e-6)
        assert_rel_error(self, asm.comp2.y, [7.738567, 0], 1e-6)

    def test_invalid_reconfig(self):
        # Test invalid reconfiguration of ArrayParameter.

        class MyComp(Component):

            x = Array([0, 0], iotype="in", dtype=float)
            y = Array([0, 0], iotype="out", dtype=float)

            def execute(self):
                self.y = self.x**2

        class MyAssembly(Assembly):

            def configure(self):
                self.add('comp1', MyComp())
                self.add('comp2', MyComp())
                driver = self.add('driver', SLSQPdriver())
                driver.add_parameter('comp1.x', low=-10, high=[10, 10])
                driver.add_parameter('comp2.x', low=-10, high=10)
                driver.add_objective('comp1.y[0] + comp2.y[1]')

        asm = set_as_top(MyAssembly())
        asm.comp1.x = [1, 2, 3]
        asm.comp2.x = [2.781828, 3.14159]
        assert_raises(self, 'asm.run()', globals(), locals(), RuntimeError,
                      "Parameter comp1.x can't be reconfigured,"
                      " 'high' was not specified as a scalar")

    def test_initial_run(self):
        # Test the fix that added a run_iteration top of the
        #   start_iteration method
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
        top.add('driver', SLSQPdriver())
        top.add('subdriver', SpecialDriver())
        top.driver.workflow.add('subdriver')
        top.subdriver.workflow.add('comp')

        top.subdriver.add_parameter('comp.xx')
        top.driver.add_parameter('comp.x')
        top.driver.add_constraint('comp.y > 1.0')
        top.driver.add_objective('comp.f_x')

        top.run()



if __name__ == "__main__":
    unittest.main()

