""" Tests the scaler/adder example in our docs. This test was inconvenient to test
in its place in the docs. """

# pylint: disable-msg=C0111,C0103
import unittest

from openmdao.lib.datatypes.api import Float
from openmdao.lib.drivers.api import SLSQPdriver
from openmdao.main.api import Assembly,Component
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.util.testutil import assert_rel_error

class Paraboloid_scale(Component):
    """ Evaluates the equation f(x,y) = (1000*x-3)^2 + (1000*x)*(0.01*y) + (0.01*y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')


    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 0.0066666666666666671; y = -733.33333333333337
        """

        x = self.x
        y = self.y

        self.f_xy = (1000.*x-3.)**2 + (1000.*x)*(0.01*y) + (0.01*y+4.)**2 - 3.
        #print "Executing, %.33f, %.33f, %.33f" % (x, y, self.f_xy)


class OptimizationUnconstrainedScale(Assembly):
        """Unconstrained optimization of the unscaled Paraboloid Component."""

        def configure(self):
            """ Creates a new Assembly containing an unscaled Paraboloid and an optimizer"""

            # Create Optimizer instance
            self.add('driver', SLSQPdriver())

            # Create Paraboloid component instances
            self.add('paraboloid', Paraboloid_scale())

            # Driver process definition
            self.driver.workflow.add('paraboloid')

            # SQLSQP Flags
            self.driver.iprint = 0

            # Objective
            self.driver.add_objective('paraboloid.f_xy')

            # Design Variables
            self.driver.add_parameter('paraboloid.x', low=-1000., high=1000., scaler=0.001)
            self.driver.add_parameter('paraboloid.y', low=-1000., high=1000., scaler=1000.0)

class Paraboloid_shift(Component):
        """ Evaluates the equation f(x,y) = (1000*x-3)^2 + (1000*x)*(0.01*(y+1000)) + (0.01*(y+1000)+4)^2 - 3  """

        # set up interface to the framework
        # pylint: disable-msg=E1101
        x = Float(0.0, iotype='in', desc='The variable x')
        y = Float(0.0, iotype='in', desc='The variable y')

        f_xy = Float(iotype='out', desc='F(x,y)')


        def execute(self):
            """f(x,y) = (1000*x-3)^2 + (1000*x)*(0.01*(y+1000)) + (0.01*(y+1000)+4)^2 - 3
            Optimal solution (minimum): x = 0.0066666666666666671; y = -1733.33333333333337
            """

            x = self.x
            y = self.y

            self.f_xy = (1000*x-3)**2 + (1000*x)*(0.01*(y+1000)) + (0.01*(y+1000)+4)**2 - 3

class OptimizationUnconstrainedScaleShift(Assembly):
    """Unconstrained optimization of the Paraboloid Component."""

    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""

        # pylint: disable-msg=E1101

        # Create Optimizer instance
        self.add('driver', SLSQPdriver())

        # Create Paraboloid component instances
        self.add('paraboloid', Paraboloid_shift())

        # Driver process definition
        self.driver.workflow.add('paraboloid')

        # SQLSQP Flags
        self.driver.iprint = 0

        # Objective
        self.driver.add_objective('paraboloid.f_xy')

        # Design Variables
        self.driver.add_parameter('paraboloid.x', low=-1000000., high=1000000.,
                                  scaler=0.001)
        self.driver.add_parameter('paraboloid.y', low=-1000000., high=1000000.,
                                  scaler=1000.0, adder=-1000.0)


class ScalerAdderExampleTestCase(unittest.TestCase):

    def test_scale(self):

        opt_problem = OptimizationUnconstrainedScale()
        opt_problem.run()

        assert_rel_error(self, opt_problem.paraboloid.x, 0.006667, 0.001)
        assert_rel_error(self, opt_problem.paraboloid.y, -733.333313, 0.001)

        J = opt_problem.driver.calc_gradient()
        Jdict = opt_problem.driver.calc_gradient(return_format='dict')

    def test_scale_gradients(self):

        opt_problem = OptimizationUnconstrainedScale()
        opt_problem.replace('driver', SimpleDriver())
        opt_problem.run()

        J = opt_problem.driver.calc_gradient()
        Jdict = opt_problem.driver.calc_gradient(return_format='dict')

        # Make sure untransforming works for dicts too
        self.assertTrue(J[0][0] == Jdict['_pseudo_0.out0']['paraboloid.x'])
        self.assertTrue(J[0][1] == Jdict['_pseudo_0.out0']['paraboloid.y'])

        Jfddict = opt_problem.driver.calc_gradient(mode='fd', return_format='dict')
        opt_problem.driver.run_iteration()
        Jfd = opt_problem.driver.calc_gradient(mode='fd')

        # Make sure untransforming works for dicts too
        self.assertTrue(Jfd[0][0] == Jfddict['_pseudo_0.out0']['paraboloid.x'])
        self.assertTrue(Jfd[0][1] == Jfddict['_pseudo_0.out0']['paraboloid.y'])

    def test_scale_adder(self):

        opt_problem = OptimizationUnconstrainedScaleShift()
        opt_problem.run()

        assert_rel_error(self, opt_problem.paraboloid.x, 0.006667, 0.001)
        assert_rel_error(self, opt_problem.paraboloid.y, -1733.333313, 0.001)


if __name__ == "__main__":
    unittest.main()

