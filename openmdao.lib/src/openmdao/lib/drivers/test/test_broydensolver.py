"""
Test the broyden solver component.
"""

import unittest
import numpy

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.lib.drivers.api import BroydenSolver
from openmdao.main.datatypes.api import Array, Float
from openmdao.util.testutil import assert_rel_error, assert_raises

# pylint: disable=E1101,E1103
# "Instance of <class> has no <attr> member"

class SellarDiscipline1(Component):
    """Component containing Discipline 1"""

    # pylint: disable=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    x1 = Float(0.0, iotype='in', desc='Local Design Variable')
    y2 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y1 = Float(iotype='out', desc='Output of this Discipline')

    def execute(self):
        """Evaluates the equation
        y1 = z1**2 + z2 + x1 - 0.2*y2"""

        z1 = self.z1
        z2 = self.z2
        x1 = self.x1
        y2 = self.y2

        self.y1 = z1**2 + z2 + x1 - 0.2*y2


class SellarDiscipline2(Component):
    """Component containing Discipline 2"""

    # pylint: disable=E1101
    z1 = Float(0.0, iotype='in', desc='Global Design Variable')
    z2 = Float(0.0, iotype='in', desc='Global Design Variable')
    y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')

    y2 = Float(iotype='out', desc='Output of this Discipline')

    def execute(self):
        """Evaluates the equation
        y1 = y1**(.5) + z1 + z2"""

        z1 = self.z1
        z2 = self.z2

        # Note: this may cause some issues. However, y1 is constrained to be
        # above 3.16, so lets just let it converge, and the optimizer will
        # throw it out
        y1 = abs(self.y1)

        self.y2 = y1**(.5) + z1 + z2


class SellarBroyden(Assembly):
    """Solution of the sellar analytical problem using MDF.

    Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based, Concur-
    rent Subspace Optimization for Multidisciplinary System Design," Proceedings
    References 79 of the 34th AIAA Aerospace Sciences Meeting and Exhibit, Reno, NV,
    January 1996.
    """

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # pylint: disable=E1101

        # create solver instance
        self.add('driver', BroydenSolver())

        self.add('dis1', SellarDiscipline1())
        self.add('dis2', SellarDiscipline2())
        self.driver.workflow.add(['dis1', 'dis2'])

        self.connect('dis1.y1', 'dis2.y1')

        # solver connections
        self.driver.add_parameter('dis1.y2')
        self.driver.add_constraint('dis2.y2 = dis1.y2')
        self.driver.itmax = 10
        self.driver.alpha = .4
        self.driver.tol = .000000001


class MIMOEquation(Component):
    """Equation with 2 inputs and 2 outputs"""

    # pylint: disable=E1101
    x = Array([1., 1., 1., 1., 1.], iotype='in', desc='Global Design Variables')

    f1 = Float(iotype='out', desc='Output of this Discipline')
    f2 = Float(iotype='out', desc='Output of this Discipline')
    f3 = Float(iotype='out', desc='Output of this Discipline')
    f4 = Float(iotype='out', desc='Output of this Discipline')
    f5 = Float(iotype='out', desc='Output of this Discipline')

    ff = Array([0., 0., 0., 0., 0.], iotype='out')

    def execute(self):
        """Should converge to x=[0,0,0,0,0]"""

        d = numpy.array([3, 2, 1.5, 1, 0.5])
        c = 0.01

        self.ff = -d*self.x - c*self.x**3

        self.f1 = self.ff[0]
        self.f2 = self.ff[1]
        self.f3 = self.ff[2]
        self.f4 = self.ff[3]
        self.f5 = self.ff[4]


class DumbComp(Component):
    """A component whose output is independent of the input."""

    # pylint: disable=E1101
    x1 = Float(1.0, iotype='in', desc='Global Design Variable')
    f1 = Float(3.14, iotype='out', desc='Output of this Discipline')

    def execute(self):
        """Do nothing"""
        pass


class DumbAssembly(Assembly):
    """Assembly with DumbComp.
    """

    def configure(self):

        # create solver instance
        self.add('driver', BroydenSolver())

        self.add('dis1', DumbComp())
        self.driver.workflow.add(['dis1'])

        # solver connections
        self.driver.add_parameter('dis1.x1')
        self.driver.add_constraint('dis1.f1 = 0.0')


class MIMOBroyden(Assembly):
    """Solution of the MIMO problem using MDF.
    """

    def configure(self):
        """ Creates a new Assembly with this problem
        root at (0,1)
        """

        # create solver instance
        self.add('driver', BroydenSolver())

        self.add('dis1', MIMOEquation())
        self.driver.workflow.add(['dis1'])

        # solver connections
        self.driver.itmax = 40
        self.driver.alpha = .8
        self.driver.tol = .000001


class TestCase(unittest.TestCase):
    """ Test the broyden solver. """

    def setUp(self):
        """ Called before each test. """
        self.prob = None

    def tearDown(self):
        """ Called after each test. """
        self.prob = None

    def test_Broyden2(self):

        self.prob = SellarBroyden()
        set_as_top(self.prob)

        self.prob.dis1.z1_in = 5.0
        self.prob.dis1.z2_in = 2.0
        self.prob.dis1.x1 = 1.0
        self.prob.dis2.z1_in = 5.0
        self.prob.dis2.z2_in = 2.0
        self.prob.driver.algorithm = "broyden2"

        self.prob.run()

        assert_rel_error(self, self.prob.dis1.y1, 0.819002, 0.0001)
        assert_rel_error(self, self.prob.dis2.y1, 0.819002, 0.0001)
        assert_rel_error(self, self.prob.dis1.y2, 0.904988, 0.0001)
        assert_rel_error(self, self.prob.dis2.y2, 0.904988, 0.0001)

    def test_Broyden3(self):

        self.prob = SellarBroyden()
        set_as_top(self.prob)

        self.prob.dis1.z1_in = 5.0
        self.prob.dis1.z2_in = 2.0
        self.prob.dis1.x1 = 1.0
        self.prob.dis2.z1_in = 5.0
        self.prob.dis2.z2_in = 2.0
        self.prob.driver.algorithm = "broyden3"

        self.prob.run()

        assert_rel_error(self, self.prob.dis1.y1, 0.819002, 0.0001)
        assert_rel_error(self, self.prob.dis2.y1, 0.819002, 0.0001)
        assert_rel_error(self, self.prob.dis1.y2, 0.904988, 0.0001)
        assert_rel_error(self, self.prob.dis2.y2, 0.904988, 0.0001)

    def test_ExcitingMixing(self):

        self.prob = SellarBroyden()
        set_as_top(self.prob)

        self.prob.dis1.z1_in = 5.0
        self.prob.dis1.z2_in = 2.0
        self.prob.dis1.x1 = 1.0
        self.prob.dis2.z1_in = 5.0
        self.prob.dis2.z2_in = 2.0
        self.prob.driver.algorithm = "excitingmixing"

        self.prob.run()

        assert_rel_error(self, self.prob.dis1.y1, 0.819002, 0.0001)
        assert_rel_error(self, self.prob.dis2.y1, 0.819002, 0.0001)
        assert_rel_error(self, self.prob.dis1.y2, 0.904988, 0.0001)
        assert_rel_error(self, self.prob.dis2.y2, 0.904988, 0.0001)

    def test_MIMO_Broyden2(self):
        # Testing Broyden on a 2 input 2 output case

        self.prob = MIMOBroyden()
        set_as_top(self.prob)

        driver = self.prob.driver
        driver.add_parameter('dis1.x[0]')
        driver.add_parameter('dis1.x[1]')
        driver.add_parameter('dis1.x[2]')
        driver.add_parameter('dis1.x[3]')
        driver.add_parameter('dis1.x[4]')

        driver.add_constraint('dis1.f1 = 0.0')
        driver.add_constraint('dis1.f2 = 0.0')
        driver.add_constraint('dis1.f3 = 0.0')
        driver.add_constraint('dis1.f4 = 0.0')
        driver.add_constraint('dis1.f5 = 0.0')

        self.prob.dis1.x = [1., 1., 1., 1., 1.]
        driver.algorithm = "broyden2"

        self.prob.run()

        assert_rel_error(self, 1.0 - self.prob.dis1.x[0], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[1], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[2], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[3], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[4], 1.0, 0.0001)

    def test_MIMO_Broyden2_array(self):
        # Testing Broyden with an ArrayParameter.

        self.prob = MIMOBroyden()
        set_as_top(self.prob)

        driver = self.prob.driver
        driver.add_parameter('dis1.x')
        driver.add_constraint('dis1.ff = 0.0')

        self.prob.dis1.x = [1., 1., 1., 1., 1.]
        self.prob.dis1.trace = True
        driver.algorithm = "broyden2"

        self.prob.run()

        assert_rel_error(self, 1.0 - self.prob.dis1.x[0], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[1], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[2], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[3], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[4], 1.0, 0.0001)

    def test_MIMO_Broyden3(self):
        # Testing Broyden on a 2 input 2 output case

        self.prob = MIMOBroyden()
        set_as_top(self.prob)

        driver = self.prob.driver
        driver.add_parameter('dis1.x[0]')
        driver.add_parameter('dis1.x[1]')
        driver.add_parameter('dis1.x[2]')
        driver.add_parameter('dis1.x[3]')
        driver.add_parameter('dis1.x[4]')

        driver.add_constraint('dis1.f1 = 0.0')
        driver.add_constraint('dis1.f2 = 0.0')
        driver.add_constraint('dis1.f3 = 0.0')
        driver.add_constraint('dis1.f4 = 0.0')
        driver.add_constraint('dis1.f5 = 0.0')

        self.prob.dis1.x = [1., 1., 1., 1., 1.]
        driver.algorithm = "broyden3"

        self.prob.run()

        assert_rel_error(self, 1.0 - self.prob.dis1.x[0], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[1], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[2], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[3], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[4], 1.0, 0.0001)

    def test_MIMO_ExcitingMixing(self):
        # Testing Broyden on a 2 input 2 output case

        self.prob = MIMOBroyden()
        set_as_top(self.prob)

        driver = self.prob.driver
        driver.add_parameter('dis1.x[0]')
        driver.add_parameter('dis1.x[1]')
        driver.add_parameter('dis1.x[2]')
        driver.add_parameter('dis1.x[3]')
        driver.add_parameter('dis1.x[4]')

        driver.add_constraint('dis1.f1 = 0.0')
        driver.add_constraint('dis1.f2 = 0.0')
        driver.add_constraint('dis1.f3 = 0.0')
        driver.add_constraint('dis1.f4 = 0.0')
        driver.add_constraint('dis1.f5 = 0.0')

        self.prob.dis1.x = [1., 1., 1., 1., 1.]
        driver.algorithm = "excitingmixing"
        driver.alpha = 0.1

        self.prob.run()

        assert_rel_error(self, 1.0 - self.prob.dis1.x[0], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[1], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[2], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[3], 1.0, 0.0001)
        assert_rel_error(self, 1.0 - self.prob.dis1.x[4], 1.0, 0.0001)

    def test_no_change_in_value(self):

        self.prob = DumbAssembly()
        set_as_top(self.prob)

        self.prob.driver.algorithm = "broyden2"
        msg = "Broyden iteration has stopped converging. Change in " \
              "input has produced no change in output. This could " \
              "indicate a problem with your component connections. " \
              "It could also mean that this solver method is " \
              "inadequate for your problem."
        assert_raises(self, 'self.prob.run()', globals(), locals(),
                      RuntimeError, msg)

        self.prob.driver.algorithm = "broyden3"
        msg = "Broyden iteration has stopped converging. Change in " \
              "input has produced no change in output. This could " \
              "indicate a problem with your component connections. " \
              "It could also mean that this solver method is " \
              "inadequate for your problem."
        assert_raises(self, 'self.prob.run()', globals(), locals(),
                      RuntimeError, msg)


    def test_AAAinitial_run(self):
        # The reason for putting the AAA in the name is so it runs
        #   first. We should have to do that. There is some kind
        #   of testing bug that is forcing us to do that

        # Test the fix that peforms an initial run
        #   at the top of the execute method
        class MyComp(Component):

            x = Float(0.0, iotype='in', low=-100000, high=100000)
            xx = Float(0.0, iotype='in', low=-100000, high=100000)
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

        self.prob = set_as_top(Assembly())
        self.prob.add('comp', MyComp())
        self.prob.add('driver', BroydenSolver())
        self.prob.add('subdriver', SpecialDriver())
        self.prob.driver.workflow.add('subdriver')
        self.prob.subdriver.workflow.add('comp')

        self.prob.subdriver.add_parameter('comp.xx')
        self.prob.driver.add_parameter('comp.x')
        self.prob.driver.add_constraint('comp.y = comp.x')
        print "initial run test"
        self.prob.run()

if __name__ == '__main__':
    import nose
    import sys

    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
