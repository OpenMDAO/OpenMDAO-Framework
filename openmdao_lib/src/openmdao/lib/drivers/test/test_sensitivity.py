"""
Test the SensitivityDriver component
"""

import unittest

# pylint: disable-msg=F0401,E0611
from openmdao.main.datatypes.api import Array, Float
from openmdao.lib.drivers.sensitivity import SensitivityDriver
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.main.api import Component, Assembly, set_as_top, Driver
from openmdao.util.testutil import assert_rel_error


class Comp(Component):
    """ Evaluates the equation y=x^2"""

    # set up interface to the framework
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in')
    u = Float(0.0, iotype='in')
    y = Float(0.0, iotype='out')
    v = Float(0.0, iotype='out')

    def execute(self):
        """ Executes it """
        self.y = (self.x)**2 + 3.0*self.u**3 + 4*self.u*self.x
        self.v = (self.x)**3 * (self.u)**2


class Assy(Assembly):
    """ Assembly with driver and comp"""

    def configure(self):

        self.add('comp', Comp())
        self.add('driver', SensitivityDriver())
        self.driver.workflow.add(['comp'])

        # Sensitivity inputs
        self.driver.add_parameter('comp.x', low=-9e99, high=9e99)
        self.driver.add_parameter('comp.u', low=-9e99, high=9e99)

        # Sensitivity outputs
        self.driver.add_objective('comp.y')
        self.driver.add_objective('comp.v')
        self.driver.add_constraint('comp.v + comp.y < 0')


class SensitivityDriverTestCase(unittest.TestCase):
    """test SensitivityDriver component"""

    def setUp(self):
        self.model = set_as_top(Assy())

    def tearDown(self):
        self.model = None

    def test_gradient(self):
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        assert_rel_error(self, self.model.driver.dF[0][0],
                               6.0, .001)
        assert_rel_error(self, self.model.driver.dF[0][1],
                               13.0, .001)
        assert_rel_error(self, self.model.driver.dF[1][0],
                               3.0, .001)
        assert_rel_error(self, self.model.driver.dF[1][1],
                               2.0, .001)
        assert_rel_error(self, self.model.driver.dG[0][0],
                               9.0, .001)
        assert_rel_error(self, self.model.driver.dG[0][1],
                               15.0, .001)
        assert_rel_error(self, self.model.driver.x[0],
                               1.0, .001)
        assert_rel_error(self, self.model.driver.F[0],
                               8.0, .001)
        assert_rel_error(self, self.model.driver.F[1],
                               1.0, .001)

    def test_error_messages(self):

        self.model.driver.clear_objectives()
        self.model.driver.clear_constraints()
        try:
            self.model.driver._check()
        except ValueError, err:
            msg = "driver: Missing outputs for gradient calculation"
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')

        self.model.driver.clear_parameters()
        try:
            self.model.driver._check()
        except ValueError, err:
            msg = "driver: Missing inputs for gradient calculation"
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')

    def test_initial_run(self):
        # Test the fix that moved the run_iteration call to the top
        #   of the execute method
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
        top.add('driver', SensitivityDriver())
        top.add('subdriver', SpecialDriver())
        top.driver.workflow.add('subdriver')
        top.subdriver.workflow.add('comp')

        top.subdriver.add_parameter('comp.xx')
        top.driver.add_parameter('comp.x')
        top.driver.add_constraint('comp.y > 1.0')
        top.driver.add_objective('comp.f_x')

        top.run()

class ArrayComp(Component):
    """ Evaluates the equation y=x^2"""

    # set up interface to the framework
    # pylint: disable-msg=E1101
    x = Array([0., 0.], iotype='in')
    y = Float(0.0, iotype='out')
    v = Float(0.0, iotype='out')

    def execute(self):
        """ Executes it """
        self.y = self.x[0]**2 + 3.0*self.x[1]**3 + 4*self.x[1]*self.x[0]
        self.v = self.x[0]**3 * self.x[1]**2


class ArrayAssy(Assembly):
    """ Assembly with driver and comp"""

    def configure(self):
        self.add('comp', ArrayComp())
        self.add('driver', SensitivityDriver())
        self.driver.workflow.add(['comp'])

        # Sensitivity inputs
        self.driver.add_parameter('comp.x', low=-9e99, high=9e99)

        # Sensitivity outputs
        self.driver.add_objective('comp.y')
        self.driver.add_objective('comp.v')
        self.driver.add_constraint('comp.v + comp.y < 0')


class ArrayTest(unittest.TestCase):
    """test SensitivityDriver component"""

    def setUp(self):
        self.model = set_as_top(ArrayAssy())

    def tearDown(self):
        self.model = None

    def test_gradient(self):
        self.model.comp.x = [1., 1.]
        self.model.run()
        assert_rel_error(self, self.model.driver.dF[0][0],  6.0, .001)
        assert_rel_error(self, self.model.driver.dF[0][1], 13.0, .001)
        assert_rel_error(self, self.model.driver.dF[1][0],  3.0, .001)
        assert_rel_error(self, self.model.driver.dF[1][1],  2.0, .001)
        assert_rel_error(self, self.model.driver.dG[0][0],  9.0, .001)
        assert_rel_error(self, self.model.driver.dG[0][1], 15.0, .001)
        assert_rel_error(self, self.model.driver.x[0],      1.0, .001)
        assert_rel_error(self, self.model.driver.F[0],      8.0, .001)
        assert_rel_error(self, self.model.driver.F[1],      1.0, .001)


if __name__ == "__main__":
    unittest.main()
