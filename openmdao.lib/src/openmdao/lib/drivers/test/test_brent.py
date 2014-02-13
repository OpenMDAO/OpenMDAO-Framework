import unittest

from openmdao.main.api import Assembly, set_as_top, Component, Driver
from openmdao.main.datatypes.api import Float
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.main.interfaces import ISolver
from openmdao.test.execcomp import ExecComp
from openmdao.util.testutil import assert_rel_error
import openmdao.main.pseudocomp as pcompmod

from openmdao.lib.drivers.brent import Brent


class TestBrentDriver(unittest.TestCase):
    
    def setUp(self):
        pcompmod._count = 0 # keep pseudocomp names consistent for each test
                            # to avoid weird stuff like hash order changes

    def test_brent_converge(self): 

        a = set_as_top(Assembly())
        comp = a.add('comp', ExecComp(exprs=["f=a * x**n + b * x - c"]))
        comp.n = 77.0/27.0
        comp.a = 1.0
        comp.b = 1.0
        comp.c = 10.0

        driver = a.add('driver', Brent())
        driver.add_parameter('comp.x', 0, 100)
        driver.add_constraint('comp.f=0')

        a.run()

        assert_rel_error(self, a.comp.x, 2.06720359226, .0001)
        assert_rel_error(self, a.comp.f, 0, .0001)
        
        self.assertTrue(has_interface(driver, ISolver))
                        
    def test_errors(self):
        a = set_as_top(Assembly())
        comp = a.add('comp', ExecComp(exprs=["f=x"]))
        driver = a.add('driver', Brent())
        driver.add_parameter('comp.x', -1e99, 1e99)
        driver.add_constraint('comp.f=0')
        comp.n = 1.0
        comp.c = 0
        driver.lower_bound = 1.0
        try:
            a.run()
        except Exception as err:
            self.assertEqual(str(err), "driver: bounds (low=1.0, high=100.0) do not bracket a root")
        else:
            self.fail("Exception expected")
        
    def test_initial_run(self):

        class MyComp(Component):

            x = Float(0.0, iotype='in', low=-100000, high=100000)
            xx = Float(0.0, iotype='in', low=-100000, high=100000)
            f_x = Float(iotype='out')
            y = Float(iotype='out')

            def execute(self):
                if self.xx != 1.0:
                    self.raise_exception("xx should be 1.0, but it's %s" % self.xx, RuntimeError)
                self.f_x = 2.0*self.x
                self.y = self.x

        @add_delegate(HasParameters)
        class SpecialDriver(Driver):

            implements(IHasParameters)

            def execute(self):
                self.set_parameters([1.0])

        top = set_as_top(Assembly())
        top.add('comp', MyComp())
        top.add('driver', Brent())
        top.add('subdriver', SpecialDriver())
        top.driver.workflow.add('subdriver')
        top.subdriver.workflow.add('comp')

        top.subdriver.add_parameter('comp.xx')
        top.driver.add_parameter('comp.x')
        top.driver.add_constraint('comp.y = 1.0')

        top.run()

if __name__ == "__main__": 

    unittest.main()
