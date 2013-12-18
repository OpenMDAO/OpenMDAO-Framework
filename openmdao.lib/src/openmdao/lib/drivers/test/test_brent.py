import unittest

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.util.testutil import assert_rel_error

from openmdao.lib.drivers.brent import Brent


class TestBrentDriver(unittest.TestCase):

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

if __name__ == "__main__": 

	unittest.main()
