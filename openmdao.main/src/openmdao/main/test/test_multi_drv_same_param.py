import unittest

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator

from openmdao.lib.optproblems import sellar
from openmdao.main.test.test_derivatives import SimpleDriver
from openmdao.examples.simple.paraboloid import Paraboloid
from openmdao.util.testutil import assert_rel_error

class Downstream(Component):
    
    x = Array([[0.0]], iotype='in')
    y = Float(0.0, iotype='out')
    
    def execute(self):
        self.y = self.x[0][0]

class Top(Assembly):
    
    def configure(self):
        
        self.add('p1pre', Paraboloid())
        #self.add('p1', Paraboloid())
        self.add('down', Downstream())
        
        self.add('driver', SimpleDriver())
        self.add('sens', SensitivityDriver())
        
        self.driver.add_parameter('p1pre.x', low=-100, high=101)
        self.driver.add_objective('down.y')
        
        self.sens.add_parameter('p1pre.x', low=100, high=101)
        self.sens.add_objective('p1pre.f_xy')
        self.connect('sens.dF', 'down.x')
        
        self.driver.workflow.add(['sens', 'down'])
        self.sens.workflow.add(['p1pre'])
        
        
class MultiDrvSameParamTestCase(unittest.TestCase):
    
    def test_MDF(self):
        top = set_as_top(Top())
        
        top.p1pre.x = 12.0
        top.p1pre.y = 13.0
        
        top.run()
        assert_rel_error(self, 31., top.sens.dF, 0.00001)
        assert_rel_error(self, 31., top.down.y, 0.00001)


if __name__ == '__main__':
    unittest.main()
