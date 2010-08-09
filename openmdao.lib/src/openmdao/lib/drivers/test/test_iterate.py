"""
Test the FixedPointIterator component
"""

import unittest

# pylint: disable-msg=F0401,E0611
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.api import Float, FixedPointIterator

class Simple1(Component):
    """ Testing convergence failure"""
    
    invar = Float(0, iotype='in')
    outvar = Float(1, iotype='out')
    
    def execute(self):
        """Will never converge"""
        
        self.outvar = self.invar + 1

class Simple2(Component):
    """ Testing convergence success"""
    
    invar = Float(1, iotype='in')
    outvar = Float(1, iotype='out')
    
    def execute(self):
        """Will always converge"""
        
        self.outvar = self.invar
        
class Simple3(Component):
    """ Testing convergence tolerance"""
    
    invar = Float(1, iotype='in')
    outvar = Float(1.01, iotype='out')
    
    def execute(self):
        """Will converge if tolerance is loose enough"""
        
        self.outvar = self.invar + .01

class FixedPointIteratorTestCase(unittest.TestCase):
    """test FixedPointIterator component"""

    def setUp(self):
        self.top = set_as_top(Assembly())
        
    def tearDown(self):
        self.top = None

    def test_success(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Simple2())
        self.top.driver.workflow.add(self.top.simple)
        
        self.top.driver.x_out = 'simple.outvar'
        self.top.driver.x_in = 'simple.invar'
        self.top.run()
        
        self.assertAlmostEqual(self.top.simple.invar, 
                               self.top.simple.outvar, places=6)
        self.assertEqual(self.top.driver.current_iteration, 1)
            
    def test_maxiteration(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Simple1())
        self.top.driver.workflow.add(self.top.simple)
        self.top.driver.x_out = 'simple.outvar'
        self.top.driver.x_in = 'simple.invar'
        self.top.driver.max_iteration = 3
        try:
            self.top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), 'driver: Max iterations exceeded ' + \
                                       'without convergence.' )
        else:
            self.fail('RuntimeError expected')
        
    def test_tolerance(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Simple3())
        self.top.driver.workflow.add(self.top.simple)
        self.top.driver.x_out = 'simple.outvar'
        self.top.driver.x_in = 'simple.invar'
        self.top.driver.max_iteration = 2
        self.top.driver.tolerance = .001
        try:
            self.top.run()
        except RuntimeError, err:
            self.assertEqual(str(err), 'driver: Max iterations exceeded ' + \
                                       'without convergence.' )
        else:
            self.fail('RuntimeError expected')   
            
        self.top.driver.tolerance = 0.1
        self.top.run()


if __name__ == "__main__":
    unittest.main()
  


