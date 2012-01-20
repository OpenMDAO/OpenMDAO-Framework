"""
Test the FixedPointIterator component
"""

import unittest

# pylint: disable-msg=F0401,E0611
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.lib.datatypes.api import Float
from openmdao.lib.drivers.iterate import FixedPointIterator, IterateUntil
from openmdao.util.testutil import assert_rel_error


class Simple1(Component):
    """ Testing convergence failure"""
    
    invar = Float(0, iotype='in')
    extra_invar = Float(0, iotype='in')
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
        
class Simple4(Component): 
    """Testing for iteration counting and stop conditions"""
    invar = Float(1,iotype="in")
    outvar = Float(0,iotype="out")
    
    def __init__(self): 
        super(Simple4,self).__init__()
        self.force_execute = True
    def execute(self):
        self.outvar = self.outvar + self.invar

class Multi(Component): 
    """Testing for iteration counting and stop conditions"""
    in1 = Float(1.0, iotype="in")
    in2 = Float(1.0, iotype="in")
    out1 = Float(0, iotype="out")
    out2 = Float(0, iotype="out")
    
    def __init__(self): 
        super(Multi, self).__init__()
    def execute(self):
        self.out1 = self.in1/10.0
        self.out2 = self.in2/10.0

class FixedPointIteratorTestCase(unittest.TestCase):
    """test FixedPointIterator component"""

    def setUp(self):
        self.top = set_as_top(Assembly())
        
    def tearDown(self):
        self.top = None

    def test_success(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Simple2())
        self.top.driver.workflow.add('simple')
        
        self.top.driver.add_constraint('simple.outvar - simple.invar = 0')
        self.top.driver.add_parameter('simple.invar', -9e99, 9e99)
        self.top.run()
        
        self.assertAlmostEqual(self.top.simple.invar, 
                               self.top.simple.outvar, places=6)
        self.assertEqual(self.top.driver.current_iteration, 1)
            
    def test_multi_success(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Multi())
        self.top.driver.workflow.add('simple')
        
        self.top.driver.add_constraint('simple.out1 = simple.in1')
        self.top.driver.add_constraint('simple.out2 = simple.in2')
        self.top.driver.add_parameter('simple.in1', -9e99, 9e99)
        self.top.driver.add_parameter('simple.in2', -9e99, 9e99)
        self.top.driver.tolerance = .02
        self.top.run()
        
        assert_rel_error(self, self.top.simple.in1, .01, .002)
        assert_rel_error(self, self.top.simple.out1, .001, .0002)
        self.assertEqual(self.top.driver.current_iteration, 2)
            
    def test_maxiteration(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Simple1())
        self.top.driver.workflow.add('simple')
        self.top.driver.add_constraint('simple.outvar - simple.invar = 0')
        self.top.driver.add_parameter('simple.invar', -9e99, 9e99)
        self.top.driver.max_iteration = 3
        
        self.top.run()
        self.assertEqual(self.top.driver.current_iteration, 2)
        
    def test_check_config(self):
        self.top.add("driver", FixedPointIterator())
        self.top.add("simple", Multi())
        self.top.driver.workflow.add('simple')
        
        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: FixedPointIterator requires a constraint equation."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')
            
        self.top.driver.add_constraint('simple.out1 - simple.in1 = 0')

        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: FixedPointIterator requires an input parameter."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')

        self.top.driver.add_parameter('simple.in1', -9e99, 9e99)
        self.top.driver.add_parameter('simple.in2', -9e99, 9e99)
        
        try:
            self.top.run()
        except RuntimeError, err:
            msg = "driver: The number of input parameters must equal the number" + \
                  " of output constraint equations in FixedPointIterator."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')


class TestIterateUntill(unittest.TestCase): 
    """Test case for the IterateUntil Driver""" 
    
    def setUp(self):
        self.top = set_as_top(Assembly())
        
    def tearDown(self):
        self.top = None
        
    def test_max_iterations(self): 
        self.top.add("driver",IterateUntil())
        self.top.driver.max_iterations = 3;
        
        self.top.driver.workflow.add('simple')
        
        self.top.add('simple',Simple4())
        self.top.simple.invar = 1
        
        
        self.top.run()
        
        self.assertEqual(self.top.driver.iteration,3)
        self.assertEqual(self.top.simple.outvar, 3)
       
    def test_stop_conditions(self): 
        self.top.add("driver",IterateUntil())
        self.top.driver.max_iterations = 10;
        
        
        self.top.driver.workflow.add('simple')
        
        self.top.add('simple',Simple4())
        self.top.simple.invar = 1
        self.top.driver.add_stop_condition("simple.outvar >= 2")
        
        self.top.run()
        
        self.assertEqual(self.top.driver.iteration,2)
        self.assertEqual(self.top.simple.outvar, 2)
     
if __name__ == "__main__":
    unittest.main()
  


