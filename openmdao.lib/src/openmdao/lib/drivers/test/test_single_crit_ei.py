import unittest

from openmdao.lib.drivers.single_crit_ei import SingleCritEI
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator

from openmdao.main.api import Assembly, Case, set_as_top

from openmdao.examples.singleEI.branin_component import BraninComponent

class TestCase(unittest.TestCase): 
    
    def setUp(self): 
        
        self.top = set_as_top(Assembly())
        self.top.add("driver",SingleCritEI())
        self.top.add('bc',BraninComponent())
        self.top.driver.workflow.add(self.top.bc)
        
        self.best_case = Case(outputs=[("f_xy",None,0.397887),('criteria',None,'f_xy')])
        self.bad_best_case = Case(outputs=[("f_xy",None,0.397887)])
        
    def tearDown(self):
        self.top = None
    
    def test_no_criteria_error(self): 
        self.top.driver.best_case = ListCaseIterator([self.best_case,])
        
        try:
            self.top.run()
        except RuntimeError,err: 
            self.assertEqual(str(err),"driver: no criteria was specified")
        else: 
            self.fail("RunTimeError expected")
        
    def test_no_parameters(self):
        self.top.driver.best_case = ListCaseIterator([self.best_case])
        self.top.driver.criteria = "f_xy"
        try:
            self.top.run()
        except RuntimeError,err: 
            self.assertEqual(str(err),"driver: no parameters were added to the driver")
        else: 
            self.fail("RuntimeError expected")
    
    def test_no_criteria_in_best_case(self): 
        
        
        self.top.driver.best_case = ListCaseIterator([self.bad_best_case])
        self.top.driver.criteria = "bc.f_xy"
        self.top.driver.add_parameter('bc.x')
        try:
            self.top.run()
        except ValueError,err: 
            self.assertEqual(str(err),"driver: best_case was not provided with a 'criteria' output, which must be present")
        else: 
            self.fail("ValueError expected")
    
    def test_ei_prediction(self): 
        pass