import unittest
from math import pi,cos

from enthought.traits.api import Instance

from openmdao.lib.drivers.single_crit_ei import SingleCritEI
from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator
from openmdao.lib.traits.float import Float

from openmdao.main.api import Assembly, Case, set_as_top, Component
from openmdao.main.uncertain_distributions import NormalDistribution, UncertainDistribution


class NoisyBraninComponent(Component): 
    x = Float(0,iotype="in",low=-5,high=10)
    y = Float(0,iotype="in",low=0,high=15)
    
    f_xy = Instance(UncertainDistribution,iotype="out")
    
    def execute(self):
        x = self.x 
        y = self.y
        f_xy = (y-(5.1/(4.*pi**2.))*x**2.+5.*x/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x)+10.
        self.f_xy = NormalDistribution(f_xy,.1)

class TestCase(unittest.TestCase): 
    
    def setUp(self): 
        
        self.top = set_as_top(Assembly())
        
        self.top.add("EIdriver",SingleCritEI())
        self.top.add("noisy_branin",NoisyBraninComponent())

        self.best_case = Case(outputs=[("noisy_branin.f_xy",None,50.)])
        self.bad_best_case = Case(outputs=[("noisy_branin.f_xyz",None,50.)])

        self.top.EIdriver.workflow.add(self.top.noisy_branin)

        self.top.driver.workflow.add(self.top.EIdriver)
        
    def tearDown(self):
        self.top = None
    
    def test_no_criteria_error(self): 
        self.top.EIdriver.best_case = ListCaseIterator([self.best_case,])
        try:
            self.top.run()
        except ValueError,err: 
            self.assertEqual(str(err),"EIdriver: The Expression 'criteria' has not been defined")
        else: 
            self.fail("ValueError expected")
        
    def test_no_parameters(self):
        self.top.EIdriver.best_case = ListCaseIterator([self.best_case])
        self.top.EIdriver.criteria = "f_xy"
        try:
            self.top.run()
        except RuntimeError,err: 
            self.assertEqual(str(err),"EIdriver: no parameters were added to the driver")
        else: 
            self.fail("RuntimeError expected")
    
    def test_no_criteria_in_best_case(self): 
        
        self.top.EIdriver.best_case = ListCaseIterator([self.bad_best_case])
        self.top.EIdriver.criteria = "noisy_branin.f_xy"
        self.top.EIdriver.add_parameter('noisy_branin.x')
        try:
            self.top.run()
        except ValueError,err: 
            self.assertEqual(str(err),
                "EIdriver: best_case did not have an output which matched the criteria, 'noisy_branin.f_xy'")
        else: 
            self.fail("ValueError expected")
        
    def test_add_parameter(self):
        """test for correct ranges on alleles for GA"""
        self.top.EIdriver.add_parameter("noisy_branin.x")
        self.top.EIdriver.add_parameter("noisy_branin.y")

        self.assertEqual(self.top.EIdriver.set_of_alleles[0][0],(-5,10))
        self.assertEqual(self.top.EIdriver.set_of_alleles[1][0],(0,15))
    
    def test_ei_prediction(self): 
        self.top.EIdriver.add_parameter("noisy_branin.x")
        self.top.EIdriver.add_parameter("noisy_branin.y")      
        self.top.EIdriver.best_case = ListCaseIterator([self.best_case])
        self.top.EIdriver.criteria = "noisy_branin.f_xy"

        self.top.run()
        
        result = [val[2] for case in self.top.EIdriver.next_case for val in case.inputs]
        self.assertAlmostEqual(result[0], 3.16, places=1)
        self.assertAlmostEqual(result[1], 2.345, places=1)
        
if __name__ == "__main__":
    unittest.main()