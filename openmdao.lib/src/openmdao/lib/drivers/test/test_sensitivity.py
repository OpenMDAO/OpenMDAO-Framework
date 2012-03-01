"""
Test the SensitivityDriver component
"""

import unittest

# pylint: disable-msg=F0401,E0611
from openmdao.main.datatypes.api import Float
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.sensitivity import SensitivityDriver
from openmdao.main.api import Component, Assembly, set_as_top
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
        self.driver.add_parameter('comp.x', low=-9e99, high=9e99, fd_step=.01)
        self.driver.add_parameter('comp.u', low=-9e99, high=9e99, fd_step=.01)
        
        # Sensitivity outputs
        self.driver.add_objective('comp.y')
        self.driver.add_objective('comp.v')
        
        self.driver.differentiator = FiniteDifference()
        
        
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
        assert_rel_error(self, self.model.driver.x[0], 
                               1.0, .001)
        assert_rel_error(self, self.model.driver.F[0], 
                               8.0, .001)
        assert_rel_error(self, self.model.driver.F[1], 
                               1.0, .001)
        
    def test_error_messages(self):
        
        self.model.driver.clear_objectives()
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
            
    
if __name__ == "__main__":
    unittest.main()    