"""
Test of the Finite Difference differentiator.
"""

import unittest

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import Float, Int
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.main.api import Component, Assembly, Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.uses_derivatives import UsesGradients, UsesHessians
from openmdao.util.testutil import assert_rel_error
from openmdao.util.decorators import add_delegate

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

        
@add_delegate(HasParameters, HasObjective, UsesGradients, \
              UsesHessians)
class Driv(Driver):
    """ Simple dummy driver"""
    
    def execute(self):
        """ do nothing """
        
        self.run_iteration()
    
    
class Assy(Assembly):
    """ Assembly with driver and comp"""
    
    def __init__(self):
        """ Initialize it"""
        
        # pylint: disable-msg=E1101
        super(Assy, self).__init__()

        self.add('comp', Comp())
        self.add('driver', Driv())
        self.driver.workflow.add(['comp'])
        
        self.driver.differentiator = FiniteDifference(self.driver)
        
        self.driver.add_objective('comp.y')
        
        # CONMIN Design Variables 
        self.driver.add_parameter('comp.x', low=-50., high=50., fd_step=.01)
        self.driver.add_parameter('comp.u', low=-50., high=50., fd_step=.01)
        

class FiniteDifferenceTestCase(unittest.TestCase):
    """ Test of Component. """

    def setUp(self):
        self.model = Assy()
        
    def test_first_order(self):
        
        self.model.driver.form = 'Central'
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.calc_gradient()
        assert_rel_error(self, self.model.driver.differentiator.gradient_obj[0],
                               6.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.gradient_obj[1],
                               13.0, .001)
        
        for key, item in self.model.driver.get_parameters().iteritems():
            self.model.driver._hasparameters._parameters[key].ffd_step = None
            
        self.model.driver.differentiator.form = 'Forward'
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.default_stepsize = 0.1
        self.model.driver.differentiator.calc_gradient()
        assert_rel_error(self, self.model.driver.differentiator.gradient_obj[0],
                               6.01, .01)

        self.model.driver.differentiator.form = 'Backward'
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.default_stepsize = 0.1
        self.model.driver.differentiator.calc_gradient()
        assert_rel_error(self, self.model.driver.differentiator.gradient_obj[0],
                               5.99, .01)

    def test_Hessian(self):
        
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.default_stepsize = .001
        self.model.driver.differentiator.calc_hessian()
        assert_rel_error(self, self.model.driver.differentiator.hessian_obj[0, 0],
                               2.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.hessian_obj[1, 1],
                               18.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.hessian_obj[0, 1],
                               4.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.hessian_obj[1, 0],
                               4.0, .001)
        

if __name__ == '__main__':
    unittest.main()



