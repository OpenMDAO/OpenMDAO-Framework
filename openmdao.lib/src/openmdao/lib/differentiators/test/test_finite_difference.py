"""
Test of the Finite Difference differentiator.
"""

import unittest

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import Float, Int
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.main.api import Component, Assembly, Driver
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective, HasObjectives
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

        
@add_delegate(HasParameters, HasObjectives, UsesGradients, \
              UsesHessians, HasConstraints)
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
        self.driver.add_objective('comp.v')
        
        # Design Variables 
        self.driver.add_parameter('comp.x', low=-50., high=50., fd_step=.01)
        self.driver.add_parameter('comp.u', low=-50., high=50., fd_step=.01)
        
        self.driver.add_constraint('comp.x + comp.y + 2.0*comp.u < 30.0', name="Con1")
        self.driver.add_constraint('comp.x + comp.y + 3.0*comp.u = 100.0', name="ConE")
        
class FiniteDifferenceTestCase(unittest.TestCase):
    """ Test of Component. """

    def setUp(self):
        self.model = Assy()
        
    def test_first_order(self):
        
        self.model.driver.form = 'central'
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.calc_gradient()
        assert_rel_error(self, self.model.driver.differentiator.get_derivative('comp.y',wrt='comp.x'),
                               6.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.get_derivative('comp.y',wrt='comp.u'),
                               13.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.get_derivative('Con1',wrt='comp.x'),
                               7.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.get_derivative('Con1',wrt='comp.u'),
                               15.0, .001) 
        assert_rel_error(self, self.model.driver.differentiator.get_derivative('ConE',wrt='comp.u'),
                               16.0, .001)
        
        grad = self.model.driver.differentiator.get_gradient('comp.y')
        self.assertEqual(len(grad), 2)
        assert_rel_error(self, grad[0], 6.0, .001)
        assert_rel_error(self, grad[1], 13.0, .001)
        
        grad = self.model.driver.differentiator.get_gradient('comp.v')
        self.assertEqual(len(grad), 2)
        assert_rel_error(self, grad[0], 3.0, .001)
        assert_rel_error(self, grad[1], 2.0, .001)
        
        grad = self.model.driver.differentiator.get_gradient('Con1')
        self.assertEqual(len(grad), 2)
        assert_rel_error(self, grad[0], 7.0, .001)
        assert_rel_error(self, grad[1], 15.0, .001)
        
        grad = self.model.driver.differentiator.get_gradient('ConE')
        self.assertEqual(len(grad), 2)
        assert_rel_error(self, grad[0], 7.0, .001)
        assert_rel_error(self, grad[1], 16.0, .001)
        
        for key, item in self.model.driver.get_parameters().iteritems():
            self.model.driver._hasparameters._parameters[key].ffd_step = None
            
        self.model.driver.differentiator.form = 'forward'
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.default_stepsize = 0.1
        self.model.driver.differentiator.calc_gradient()
        assert_rel_error(self, self.model.driver.differentiator.get_derivative('comp.y',wrt='comp.x'),
                               6.01, .01)

        self.model.driver.differentiator.form = 'backward'
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.default_stepsize = 0.1
        self.model.driver.differentiator.calc_gradient()
        assert_rel_error(self, self.model.driver.differentiator.get_derivative('comp.y',wrt='comp.x'),
                               5.99, .01)

    def test_Hessian(self):
        
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.default_stepsize = .001
        self.model.driver.differentiator.calc_hessian()
        assert_rel_error(self, self.model.driver.differentiator.get_2nd_derivative('comp.y',wrt=('comp.x', 'comp.x')),
                               2.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.get_2nd_derivative('comp.y',wrt=('comp.u', 'comp.u')),
                               18.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.get_2nd_derivative('comp.y',wrt=('comp.x', 'comp.u')),
                               4.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.get_2nd_derivative('comp.y',wrt=('comp.u', 'comp.x')),
                               4.0, .001)
        assert_rel_error(self, self.model.driver.differentiator.get_2nd_derivative('ConE',wrt=('comp.x', 'comp.x')),
                               2.0, .001)        
        assert_rel_error(self, self.model.driver.differentiator.get_2nd_derivative('ConE',wrt=('comp.u', 'comp.x')),
                               4.0, .001)        
        
        hess = self.model.driver.differentiator.get_Hessian('comp.y')
        print hess
        assert_rel_error(self, hess[0][0], 2.0, .001)
        assert_rel_error(self, hess[1][1], 18.0, .001)
        assert_rel_error(self, hess[0][1], 4.0, .001)
        assert_rel_error(self, hess[1][0], 4.0, .001)
        
    def test_reset_state(self):
        
        self.model.driver.form = 'central'
        self.model.comp.x = 1.0
        self.model.comp.u = 1.0
        self.model.run()
        self.model.driver.differentiator.calc_gradient()
        assert_rel_error(self, self.model.comp.u,
                              0.99, .0001)
        self.model.driver.differentiator.reset_state()
        assert_rel_error(self, self.model.comp.u,
                              1.0, .0001)

if __name__ == '__main__':
    unittest.main()



