"""
Test of the Finite Difference differentiator.
"""

import unittest
from nose import SkipTest

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import Float, Int
from openmdao.lib.differentiators.chain_rule import ChainRule
from openmdao.main.api import ComponentWithDerivatives, Assembly, set_as_top
from openmdao.main.driver_uses_derivatives import DriverUsesDerivatives
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.main.hasparameters import HasParameters
from openmdao.test.execcomp import ExecCompWithDerivatives
from openmdao.util.testutil import assert_rel_error
from openmdao.util.decorators import add_delegate

class Comp(ComponentWithDerivatives):
    """ Evaluates the equation y=x^2"""
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in')
    u = Float(0.0, iotype='in')
    y = Float(0.0, iotype='out')
    v = Float(0.0, iotype='out')
    

    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(Comp, self).__init__()
        
        self.derivatives.declare_first_derivative('y', 'x')
        self.derivatives.declare_first_derivative('y', 'u')
        self.derivatives.declare_first_derivative('v', 'x')
        self.derivatives.declare_first_derivative('v', 'u')

    def execute(self):
        """ Executes it """
        
        self.y = (self.x)**2 + 3.0*self.u**3 + 4.0*self.u*self.x
        self.v = (self.x)**3 * (self.u)**2

    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
        
        dy_dx = 2.0*self.x + 4.0*self.u
        dy_du = 9.0*self.u**2 + 4.0*self.x
        dv_dx = 3.0*self.x**2
        dv_du = 2.0*self.u
    
        self.derivatives.set_first_derivative('y', 'x', dy_dx)
        self.derivatives.set_first_derivative('y', 'u', dy_du)
        self.derivatives.set_first_derivative('v', 'x', dv_dx)
        self.derivatives.set_first_derivative('v', 'u', dv_du)

        
@add_delegate(HasParameters, HasObjectives, HasConstraints)
class Driv(DriverUsesDerivatives):
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
        
        self.driver.differentiator = ChainRule()
        
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
        

    def test_Hessian(self):
        
        raise SkipTest("Hessians not supported yet.")
        
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
        
        
    def test_reset_state(self):
        
        raise SkipTest("Test not needed yet.")
        
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



