"""
Test of the Analytic differentiator.
"""

import unittest
from nose import SkipTest

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import Float, Int
from openmdao.lib.differentiators.analytic import Analytic
from openmdao.lib.differentiators.api import FiniteDifference
from openmdao.lib.drivers.api import FixedPointIterator, BroydenSolver
from openmdao.main.api import ComponentWithDerivatives, Assembly, set_as_top
from openmdao.main.driver_uses_derivatives import DriverUsesDerivatives
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.main.hasparameters import HasParameters
from openmdao.test.execcomp import ExecComp, ExecCompWithDerivatives
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

        
class CompFoot(ComponentWithDerivatives):
    """ Evaluates the equation y=x^2"""
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    x = Float(1.0, iotype='in', units='ft')
    y = Float(1.0, iotype='out', units='ft')

    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(CompFoot, self).__init__()
        
        self.derivatives.declare_first_derivative('y', 'x')

    def execute(self):
        """ Executes it """
        
        self.y = 2.0*self.x

    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
        
        dy_dx = 2.0
        self.derivatives.set_first_derivative('y', 'x', dy_dx)
        
class CompInch(ComponentWithDerivatives):
    """ Evaluates the equation y=x^2"""
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    x = Float(1.0, iotype='in', units='inch')
    y = Float(1.0, iotype='out', units='inch')

    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(CompInch, self).__init__()
        
        self.derivatives.declare_first_derivative('y', 'x')

    def execute(self):
        """ Executes it """
        
        self.y = 2.0*self.x

    def calculate_first_derivatives(self):
        """Analytical first derivatives"""
        
        dy_dx = 2.0
        self.derivatives.set_first_derivative('y', 'x', dy_dx)
        
        
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
        
        self.driver.differentiator = Analytic()
        
        self.driver.add_objective('comp.y')
        self.driver.add_objective('comp.v')
        
        # Design Variables 
        self.driver.add_parameter('comp.x', low=-50., high=50., fd_step=.01)
        self.driver.add_parameter('comp.u', low=-50., high=50., fd_step=.01)
        
        self.driver.add_constraint('comp.x + comp.y + 2.0*comp.u < 30.0', name="Con1")
        self.driver.add_constraint('comp.x + comp.y + 3.0*comp.u = 100.0', name="ConE")
        
class AnalyticTestCase(unittest.TestCase):
    """ Test of the Analytic differentiator. """

    def setUp(self):
        self.model = Assy()
        
    def test_simple(self):
        
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
        
    def test_large_dataflow(self):
        
        self.top = set_as_top(Assembly())
    
        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']
    
        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']
        
        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']
    
        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']
        
        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']
        
        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))
    
        self.top.add('driver', Driv())
        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])
        
        self.top.driver.differentiator = Analytic()
        
        obj = 'comp5.y1'
        con = 'comp5.y1-comp3.y1 > 0'
        self.top.driver.add_parameter('comp1.x1', low=-50., high=50., fd_step=.0001)
        self.top.driver.add_objective(obj)
        self.top.driver.add_constraint(con)
        
        self.top.connect('1.0*comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('2.0*comp4.y1', 'comp5.x1')
        self.top.connect('2.0*comp4.y2', 'comp5.x2')
        self.top.connect('2.0*comp4.y3', 'comp5.x3')
    
        self.top.comp1.x1 = 2.0
        self.top.run()
        self.top.driver.differentiator.calc_gradient()
        
        grad = self.top.driver.differentiator.get_gradient(obj)
        assert_rel_error(self, grad[0], 626.0, .001)
        
        grad = self.top.driver.differentiator.get_gradient('comp5.y1-comp3.y1>0')
        assert_rel_error(self, grad[0], -626.0+10.5, .001)
    
    def test_large_dataflow_nested_assys(self):
        
        self.top = set_as_top(Assembly())
    
        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']
    
        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']
        
        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']
    
        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']
        
        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']
        
        self.top.add('nest1', Assembly())
        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.nest1.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.nest1.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.nest1.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))
        
        self.top.add('driver', Driv())
        self.top.driver.workflow.add(['comp1', 'nest1', 'comp5'])
        self.top.nest1.driver.workflow.add(['comp2', 'comp3', 'comp4'])
        
        self.top.driver.differentiator = Analytic()
        
        obj = 'comp5.y1'
        con = 'comp5.y1-nest1.comp3.y1 > 0'
        self.top.driver.add_parameter('comp1.x1', low=-50., high=50., fd_step=.0001)
        self.top.driver.add_objective(obj)
        self.top.driver.add_constraint(con)
        
        self.top.nest1.add('real_c2_x1', Float(iotype='in', desc='I am really here'))
        self.top.nest1.add('real_c3_x1', Float(iotype='in', desc='I am really here'))
        self.top.nest1.add('real_c4_y1', Float(iotype='out', desc='I am really here'))
        self.top.nest1.add('real_c4_y2', Float(iotype='out', desc='I am really here'))
        self.top.nest1.add('real_c4_y3', Float(iotype='out', desc='I am really here'))
    
        #self.top.connect('comp1.y1', 'nest1.comp2.x1')
        self.top.connect('comp1.y1', 'nest1.real_c2_x1')
        self.top.connect('comp1.y2', 'nest1.real_c3_x1')
        self.top.nest1.connect('real_c2_x1', 'comp2.x1')
        self.top.nest1.connect('real_c3_x1', 'comp3.x1')
        self.top.nest1.connect('comp2.y1', 'comp4.x1')
        self.top.nest1.connect('comp3.y1', 'comp4.x2')
        self.top.nest1.connect('comp4.y1', 'real_c4_y1')
        self.top.nest1.connect('comp4.y2', 'real_c4_y2')
        self.top.nest1.connect('comp4.y3', 'real_c4_y3')
        self.top.connect('nest1.real_c4_y1', 'comp5.x1')
        self.top.connect('nest1.real_c4_y2', 'comp5.x2')
        self.top.connect('nest1.real_c4_y3', 'comp5.x3')
        #self.top.connect('nest1.comp4.y1', 'comp5.x1')
        #self.top.connect('nest1.comp4.y3', 'comp5.x3')
    
        self.top.comp1.x1 = 2.0
        self.top.run()
        self.top.driver.differentiator.calc_gradient()
        
        grad = self.top.driver.differentiator.get_gradient(obj)
        assert_rel_error(self, grad[0], 313.0, .001)
        
        grad = self.top.driver.differentiator.get_gradient('comp5.y1-nest1.comp3.y1>0')
        assert_rel_error(self, grad[0], -313.0+10.5, .001)
    
    def test_simple_units(self):
        
        self.top = set_as_top(Assembly())
        
        self.top.add('comp1', CompFoot())
        self.top.add('comp2', CompInch())
        
        self.top.connect('comp1.y', 'comp2.x')
        
        self.top.add('driver', Driv())
        self.top.driver.workflow.add(['comp1', 'comp2'])
        
        self.top.driver.differentiator = Analytic()
        
        obj = 'comp2.y'
        self.top.driver.add_parameter('comp1.x', low=-50., high=50., fd_step=.0001)
        self.top.driver.add_objective(obj)
        
        self.top.comp1.x = 2.0
        self.top.run()
        self.top.driver.differentiator.calc_gradient()
        
        grad = self.top.driver.differentiator.get_gradient(obj)
        assert_rel_error(self, grad[0], 48.0, .001)
        
    def test_subassy_units(self):
        
        self.top = set_as_top(Assembly())
        
        self.top.add('nest1', Assembly())
        self.top.add('comp1', CompFoot())
        self.top.nest1.add('comp2', CompInch())
        self.top.add('comp3', CompFoot())
        
        self.top.nest1.add('nestx', Float(iotype='in', units='inch', desc='Legit connection'))
        self.top.nest1.add('nesty', Float(iotype='out', units='inch', desc='Legit connection'))
        
        #self.top.connect('comp1.y', 'nest1.comp2.x')
        self.top.connect('1.0*comp1.y', 'nest1.nestx')
        self.top.nest1.connect('nestx', 'comp2.x')
        #self.top.connect('nest1.comp2.y', 'comp3.x')
        self.top.nest1.connect('comp2.y', 'nesty')
        self.top.connect('1.0*nest1.nesty', 'comp3.x')
        
        self.top.add('driver', Driv())
        self.top.driver.workflow.add(['comp1', 'nest1', 'comp3'])
        self.top.nest1.driver.workflow.add(['comp2'])
        
        self.top.driver.differentiator = Analytic()
        
        obj = 'comp3.y'
        con = 'nest1.nesty>0'
        self.top.driver.add_parameter('comp1.x', low=-50., high=50., fd_step=.0001)
        self.top.driver.add_objective(obj)
        self.top.driver.add_constraint(con)
        
        self.top.comp1.x = 1.0
        self.top.run()
        self.top.driver.differentiator.calc_gradient()
        
        grad = self.top.driver.differentiator.get_gradient(obj)
        assert_rel_error(self, grad[0], 8.0, .001)
        grad = self.top.driver.differentiator.get_gradient(con)
        assert_rel_error(self, grad[0], -48.0, .001)
        
        # Testing conversion at this boundary (ft instead of inch)
        self.top.nest1.add('nestx', Float(iotype='in', units='ft', desc='Legit connection'))
        self.top.nest1.add('nesty', Float(iotype='out', units='inch', desc='Legit connection'))
        
        self.top.comp1.x = 1.0
        self.top.run()
        self.top.driver.differentiator.calc_gradient()
        
        grad = self.top.driver.differentiator.get_gradient(obj)
        assert_rel_error(self, grad[0], 8.0, .001)
        grad = self.top.driver.differentiator.get_gradient(con)
        assert_rel_error(self, grad[0], -48.0, .001)
        
    def test_simple_coupled(self):
        
        self.top = set_as_top(Assembly())
    
        exp1 = ['y2 = 0.5*sin(x1) - 0.5*x1*y1']
        deriv1 = ['dy2_dx1 = 0.5*cos(x1) - 0.5*y1',
                  'dy2_dy1 = -0.5*x1']
        
        exp2 = ['y1 = x2*x2*y2']
        deriv2 = ['dy1_dx2 = 2.0*y2*x2',
                  'dy1_dy2 = x2*x2']
        
        self.top.add('driver', Driv())
    
        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        #self.top.add('comp1', ExecComp(exp1))
        #self.top.add('comp2', ExecComp(exp2))
        self.top.add('solver', BroydenSolver())
            
        self.top.driver.workflow.add(['solver'])
        self.top.solver.workflow.add(['comp1', 'comp2'])
        self.top.connect('comp1.y2', 'comp2.y2')
        
        # Solver setup
        self.top.solver.add_parameter('comp1.y1', low=-1.e99, high=1.e99)
        self.top.solver.add_constraint('comp2.y1 = comp1.y1')
        
        # Top driver setup
        #self.top.driver.differentiator = FiniteDifference()
        self.top.driver.differentiator = Analytic()
        obj = 'comp2.y1'
        self.top.driver.add_parameter('comp1.x1', low=-100., high=100., fd_step=.001)
        #self.top.driver.add_parameter('comp2.x2', low=-100., high=100., fd_step=.0001)
        self.top.driver.add_objective(obj)
    
        self.top.comp1.x1 = 1.0
        self.top.comp2.x2 = 1.0
        self.top.run()
        self.top.driver.differentiator.calc_gradient()
        
        grad = self.top.driver.differentiator.get_gradient(obj)
        assert_rel_error(self, grad[0], 0.08660, .001)
        
    def test_simple_coupled_adjoint(self):
        
        self.top = set_as_top(Assembly())
    
        exp1 = ['y2 = 0.5*sin(x1) - 0.5*x1*y1']
        deriv1 = ['dy2_dx1 = 0.5*cos(x1) - 0.5*y1',
                  'dy2_dy1 = -0.5*x1']
        
        exp2 = ['y1 = x2*x2*y2']
        deriv2 = ['dy1_dx2 = 2.0*y2*x2',
                  'dy1_dy2 = x2*x2']
        
        self.top.add('driver', Driv())
    
        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        #self.top.add('comp1', ExecComp(exp1))
        #self.top.add('comp2', ExecComp(exp2))
        self.top.add('solver', BroydenSolver())
            
        self.top.driver.workflow.add(['solver'])
        self.top.solver.workflow.add(['comp1', 'comp2'])
        self.top.connect('comp1.y2', 'comp2.y2')
        
        # Solver setup
        self.top.solver.add_parameter('comp1.y1', low=-1.e99, high=1.e99)
        self.top.solver.add_constraint('comp2.y1 = comp1.y1')
        
        # Top driver setup
        self.top.driver.differentiator = Analytic()
        self.top.driver.differentiator.mode = 'adjoint'
        obj = 'comp2.y1'
        self.top.driver.add_parameter('comp1.x1', low=-100., high=100., fd_step=.001)
        self.top.driver.add_parameter('comp2.x2', low=-100., high=100., fd_step=.0001)
        self.top.driver.add_objective(obj)
    
        self.top.comp1.x1 = 1.0
        self.top.comp2.x2 = 1.0
        self.top.run()
        self.top.driver.differentiator.calc_gradient()
        
        grad = self.top.driver.differentiator.get_gradient(obj)
        assert_rel_error(self, grad[0], 0.08660, .001)
        
    def test_medium_coupled(self):
        
        self.top = set_as_top(Assembly())
    
        exp0 = ['y=x']
        deriv0 = ['dy_dx = 1.0']
        
        exp1 = ['y2 = 0.5*sin(x1) - 0.5*x1*y1']
        deriv1 = ['dy2_dx1 = 0.5*cos(x1) - 0.5*y1',
                  'dy2_dy1 = -0.5*x1']
        
        exp2 = ['y1 = x2*x2*y2']
        deriv2 = ['dy1_dx2 = 2.0*y2*x2',
                  'dy1_dy2 = x2*x2']
        
        exp4 = ['y=3.0*x']
        deriv4 = ['dy_dx = 3.0']
        
        self.top.add('driver', Driv())
    
        self.top.add('comp0', ExecCompWithDerivatives(exp0, deriv0))
        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp4, deriv4))
        #self.top.add('comp1', ExecComp(exp1))
        #self.top.add('comp2', ExecComp(exp2))
        self.top.add('solver', BroydenSolver())
            
        self.top.driver.workflow.add(['solver'])
        self.top.solver.workflow.add(['comp0', 'comp1', 'comp2', 'comp3'])
        self.top.connect('comp0.y', 'comp1.x1')
        self.top.connect('comp1.y2', 'comp2.y2')
        self.top.connect('comp2.y1', 'comp3.x')
        
        # Solver setup
        self.top.solver.add_parameter('comp1.y1', low=-1.e99, high=1.e99)
        self.top.solver.add_constraint('comp2.y1 = comp1.y1')
        
        # Top driver setup
        #self.top.driver.differentiator = FiniteDifference()
        self.top.driver.differentiator = Analytic()
        obj = 'comp3.y'
        self.top.driver.add_parameter('comp0.x', low=-100., high=100., fd_step=.001)
        self.top.driver.add_objective(obj)
    
        self.top.comp0.x = 1.0
        self.top.comp2.x2 = 1.0
        self.top.run()
        self.top.driver.differentiator.calc_gradient()
        
        grad = self.top.driver.differentiator.get_gradient(obj)
        assert_rel_error(self, grad[0], 3.0*0.08660, .001)
        
        
if __name__ == '__main__':
    unittest.main()



