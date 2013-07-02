"""
This mainly tests the CyclicWorkflow's ability to generate its topological
sort.
"""

import unittest

try:
    from numpy import zeros, array, identity
except ImportError as err:
    from openmdao.main.numpy_fallback import zeros, array

from openmdao.main.api import Component, VariableTree, Driver, Assembly, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.derivatives import applyJ
from openmdao.test.execcomp import ExecCompWithDerivatives, ExecComp
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_rel_error

class Tree2(VariableTree):

    d1 = Array(zeros((1, 2)))

class Tree1(VariableTree):

    a1 = Float(3.)
    vt1 = VarTree(Tree2())

class MyComp(Component):

    x1 = Float(0.0, iotype='in')
    x2 = Float(0.0, iotype='in')
    x3 = Array(zeros((2, 1)), iotype='in')
    x4 = Array(zeros((2, 2)), iotype='in')
    vt = VarTree(Tree1(), iotype='in')

    xx1 = Float(0.0, iotype='out')
    xx2 = Float(0.0, iotype='out')
    xx3 = Array(zeros((2, 1)), iotype='out')
    xx4 = Array(zeros((2, 2)), iotype='out')
    vvt = VarTree(Tree1(), iotype='out')

    def execute(self):
        """ doubler """
        pass

    def linearize(self):
        """ calculates the Jacobian """

        self.J = array([[1.5, 3.7, 2.5, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1],
                        [7.4, 23.7, 1.1, 4.2, 5.2, 6.2, 7.2, 8.2, 9.2, 10.2, 11.2],
                        [5.5, 8.7, 1.9, 4.3, 5.3, 6.3, 7.3, 8.3, 9.3, 10.3, 11.3],
                        [1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 7.4, 8.4, 9.4, 10.4, 11.4],
                        [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5],
                        [1.6, 2.6, 3.6, 4.6, 5.6, 6.6, 7.6, 8.6, 9.6, 10.6, 11.6],
                        [1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 9.7, 10.7, 11.7],
                        [1.8, 2.8, 3.8, 4.8, 5.8, 6.8, 7.8, 8.8, 9.8, 10.8, 11.8],
                        [1.9, 2.9, 3.9, 4.9, 5.9, 6.9, 7.9, 8.9, 9.9, 10.9, 11.9],
                        [1.10, 2.10, 3.10, 4.10, 5.10, 6.10, 7.10, 8.10, 9.10, 10.10, 11.10],
                        [1.11, 2.11, 3.11, 4.11, 5.11, 6.11, 7.11, 8.11, 9.11, 10.11, 11.11]])

    def provideJ(self):
        """ returns the Jacobian """

        input_keys = ('x1', 'x2', 'x3', 'x4', 'vt.a1', 'vt.vt1.d1')
        output_keys = ('xx1', 'xx2', 'xx3', 'xx4', 'vvt.a1', 'vvt.vt1.d1')

        return input_keys, output_keys, self.J


class Testcase_applyJ(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        pass

    def tearDown(self):
        """ Called after each test. """
        pass
    
    def test_provideJ(self):

        comp = MyComp()
        comp.linearize()

        inputs = {}
        outputs = { 'xx1': None,
                    'xx2': None,
                    'xx3': None,
                    'xx4': None,
                    'vvt.a1': None,
                    'vvt.vt1.d1': None}

        num = 11
        ident = identity(num)

        for i in range(num):

            inputs['x1'] = ident[i, 0]
            inputs['x2'] = ident[i, 1]
            inputs['x3'] = ident[i, 2:4].reshape((2, 1))
            inputs['x4'] = ident[i, 4:8].reshape((2, 2))
            inputs['vt.a1'] = ident[i, 8]
            inputs['vt.vt1.d1'] = ident[i, 9:11].reshape((1, 2))

            inputs['xx1'] = 0
            inputs['xx2'] = 0
            inputs['xx3'] = zeros((2, 1))
            inputs['xx4'] = zeros((2, 2))
            inputs['vvt.a1'] = 0
            inputs['vvt.vt1.d1'] = zeros((1, 2))

            applyJ(comp, inputs, outputs)

            self.assertEqual(outputs['xx1'], comp.J[0, i])
            self.assertEqual(outputs['xx2'], comp.J[1, i])
            for j in range(2):
                self.assertEqual(outputs['xx3'][j], comp.J[2+j, i])
            for j in range(4):
                self.assertEqual(outputs['xx4'].flat[j], comp.J[4+j, i])
            self.assertEqual(outputs['vvt.a1'], comp.J[8, i])
            for j in range(2):
                self.assertEqual(outputs['vvt.vt1.d1'].flat[j], comp.J[9+j, i])


class Paraboloid(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
    # set up interface to the framework  
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')

    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """
        
        x = self.x
        y = self.y
        
        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0
        
    def linearize(self):
        """Analytical first derivatives"""
        
        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x
    
        self.J = array([[df_dx, df_dy]])
        
    def provideJ(self):
        
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        return input_keys, output_keys, self.J


@add_delegate(HasParameters, HasObjective)
class SimpleDriver(Driver):
    """Driver with Parameters"""

    implements(IHasParameters)
    
        
class CompFoot(Component):
    """ Evaluates the equation y=x^2"""
    
    x = Float(1.0, iotype='in', units='ft')
    y = Float(1.0, iotype='out', units='ft')

    def execute(self):
        """ Executes it """
        
        self.y = 2.0*self.x

    def linearize(self):
        """Analytical first derivatives"""
        
        dy_dx = 2.0
        self.J = array([[dy_dx]])
        
    def provideJ(self):
        
        input_keys = ('x',)
        output_keys = ('y',)
        return input_keys, output_keys, self.J

        
class CompInch(Component):
    """ Evaluates the equation y=x^2"""
    
    x = Float(1.0, iotype='in', units='inch')
    y = Float(1.0, iotype='out', units='inch')

    def execute(self):
        """ Executes it """
        
        self.y = 2.0*self.x

    def linearize(self):
        """Analytical first derivatives"""
        
        dy_dx = 2.0
        self.J = array([[dy_dx]])

    def provideJ(self):
        
        input_keys = ('x',)
        output_keys = ('y',)
        return input_keys, output_keys, self.J


class Testcase_derivatives(unittest.TestCase):
    """ Test derivative aspects of a simple workflow. """

    def test_first_derivative(self):
        
        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, 
                                           high=1000)
        top.driver.add_parameter('comp.y', low=-1000, 
                                           high=1000)

        top.comp.x = 3
        top.comp.y = 5
        top.comp.run()
        
        J = top.driver.workflow.calc_gradient(outputs=['comp.f_xy'])
        assert_rel_error(self, J[0,0], 5.0, 0.0001)
        assert_rel_error(self, J[0,1], 21.0, 0.0001)

    def test_nested(self):
        
        top = Assembly()
        top.add('nest', Assembly())
        top.nest.add('comp', Paraboloid())
        top.driver.workflow.add(['nest'])
        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')
        top.nest.create_passthrough('comp.f_xy')
        top.nest.x = 3
        top.nest.y = 5
        top.run()
        
        J = top.driver.workflow.calc_gradient(inputs=['nest.x', 'nest.y'],
                                              outputs=['nest.f_xy'])
        assert_rel_error(self, J[0,0], 5.0, 0.0001)
        assert_rel_error(self, J[0,1], 21.0, 0.0001)

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
    
        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])
        
        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('1.0*comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')
        
        #obj = 'comp5.y1'
        #con = 'comp5.y1-comp3.y1 > 0'
        #self.top.driver.add_parameter('comp1.x1', low=-50., high=50., fd_step=.0001)
        #self.top.driver.add_objective(obj)
        #self.top.driver.add_constraint(con)
    
        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.workflow.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'])
        
        assert_rel_error(self, J[0, 0], 313.0, .001)
        
        #grad = self.top.driver.differentiator.get_gradient(obj)
        #assert_rel_error(self, grad[0], 626.0, .001)
        
        #grad = self.top.driver.differentiator.get_gradient('comp5.y1-comp3.y1>0')
        #assert_rel_error(self, grad[0], -626.0+10.5, .001)
        
        
    def test_nondifferentiable_blocks(self):
        
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
        
        #self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp1', ExecComp(exp1))
        self.top.add('comp2', ExecComp(exp2))
        self.top.add('comp3', ExecComp(exp3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecComp(exp5))
    
        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])
        
        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')
    
        # Case 1 - differentiable (comp4)
        
        iterlist = self.top.driver.workflow.group_nondifferentiables()
        self.assertTrue(['~~0', 'comp4', '~~1'] == iterlist)
        
        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.workflow.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'])
        
        assert_rel_error(self, J[0, 0], 313.0, .001)
        
        # Case 2 - differentiable (none)
        
        self.top.replace('comp4', ExecComp(exp4))
        iterlist = self.top.driver.workflow.group_nondifferentiables()
        self.assertTrue(['~~0'] == iterlist)
        
        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.workflow.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'])
        
        assert_rel_error(self, J[0, 0], 313.0, .001)
        
        # Case 3 - differentiable (comp5)
        
        self.top.replace('comp5', ExecCompWithDerivatives(exp5, deriv5))
        iterlist = self.top.driver.workflow.group_nondifferentiables()
        self.assertTrue(['~~0', 'comp5'] == iterlist)
        removed = set([('comp1', 'comp2'),
                       ('comp1', 'comp3'),
                       ('comp2', 'comp4'),
                       ('comp3', 'comp4')])
        self.assertTrue(removed, self.top.driver.workflow._hidden_edges)
        
        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.workflow.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'])
        
        assert_rel_error(self, J[0, 0], 313.0, .001)
        
        # Case 4 - differentiable (comp1, comp3, comp5)
        
        self.top.replace('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.replace('comp3', ExecCompWithDerivatives(exp3, deriv3))
        iterlist = self.top.driver.workflow.group_nondifferentiables()
        self.assertTrue(['comp1', 'comp3', '~~0', 'comp5'] == iterlist)
        removed = set([('comp3', 'comp4')])
        self.assertTrue(removed, self.top.driver.workflow._hidden_edges)
        
        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.workflow.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'])
        
        assert_rel_error(self, J[0, 0], 313.0, .001)
        
        # Put everything in a single pseudo-assy, and run fd with no fake.
        #self.top.run()
        #self.top.driver.workflow.check_gradient(inputs=['comp1.x1'],
        #                                           outputs=['comp5.y1'])
        J = self.top.driver.workflow.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'], 
                                                   fd=True)
        assert_rel_error(self, J[0, 0], 313.0, .001)
        
        
    def test_first_derivative_with_units(self):
        top = set_as_top(Assembly())
        
        top.add('comp1', CompFoot())
        top.add('comp2', CompInch())
        
        top.connect('comp1.y', 'comp2.x')
        
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])
        
        top.driver.add_parameter('comp1.x', low=-50., high=50., fd_step=.0001)
        top.driver.add_objective('comp2.y')
        
        top.comp1.x = 2.0
        top.run()
        #top.driver.differentiator.calc_gradient()
        
        J = top.driver.workflow.calc_gradient(outputs=['comp2.y'])
        assert_rel_error(self, J[0,0], 48.0, .001)
        
if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

