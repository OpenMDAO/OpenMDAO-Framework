"""
Testing for complex step support in the framework.
"""

import unittest

try:
    from numpy import zeros, array, identity, random
except ImportError as err:
    from openmdao.main.numpy_fallback import zeros, array, identity, random
    
from openmdao.main.api import Component, VariableTree, Assembly, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree
from openmdao.util.testutil import assert_rel_error

class SimpleCompFloat(Component):

    x = Float(3.0, iotype='in')
    y = Float(6.0, iotype='out')

    def execute(self):
        self.y = 2.0*self.x

    def provideJ(self):
        return array([[2.0]])

    def list_deriv_vars(self):
        return ('x',), ('y',)
    
class SimpleCompArray(Component):

    x = Array(array([[2.0, 4.0], [1.0, 3.0]]), iotype='in')
    y = Array(iotype='out')

    def execute(self):
        self.J = array([[1.0, 3.0, 2.0, 5.0], 
                        [-1.0, 3.0, 7.0, -5.0],
                        [4.0, 4.0, 3.0, -3.0],
                        [2.0, 5.0, 1.5, 2.0]])
        self.y = self.J.dot(self.x.flatten()).reshape((2, 2))

    def provideJ(self):
        return self.J

    def list_deriv_vars(self):
        return ('x',), ('y',)
    
    
class TreeWithFloat(VariableTree):

    x = Float(5.0)

class TreeWithSubTree(VariableTree):

    x = Float(3.)
    sub = VarTree(TreeWithFloat())
   
class CompWithVarTreeSubTree(Component):

    ins = VarTree(TreeWithSubTree(), iotype="in")
    outs = VarTree(TreeWithSubTree(), iotype="out")

    def execute(self):

        self.outs.x = 2.0*self.ins.x + 3.0*self.ins.sub.x
        self.outs.sub.x = 4.0*self.ins.x + 1.0*self.ins.sub.x

    def provideJ(self):

        self.J = array([2.0, 3.0], [4.0, 1.0])
        return self.J

    def list_deriv_vars(self):
        ins = ('ins.x', 'ins.sub.x')
        outs = ('outs.x','outs.sub.x')

        return ins, outs
    
class TreeWithArray(VariableTree):

    x = Array(array([[2.0, 4.0], [1.0, 3.0]]))

class TreeWithArraySubTree(VariableTree):

    x = Array(array([[2.0, 4.0], [1.0, 3.0]]))
    sub = VarTree(TreeWithArray())
   
class CompWithArrayVarTreeSubTree(Component):

    ins = VarTree(TreeWithArraySubTree(), iotype="in")
    outs = VarTree(TreeWithArraySubTree(), iotype="out")

    def execute(self):
        self.J1 = array([[1.0, 3.0, 2.0, 5.0], 
                        [-1.0, 3.0, 7.0, -5.0],
                        [4.0, 4.0, 3.0, -3.0],
                        [2.0, 5.0, 1.0, 2.0]])
        self.J2 = .9*self.J1
        self.J3 = 1.1*self.J1
        self.J4 = .7*self.J1
        
        x1 = self.ins.x.flatten()
        x2 = self.ins.sub.x.flatten()
        
        self.outs.x = (self.J1.dot(x1) + self.J2.dot(x2)).reshape((2, 2))
        self.outs.sub.x = (self.J3.dot(x1) + self.J4.dot(x2)).reshape((2, 2))

    def provideJ(self):

        self.J = zeros((8, 8))
        self.J[:4, :4] = self.J1
        self.J[:4, 4:] = self.J2
        self.J[4:, :4] = self.J3
        self.J[4:, 4:] = self.J4
        return self.J

    def list_deriv_vars(self):
        ins = ('ins.x', 'ins.sub.x')
        outs = ('outs.x','outs.sub.x')

        return ins, outs
    
class Testcase_ComplexStep_Traits(unittest.TestCase):
    """ Make sure trait Float works for complex stepping. """


    def test_float(self):
        
        model = set_as_top(Assembly())
        model.add('comp', SimpleCompFloat())
        model.driver.workflow.add('comp')
        
        model.comp._complex_step = True
        
        model.comp.x = 3+4j
        model.run()
        
        self.assertEqual(model.comp.x, 3+4j)
        self.assertEqual(model.comp.y, 6+8j)
        
    def test_float_in_vartree(self):
        
        model = set_as_top(Assembly())
        model.add('comp', CompWithVarTreeSubTree())
        model.driver.workflow.add('comp')
        
        model.comp._complex_step = True
        
        model.comp.ins.x = 2+1j
        model.comp.ins.sub.x = 5+3j
        model.run()
        
        self.assertEqual(model.comp.ins.x, 2+1j)
        self.assertEqual(model.comp.ins.sub.x, 5+3j)
        self.assertEqual(model.comp.outs.x, 19+11j)
        self.assertEqual(model.comp.outs.sub.x, 13+7j)
        
    def test_array(self):
        
        model = set_as_top(Assembly())
        model.add('comp', SimpleCompArray())
        model.driver.workflow.add('comp')
        
        model.comp._complex_step = True
        
        model.comp.x = model.comp.x.astype('complex')
        model.comp.x[1, 1] = 3+4j
        model.run()
        
        print model.comp.y
        
        y_check = array([[31.0+20.0j, 2.0-20.0j], [18.0-12.0j, 31.5+8.0j]])
        self.assertEqual(model.comp.x[1, 1], 3+4j)
        self.assertEqual(model.comp.y[0, 0], y_check[0, 0])
        self.assertEqual(model.comp.y[0, 1], y_check[0, 1])
        self.assertEqual(model.comp.y[1, 0], y_check[1, 0])
        self.assertEqual(model.comp.y[1, 1], y_check[1, 1])
        
    def test_array_in_vartree(self):
        
        model = set_as_top(Assembly())
        model.add('comp', CompWithArrayVarTreeSubTree())
        model.driver.workflow.add('comp')
        
        model.comp._complex_step = True
        
        model.comp.ins.x = model.comp.ins.x.astype('complex')
        model.comp.ins.x[1, 1] = 1+2j
        model.comp.ins.sub.x = model.comp.ins.sub.x.astype('complex')
        model.comp.ins.sub.x[1, 1] = 3+4j
        model.run()

        print model.comp.ins.x, model.comp.ins.sub.x
        print model.comp.outs.x, model.comp.outs.sub.x
        
        y1_check = array([[48.9+28.j, 13.80-28.j], [40.20-16.8j, 54.9+11.2j]])
        y2_check = array([[44.8+25.j,  14.6-25.j], [ 39.0-15.j, 51.4+10.j]])
        
        self.assertEqual(model.comp.ins.x[1, 1], 1+2j)
        self.assertEqual(model.comp.ins.sub.x[1, 1], 3+4j)
        diff = abs(y1_check - model.comp.outs.x).max()
        assert_rel_error(self, diff, 0.0, .0001)
        diff = abs(y2_check - model.comp.outs.sub.x).max()
        assert_rel_error(self, diff, 0.0, .0001)
        