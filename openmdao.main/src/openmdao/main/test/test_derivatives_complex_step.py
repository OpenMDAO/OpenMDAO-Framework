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


class SimpleCompFloat(Component):

    x = Float(3.0, iotype='in')
    y = Float(6.0, iotype='out')

    def execute(self):
        self.y = 2.0*self.x

    def provideJ(self):
        return array([[2.0]])

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
        