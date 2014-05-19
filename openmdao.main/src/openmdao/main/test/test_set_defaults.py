"""
Test for setting input variables back to their default values.
"""

import unittest

from openmdao.main.api import Component, Assembly
from openmdao.main.datatypes.api import Float, List, Array
from numpy import array, zeros
    
class MyDefComp(Component):
    f_in = Float(3.14, iotype='in')
    f_out = Float(iotype='out')
    arr_in = Array([1.,2.,3.], iotype='in')
    list_in = List(value=['a','b','c'], iotype='in')
    
    def execute(self):
        self.f_out = self.f_in + 1.        

class MyNoDefComp(Component):
    f_in = Float(iotype='in')
    f_out = Float(iotype='out')
    arr_in = Array(iotype='in')
    list_in = List(iotype='in')
    
    def execute(self):
        self.f_out = self.f_in + 1.
        

class SetDefaultsTestCase(unittest.TestCase):

    def test_set_to_unset_default(self):
        comp = MyNoDefComp()
        self.assertEqual(0., comp.f_in)
        comp.f_in = 42.
        comp.arr_in = array([88., 32.])
        comp.list_in = [1,2,3]
        comp.run()
        comp.revert_to_defaults()
        # make sure reverting to defaults invalidates our outputs
        self.assertEqual(0., comp.f_in)
        self.assertTrue(all(zeros(0,'d')==comp.arr_in))
        self.assertEqual([], comp.list_in)
    
    def test_set_to_default(self):
        comp = MyDefComp()
        self.assertEqual(3.14, comp.f_in)
        comp.f_in = 42.
        comp.arr_in = array([88., 32.])
        self.assertFalse(array([1.,2.,3.])==comp.arr_in)
        comp.run()
        comp.revert_to_defaults()
        # make sure reverting to defaults invalidates our outputs
        self.assertEqual(3.14, comp.f_in)
        self.assertTrue(all(array([1.,2.,3.])==comp.arr_in))
        
    def test_set_recursive(self):
        asm = Assembly()
        asm.add('defcomp', MyDefComp())
        asm.add('nodefcomp', MyNoDefComp())
        self.assertEqual(0., asm.nodefcomp.f_in)
        self.assertEqual(3.14, asm.defcomp.f_in)
        asm.nodefcomp.f_in = 99
        asm.defcomp.f_in = 99
        asm.revert_to_defaults()
        self.assertEqual(0., asm.nodefcomp.f_in)
        self.assertEqual(3.14, asm.defcomp.f_in)
    
if __name__ == '__main__':
    unittest.main()

