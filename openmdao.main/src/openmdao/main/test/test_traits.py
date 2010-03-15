"""
Tests involving traits. This will notify us if any future versions of Traits we
switch to have changed the tested behavior, and it also helps refresh my memory
about how these things work.
"""

import unittest

from enthought.traits.api import HasTraits, TraitType
from openmdao.lib.api import Int, Instance

class MyClass(object):
    def __init__(self):
        self.val = 2

class MyProp(TraitType):
    def init(self):
        self.val = 47
        
    def get(self, object, name):
        return self.val
    
    def set(self, object, name, value):
        self.val = value

class MyHasTraits(HasTraits):
    explicit_int = Int
    explicit_int_def = Int(7)
    explicit_property = MyProp()
    
    inst = Instance(MyClass)
    
    impval_ = Int(1)
    _ = MyProp()
    
class MyHasTraits2(MyHasTraits):
    pass

class TraitsTestCase(unittest.TestCase):

    def test_trait_names(self):
        mht = MyHasTraits()
        names = mht.trait_names()
        expected = ['explicit_int', 'explicit_int_def', 'explicit_property','inst']
        for name in expected:
            self.assertTrue(name in names)
            
        # implicitly added property traits don't show up in trait_names
        mht.implicit_property = 88
        self.assertFalse('implicit_property' in mht.trait_names())
        
        # dynamically added property traits don't show up in trait_names
        # even after they are set
        mht.add_trait('added_property', MyProp())
        self.assertFalse('added_property' in mht.trait_names())
        mht.added_property = 6
        self.assertFalse('added_property' in mht.trait_names())
        
        # dynamically added normal traits don't show up in trait_names
        # UNTIL their value is set (even if set to same as their default)
        mht.add_trait('added_int', Int(6))
        self.assertFalse('added_int' in mht.trait_names())
        mht.added_int = 6
        self.assertTrue('added_int' in mht.trait_names())
        
        # dynamically added traits (but not implicitly defined ones) 
        # show up in _instance_traits()
        instnames = mht._instance_traits()
        self.assertTrue('added_int' in instnames)
        self.assertTrue('added_property' in instnames)
        self.assertTrue(len(instnames)==2)
        
        # make sure instance traits don't show up in other instances
        mht2 = MyHasTraits()
        inames = mht2._instance_traits()
        self.assertTrue(len(inames)==0)
        
    def test_class_trait_names(self):
        mht = MyHasTraits2()
        names = mht.class_trait_names()
        expected = ['explicit_int', 'explicit_int_def', 'explicit_property','inst']
        for name in expected:
            self.assertTrue(name in names)
            
        # implicitly added property traits don't show up in class_trait_names
        mht.implicit_property = 88
        self.assertFalse('implicit_property' in mht.class_trait_names())
        
        # dynamically added property traits show up in class_trait_names
        mht.add_class_trait('added_property', MyProp())
        self.assertTrue('added_property' in mht.class_trait_names())
        
        # dynamically added normal traits show up in class_trait_names
        mht.add_class_trait('added_int', Int())
        self.assertTrue('added_int' in mht.class_trait_names())
        
    def test_all_trait_names(self):
        mht = MyHasTraits()
        
        # dynamically added traits DO NOT show up in all_trait_names(), 
        # even after being set
        allnames = mht.all_trait_names()
        self.assertFalse('added_int' in allnames)
        self.assertFalse('added_property' in allnames)
        mht.added_int = 8
        mht.added_property = 9
        self.assertFalse('added_int' in allnames)
        self.assertFalse('added_property' in allnames)
        
    def test_reset_traits(self):
        mht = MyHasTraits()
        mht.implicit_property = 999
        mht.impval1 = 999
        mht.explicit_int_def = 999
        mht.explicit_int = 999
        mht.explicit_property = 999
        mht.inst = MyClass()
        mht.inst.val = 999
        mht.add_trait('added_int_def', Int(42))
        mht.added_int_def = 999
        mht.add_trait('added_int', Int())
        mht.added_int = 999

        self.assertEqual(mht.implicit_property, 999)
        self.assertEqual(mht.impval1, 999)
        self.assertEqual(mht.explicit_int_def, 999)
        self.assertEqual(mht.explicit_int, 999)
        self.assertEqual(mht.added_int, 999)
        self.assertEqual(mht.added_int_def, 999)
        self.assertEqual(mht.explicit_property, 999)
        self.assertEqual(mht.inst.val, 999)
        
        unresetable = mht.reset_traits()
        # failed to reset the explicit property
        self.assertEqual(unresetable, ['explicit_property'])
        
        # implicit property traits are not reset
        self.assertEqual(mht.implicit_property, 999)
        
        # implicit value traits are reset
        self.assertEqual(mht.impval1, 1)
        
        # dynamically added traits are reset, because after 
        # their values have been set, they show up in trait_names()
        # which is what reset_traits() calls to get the list
        # of traits (if not explicitly specified)
        self.assertEqual(mht.added_int_def, 42)
        self.assertEqual(mht.added_int, 0)
        
        # other traits are reset
        self.assertEqual(mht.explicit_int_def, 7)
        self.assertEqual(mht.explicit_int, 0)
        
        # instance trait is reset to None
        self.assertEqual(mht.inst, None)
        
        # even if explicitly specified, implicit property traits are not reset
        unresetable = mht.reset_traits(traits=['implicit_property'])
        self.assertEqual(unresetable, ['implicit_property'])
        self.assertEqual(mht.implicit_property, 999)
        
if __name__ == '__main__':
    unittest.main()

