"""
Tests involving traits. Not really here to test the traits package, but
more to spell out in some simple examples how some of the traits functions
work.
"""

import unittest

from enthought.traits.api import HasTraits, TraitType, Int, Instance

class MyClass(object):
    pass

class MyProp(TraitType):
    def get(self, object, name):
        return 47
    
    def set(self, object, name, value):
        pass

class MyHasTraits(HasTraits):
    explicit_int = Int
    explicit_int_def = Int(7)
    explicit_property = MyProp()
    
    inst = Instance(MyClass)
    
    _ = MyProp()

class TraitsTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        pass

    def tearDown(self):
        """this teardown function will be called after each test"""
        pass

    def test_trait_names(self):
        mht = MyHasTraits()
        names = mht.trait_names()
        expected = ['explicit_int', 'explicit_int_def', 'explicit_property']
        for name in expected:
            self.assertTrue(name in names)
            
        # implicitly added property traits don't show up in trait_names
        mht.implicit_property = 88
        names = mht.trait_names()
        self.assertFalse('implicit_property' in names)
        
        # dynamically added property traits don't show up in trait_names
        mht.add_trait('added_property', MyProp())
        names = mht.trait_names()
        self.assertFalse('added_property' in names)
        
        # dynamically added normal traits don't show up in trait_names
        mht.add_trait('added_int', Int())
        names = mht.trait_names()
        self.assertFalse('added_int' in names)
        
        # dynamically added traits (but not implicitly defined ones) show up in _instance_traits()
        instnames = mht._instance_traits()
        self.assertTrue('added_int' in instnames)
        self.assertTrue('added_property' in instnames)
        self.assertTrue(len(instnames)==2)
        
        # make sure instance traits don't show up in other instances
        mht2 = MyHasTraits()
        inames = mht2._instance_traits()
        self.assertTrue(len(inames)==0)
        
    def test_class_trait_names(self):
        mht = MyHasTraits()
        names = mht.class_trait_names()
        expected = ['explicit_int', 'explicit_int_def', 'explicit_property']
        for name in expected:
            self.assertTrue(name in names)
            
        # implicitly added property traits don't show up in trait_names
        mht.implicit_property = 88
        names = mht.class_trait_names()
        self.assertFalse('implicit_property' in names)
        
        # dynamically added property traits don't show up in trait_names
        mht.add_trait('added_property', MyProp())
        names = mht.class_trait_names()
        self.assertFalse('added_property' in names)
        
        # dynamically added normal traits don't show up in trait_names
        mht.add_trait('added_int', Int())
        names = mht.class_trait_names()
        self.assertFalse('added_int' in names)
        
    def test_all_trait_names(self):
        mht = MyHasTraits()
        
        # dynamically added property traits don't show up in trait_names
        mht.add_trait('added_property', MyProp())
        names = mht.class_trait_names()
        self.assertFalse('added_property' in names)
        
        # dynamically added normal traits don't show up in trait_names
        mht.add_trait('added_int', Int())
        names = mht.class_trait_names()
        self.assertFalse('added_int' in names)
        
        # added traits DO NOT show up in all_trait_names()
        allnames = mht.all_trait_names()
        self.assertFalse('added_int' in allnames)
        self.assertFalse('added_property' in allnames)
                

if __name__ == '__main__':
    unittest.main()

