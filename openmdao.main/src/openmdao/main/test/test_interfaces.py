"""
Tests involving checking of Interfaces
"""

import unittest

from enthought.traits.api import HasTraits
from enthought.traits.trait_types import validate_implements

from openmdao.lib.datatypes.api import Int, Str, Instance, Interface, \
     implements, TraitType

class IMyMarkerInterface(Interface):
    """Just an empty marker interface"""
    
class IMarkerPlusAttribute(Interface):
    """An empty interface except for an attribute"""
    myattr = Int(2)

class IMarkerPlusFunction(Interface):
    """An iface with one function"""
    def foo(self):
        pass
    
class MyClass(object):
    
    implements(IMyMarkerInterface)
    
    def __init__(self):
        self.val = 2
        
    def blah(self):
        pass
        
class MyClass2(object):
    pass

class MyClass3(object):
    implements(IMarkerPlusFunction)
    
class MyHasTraits(HasTraits):
    implements(IMyMarkerInterface)
    
class MyHasTraits2(HasTraits):
    implements(IMarkerPlusAttribute)
    myattr = Instance(MyClass2)  # right attribute, wrong type
    
class MyHasTraits3(HasTraits):
    implements(IMarkerPlusAttribute, IMarkerPlusFunction)


class InterfaceTestCase(unittest.TestCase):

    def test_has_traits_interface(self):
        # The results of this test show that has_traits_interface just believes
        # whatever the class tells it.  There is no actual checking for the
        # presence of member functions or attributes. I think this
        # behavior may be configurable, but this is the default behavior.
        mht = MyHasTraits()
        self.assertEqual(mht.has_traits_interface(IMyMarkerInterface), True)
        
        mht2 = MyHasTraits2()
        self.assertEqual(mht2.has_traits_interface(IMyMarkerInterface), False)
        
        mht3 = MyHasTraits3() # has no real support for either interface
        self.assertEqual(mht3.has_traits_interface(IMarkerPlusAttribute), True)
        self.assertEqual(mht3.has_traits_interface(IMarkerPlusFunction), True)
        
        
    def test_validate_implements(self):
        # The results of this test show that validate_implements ignores what
        # the class says it implements and actually tries to verify that the
        # functions and attributes specified in the Interface spec are present.
        # It does not, however, perform any type checking on attributes.

        mht2 = MyHasTraits2()
        # HasTraits2 has an attribute that matches the one in IMarkerPlusAttribute,
        # but it's the wrong type. This shows the no type checking is done.
        self.assertEqual(validate_implements(mht2, IMarkerPlusAttribute), True)
        
        mht3 = MyHasTraits3()
        # However, validate_implements does detect if specified member functions
        # or attributes don't exist in the class.
        self.assertEqual(validate_implements(mht3, IMarkerPlusFunction), False)
        self.assertEqual(validate_implements(mht3, IMarkerPlusAttribute), False)
        
        #
        # Now do some testing with non-HasTraits classes
        #
        mc = MyClass()
        self.assertEqual(validate_implements(mc, IMyMarkerInterface), True)
        
        mc2 = MyClass2()
        # for empty interfaces, validate_implements will always return True.
        self.assertEqual(validate_implements(mc2, IMyMarkerInterface), True)
        
        # For Interfaces with attributes only: validate_implements DOES NOT check them.
        self.assertEqual(validate_implements(mc2, IMarkerPlusAttribute), True)
        
        # if the iface has functions, it works as expected
        self.assertEqual(validate_implements(mc2, IMarkerPlusFunction), False)
    
        
        
if __name__ == '__main__':
    unittest.main()

