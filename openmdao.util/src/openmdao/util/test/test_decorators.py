
import unittest
from inspect import getmembers, ismethod
from types import NoneType

from traits.api import HasTraits, Float
from zope.interface import Interface, implements, implementedBy

from openmdao.main.interfaces import obj_has_interface

from openmdao.util.decorators import add_delegate, \
     stub_if_missing_deps, method_accepts, function_accepts


class GoodDelegate(object):
    cls_x = 4
    
    def __init__(self, parent):
        self.inst_y = 3
        
    def del_amethod(self, a, b, c):
        return sum([a,b,c,self.inst_y])
    
    def _priv_method(self):
        pass
    
class InheritedDelegate(GoodDelegate):
    def inherited_amethod(self, a, b, c):
        return sum([a,b,c,self.inst_y])
    
class BadDelegate1(object):
    
    def __init__(self, parent):
        self.inst_y = 3
        
    def del_amethod(self, a, b, c):
        return sum(a,b,c,self.inst_y)
    
    def cls_x(self):
        return 9
    
    def _priv_method(self):
        pass

class BadDelegate2(object):
    def __init__(self, parent):
        self.inst_y = 3
        
    def amethod(self):
        return self.inst_y
    
class I(Interface):
    def i(a):
        pass

class J(I):
    def j(a):
        pass

class DelegateI(object):
    implements(I)

    def __init__(self, parent):
        pass

    def i(self, a):
        return a + 1

class DelegateJ(object):
    implements(J)

    def __init__(self, parent):
        pass

    def i(self, a):
        return a + 1

    def j(self, a):
        return a + 2

class InheritedDelegateI(DelegateI):
    def __init__(self, parent):
        super(InheritedDelegateI, self).__init__(parent)

class DelegateTestCase(unittest.TestCase):

    def test_add_delegate(self):
        @add_delegate(GoodDelegate)
        class Foo(object):
            cls_x = 1
            def __init__(self):
                self.inst_x = 9
            def amethod(self, a, b, c='foo'): pass
            def genmethod(self, *args, **kwargs): pass
            def _priv_method(self): pass
    
        mems = set([n for n,v in getmembers(Foo,ismethod) if not n.startswith('_')])
        self.assertEqual(mems, set(['amethod','del_amethod','genmethod']))
        f = Foo()
        self.assertEqual(f.del_amethod(1,2,0), 6)
        self.assertTrue(hasattr(f,'_gooddelegate'))

    def test_inheritance(self):
        @add_delegate(InheritedDelegate)
        class Foo(object):
            cls_x = 1
            def __init__(self):
                self.inst_x = 9
            def amethod(self, a, b, c='foo'): pass
            def genmethod(self, *args, **kwargs): pass
            def _priv_method(self): pass
    
        mems = set([n for n,v in getmembers(Foo,ismethod) if not n.startswith('_')])
        self.assertEqual(mems, set(['amethod','inherited_amethod','del_amethod','genmethod']))
        f = Foo()
        self.assertEqual(f.inherited_amethod(1,2,0), 6)

    def test_add_delegate_bad1(self):
        @add_delegate(BadDelegate1)
        class Foo(object):
            cls_x = 1234    # BadDelegate1 defines this as a method
            def __init__(self):
                self.inst_x = 9
            def amethod(self, a, b, c='foo'): pass
            def genmethod(self, *args, **kwargs): pass
            def _priv_method(self): pass
            
        f = Foo()
        self.assertEqual(f.cls_x, 1234)
    
    def test_add_delegate_bad2(self):
        @add_delegate(BadDelegate2)
        class Foo2(object):
            cls_x = 4567
            def __init__(self):
                self.inst_x = 9
            def amethod(self, a, b, c='foo'): return 87  #BadDelegate2 defines this
            def genmethod(self, *args, **kwargs): pass
            def _priv_method(self): pass
            
        f = Foo2()
        self.assertEqual(f.amethod(1,2), 87)

    def test_add_delegate_bad1_hastraits(self):
        @add_delegate(BadDelegate1)
        class Foo3(HasTraits):
            cls_x = Float(3.456,iotype='in')
            def __init__(self):
                self.inst_x = 9
            def amethod(self, a, b, c='foo'): pass
            def genmethod(self, *args, **kwargs): pass
            def _priv_method(self): pass
    
        f = Foo3()
        self.assertEqual(f.cls_x, 3.456)
    
    def test_add_delegate_bad2_hastraits(self):
        @add_delegate(BadDelegate2)
        class Foo4(HasTraits):
            cls_x = 1
            def __init__(self):
                self.inst_x = 9
            def amethod(self, a, b, c='foo'): return -54.3
            def genmethod(self, *args, **kwargs): pass
            def _priv_method(self): pass

        f = Foo4()
        self.assertEqual(f.amethod(1,2), -54.3)

    def test_add_delegatei(self):
        @add_delegate(DelegateI)
        class Delegator(object):
            def __init__(self):
                self.x = 1
        
        mems = set([n for n,v in getmembers(Delegator, ismethod) if not n.startswith('_')])
        self.assertEqual(mems, set(['i']))
        interfaces = list(implementedBy(Delegator))
        self.assertEqual(interfaces, list(implementedBy(DelegateI)))
        delegator = Delegator()
        for interface in interfaces:
            self.assertTrue(obj_has_interface(delegator, interface))
        self.assertEqual(delegator.i(delegator.x), 2)

    def test_add_delegatej(self):
        @add_delegate(DelegateJ)
        class Delegator1(object):
            def __init__(self):
                self.x = 1
        
        mems = set([n for n,v in getmembers(Delegator1, ismethod) if not n.startswith('_')])
        self.assertEqual(mems, set(['i', 'j']))
        interfaces = list(implementedBy(Delegator1))
        self.assertEqual(interfaces, list(implementedBy(DelegateJ)))
        delegator = Delegator1()
        for interface in interfaces:
            self.assertTrue(obj_has_interface(delegator, interface))
        self.assertEqual(delegator.i(delegator.x), 2)
        self.assertEqual(delegator.j(delegator.x), 3)

    def test_add_inheriteddelegatei(self):
        @add_delegate(InheritedDelegateI)
        class Delegator2(object):
            def __init__(self):
                self.x = 1
        
        mems = set([n for n,v in getmembers(Delegator2, ismethod) if not n.startswith('_')])
        self.assertEqual(mems, set(['i']))
        interfaces = list(implementedBy(Delegator2))
        self.assertEqual(interfaces, list(implementedBy(InheritedDelegateI)))
        delegator = Delegator2()
        for interface in interfaces:
            self.assertTrue(obj_has_interface(delegator, interface))
        self.assertEqual(delegator.i(delegator.x), 2)
        
class StubIfMissingTestCase(unittest.TestCase):
    def test_stub_class(self):
        @stub_if_missing_deps('FooBar', 'math', 'blah')
        class MyClass(object):
            pass
        
        try:
            c = MyClass()
        except RuntimeError as err:
            self.assertEqual(str(err), 
                "The MyClass class depends on the following modules or attributes which were not found on your system: ['FooBar', 'blah']")
        else:
            self.fail("expected RuntimeError")
    
    def test_stub_funct(self):
        
        @stub_if_missing_deps('abcdef', 'Krusty', 'math')
        def funct(a, b, c, d):
            return True
        
        try:
            f = funct(1,2,3,4)
        except RuntimeError as err:
            self.assertEqual(str(err), "The funct function depends on the following modules or attributes which were not found on your system: ['abcdef', 'Krusty']")
        else:
            self.fail("expected RuntimeError")

    def test_stub_method(self):
        
        class TheClass(object):
            @stub_if_missing_deps('somemodule','fooey')
            def my_method(self, a, b, c, d):
                return True
        
        c = TheClass()
        try:
            c.my_method(1,2,3,4)
        except RuntimeError as err:
            self.assertEqual(str(err), "The my_method function depends on the following modules or attributes which were not found on your system: ['somemodule', 'fooey']")
        else:
            self.fail("expected RuntimeError")


class AcceptsTestCase(unittest.TestCase):

    def test_method_accepts(self):

        class Foo(object):
            @method_accepts(TypeError,
                            compnames=(str,list,tuple),
                            index=(int,NoneType),
                            check=bool)
            def add(self, compnames, index=None, check=False):
                print 'ok'

        f = Foo()
        try: 
            f.add( 'comp1', 'comp2' )
        except TypeError as err:
            self.assertEqual(str(err), "Method argument 'index' with "
                                       "a value of 'comp2' does "
                                       "not match the allowed types "
                                       "(<type 'int'>, <type 'NoneType'>). \n"
                                       "           The arguments of this method have allowed "
                                       "types of index=(<type 'int'>, <type 'NoneType'>), "
                                       "check=<type 'bool'>, "
                                       "compnames=(<type 'str'>, <type 'list'>, <type 'tuple'>)")
        else:
            self.fail("expected TypeError")

        try:
            f.add( 'comp1',  check = 1.0 )
        except TypeError as err:
            self.assertEqual(str(err), "Method argument 'check' with a "
                             "value of 1.0 does not match one "
                             "of the allowed types <type 'bool'>. \n"
                             "           The arguments of this method "
                             "have allowed types of "
                             "index=(<type 'int'>, "
                             "<type 'NoneType'>), check=<type 'bool'>, "
                             "compnames=(<type 'str'>, <type 'list'>, <type 'tuple'>)" )
        else:
            self.fail("expected TypeError")

    def test_function_accepts(self):

        @function_accepts(TypeError,
                        compnames=(str,list,tuple),
                        index=(int,NoneType),
                        check=bool)
        def add(compnames, index=None, check=False):
                print 'ok'

        try:
            add( 'comp1', 'comp2' )
        except TypeError as err:
            self.assertEqual(str(err), "Function argument 'index' with "
                                       "a value of 'comp2' does "
                                       "not match the allowed types "
                                       "(<type 'int'>, <type 'NoneType'>). \n"
                                       "           The arguments of this function "
                                       "have allowed types of "
                                       "index=(<type 'int'>, "
                                       "<type 'NoneType'>), check=<type 'bool'>, "
                                       "compnames=(<type 'str'>, <type 'list'>, <type 'tuple'>)")
        else:
            self.fail("expected TypeError")

        try:
            add( 'comp1',  check = 1.0 )
        except TypeError as err:
            self.assertEqual(str(err), "Function argument 'check' with a "
                             "value of 1.0 does not match one "
                             "of the allowed types <type 'bool'>. \n"
                             "           The arguments of this function have "
                             "allowed types of "
                             "index=(<type 'int'>, <type 'NoneType'>), "
                             "check=<type 'bool'>, "
                             "compnames=(<type 'str'>, <type 'list'>, <type 'tuple'>)" )
        else:
            self.fail("expected TypeError")

if __name__ == '__main__':
    unittest.main()
    
