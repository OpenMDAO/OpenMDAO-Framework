
import unittest
from inspect import getmembers, ismethod, isfunction

from enthought.traits.api import HasTraits, Float

from openmdao.util.decorators import add_delegate


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
    

class decoratorTestCase(unittest.TestCase):

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


if __name__ == '__main__':
    unittest.main()
    