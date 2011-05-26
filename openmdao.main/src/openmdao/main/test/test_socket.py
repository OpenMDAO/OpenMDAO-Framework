
import unittest

from enthought.traits.api import Int

from openmdao.main.api import Assembly, Component, Container, Case, Slot
from openmdao.main.interfaces import implements, ICaseIterator

import zope.interface

class CIterator(object):
    implements(ICaseIterator)
    def __iter__(self):
        return iter([])
    
class SlotComp(Assembly):
    iterator = Slot(ICaseIterator, allow_none=False, desc='cases to evaluate')
    num_cases = Int(0, iotype='out')
    
    def __init__(self):
        super(SlotComp, self).__init__('SlotComp')
        
    def execute(self):
        self.num_cases = 0
        for case in self.iterator:
            self.num_cases += 1

class SlotComp2(SlotComp):
    somesocket = Slot(Assembly)
    def __init__(self):
        super(SlotComp2, self).__init__()
        
class SlotComp3(SlotComp2):
    iterator = Slot(Assembly, desc='another dumb socket')
    
    def __init__(self):
        super(SlotComp3, self).__init__()
        
class SlotComp4(SlotComp3):
    def __init__(self):
        super(SlotComp4, self).__init__()
        
class SlotTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.sc = SlotComp()
    
    def tearDown(self):
        """this teardown function will be called after each test"""
        pass

    def test_normal(self):
        self.sc.iterator = CIterator()
        self.sc.run()
        self.assertEqual(self.sc.num_cases, 0)

    def test_no_socket(self):
        try:
            plugin = self.sc.no_socket
        except AttributeError, exc:
            self.assertEqual("'SlotComp' object has no attribute 'no_socket'",                             str(exc))
        else:
            self.fail('AttributeError expected')

    def test_no_plugin(self):
        plugin = self.sc.iterator
        self.assertEqual(plugin, None)

    def test_wrong_interface(self):
        try:
            self.sc.iterator = Component('dummy')
        except TypeError, exc:
            self.assertEqual(str(exc), ": iterator must provide interface 'ICaseIterator'")
        else:
            self.fail('TypeError expected')

    def test_socket_filled(self):
        self.assertEqual(self.sc.iterator, None)
        self.sc.iterator = CIterator()
        self.assertNotEqual(self.sc.iterator, None)

        try:
            x = self.sc.no_socket
        except AttributeError, exc:
            self.assertEqual("'SlotComp' object has no attribute 'no_socket'",
                             str(exc))
        else:
            self.fail('AttributeError expected')
            
    def test_inherit_sockets(self):
        sc2 = SlotComp2()
        self.assertEqual(sc2.iterator, None)
        lci = CIterator()
        sc2.iterator = lci
        self.assertEqual(sc2.iterator, lci)

        sc3 = SlotComp3()
        self.assertEqual(sc3.somesocket, None)
        asm = Assembly()
        sc3.somesocket = asm
        self.assertEqual(sc3.somesocket, asm)
        
    def test_socket_override(self):
        sc2 = SlotComp2()
        sc2.iterator = CIterator()
        try:
            sc2.iterator = Assembly()
        except TypeError:
            pass
        else:
            self.fail('TypeError expected')
            
        sc4 = SlotComp4()
        sc4.iterator = Assembly()
        try:
            sc4.iterator = CIterator()
        except TypeError:
            pass
        else:
            self.fail('TypeError expected')
        
class MyIface(zope.interface.Interface):
    
    xx = zope.interface.Attribute("some attribute")

    def myfunct(a, b):
        """some function"""
    
class MyClass(object):
    implements(MyIface)
    
    def __init__(self):
        self.x = 1

    def myfunct(a, b):
        return a+b
    
class MyOtherClass(object):
    def __init__(self):
        self.x = 1

    def myfunct(a, b):
        return a+b

class SlotTestCase2(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        self.hobj.add_trait('iface_sock', Slot(MyIface))
        self.hobj.add_trait('class_sock', Slot(MyClass))
                       
    def test_set(self):
        mc = MyClass()
        self.hobj.class_sock = mc
        self.hobj.iface_sock = mc
        
    def test_bad_set(self):
        moc = MyOtherClass()
        try:
            self.hobj.iface_sock = moc
        except TypeError as err:
            self.assertEqual(str(err), ": iface_sock must provide interface 'MyIface'")
            
        try:
            self.hobj.class_sock = 3.14
        except TypeError as err:
            self.assertEqual(str(err), ": class_sock must be an instance of class 'MyClass'")
        
            
if __name__ == "__main__":
    unittest.main()

