
import unittest

from enthought.traits.api import Int

from openmdao.main.api import Assembly, Component, Container, Case, Socket
from openmdao.main.interfaces import implements, ICaseIterator

import zope.interface

class CIterator(object):
    implements(ICaseIterator)
    def __iter__(self):
        return iter([])
    
class SocketComp(Assembly):
    iterator = Socket(ICaseIterator, allow_none=False, desc='cases to evaluate')
    num_cases = Int(0, iotype='out')
    
    def __init__(self):
        super(SocketComp, self).__init__('SocketComp')
        
    def execute(self):
        self.num_cases = 0
        for case in self.iterator:
            self.num_cases += 1

class SocketComp2(SocketComp):
    somesocket = Socket(Assembly)
    def __init__(self):
        super(SocketComp2, self).__init__()
        
class SocketComp3(SocketComp2):
    iterator = Socket(Assembly, desc='another dumb socket')
    
    def __init__(self):
        super(SocketComp3, self).__init__()
        
class SocketComp4(SocketComp3):
    def __init__(self):
        super(SocketComp4, self).__init__()
        
class SocketTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.sc = SocketComp()
    
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
            self.assertEqual("'SocketComp' object has no attribute 'no_socket'",                             str(exc))
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
            self.assertEqual("'SocketComp' object has no attribute 'no_socket'",
                             str(exc))
        else:
            self.fail('AttributeError expected')
            
    def test_inherit_sockets(self):
        sc2 = SocketComp2()
        self.assertEqual(sc2.iterator, None)
        lci = CIterator()
        sc2.iterator = lci
        self.assertEqual(sc2.iterator, lci)

        sc3 = SocketComp3()
        self.assertEqual(sc3.somesocket, None)
        asm = Assembly()
        sc3.somesocket = asm
        self.assertEqual(sc3.somesocket, asm)
        
    def test_socket_override(self):
        sc2 = SocketComp2()
        sc2.iterator = CIterator()
        try:
            sc2.iterator = Assembly()
        except TypeError:
            pass
        else:
            self.fail('TypeError expected')
            
        sc4 = SocketComp4()
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

class SocketTestCase2(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        self.hobj.add_trait('iface_sock', Socket(MyIface))
        self.hobj.add_trait('class_sock', Socket(MyClass))
                       
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

