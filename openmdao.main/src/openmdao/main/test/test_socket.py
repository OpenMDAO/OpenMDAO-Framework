
import unittest

from openmdao.main.api import Assembly, Component, Case
from openmdao.main.interfaces import ICaseIterator
from openmdao.lib.caseiterators.api import ListCaseIterator
from openmdao.lib.datatypes.api import Int, Instance, TraitError

class SocketComp(Assembly):
    iterator = Instance(ICaseIterator, allow_none=False, desc='cases to evaluate')
    num_cases = Int(0, iotype='out')
    
    def __init__(self):
        super(SocketComp, self).__init__('SocketComp')
        
    def execute(self):
        self.num_cases = 0
        for case in self.iterator:
            self.num_cases += 1

class SocketComp2(SocketComp):
    somesocket = Instance(Assembly)
    def __init__(self):
        super(SocketComp2, self).__init__()
        
class SocketComp3(SocketComp2):
    iterator = Instance(Assembly, desc='another dumb socket')
    
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
        self.sc.iterator = ListCaseIterator([Case(), Case(), Case()])
        self.sc.run()
        self.assertEqual(self.sc.num_cases, 3)

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
        except TraitError, exc:
            self.assertTrue(str(exc).startswith(
                "The 'iterator' trait of a SocketComp instance must be an ICaseIterator, but a value of"))
        else:
            self.fail('TraitError expected')

    def test_socket_filled(self):
        self.assertEqual(self.sc.iterator, None)
        self.sc.iterator = ListCaseIterator([Case(), Case(), Case()])
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
        lci = ListCaseIterator([Case(), Case(), Case()])
        sc2.iterator = lci
        self.assertEqual(sc2.iterator, lci)

        sc3 = SocketComp3()
        self.assertEqual(sc3.somesocket, None)
        asm = Assembly()
        sc3.somesocket = asm
        self.assertEqual(sc3.somesocket, asm)
        
    def test_socket_override(self):
        sc2 = SocketComp2()
        sc2.iterator = ListCaseIterator([Case(), Case(), Case()])
        try:
            sc2.iterator = Assembly()
        except TraitError:
            pass
        else:
            self.fail('TraitError expected')
            
        sc4 = SocketComp4()
        sc4.iterator = Assembly()
        try:
            sc4.iterator = ListCaseIterator([Case(), Case(), Case()])
        except TraitError:
            pass
        else:
            self.fail('TraitError expected')
        

if __name__ == "__main__":
    unittest.main()

