
import unittest

from openmdao.main import Assembly, Component, ListCaseIterator, Case, Int
from openmdao.main.interfaces import ICaseIterator, IAssembly
from openmdao.main.variable import OUTPUT
from openmdao.main.socket import Socket


class SocketComp(Assembly):
    iterator = Socket(ICaseIterator, 'cases to evaluate')
    
    def __init__(self):
        super(SocketComp, self).__init__('SocketComp')
        Int('num_cases', self, OUTPUT, default=0)
        
    def execute(self):
        self.num_cases = 0
        for case in self.iterator:
            self.num_cases += 1

class SocketComp2(SocketComp):
    somesocket = Socket(None, 'a dumb socket')
    
    def __init__(self):
        super(SocketComp2, self).__init__()
        
class SocketComp3(SocketComp2):
    iterator = Socket(IAssembly, 'another dumb socket')
    
    def __init__(self):
        super(SocketComp3, self).__init__()
        
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
        try:
            plugin = self.sc.iterator
        except RuntimeError, exc:
            self.assertEqual("SocketComp: socket 'iterator' is empty", str(exc))
        else:
            self.fail('RuntimeError expected')

    def test_wrong_interface(self):
        try:
            self.sc.iterator = Component('dummy')
        except ValueError, exc:
            self.assertEqual(
                "SocketComp: Socket 'iterator' requires interface 'ICaseIterator'",
                str(exc))
        else:
            self.fail('ValueError expected')

    def test_socket_filled(self):
        self.assertEqual(self.sc.socket_filled('iterator'), False)
        self.sc.iterator = ListCaseIterator([Case(), Case(), Case()])
        self.assertEqual(self.sc.socket_filled('iterator'), True)

        try:
            self.sc.socket_filled('no_socket')
        except AttributeError, exc:
            self.assertEqual("SocketComp: no Socket named 'no_socket'",
                             str(exc))
        else:
            self.fail('AttributeError expected')
            
    def test_inherit_sockets(self):
        sc2 = SocketComp2()
        self.assertEqual(sc2.socket_filled('iterator'), False)
        sc2.iterator = ListCaseIterator([Case(), Case(), Case()])
        self.assertEqual(sc2.socket_filled('iterator'), True)

        self.assertEqual(sc2.socket_filled('somesocket'), False)
        sc2.somesocket = Case()
        self.assertEqual(sc2.socket_filled('somesocket'), True)
        
        self.assertEqual(['iterator', 'somesocket'], sorted(sc2.list_sockets()))
        
    def test_socket_override(self):
        sc3 = SocketComp3()
        self.assertEqual(sc3._sockets['iterator'][0].iface, IAssembly)
        

if __name__ == "__main__":
    unittest.main()

