
import unittest

from openmdao.main import Assembly, Component, ListCaseIterator, Case, Int
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.variable import OUTPUT


class SocketComp(Assembly):
    def __init__(self):
        super(SocketComp, self).__init__('SocketComp')
        self.add_socket('iterator', ICaseIterator, 'cases to evaluate')
        Int('num_cases', self, OUTPUT, default=0)
        
    def execute(self):
        self.num_cases = 0
        for case in self.iterator:
            self.num_cases += 1


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
            self.assertEqual("SocketComp: plugin does not support 'ICaseIterator'",
                             str(exc))
        else:
            self.fail('ValueError expected')

    def test_remove_socket(self):
        self.sc.iterator = ListCaseIterator([Case(), Case(), Case()])
        plugin = self.sc.iterator
        self.sc.remove_socket('iterator')
        try:
            plugin = self.sc.iterator
        except AttributeError, exc:
            self.assertEqual("SocketComp: no such socket 'iterator'", str(exc))
        else:
            self.fail('AttributeError expected')

        try:
            self.sc.iterator = None
        except AttributeError, exc:
            self.assertEqual("SocketComp: no such socket 'iterator'", str(exc))
        else:
            self.fail('AttributeError expected')

    def test_socket_filled(self):
        self.assertEqual(self.sc.socket_filled('iterator'), False)
        self.sc.iterator = ListCaseIterator([Case(), Case(), Case()])
        self.assertEqual(self.sc.socket_filled('iterator'), True)

        try:
            self.sc.socket_filled('no_socket')
        except AttributeError, exc:
            self.assertEqual("SocketComp: no such socket 'no_socket'",                             str(exc))
        else:
            self.fail('AttributeError expected')


if __name__ == "__main__":
    unittest.main()

