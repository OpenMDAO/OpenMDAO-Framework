
import unittest
import pickle
import warnings

from openmdao.main.api import Assembly, Component, Container, Case, VariableTree
from openmdao.main.datatypes.api import Slot, Int, List, Dict, Str
from openmdao.main.interfaces import implements, ICaseIterator
from openmdao.util.testutil import assert_raises

import zope.interface


class CIterator(object):
    implements(ICaseIterator)

    def __iter__(self):
        return iter([])


class SlotComp(Assembly):
    iterator = Slot(ICaseIterator, allow_none=False, desc='cases to evaluate')
    num_cases = Int(0, iotype='out')

    def __init__(self):
        super(SlotComp, self).__init__()

    def execute(self):
        self.num_cases = 0
        for case in self.iterator:
            self.num_cases += 1


class SlotComp2(SlotComp):
    somesocket = Slot(Assembly)


class SlotComp3(SlotComp2):
    iterator = Slot(Assembly, desc='another dumb socket')


class SlotComp4(SlotComp3):
    pass




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
            self.assertEqual("'SlotComp' object has no attribute 'no_socket'",
                             str(exc))
        else:
            self.fail('AttributeError expected')

    def test_no_plugin(self):
        plugin = self.sc.iterator
        self.assertEqual(plugin, None)

    def test_wrong_interface(self):
        try:
            self.sc.iterator = Component()
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

    def test_pickle(self):
        s = Slot()
        out = pickle.dumps(s)

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

    def test_variabletree(self):
        # Ensure VariableTree is rejected.
        msg = 'Slotting of VariableTrees is not supported,' \
              ' please use VarTree instead'

        code = 'Slot(VariableTree)'
        assert_raises(self, code, globals(), locals(), TypeError, msg)

        code = 'Slot(VariableTree())'
        assert_raises(self, code, globals(), locals(), TypeError, msg)

    def test_deprecated_metadata(self):
        with warnings.catch_warnings(record=True) as w:
            Slot(Assembly, iotype="in")
            assert len(w) == 1
            assert issubclass(w[-1].category, FutureWarning)
            assert "Slot" in str(w[-1].message)
            assert "iotype" in str(w[-1].message)


class MyIface(zope.interface.Interface):

    xx = zope.interface.Attribute("some attribute")

    def myfunct(a, b):
        """some function"""


class MyClass(Container):
    implements(MyIface)

    def __init__(self):

        super(MyClass, self).__init__()
        self.x = 1

    def myfunct(a, b):
        return a+b


class MyOtherClass(Container):
    def __init__(self):

        super(MyOtherClass, self).__init__()
        self.x = 1

    def myfunct(a, b):
        return a+b


class SlotTestCase2(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        self.hobj.add('iface_sock', Slot(MyIface))
        self.hobj.add('class_sock', Slot(MyClass))

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
            self.assertEqual(str(err), ": class_sock must be an instance of"
                             " class 'MyClass', got <type 'float'>")


class SlotComp5(Assembly):
    iterator = Slot(CIterator(), allow_none=False, desc='cases to evaluate')
    num_cases = Int(0, iotype='out')

    def execute(self):
        self.num_cases = 0
        for case in self.iterator:
            self.num_cases += 1


class SlotTestCase3(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.sc = SlotComp5()

    def tearDown(self):
        """this teardown function will be called after each test"""
        pass

    def test_normal(self):

        # Run as is
        self.sc.run()
        self.assertEqual(self.sc.num_cases, 0)

        # Slot a new instance
        self.sc.iterator = CIterator()
        self.sc.run()
        self.assertEqual(self.sc.num_cases, 0)


if __name__ == "__main__":
    unittest.main()
