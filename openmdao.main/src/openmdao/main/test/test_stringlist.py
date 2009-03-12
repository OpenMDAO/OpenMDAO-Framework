# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.exceptions import ConstraintError
from openmdao.main import Container, StringList
from openmdao.main.variable import Variable, INPUT, OUTPUT

class StringListTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container('h1', None)
        self.hobj.internal_sl1 = ["abcd", "def"]
        self.hobj.internal_sl2 = []
        self.hobj.internal_sl3 = ["zz"]
        self.sl1 = StringList('sl1', self.hobj, INPUT, 
                       ref_name='internal_sl1', default=["foo"])
        self.sl2 = StringList('sl2', self.hobj, OUTPUT, 
                       ref_name='internal_sl2', default=["bar"],
                       min_length=1)
        self.sl3 = StringList('sl3', self.hobj, INPUT, 
                       ref_name='internal_sl3',
                       min_length=1, max_length=5)
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_assignment(self):
        # check starting value
        self.assertEqual(["abcd", "def"], self.sl1.value)
        # check default value
        self.assertEqual(["foo"], self.sl1.default)
        self.sl1.value = self.sl2.value
        self.assertEqual([], self.sl1.value)
        # make sure value gets transferred to internal variable
        self.assertEqual([], self.hobj.internal_sl1)
        self.sl1.value = ["qq"]
        self.assertEqual(["qq"], self.sl1.value)
        self.assertEqual(["qq"], self.hobj.internal_sl1)
    
    def test_get(self):
        val = self.sl1.get(None, [1])
        self.assertEqual("def", val)
        val = self.sl1.get('value', [1])
        self.assertEqual("def", val)
        
    def test_set(self):
        self.sl1.set(None, '___', [1])
        self.assertEqual(["abcd", "___"], self.sl1.value)
        self.sl1.set('value', '000', [0])
        self.assertEqual(["000", "___"], self.sl1.value)
        
    def test_bad_set(self):
        try:
            self.sl1.set(None, '___', [99])
        except IndexError, err:
            self.assertEqual(str(err), "h1.sl1: index 99 out of range")
        else:
            self.fail('IndexError expected')
        try:
            self.sl1.set('value', '000', [-3])
        except IndexError, err:
            self.assertEqual(str(err), "h1.sl1: index -3 out of range")
        else:
            self.fail('IndexError expected')
        try:
            self.sl1.set(None, ["foo"], [0])
        except ValueError, err:
            self.assertEqual(str(err), 
                "h1.sl1: cannot assign a value of type <type 'list'> to a StringList entry")
        else:
            self.fail('ValueError expected')
        try:
            self.sl1.set('value', ["foo"], [0])
        except ValueError, err:
            self.assertEqual(str(err), 
                "h1.sl1: cannot assign a value of type <type 'list'> to a StringList entry")
        else:
            self.fail('ValueError expected')
            
        
    def test_bad_assignment(self):
        try:
            self.sl1.value = ["foo", 1, 2]
        except ValueError, err:
            self.assertEqual(str(err), 
                "h1.sl1: list contains non-string entries")
        else:
            self.fail("ValueError expected")
        
    def test_constraint_violations(self):
        try:
            self.sl3.value = []
        except ConstraintError, err:
            self.assertEqual(str(err), 
                "h1.sl3: min length constraint '0 >= 1' has been violated")
        else:
            self.fail('ConstraintError expected')
        try:
            self.sl3.value = ["a", "b", "c", "d", "e", "f", "g"]
        except ConstraintError, err:
            self.assertEqual(str(err), 
                "h1.sl3: max length constraint '7 <= 5' has been violated")
        else:
            self.fail('ConstraintError expected')

    def test_bad_connection(self):
        self.hobj.vv = 1
        vv = Variable('vv', self.hobj, OUTPUT)
        self.sl1.validate_var(self.sl2)
        try:
            self.sl3.validate_var(vv)
        except TypeError, err:
            self.assertEqual(str(err), 
                "h1.sl3: assignment to incompatible variable 'h1.vv'"+
                " of type '<class 'openmdao.main.variable.Variable'>'")
        else:
            self.fail('TypeError expected')

    
    def test_constraint_violation_setvar(self):
        """violate a constraint using setvar"""
        self.hobj.internal_sl2 = ["a", "b", "c", "d", "e", "f", "g"]
        self.sl3.validate_var(self.sl2) # should be OK since no value xfer
        try:
            self.sl3.setvar(None, self.sl2)
        except ValueError, err:
            self.assertEqual(str(err), 
                "h1.sl3: max length constraint '7 <= 5' has been violated")
        else:
            self.fail("ValueError expected")

    def test_bad_default(self):
        self.hobj.internal_sl4 = ['a', 'b', 'c', 'd', 'e', 'f']
        try:
            sl4 = StringList('sl4', self.hobj, INPUT, 
                             ref_name='internal_sl4',
                             min_length=1, max_length=5)
        except ValueError, err:
            self.assertEqual(str(err), 
                "h1.sl4: invalid default value:  max length "+
                "constraint '6 <= 5' has been violated")
        else:
            self.fail("ValueError expected")
        try:
            sl4 = StringList('sl4', self.hobj, INPUT, default=5,
                             ref_name='internal_sl4',
                             min_length=1, max_length=5)
        except ValueError, err:
            self.assertEqual(str(err), "h1.sl4: invalid default value:  "+
                             "incompatible with type <type 'int'>")
        else:
            self.fail("ValueError expected")

if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

