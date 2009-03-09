# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.exceptions import ConstraintError
from openmdao.main import Container,String
from openmdao.main.variable import Variable, INPUT, OUTPUT

class StringTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container('h1', None)
        self.hobj.internal_s1 = "abcd"
        self.hobj.internal_s2 = "xyz"
        self.hobj.internal_s3 = "z"
        self.s1 = String('s1', self.hobj, INPUT, 
                       ref_name='internal_s1', default="foo")
        self.s2 = String('s2', self.hobj, OUTPUT, 
                       ref_name='internal_s2', default="bar",
                       min_length=1)
        self.s3 = String('s3', self.hobj, INPUT, 
                       ref_name='internal_s3',
                       min_length=1, max_length=5)
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_assignment(self):
        # check starting value
        self.assertEqual("abcd", self.s1.value)
        # check default value
        self.assertEqual("foo", self.s1.default)
        self.s1.value = self.s2.value
        self.assertEqual("xyz", self.s1.value)
        # make sure value gets transferred to internal variable
        self.assertEqual("xyz", self.hobj.internal_s1)
        self.s1.value = "qq"
        self.assertEqual("qq", self.s1.value)
        self.assertEqual("qq", self.hobj.internal_s1)
        
    def test_constraint_violations(self):
        try:
            self.s3.value = ""
        except ConstraintError, err:
            self.assertEqual(str(err), 
                    "h1.s3: min length constraint '0 >= 1' has been violated")
        else:
            self.fail('ConstraintError expected')
        try:
            self.s3.value = "abcdefghi"
        except ConstraintError, err:
            self.assertEqual(str(err), 
                    "h1.s3: max length constraint '9 <= 5' has been violated")
        else:
            self.fail('ConstraintError expected')

    def test_bad_connection(self):
        self.hobj.vv = 1
        vv = Variable('vv', self.hobj, OUTPUT)
        self.s1.validate_var(self.s2)
        try:
            self.s3.validate_var(vv)
        except TypeError, err:
            self.assertEqual(str(err), 
                "h1.s3: assignment to incompatible variable 'h1.vv' of type"+
                " '<class 'openmdao.main.variable.Variable'>'")
        else:
            self.fail('TypeError expected')

    
    def test_constraint_violation_setvar(self):
        """violate a constraint using setvar"""
        self.hobj.internal_s2 = "sdfasdfasf"
        self.s3.validate_var(self.s2) # should be OK since no value xfer
        try:
            self.s3.setvar(None, self.s2)
        except ValueError, err:
            self.assertEqual(str(err), 
                "h1.s3: max length constraint '10 <= 5' has been violated")
        else:
            self.fail("ValueError expected")
            

if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

