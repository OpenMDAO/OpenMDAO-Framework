# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.exceptions import ConstraintError
from openmdao.main.api import Container
from openmdao.main.datatypes.enum import Enum

class IntTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

        
    def test_constructors(self):
        
        try:
            self.hobj.add('nothing',
                            Enum(iotype='in'))
        except Exception, err:
            errstring = "Enum must contain at least one value."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")

        self.hobj.add('simplest',
                            Enum(1, iotype='in'))
        self.assertEqual(1, self.hobj.simplest)

        self.hobj.add('strangest',
                            Enum(1, 1, iotype='in'))
        self.assertEqual(1, self.hobj.strangest)

        self.hobj.add('simple_nodefault',
                            Enum((1,2,3), iotype='in'))
        self.assertEqual(1, self.hobj.simple_nodefault)

        self.hobj.add('simple_default',
                            Enum(2,(1,2,3), iotype='in'))
        self.assertEqual(2, self.hobj.simple_default)
        
        try:
            self.hobj.add('out_of_bounds',
                            Enum(4,(1,2,3), iotype='in'))
        except Exception, err:
            errstring = "Default value not in values."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
        
        try:
            self.hobj.add('bad_alias_size',
                            Enum(3,(1,2,3), iotype='in',
                                 aliases=('a','b')))
        except Exception, err:
            errstring = "Length of aliases does not match length of values."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
            
        self.hobj.add('good_alias_size',
                        Enum(3,(1,2,3), iotype='in',
                                aliases=('a','b','c')))
        self.assertEqual(3, self.hobj.good_alias_size)
            
    def test_set_to_default(self):
        
        self.hobj.add('e1', Enum(2, (0,2,3), iotype='in'))
        self.assertEqual(2, self.hobj.e1)
        self.hobj.e1 = 3
        self.assertEqual(3, self.hobj.e1)
        
        self.hobj.revert_to_defaults()
        
        self.assertEqual(2, self.hobj.e1)

        
    def test_set_to_bad_value(self):

        self.hobj.add('e1', Enum("red", ("red","green","blue"), iotype='in'))
        self.assertEqual('red', self.hobj.e1)
        try:
            self.hobj.e1 = "brown"
        except Exception, err:
            errstring = ": Variable 'e1' must be in ('red', 'green', 'blue'), but a value of brown <type 'str'> was specified."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")

    def enum_typecheck(self):
        
        self.hobj.add('E_float', Enum('2.0', (1.0, 2.0, 3.0), iotype='in'))
        self.hobj.E_float = 2
        self.assertTrue(isinstance(self.hobj.E_float, float))
        self.hobj.add('E_int', Enum('2', (1, 2, 3), iotype='in'))
        self.hobj.E_float = 3.0000
        self.assertTrue(isinstance(self.hobj.E_float, int))
        
if __name__ == "__main__":
    unittest.main()

