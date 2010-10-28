# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError

from openmdao.main.exceptions import ConstraintError
from openmdao.main.api import Container
from openmdao.lib.datatypes.enum import Enum

class IntTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

        
    def test_constructors(self):
        
        try:
            self.hobj.add_trait('nothing',
                            Enum(iotype='in'))
        except TraitError, err:
            errstring = "Enum must contain at least one value."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")

        self.hobj.add_trait('simplest',
                            Enum(1, iotype='in'))
        self.assertEqual(1, self.hobj.simplest)

        self.hobj.add_trait('strangest',
                            Enum(1, 1, iotype='in'))
        self.assertEqual(1, self.hobj.strangest)

        self.hobj.add_trait('simple_nodefault',
                            Enum((1,2,3), iotype='in'))
        self.assertEqual(1, self.hobj.simple_nodefault)

        self.hobj.add_trait('simple_default',
                            Enum(2,(1,2,3), iotype='in'))
        self.assertEqual(2, self.hobj.simple_default)
        
        try:
            self.hobj.add_trait('out_of_bounds',
                            Enum(4,(1,2,3), iotype='in'))
        except TraitError, err:
            errstring = "Default value not in values."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
        
        try:
            self.hobj.add_trait('bad_alias_size',
                            Enum(3,(1,2,3), iotype='in',
                                 aliases=('a','b')))
        except TraitError, err:
            errstring = "Length of aliases does not match length of values."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
            
        self.hobj.add_trait('good_alias_size',
                        Enum(3,(1,2,3), iotype='in',
                                aliases=('a','b','c')))
        self.assertEqual(3, self.hobj.good_alias_size)
            
    def test_set_to_default(self):
        
        self.hobj.add_trait('e1', Enum(2, (0,2,3), iotype='in'))
        self.assertEqual(2, self.hobj.e1)
        self.hobj.e1 = 3
        self.assertEqual(3, self.hobj.e1)
        
        self.hobj.revert_to_defaults()
        
        self.assertEqual(2, self.hobj.e1)

        
    def test_set_to_bad_value(self):

        self.hobj.add_trait('e1', Enum("red", ("red","green","blue"), iotype='in'))
        self.assertEqual('red', self.hobj.e1)
        try:
            self.hobj.e1 = "brown"
        except TraitError, err:
            errstring = ": Trait 'e1' must be in ('red', 'green', 'blue'), but a value of brown <type 'str'> was specified."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")

        
if __name__ == "__main__":
    unittest.main()

