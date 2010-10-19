# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError

from openmdao.main.exceptions import ConstraintError
from openmdao.main.api import Container
from openmdao.lib.api import Int

class IntTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        self.hobj.add_trait('int1', Int(98, low=0, high=99, iotype='in'))
        self.hobj.add_trait('int2', Int(13, iotype='out'))
        self.hobj.add_trait('int3', Int(low=0, high=99, iotype='in'))
        
        self.hobj.int1 = 3
        self.hobj.int2 = 42
        self.hobj.int3 = 1
                       
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_set_to_default(self):
        self.assertEqual(3, self.hobj.int1)
        self.hobj.add_trait('int4', Int(iotype='in'))
        self.assertEqual(0, self.hobj.int4)
        self.hobj.int4 = 6
        self.assertEqual(6, self.hobj.int4)
        
        self.hobj.revert_to_defaults()
        
        self.assertEqual(98, self.hobj.int1)
        self.assertEqual(0, self.hobj.int4)
        
    def test_attributes(self):
        
        try:
            self.hobj.add_trait('inta', Int(98.0, low=0, high=99, iotype='in'))
        except TraitError, err:
            errstring = "Default value for an Int must be an integer."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
        
        try:
            self.hobj.add_trait('inta', Int(98, low=0.01, high=99, iotype='in'))
        except TraitError, err:
            errstring = "Lower bound for an Int must be an integer."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
        
        try:
            self.hobj.add_trait('inta', Int(98, low=0, high=99.9, iotype='in'))
        except TraitError, err:
            errstring = "Upper bound for an Int must be an integer."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
        
        try:
            self.hobj.add_trait('badbounds', Int(98, low=100, high=0, iotype='in'))
        except TraitError, err:
            errstring = "Lower bound is greater than upper bound."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
        
    def test_assignment(self):
        # check starting value
        self.assertEqual(3, self.hobj.int1)

        # check default value
        self.assertEqual(98, self.hobj.get_trait('int1').default)

        #check assignment with float
        try:
            self.hobj.int1 = 3.1
        except TraitError, err:
            errstring = ": Trait 'int1' must be 0 <= an integer <= 99, but " + \
                        "a value of 3.1 <type 'float'> was specified."
            self.assertEqual(str(err), errstring)
        else:
            self.fail('TraitError expected')


    def test_get(self):
        self.assertEqual(self.hobj.int1, 3)
        self.assertEqual(self.hobj.get('int1'), 3)
        
    def test_array_assign(self):
        try:
            self.hobj.int1[3] = 43
        except Exception, err:
            self.assertEqual(str(err), 
                "'int' object does not support item assignment")
        else:
            self.fail("Exception expected")
        
    def test_constraint_violations(self):
        try:
            self.hobj.int1 = 124
        except TraitError, err:
            errstring = ": Trait 'int1' must be" + \
                      " 0 <= an integer <= 99, but a value of 124" + \
                      " <type 'int'> was specified."
            self.assertEqual(str(err), errstring)
        else:
            self.fail('TraitError expected')
        try:
            self.hobj.int1 = -3
        except TraitError, err:
            errstring = ": Trait 'int1' must be" + \
                      " 0 <= an integer <= 99, but a value of -3" + \
                      " <type 'int'> was specified."
            self.assertEqual(str(err), errstring)
        else:
            self.fail('TraitError exception')

    def test_constructor_defaults(self):
        
        self.hobj.add_trait('int_nodefault1',
                            Int(low=3, high=4, iotype='in'))
        self.assertEqual(3, self.hobj.int_nodefault1)
        
        self.hobj.add_trait('int_nodefault2',
                            Int(high=4, iotype='in'))
        self.assertEqual(4, self.hobj.int_nodefault2)
        
        self.hobj.add_trait('int_nodefault3',
                            Int(iotype='in'))
        self.assertEqual(0, self.hobj.int_nodefault3)
            
    def test_exclusions(self):
        
        self.hobj.add_trait('int4', Int(low=3, high=4, \
                                  exclude_low=True, exclude_high=True, \
                                  iotype='in'))
        try:
            self.hobj.int4 = 3
        except TraitError, err:
            errstring = ": Trait 'int4' must be" + \
                      " 3 < an integer < 4, but a value of 3" + \
                      " <type 'int'> was specified."
            self.assertEqual(str(err), errstring)
        else:
            self.fail('TraitError expected')
        
            
if __name__ == "__main__":
    unittest.main()

