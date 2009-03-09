# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.exceptions import ConstraintError
from openmdao.main import Container,Float
from openmdao.main.variable import INPUT, OUTPUT, UNDEFINED

class FloatTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container('h1', None)
        self.hobj.internal_float1 = 3.1415926
        self.hobj.internal_float2 = 42.
        self.hobj.internal_float3 = 1.1
        self.float1 = Float('float1', self.hobj, INPUT, 
                       ref_name='internal_float1', default=98.9,
                       min_limit=0., max_limit=99., units='ft')
        self.float2 = Float('float2', self.hobj, OUTPUT, default=13.2, 
                       ref_name='internal_float2', units='inch')
        self.float3 = Float('float3', self.hobj, INPUT, 
                       ref_name='internal_float3',
                       min_limit=0., max_limit=99., units='kg')
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_assignment(self):
        # check starting value
        self.assertEqual(3.1415926, self.float1.value)
        # check default value
        self.assertEqual(98.9, self.float1.default)
        self.float1.value = self.float2.value
        self.assertEqual(42., self.float1.value)
        # make sure value gets transferred to internal variable
        self.assertEqual(42., self.hobj.internal_float1)
        self.float1.value = 32.1
        self.assertEqual(32.1, self.float1.value)
        self.assertEqual(32.1, self.hobj.internal_float1)

    def test_unit_conversion(self):
        self.hobj.internal_float2 = 12.  # inches
        self.float1.setvar(None, self.float2)
        self.assertEqual(self.hobj.internal_float1, 1.) # 12 inches = 1 ft
        
        # now set to a value that will violate constraint after conversion
        self.hobj.internal_float2 = 1200.  # inches
        try:
            self.float1.setvar(None, self.float2)
        except ConstraintError, err:
            self.assertEqual(str(err), 
                             "h1.float1: constraint '100.0 <= 99.0' has been violated")
        else:
            self.fail('ConstraintError expected')
        
    def test_bogus_units(self):
        try:
            self.float1.units = 'bogus'
        except ValueError, err:
            self.assertEqual(str(err), 
                             "h1.float1: Units of 'bogus' are invalid")
        else:
            self.fail('ValueError expected')
        
    def test_get(self):
        val = self.float1.get(None)
        self.assertEqual(val, 3.1415926)
        val = self.float1.get('value')
        self.assertEqual(val, 3.1415926)
        
    def test_set_attribute(self):
        self.float1.set("units", "ft**2")
        self.assertEqual(self.float1.units, "ft**2")
        try:
            self.float1.set("units", "inch**2", [2])
        except ValueError, err:
            self.assertEqual(str(err), 
                "h1.float1: array indexing of Variable attributes not supported")
        else:
            self.fail("ValueError expected")
        
    def test_array_assign(self):
        try:
            self.float1.set(None, 1.3, [3])
        except NotImplementedError, err:
            self.assertEqual(str(err), "h1.float1: _pre_assign_entry")
        else:
            self.fail("NotImplementedError expected")
        
    def test_constraint_violations(self):
        try:
            self.float1.value = 124
        except ConstraintError, err:
            self.assertEqual(str(err), 
                        "h1.float1: constraint '124 <= 99.0' has been violated")
        else:
            self.fail(
                'expected exception for max_limit violation did not happen')
        try:
            self.float1.value = -3
        except ConstraintError, err:
            self.assertEqual(str(err), 
                        "h1.float1: constraint '-3 >= 0.0' has been violated")
        else:
            self.fail('ConstraintError exception')

    def test_bad_connection(self):
        self.float1.validate_var(self.float2)
        self.float1.units = 'lb/ft**2'
        self.float2.units = 'kg'
        try:
            self.float1.validate_var(self.float2)
        except TypeError, err:
            self.assertEqual(str(err), 'h1.float2 units (kg) are incompatible'+
                             ' with units (lb/ft**2) of h1.float1')
        else:
            self.fail('TypeError expected')

if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

