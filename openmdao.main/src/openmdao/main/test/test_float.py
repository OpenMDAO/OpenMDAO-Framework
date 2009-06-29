# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError

from openmdao.main.exceptions import ConstraintError
from openmdao.main.api import Container, UnitsFloat

class FloatTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container('h1', None)
        self.hobj.add_trait('float1', 
                            UnitsFloat(98.9, low=0., high=99.,
                                  iostatus='in', units='ft'))
        self.hobj.add_trait('float2', 
                            UnitsFloat(13.2, iostatus='out', units='inch'))
        self.hobj.add_trait('float3', 
                            UnitsFloat(low=0., high=99.,
                                       iostatus='in', units='kg'))
        
        self.hobj.float1 = 3.1415926
        self.hobj.float2 = 42.
        self.hobj.float3 = 1.1
                       
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_assignment(self):
        # check starting value
        self.assertEqual(3.1415926, self.hobj.float1)
        # check default value
        self.assertEqual(98.9, self.hobj.trait('float1').default)
        
        # use tuple (value,units) to perform unit conversion
        self.hobj.assign('float2', 'float1')
        self.assertAlmostEqual(3.5, self.hobj.float1)
        
        # make sure value gets transferred to internal variable
        self.assertEqual(42., self.hobj.float1)
        self.hobj.float1 = 32.1
        self.assertEqual(32.1, self.hobj.float1)

    def test_unit_conversion(self):
        self.hobj.float2 = 12.  # inches
        self.hobj.float1 = self.hobj.in_units_of(self.hobj.float2, self.hobj.trait('float2').units)
        self.assertEqual(self.hobj.float1, 1.) # 12 inches = 1 ft
        
        # now set to a value that will violate constraint after conversion
        self.hobj.float2 = 1200.  # inches
        try:
            self.hobj.assign('float2', 'float1')
        except TraitError, err:
            self.assertEqual(str(err), 
                             "h1.float1: constraint '100.0 <= 99.0' has been violated")
        else:
            self.fail('ConstraintError expected')
        
    def test_bogus_units(self):
        try:
            uf = UnitsFloat(0., iostatus='in', units='bogus')
        except TraitError, err:
            self.assertEqual(str(err), 
                             "Units of 'bogus' are invalid")
        else:
            self.fail('ValueError expected')
        
    def test_get(self):
        self.assertEqual(self.hobj.float1, 3.1415926)
        self.assertEqual(self.hobj.get('float1'), 3.1415926)
        
    #def test_set_attribute(self):
        #self.hobj.float1.set("units", "ft**2")
        #self.assertEqual(self.hobj.float1.units, "ft**2")
        #try:
            #self.hobj.float1.set("units", "inch**2", [2])
        #except ValueError, err:
            #self.assertEqual(str(err), 
                #"h1.float1: array indexing of Variable attributes not supported")
        #else:
            #self.fail("ValueError expected")
        
    def test_array_assign(self):
        try:
            self.hobj.float1[3] = 1.3
        except Exception, err:
            self.assertEqual(str(err), 
                "'float' object does not support item assignment")
        else:
            self.fail("Exception expected")
        
    def test_constraint_violations(self):
        try:
            self.hobj.float1 = 124
        except TraitError, err:
            self.assertEqual(str(err), 
                "The 'float1' trait of a Container instance must be 0.0 <= a floating point number <= 99.0, but a value of 124 <type 'int'> was specified.")
        else:
            self.fail('TraitError expected')
        try:
            self.hobj.float1 = -3
        except TraitError, err:
            self.assertEqual(str(err), 
                "The 'float1' trait of a Container instance must be 0.0 <= a floating point number <= 99.0, but a value of -3 <type 'int'> was specified.")
        else:
            self.fail('TraitError exception')

    def test_bad_connection(self):
        self.hobj.trait('float1').validate_with_trait(self.hobj, 'float1', 
                                                      self.hobj.float2,
                                                      self.hobj.trait('float2'))
        try:
            self.hobj.trait('float3').validate_with_trait(self.hobj, 'float3', 
                                                          self.hobj.float2,
                                                          self.hobj.trait('float2'))
        except Exception, err:
            self.assertEqual(str(err), 
                "float3: units 'inch' are incompatible with assigning units of 'kg'")
        else:
            self.fail('Exception expected')

if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

