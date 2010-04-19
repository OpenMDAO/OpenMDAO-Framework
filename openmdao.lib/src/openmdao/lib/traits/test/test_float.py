# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError

from openmdao.main.exceptions import ConstraintError
from openmdao.main.api import Container
from openmdao.lib.api import Float
from openmdao.lib.traits.float import convert_units

class FloatTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        self.hobj.add_trait('float1', 
                            Float(98.9, low=0., high=99.,
                                  iotype='in', units='ft'))
        self.hobj.add_trait('float2', 
                            Float(13.2, iotype='out', units='inch'))
        self.hobj.add_trait('float3', 
                            Float(low=0., high=99.,
                                       iotype='in', units='kg'))
        
        self.hobj.float1 = 3.1415926
        self.hobj.float2 = 42.
        self.hobj.float3 = 1.1
                       
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_set_to_default(self):
        self.assertEqual(3.1415926, self.hobj.float1)
        self.hobj.add_trait('float4',
                            Float(iotype='in', units='kg'))
        self.assertEqual(0., self.hobj.float4)
        self.hobj.float4 = 6.5
        self.assertEqual(6.5, self.hobj.float4)
        
        self.hobj.revert_to_defaults()
        
        self.assertEqual(98.9, self.hobj.float1)
        self.assertEqual(0., self.hobj.float4)

        
    def test_assignment(self):
        # check starting value
        self.assertEqual(3.1415926, self.hobj.float1)
        # check default value
        self.assertEqual(98.9, self.hobj.trait('float1').default)
        
        # use unit_convert to perform unit conversion
        self.hobj.float1 = 3.
        self.hobj.float2 = convert_units(self.hobj.float1, self.hobj.trait('float1').units,
                                         'inch')
        self.assertAlmostEqual(36., self.hobj.float2,5)

    def test_unit_conversion(self):
        self.hobj.float2 = 12.  # inches
        self.hobj.float1 = convert_units(self.hobj.float2, self.hobj.trait('float2').units,
                                         'ft')
        self.assertEqual(self.hobj.float1, 1.) # 12 inches = 1 ft
        
        # now set to a value that will violate constraint after conversion
        self.hobj.float2 = 1200.  # inches
        try:
            self.hobj.float1 = self.hobj.get_wrapped_attr('float2')
        except TraitError, err:
            self.assertEqual(str(err), 
                ": Trait 'float1' must be a float in the range [0.0, 99.0] but attempted value is 100.0")
        else:
            self.fail('ConstraintError expected')
        
    def test_bogus_units(self):
        try:
            uf = Float(0., iotype='in', units='bogus')
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
        
    def test_intvalues(self):
        f1 = Float(3,low=2,high=4)
        d1 = f1.default_value/2
        self.assertAlmostEqual(d1, 1.5, places=4)
        
    def test_constraint_violations(self):
        try:
            self.hobj.float1 = 124
        except TraitError, err:
            self.assertEqual(str(err), 
                ": Trait 'float1' must be a float in the range [0.0, 99.0] but attempted value is 124")
        else:
            self.fail('TraitError expected')
        try:
            self.hobj.float1 = -3
        except TraitError, err:
            self.assertEqual(str(err),
                ": Trait 'float1' must be a float in the range [0.0, 99.0] but attempted value is -3")
        else:
            self.fail('TraitError exception')

    def test_attributes(self):
        try:
            self.hobj.add_trait('badbounds', Float(98.0, low=100.0, high=0.0, iotype='in'))
        except TraitError, err:
            errstring = "Lower bounds is greater than upper bounds."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
        
    def test_bad_connection(self):
        srcwrapper = self.hobj.get_wrapped_attr('float2')
        self.hobj.float1 = srcwrapper
        try:
            self.hobj.float3 = srcwrapper
        except Exception, err:
            self.assertEqual(str(err), 
                "float3: units 'inch' are incompatible with assigning units of 'kg'")
        else:
            self.fail('Exception expected')

    def test_constructor_defaults(self):
        
        self.hobj.add_trait('float_nodefault1',
                            Float(low=3.0, high=4.0, iotype='in', units='kg'))
        self.assertEqual(3.0, self.hobj.float_nodefault1)
        
        self.hobj.add_trait('float_nodefault2',
                            Float(high=4.0, iotype='in', units='kg'))
        self.assertEqual(4.0, self.hobj.float_nodefault2)
        
        self.hobj.add_trait('float_nodefault3',
                            Float(iotype='in', units='kg'))
        self.assertEqual(0.0, self.hobj.float_nodefault3)
            
        self.hobj.add_trait('float_nounits',
                            Float(low=3.0, high=4.0, iotype='in'))
        if hasattr(self.hobj.float_nounits,'units'):
            self.fail("Unitless Float should not have units")

    def test_exclusions(self):
        
        self.hobj.add_trait('float4', Float(low=3.0, high=4.0, \
                                  exclude_low=True, exclude_high=True, \
                                  iotype='in', units='kg'))
        try:
            self.hobj.float4 = 3.0
        except TraitError, err:
            self.assertEqual(str(err), 
                ": Trait 'float4' must be a float in the range (3.0, 4.0) but attempted value is 3.0")
        else:
            self.fail('TraitError expected')
        
            
if __name__ == "__main__":
    unittest.main()

