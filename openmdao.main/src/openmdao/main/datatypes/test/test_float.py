# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Container
from openmdao.main.datatypes.float import Float
from openmdao.units import convert_units

class FloatTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        self.hobj.add('float1', 
                            Float(98.9, low=0., high=99.0, desc="Stuff",
                                  iotype='in', units='ft'))
        self.hobj.add('float2', 
                            Float(13.2, iotype='out', units='inch', low=-9999.))
        self.hobj.add('float3', 
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
        self.hobj.add('float4',
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
        self.assertEqual(98.9, self.hobj.get_trait('float1').default)
        
        # use unit_convert to perform unit conversion
        self.hobj.float1 = 3.
        self.hobj.float2 = convert_units(self.hobj.float1, self.hobj.get_trait('float1').units,
                                         'inch')
        self.assertAlmostEqual(36., self.hobj.float2,5)
        
    def test_bogus_units(self):
        try:
            uf = Float(0., iotype='in', units='bogus')
        except ValueError, err:
            self.assertEqual(str(err), 
                             "Units of 'bogus' are invalid")
        else:
            self.fail('ValueError expected')
        
    def test_get(self):
        self.assertEqual(self.hobj.float1, 3.1415926)
        self.assertEqual(self.hobj.get('float1'), 3.1415926)
                
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
        
    def test_range_violations(self):
        try:
            self.hobj.float1 = 124
        except ValueError, err:
            self.assertEqual(str(err), 
                ": Variable 'float1' must be a float in the range [0.0, 99.0], but a value of 124 <type 'int'> was specified.")
        else:
            self.fail('ValueError expected')
        try:
            self.hobj.float1 = -3
        except ValueError, err:
            self.assertEqual(str(err),
                ": Variable 'float1' must be a float in the range [0.0, 99.0], but a value of -3 <type 'int'> was specified.")
        else:
            self.fail('ValueError exception')

    def test_attributes(self):
        try:
            self.hobj.add('badbounds', Float(98.0, low=100.0, high=0.0, iotype='in'))
        except Exception, err:
            errstring = "Lower bound is greater than upper bound."
            self.assertEqual(str(err), errstring)
        else:
            self.fail("Exception expected")
        
    def test_constructor_defaults(self):
        
        self.hobj.add('float_nodefault1',
                            Float(low=3.0, high=4.0, iotype='in', units='kg'))
        self.assertEqual(3.0, self.hobj.float_nodefault1)
        
        self.hobj.add('float_nodefault2',
                            Float(high=4.0, iotype='in', units='kg'))
        self.assertEqual(4.0, self.hobj.float_nodefault2)
        
        self.hobj.add('float_nodefault3',
                            Float(iotype='in', units='kg'))
        self.assertEqual(0.0, self.hobj.float_nodefault3)
            
        self.hobj.add('float_nounits',
                            Float(low=3.0, high=4.0, iotype='in'))
        if hasattr(self.hobj.float_nounits,'units'):
            self.fail("Unitless Float should not have units")

    def test_exclusions(self):
        
        self.hobj.add('float4', Float(low=3.0, high=4.0, \
                                  exclude_low=True, exclude_high=True, \
                                  iotype='in', units='kg'))
        try:
            self.hobj.float4 = 3.0
        except ValueError, err:
            self.assertEqual(str(err), 
                ": Variable 'float4' must be a float in the range (3.0, 4.0), but a value of 3.0 <type 'float'> was specified.")
        else:
            self.fail('ValueError expected')
        
    def test_int_limits(self):
        # Ensure limits that are ints don't cause something like this:
        #     Trait 'symmetry_angle' must be a float in the range (0, 180]
        #     but attempted value is 11.25
        self.hobj.add('symmetry_angle',
                            Float(low=0, exclude_low=True, high=180))
        self.hobj.symmetry_angle = 11.25
        self.assertEqual(self.hobj.symmetry_angle, 11.25)

    def test_default_value_type(self):
        try:
            self.hobj.add('bad_default',
                                Float('Bad Wolf'))
        except ValueError, err:
            self.assertEqual(str(err), 
                "Default value should be a float.")
        else:
            self.fail('ValueError expected')

    def test_default_value(self):
        try:
            self.hobj.add('out_of_bounds',
                                Float(5.0, low=3, high=4))
        except ValueError, err:
            self.assertEqual(str(err), 
                "Default value is outside of bounds [3.0, 4.0].")
        else:
            self.fail('ValueError expected')


if __name__ == "__main__":
    unittest.main()

