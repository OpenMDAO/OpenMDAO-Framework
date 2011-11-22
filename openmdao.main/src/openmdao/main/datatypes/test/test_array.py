# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.numpy_fallback import array

from openmdao.main.api import Component
from openmdao.main.datatypes.array import Array
from openmdao.units import convert_units

class ArrayTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Component()
        self.hobj.add('arr1',Array(array([98.9]), iotype='in', units='ft'))
        self.hobj.add('arr2', Array(array([13.2]), iotype='out', units='inch'))
        self.hobj.add('arr3', Array(iotype='in', units='kg', desc='stuff'))
        
        self.hobj.arr1 = [1.0, 2.0, 3.0]
        self.hobj.arr2 = [[1.,2.],[3.,4.]]
        self.hobj.arr3 = [1.1]
                       
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_set_to_default(self):
        self.hobj.add('arr4', Array(iotype='in', units='kg'))
        self.assertTrue(all(array([]) == self.hobj.arr4))
        self.hobj.arr4 = [6.5]
        self.assertEqual(6.5, self.hobj.arr4[0])
        
        self.hobj.revert_to_defaults()
        
        self.assertTrue(all(array([98.9]) == self.hobj.arr1))
        self.assertTrue(all(array([]) == self.hobj.arr4))

    def test_assignment(self):
        # check starting value
        self.assertTrue(all(array([1.,2.,3.]) == self.hobj.arr1))
        # check default value
        self.assertEqual([98.9], self.hobj.get_trait('arr1').trait_type.default_value)
        
        # use convert_units to perform unit conversion
        self.hobj.arr2 = convert_units(self.hobj.arr1, self.hobj.get_trait('arr1').units,
                                         'inch')
        self.assertAlmostEqual(12., self.hobj.arr2[0], 5)
        self.assertAlmostEqual(24., self.hobj.arr2[1], 5)
        self.assertAlmostEqual(36., self.hobj.arr2[2], 5)

    def test_unit_conversion(self):
        self.hobj.arr1 = [1.,2.,3.]
        self.hobj.arr2 = self.hobj.get_wrapped_attr('arr1')
        self.assertAlmostEqual(12., self.hobj.arr2[0])
        self.assertAlmostEqual(24., self.hobj.arr2[1])
        self.assertAlmostEqual(36., self.hobj.arr2[2])
        
        # unit to unitless
        self.hobj.add('arr5', Array(iotype='in'))
        self.hobj.arr5 = [1., 2., 4.]
        self.hobj.arr2 = self.hobj.get_wrapped_attr('arr5')
        self.assertAlmostEqual(1., self.hobj.arr2[0])
        self.assertAlmostEqual(2., self.hobj.arr2[1])
        self.assertAlmostEqual(4., self.hobj.arr2[2])
        
    def test_bogus_units(self):
        try:
            uf = Array([0.], iotype='in', units='bogus')
        except ValueError, err:
            self.assertEqual(str(err), 
                             "Units of 'bogus' are invalid")
        else:
            self.fail('ValueError expected')
        
    def test_get(self):
        self.assertTrue(all(self.hobj.get('arr1') == array([1.,2.,3.])))
        
    def test_array_assign(self):
        self.hobj.arr1 = [1.,2,3,4,5]
        self.hobj.arr1[3] = 1.3
        self.assertEqual(self.hobj.arr1[3], 1.3)
        
    def test_intvalues(self):
        f1 = Array([3.])
        d1 = f1.default_value/2
        self.assertAlmostEqual(d1[0], 1.5, places=4)
        
    def test_bad_connection(self):
        srcwrapper = self.hobj.get_wrapped_attr('arr2')
        self.hobj.arr1 = srcwrapper
        try:
            self.hobj.arr3 = srcwrapper
        except Exception, err:
            self.assertEqual(str(err), 
                "arr3: units 'inch' are incompatible with assigning units of 'kg'")
        else:
            self.fail('Exception expected')

    def test_constructor_defaults(self):
        
        self.hobj.add('arr_nodefault3',
                            Array(iotype='in', units='kg'))
        self.assertTrue(all(array([]) == self.hobj.arr_nodefault3))
            
        self.hobj.add('arr_nounits', Array(iotype='in'))
        if hasattr(self.hobj.arr_nounits, 'units'):
            self.fail("Unitless Array should not have units")

    def test_default_value_type(self):
        try:
            self.hobj.add('bad_default', Array('bad'))
        except TypeError, err:
            self.assertEqual(str(err), "Default value should be an array-like object, not a <type 'str'>.")
        else:
            self.fail('TypeError expected')
            
    def test_shapes(self):

        self.hobj.add('sh1', Array(array([[2.0, 4.5],[3.14, 2.5]]), iotype='in', units='kg', shape=(2,2)))
        self.assertEqual(self.hobj.sh1[1][1], 2.5)
        
        try:
            self.hobj.add('sh1', Array(array([2.0, 2.5]), iotype='in', units='kg', shape=(2,2)))
        except ValueError, err:
            msg = "Shape of the default value does not match the shape attribute."
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')
            
        self.hobj.sh1 = array([[9.0, 11.0], [1.0, 2.0]])
        self.assertEqual(self.hobj.sh1[1][1], 2.0)
        
        try:
            self.hobj.sh1 = array([[11.0, 2.0]])
        except ValueError, err:
            msg = ": Variable 'sh1' must be an array-like object of shape (2, 2), but a shape of (1, 2) (<type 'numpy.ndarray'>) was specified."
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')
            
        
if __name__ == "__main__":
    unittest.main()

