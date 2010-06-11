# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import TraitError

from numpy import array

from openmdao.main.exceptions import ConstraintError
from openmdao.main.api import Component
from openmdao.lib.api import Array
from openmdao.units import convert_units

class ArrayTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Component()
        self.hobj.add_trait('arr1',Array(array([98.9]), iotype='in', units='ft'))
        self.hobj.add_trait('arr2', Array(array([13.2]), iotype='out', units='inch'))
        self.hobj.add_trait('arr3', Array(iotype='in', units='kg'))
        
        self.hobj.arr1 = [3.1415926]
        self.hobj.arr2 = [42.]
        self.hobj.arr3 = [1.1]
                       
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_set_to_default(self):
        self.assertEqual(3.1415926, self.hobj.arr1[0])
        self.hobj.add_trait('arr4', Array(iotype='in', units='kg'))
        self.assertTrue(all(array([]) == self.hobj.arr4))
        self.hobj.arr4 = [6.5]
        self.assertEqual(6.5, self.hobj.arr4[0])
        
        self.hobj.revert_to_defaults()
        
        self.assertEqual([98.9], self.hobj.arr1[0])
        self.assertTrue(all(array([]) == self.hobj.arr4))

    def test_assignment(self):
        # check starting value
        self.assertEqual(3.1415926, self.hobj.arr1[0])
        # check default value
        self.assertEqual([98.9], self.hobj.trait('arr1').trait_type.default_value)
        
        # use unit_convert to perform unit conversion
        self.hobj.arr1 = [3.]
        self.hobj.arr2 = convert_units(self.hobj.arr1, self.hobj.trait('arr1').units,
                                         'inch')
        self.assertAlmostEqual(36., self.hobj.arr2[0], 5)

    def test_unit_conversion(self):
        self.hobj.arr2 = [12.]  # inches
        self.hobj.arr1 = convert_units(self.hobj.arr2, self.hobj.trait('arr2').units,
                                         'ft')
        self.assertEqual(self.hobj.arr1[0], 1.) # 12 inches = 1 ft
        
    def test_bogus_units(self):
        try:
            uf = Array([0.], iotype='in', units='bogus')
        except TraitError, err:
            self.assertEqual(str(err), 
                             "Units of 'bogus' are invalid")
        else:
            self.fail('ValueError expected')
        
    def test_get(self):
        self.assertEqual(self.hobj.arr1, 3.1415926)
        self.assertEqual(self.hobj.get('arr1'), 3.1415926)
        
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
        
        self.hobj.add_trait('arr_nodefault3',
                            Array(iotype='in', units='kg'))
        self.assertTrue(all(array([]) == self.hobj.arr_nodefault3))
            
        self.hobj.add_trait('arr_nounits', Array(iotype='in'))
        if hasattr(self.hobj.arr_nounits, 'units'):
            self.fail("Unitless Array should not have units")

    def test_default_value_type(self):
        try:
            self.hobj.add_trait('bad_default', Array('bad'))
        except TraitError, err:
            self.assertEqual(str(err), "Default value should be a numpy array, not a <type 'str'>.")
        else:
            self.fail('TraitError expected')

if __name__ == "__main__":
    unittest.main()

