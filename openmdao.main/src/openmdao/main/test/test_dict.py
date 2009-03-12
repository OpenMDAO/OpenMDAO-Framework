"""
Test for Dict variables.
"""

import unittest

from openmdao.main import Container, Dict, Int
from openmdao.main.variable import INPUT, OUTPUT

class DictTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container('h1', None)
        self.hobj.internal_dict1 = {'item1':1, 'item2':2}
        self.hobj.internal_dict2 = {'key1':'a', 'key2':'b'}
        self.hobj.internal_int = 42
        self.dict1 = Dict('dict1', self.hobj, INPUT, 
                          ref_name='internal_dict1', default={})
        self.dict2 = Dict('dict2', self.hobj, OUTPUT,
                          ref_name='internal_dict2', default={})
        self.integer = Int('integer', self.hobj, OUTPUT,
                           ref_name='internal_int', default=42)

    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_assignment(self):
        # check starting value
        self.assertEqual(self.dict1.value, {'item1':1, 'item2':2})
        self.assertEqual(self.dict2.value, {'key1':'a', 'key2':'b'})

        # check default value
        self.assertEqual(self.dict1.default, {})
        self.assertEqual(self.dict2.default, {})

        # assignment
        self.dict1.value = self.dict2.value
        self.assertEqual(self.dict1.value, {'key1':'a', 'key2':'b'})

        # make sure value gets transferred to internal variable
        self.assertEqual(self.hobj.internal_dict1, {'key1':'a', 'key2':'b'})
        self.dict1.value = {'item1':1, 'item2':2}
        self.assertEqual(self.dict1.value, {'item1':1, 'item2':2})
        self.assertEqual(self.hobj.internal_dict1, {'item1':1, 'item2':2})

    def test_get(self):
        val = self.dict1.get(None)
        self.assertEqual(val, {'item1':1, 'item2':2})
        val = self.dict1.get('value')
        self.assertEqual(val, {'item1':1, 'item2':2})

    def test_set_attribute(self):
        self.dict1.set('doc', 'xyzzy')
        self.assertEqual(self.dict1.doc, 'xyzzy')
        try:
            self.dict1.set('doc', 'froboz', [2])
        except ValueError, err:
            self.assertEqual(str(err), 
                'h1.dict1: array indexing of Variable attributes not supported')
        else:
            self.fail('ValueError expected')

    def test_array_assign(self):
        try:
            self.dict1.set(None, False, [3])
        except NotImplementedError, err:
            self.assertEqual(str(err), 'h1.dict1: _pre_assign_entry')
        else:
            self.fail('NotImplementedError expected')

    def test_bad_connection(self):
        self.dict1.validate_var(self.dict2)
        try:
            self.dict1.validate_var(self.integer)
        except TypeError, err:
            self.assertEqual(str(err),
                             "h1.dict1: assignment to incompatible variable"
                             " 'h1.integer' of type"
                             " '<class 'openmdao.main.int.Int'>'")
        else:
            self.fail('TypeError expected')

if __name__ == '__main__':
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(DictTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

