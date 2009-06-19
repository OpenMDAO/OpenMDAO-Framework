"""
Test for Bool variables.
"""

import unittest

from enthought.traits.api import Bool, Int

from openmdao.main.api import Container, Bool, Int

class BoolTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container('h1', None)
        self.hobj.internal_bool1 = True
        self.hobj.internal_bool2 = False
        self.hobj.internal_int = 42
        self.bool1 = Bool('bool1', self.hobj, iostatus='in', 
                          ref_name='internal_bool1', default=True)
        self.bool2 = Bool('bool2', self.hobj, OUTPUT,
                          ref_name='internal_bool2', default=False)
        self.integer = Int('integer', self.hobj, OUTPUT,
                           ref_name='internal_int', default=42)

    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_assignment(self):
        # check starting value
        self.assertEqual(True, self.bool1.get_value())
        self.assertEqual(False, self.bool2.get_value())

        # check default value
        self.assertEqual(True, self.bool1.default)
        self.assertEqual(False, self.bool2.default)

        # assignment
        self.bool1.set_value(self.bool2.get_value())
        self.assertEqual(False, self.bool1.get_value())

        # make sure value gets transferred to internal variable
        self.assertEqual(False, self.hobj.internal_bool1)
        self.bool1.set_value(True)
        self.assertEqual(True, self.bool1.get_value())
        self.assertEqual(True, self.hobj.internal_bool1)

    def test_get(self):
        val = self.bool1.get(None)
        self.assertEqual(val, True)
        val = self.bool1.get('value')
        self.assertEqual(val, True)

    def test_set_attribute(self):
        self.bool1.set('doc', 'xyzzy')
        self.assertEqual(self.bool1.doc, 'xyzzy')
        try:
            self.bool1.set('doc', 'froboz', [2])
        except ValueError, err:
            self.assertEqual(str(err), 
                'h1.bool1: array indexing of Variable attributes not supported')
        else:
            self.fail('ValueError expected')

    def test_array_assign(self):
        try:
            self.bool1.set(None, False, [3])
        except NotImplementedError, err:
            self.assertEqual(str(err), 'h1.bool1: _pre_assign_entry')
        else:
            self.fail('NotImplementedError expected')

    def test_bad_connection(self):
        self.bool1.validate_var(self.bool2)
        try:
            self.bool1.validate_var(self.integer)
        except TypeError, err:
            self.assertEqual(str(err),
                             "h1.bool1: assignment to incompatible variable"
                             " 'h1.integer' of type"
                             " '<class 'openmdao.main.int.Int'>'")
        else:
            self.fail('TypeError expected')


if __name__ == '__main__':
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(BoolTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

