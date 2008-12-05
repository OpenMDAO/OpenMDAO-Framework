import unittest

from openmdao.main.exceptions import ConstraintError
from openmdao.main.container import Container
from openmdao.main.variable import Variable, INPUT, OUTPUT
from openmdao.main.float import Float
from openmdao.main.interfaces import IVariable

class FloatTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container('h1', None)
        self.hobj.internal_float1 = 3.1415926
        self.hobj.internal_float2 = 42.
        self.hobj.internal_float3 = 1.1
        self.float1 = Float('float1', self.hobj, INPUT, 
                       ref_name='internal_float1', default=98.9,
                       min_limit=0., max_limit=99.)
        self.float2 = Float('float2', self.hobj, OUTPUT, default=13.2, 
                       ref_name='internal_float2')
        self.float3 = Float('float3', self.hobj, INPUT, 
                       ref_name='internal_float3',
                       min_limit=0., max_limit=99.)
        
    def tearDown(self):
        """this teardown function will be called after each test in this class"""
        self.hobj = None

    def test_assignment(self):
        # check starting value
        self.assertEqual(3.1415926,self.float1.value)
        # check default value
        self.assertEqual(98.9,self.float1.default)
        self.float1.value = self.float2.value
        self.assertEqual(42.,self.float1.value)
        # make sure value gets transferred to internal variable
        self.assertEqual(42.,self.hobj.internal_float1)
        self.float1.value = 32.1
        self.assertEqual(32.1,self.float1.value)
        self.assertEqual(32.1,self.hobj.internal_float1)
        
    def test_constraint_violations(self):
        try:
            self.float1.value = 124
        except ConstraintError, err:
            self.assertEqual(str(err), "h1.float1: constraint '124 <= 99.0' has been violated")
        else:
            self.fail('expected exception for max_limit violation did not happen')
        try:
            self.float1.value = -3
        except ConstraintError, err:
            self.assertEqual(str(err), "h1.float1: constraint '-3 >= 0.0' has been violated")
        else:
            self.fail('ConstraintError exception')

    def test_connection(self):
        self.float1.validate_var(self.float2)
        self.float1.units = 'lb/ft^2'
        self.float2.units = 'kg'
        try:
            self.float1.validate_var(self.float2)
        except TypeError, err:
            self.assertEqual(str(err),'h1.float2 units (kg) are incompatible'+
                             ' with units (lb/ft^2) of h1.float1')
        else:
            self.fail('TypeError expected')

if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

