import unittest

from openmdao.main.exceptions import ConstraintError
from openmdao.main.hierarchy import HierarchyMember
from openmdao.main.variable import Variable, Float
from openmdao.main.interfaces import IVariable

class FloatTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = HierarchyMember('h1')
        self.hobj.internal_float1 = 0.
        self.hobj.internal_float2 = 0.
        self.float1 = Float('float1', default=3.1415926, 
                         ref_name='internal_float1',parent=self.hobj,
                         min_limit=0., max_limit=99.)
        self.float2 = Float('float2', default=42., 
                         ref_name='internal_float2',parent=self.hobj)
        
    def tearDown(self):
        """this teardown function will be called after each test in this class"""
        self.float1 = None
        self.float2 = None

    def test_assignment(self):
        self.assertEqual(3.1415926,self.float1.value)
        self.float1.value = self.float2
        self.assertEqual(42.,self.float1.value)
        self.float1.value = 32.1
        self.assertEqual(32.1,self.float1.value)
        
    def test_constraint_violations(self):
        try:
            self.float1.value = 124
        except ConstraintError, err:
            self.assertEqual(str(err), 'h1.float1 max_limit violated: 124 > 99.0')
        else:
            self.fail('expected exception for max_limit violation did not happen')
        try:
            self.float1.value = -3
        except ConstraintError, err:
            self.assertEqual(str(err), 'h1.float1 min_limit violated: -3 < 0.0')
        else:
            self.fail('expected exception for min_limit violation did not happen')

    def test_connection(self):
        self.float1.validate_connection(self.float2)
        self.float1.units = 'lb/ft^2'
        self.float2.units = 'kg'
        try:
            self.float1.validate_connection(self.float2)
        except TypeError, err:
            self.assertEqual(str(err),'h1.float2 units (kg) are incompatible'+
                             ' with units (lb/ft^2) of h1.float1')

if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

