# pylint: disable-msg=C0111,C0103

import unittest

import numpy

from openmdao.main.container import Container
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.arrayvar import ArrayVariable
from openmdao.main.float import Float

class ArrayVarTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container('h1', None)
        self.hobj.internal_arr1 = numpy.array([1.2, 3.4, 5.6])
        self.hobj.internal_arr2 = numpy.array([[1.2, 3.4, 5.6],
                                               [7.8,9.10,11.12]])
        self.array1 = ArrayVariable('array1', self.hobj, INPUT, float,
                               ref_name='internal_arr1')
        self.array1out = ArrayVariable('array1out', self.hobj, OUTPUT, float,
                               ref_name='internal_arr1')
        self.array2 = ArrayVariable('array2', self.hobj, INPUT, float, 
                                    fixed_size=(2,3),
                                    ref_name='internal_arr2')
        
    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None
        self.array1 = None
        self.array1out = None
        self.array2 = None

    def test_assignment(self):
        assert(numpy.all(numpy.array([1.2,3.4,5.6])==self.array1.value))
        self.hobj.set('array1', numpy.array([1.,2.,3.,4.,5.]))
        assert(numpy.all(numpy.array([1.,2.,3.,4.,5.])==self.array1.value))
        
    def test_bad_assignment(self):
        self.hobj.float1 = 2.1
        var = Float('var', self.hobj, OUTPUT, ref_name='float1')
        try:
            self.hobj.setvar('array1', var)
        except TypeError,err:
            self.assertEqual(str(err), 
                    "h1.array1: assignment to incompatible variable "+
                    "'h1.var' of type '<class 'openmdao.main.float.Float'>'")
        else:
            self.fail('TypeError expected')
        try:    
            self.hobj.set('array1', 'foobar')
        except ValueError,err:
            self.assertEqual(str(err), 
                "h1.array1: new type 'str'is not compatible with type 'ndarray")
        else:
            self.fail('ValueError expected')
               
    def test_assign_size_violation(self):
        try:
            self.hobj.setvar('array2', self.hobj.getvar('array1out'))
        except ValueError, err:
            self.assertEqual(str(err), 
                    'h1.array2: expected array of size (2, 3) but got (1, 3)')
        else:
            self.fail('ValueError expected')
                    
    def test_connection(self):
        self.hobj.newarray = numpy.array([[12.,34.,56.],[78.,910.,1112.]])
        newarray = ArrayVariable('newarray',self.hobj, OUTPUT, float)
        self.hobj.setvar('array2',newarray)

if __name__ == "__main__":
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(ContainerTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)    

