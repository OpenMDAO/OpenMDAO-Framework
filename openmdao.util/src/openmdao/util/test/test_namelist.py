"""
Testing the namelist writer.
"""

import os.path
import sys
import unittest

from numpy import float32 as numpy_float32
from numpy import zeros

from openmdao.lib.datatypes.api import Float, Bool, Int, Str, Array, File
from openmdao.main.api import Container, Component
from openmdao.util.namelist_util import Namelist

class Var_Container(Container):
    """Contains some vars"""

    bool_var = Bool(True, iotype='in')
    int_var = Int(7777, iotype='in')
    float_var = Float(2.14543, iotype='in')
    text_var = Str("Hey", iotype='in')

class Var_Component(Component):
    """Contains some vars"""

    bool_var = Bool(False, iotype='in')
    int_var = Int(333, iotype='in')
    float_var = Float(-16.54, iotype='in')
    text_var = Str("This", iotype='in')
    array_var = Array(iotype='in')
    
    def __init__(self, directory=''):
        
        super(Var_Component, self).__init__(directory)

        # Variable Containers
        self.add('var_container',  Var_Container())

    
    
class TestCase(unittest.TestCase):
    """ Test namelist writer functions. """

    def setUp(self):
        self.filename = 'test_namelist.dat'

    def tearDown(self):
        if os.path.exists(self.filename):
            os.remove(self.filename)

    def test_writes(self):

        my_comp = Var_Component()
        sb = Namelist(my_comp)
        
        sb.set_filename(self.filename)
        sb.set_title("Testing")

        sb.add_group('&OPTION')
        sb.add_comment("This is a comment")
        sb.add_var("bool_var")
        sb.add_var("int_var")
        sb.add_var("float_var")
        sb.add_var("text_var")
        
        sb.add_newvar("new_card", "new value")
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        compare = "Testing\n" + \
                  "&OPTION\n" + \
                  "  This is a comment\n" + \
                  "  bool_var = False\n" + \
                  "  int_var = 333\n" + \
                  "  float_var = -16.54\n" + \
                  "  text_var = 'This'\n" + \
                  "  new_card = 'new value'\n" + \
                  "/\n"

        self.assertEqual(contents, compare)
        
    def test_container_write(self):
        
        my_comp = Var_Component()
        sb = Namelist(my_comp)
        
        sb.set_filename(self.filename)
        sb.add_group('&Test')
        sb.add_container("var_container")
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        self.assertEqual("bool_var = True" in contents, True)
        self.assertEqual("text_var = 'Hey'" in contents, True)
        self.assertEqual("float_var = 2.14543" in contents, True)
        self.assertEqual("int_var = 7777" in contents, True)

        
    def test_1Darray_write(self):
        
        my_comp = Var_Component()
        sb = Namelist(my_comp)
        
        my_comp.array_var = zeros(3, dtype=numpy_float32)
        my_comp.array_var[2] = 3.7
        
        sb.set_filename(self.filename)
        sb.add_group('&Test')
        sb.add_var("array_var")
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        compare = "\n" + \
                  "&Test\n" + \
                  "  array_var = 0.0, 0.0, 3.7\n" + \
                  "/\n"

        self.assertEqual(contents, compare)

    def test_2Darray_write(self):
        
        my_comp = Var_Component()
        sb = Namelist(my_comp)
        
        my_comp.array_var = zeros([3,2], dtype=numpy_float32)
        my_comp.array_var[0,1] = 3.7
        my_comp.array_var[2,0] = 7.88
        
        sb.set_filename(self.filename)
        sb.add_group('&Test')
        sb.add_var("array_var")
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        compare = "\n" + \
                  "&Test\n" + \
                  "  array_var(1,1) = 0.0,  3.7, \n" + \
                  "array_var(1,2) = 0.0,  0.0, \n" + \
                  "array_var(1,3) = 7.88,  0.0, \n" + \
                  "/\n"

        self.assertEqual(contents, compare)

    def test_unsupported_array(self):
        
        my_comp = Var_Component()
        sb = Namelist(my_comp)
        
        my_comp.array_var = zeros([2,2,2], dtype=numpy_float32)
        
        sb.set_filename(self.filename)
        sb.add_group('&Test')
        sb.add_var("array_var")
        
        try:
            sb.generate()
        except RuntimeError, err:
            self.assertEqual(str(err),
                             "Don't know how to handle array of" + \
                                           "3 dimensions")
        else:
            self.fail('RuntimeError expected')        

    def test_unsupported_traits(self):
        
        my_comp = Var_Component()
        my_comp.add_trait('unsupported', File(iotype='in'))
        sb = Namelist(my_comp)
        
        sb.set_filename(self.filename)
        sb.add_group('&Test')
        sb.add_var("unsupported")
        
        try:
            sb.generate()
        except RuntimeError, err:
            self.assertEqual(str(err),
                             "Error generating input file. Don't" + \
                             "know how to handle data in variable" + \
                             "unsupported in group &Test.")
        else:
            self.fail('RuntimeError expected')        

if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

