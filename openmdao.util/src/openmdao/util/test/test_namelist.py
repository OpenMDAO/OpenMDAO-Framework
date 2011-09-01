"""
Testing the namelist writer.
"""

import os.path
import sys
import unittest

from numpy import float32 as numpy_float32
from numpy import int32 as numpy_int32
from numpy import array, zeros

from openmdao.lib.datatypes.api import Float, Bool, Int, Str, Array, File, List, Enum

from openmdao.main.api import Container, Component
from openmdao.util.namelist_util import Namelist, ToBool

class VarContainer(Container):
    """Contains some vars"""

    boolvar = Bool(True, iotype='in')
    intvar = Int(7777, iotype='in')
    floatvar = Float(2.14543, iotype='in')
    textvar = Str("Hey", iotype='in')
    listenumvar = List(Enum(1,(1,2,3)), iotype='in')


class VarComponent(Component):
    """Contains some vars"""

    boolvar = Bool(False, iotype='in')
    intvar = Int(333, iotype='in')
    floatvar = Float(-16.54, iotype='in')
    expvar1 = Float(1.2, iotype='in')
    expvar2 = Float(1.2, iotype='in')
    textvar = Str("This", iotype='in')
    arrayvar = Array(iotype='in')
    arrayvarsplit = Array(iotype='in')
    arrayvarsplit2 = Array(iotype='in')
    arrayvarzerod = Array(zeros(shape=(0,0)), iotype='in')
    arrayvartwod = Array(zeros(shape=(1,3)), iotype='in')
    arraysmall = Array(iotype='in')
    arrayshorthand = Array(iotype='in')
    single = Array(iotype='in')
    singleint = Array(iotype='in', dtype=numpy_int32)
    singlebool = Array(iotype='in', dtype=bool)
    stringarray = List([], iotype='in')
    listenumvar = List(Enum(1,(1,2,3)), iotype='in')
    listenumvar2 = List(Enum(1.5,(1.5,2.4,3.3)), iotype='in')
    listenumvar3 = List(Enum('a',('a','b','c')), iotype='in')
    listenumvar4 = List(Enum(True,(True, False)), iotype='in')
    
    def __init__(self, directory=''):
        
        super(VarComponent, self).__init__(directory)

        # Variable Containers
        self.add('varcontainer',  VarContainer())

    
    
class TestCase(unittest.TestCase):
    """ Test namelist writer functions. """

    def setUp(self):
        self.filename = 'test_namelist.dat'

    def tearDown(self):
        if os.path.exists(self.filename):
            os.remove(self.filename)

    def test_forgot_to_read(self):
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        try:
            sb.load_model()
        except RuntimeError, err:
            msg = "Input file must be read with parse_file before " \
                  "load_model can be executed."
            self.assertEqual(str(err),msg)
        else:
            self.fail('RuntimeError expected')        

    def test_writes(self):

        my_comp = VarComponent()
        my_comp.listenumvar = [1,2,1,3]
        my_comp.listenumvar2 = [1.5,1.5]
        my_comp.listenumvar3 = ['b']
        my_comp.listenumvar4 = [False, False, False]
        sb = Namelist(my_comp)
        
        sb.set_filename(self.filename)
        sb.set_title("Testing")

        sb.add_group('FREEFORM')
        sb.add_group('OPTION')
        sb.add_comment("This is a comment")
        sb.add_var("boolvar")
        sb.add_var("intvar")
        sb.add_var("floatvar")
        sb.add_var("textvar")
        sb.add_var("listenumvar")
        sb.add_var("listenumvar2")
        sb.add_var("listenumvar3")
        sb.add_var("listenumvar4")
        
        sb.add_newvar("newcard", "new value")
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        compare = "Testing\n" + \
                  "FREEFORM\n" + \
                  "&OPTION\n" + \
                  "  This is a comment\n" + \
                  "  boolvar = F\n" + \
                  "  intvar = 333\n" + \
                  "  floatvar = -16.54\n" + \
                  "  textvar = 'This'\n" + \
                  "  listenumvar = 1, 2, 1, 3\n" + \
                  "  listenumvar2 = 1.5, 1.5\n" + \
                  "  listenumvar3 = 'b'\n" + \
                  "  listenumvar4 = F, F, F\n" + \
                  "  newcard = 'new value'\n" + \
                  "/\n"

        self.assertEqual(contents, compare)
        
    def test_read1(self):
        # Put variables in top container, so no rules_dict

        namelist1 = "Testing\n" + \
                    "  \n" + \
                    "&OPTION\n" + \
                    "  This is a comment\n" + \
                    "  INTVAR = 777, single(1) = 15.0, floatvar = -3.14\n" + \
                    "  singleint(2) = 3,4,5\n" + \
                    "  stringarray(3) = 'xyz'\n" + \
                    "  boolvar = T\n" + \
                    "  textvar = 'That'\n" + \
                    "  ! This is a comment too\n" + \
                    "  listenumvar = 3,3,2,2\n" + \
                    "  listenumvar2 = 1.5\n" + \
                    "  arrayvar = 3.5, 7.76, 1.23\n" + \
                    "  arrayvarsplit = 3.5, 7.76\n" + \
                    "                  5.45, 22.0\n" + \
                    "                  1.23\n" + \
                    "  arrayvarsplit2 = 1\n" + \
                    "                   2\n" + \
                    "                   3\n" + \
                    "  arraysmall = 1.75\n" + \
                    "  arrayshorthand = 3.456*8\n" + \
                    "  expvar1 = 1.5e-12\n" + \
                    "  expvar2 = -1.5D12\n" + \
                    "/\n"

        outfile = open(self.filename, 'w')
        outfile.write(namelist1)
        outfile.close()
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        sb.set_filename(self.filename)

        sb.parse_file()
        
        sb.load_model()
        
        self.assertEqual(sb.title, 'Testing')
        self.assertEqual(my_comp.intvar, 777)
        self.assertEqual(my_comp.boolvar, True)
        self.assertEqual(my_comp.floatvar, -3.14)
        self.assertEqual(my_comp.expvar1, 1.5e-12)
        self.assertEqual(my_comp.expvar2, -1.5e12)
        self.assertEqual(my_comp.textvar, 'That')
        self.assertEqual(my_comp.listenumvar, [3,3,2,2])
        self.assertEqual(my_comp.listenumvar2, [1.5])
        self.assertEqual(my_comp.arrayvar[0], 3.5)
        self.assertEqual(my_comp.arrayvar[1], 7.76)
        self.assertEqual(my_comp.arrayvar[2], 1.23)
        self.assertEqual(my_comp.arrayvarsplit[0], 3.5)
        self.assertEqual(my_comp.arrayvarsplit[1], 7.76)
        self.assertEqual(my_comp.arrayvarsplit[2], 5.45)
        self.assertEqual(my_comp.arrayvarsplit[3], 22.0)
        self.assertEqual(my_comp.arrayvarsplit[4], 1.23)
        self.assertEqual(my_comp.arrayvarsplit2[0], 1)
        self.assertEqual(my_comp.arrayvarsplit2[1], 2)
        self.assertEqual(my_comp.arrayvarsplit2[2], 3)
        self.assertEqual(my_comp.arraysmall[0], 1.75)
        self.assertEqual(len(my_comp.arraysmall), 1)
        self.assertEqual(my_comp.arrayshorthand[4], 3.456)
        self.assertEqual(len(my_comp.arrayshorthand), 8)
        self.assertEqual(my_comp.single[0], 15.0)
        self.assertEqual(my_comp.singleint[3], 5)
        self.assertEqual(my_comp.stringarray[2], 'xyz')
        self.assertEqual(type(my_comp.singleint[2]), numpy_int32)
        
    def test_read2(self):
        # Put variables in container, using rules_dict

        namelist1 = "Testing\n" + \
                    "  \n" + \
                    "&OPTION\n" + \
                    "  This is a comment\n" + \
                    "  intvar = 777\n" + \
                    "  boolvar = .FALSE.\n" + \
                    "  floatvar = -3.14\n" + \
                    "  extravar = 555\n" + \
                    "  TEXTVAR = 'That'\n" + \
                    "  ! This is a comment too\n" + \
                    "  listenumvar = 3,3,2,2\n" + \
                    "/\n" + \
                    "&NODICT\n" + \
                    "  noval = 0\n" + \
                    "/\n" + \
                    "&DUPE\n" + \
                    "  some = 0\n" + \
                    "/\n" + \
                    "&DUPE\n" + \
                    "  some = 0\n" + \
                    "/\n" + \
                    "FREEFORM\n"

        outfile = open(self.filename, 'w')
        outfile.write(namelist1)
        outfile.close()
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        sb.set_filename(self.filename)

        sb.parse_file()
        
        rules_dict = { "OPTION" : ["varcontainer"] }
        n1, n2, n3 = sb.load_model(rules_dict)
        
        self.assertEqual(n1[4], 'FREEFORM')
        self.assertEqual(n2[1], 'NODICT')
        self.assertEqual(n2[2], 'DUPE')
        self.assertEqual(n2[3], 'DUPE')
        self.assertEqual(n3, ['extravar'])
        self.assertEqual(my_comp.varcontainer.intvar, 777)
        self.assertEqual(my_comp.varcontainer.boolvar, False)
        self.assertEqual(my_comp.varcontainer.floatvar, -3.14)
        self.assertEqual(my_comp.varcontainer.textvar, 'That')
        self.assertEqual(my_comp.varcontainer.listenumvar, [3,3,2,2])
        
    def test_read3(self):
        # Parse a single group in a deck with non-unique group names

        namelist1 = "Testing\n" + \
                    "$GROUP\n" + \
                    "  intvar = 99\n" + \
                    "$END\n" + \
                    "$GROUP\n" + \
                    "  floatvar = 3.5e-23\n" + \
                    "$END\n"
        
        outfile = open(self.filename, 'w')
        outfile.write(namelist1)
        outfile.close()
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        sb.set_filename(self.filename)

        sb.parse_file()
        
        sb.load_model(single_group=1)

        # Unchanged
        self.assertEqual(my_comp.intvar, 333)
        # Changed
        self.assertEqual(my_comp.floatvar, 3.5e-23)
        
    def test_read4(self):
        # Variables on same line as header

        namelist1 = "Testing\n" + \
                    "  \n" + \
                    "&OPTION boolvar = T, arrayvar = 3.5, 7.76, 1.23\n" + \
                    "/\n"

        outfile = open(self.filename, 'w')
        outfile.write(namelist1)
        outfile.close()
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        sb.set_filename(self.filename)

        sb.parse_file()
        
        sb.load_model()
        
        self.assertEqual(my_comp.boolvar, True)
        self.assertEqual(my_comp.arrayvar[0], 3.5)

        namelist1 = "Testing\n" + \
                    "  \n" + \
                    "$OPTION boolvar = T, arrayvar = 3.5, 7.76, 1.23, $END\n"

        outfile = open(self.filename, 'w')
        outfile.write(namelist1)
        outfile.close()
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        sb.set_filename(self.filename)

        sb.parse_file()
        
        sb.load_model()
        
        self.assertEqual(my_comp.boolvar, True)
        self.assertEqual(my_comp.arrayvar[0], 3.5)
        
    def test_2Darray_read(self):
        
        namelist1 = "Testing\n" + \
                    "$GROUP\n" + \
                    "  arrayvartwod(1,1) = 12, 24, 36\n" + \
                    "  arrayvartwod(1,2) = 33, 66, 99\n" + \
                    "$END\n"
        
        outfile = open(self.filename, 'w')
        outfile.write(namelist1)
        outfile.close()
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        sb.set_filename(self.filename)

        sb.parse_file()
        
        sb.load_model()

        # Unchanged
        self.assertEqual(my_comp.arrayvartwod[0][0], 12)
        self.assertEqual(my_comp.arrayvartwod[1][2], 99)

    def test_container_write(self):
        
        my_comp = VarComponent()
        my_comp.varcontainer.listenumvar = [1,2,1,3]
        sb = Namelist(my_comp)
        
        sb.set_filename(self.filename)
        sb.add_group('Test')
        sb.add_container("varcontainer")
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        self.assertEqual("boolvar = T" in contents, True)
        self.assertEqual("textvar = 'Hey'" in contents, True)
        self.assertEqual("floatvar = 2.14543" in contents, True)
        self.assertEqual("intvar = 7777" in contents, True)
        self.assertEqual("listenumvar = 1, 2, 1, 3" in contents, True)

        # now test skipping
        
        sb = Namelist(my_comp)
        my_comp.varcontainer.boolvar=True
        my_comp.varcontainer.textvar="Skipme"
        
        sb.set_filename(self.filename)
        sb.add_group('Test')
        sb.add_container("varcontainer", skip='textvar')
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        self.assertEqual("boolvar = T" in contents, True)
        self.assertEqual("textvar = 'Skipme'" in contents, False)
        
        
    def test_1Darray_write(self):
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        
        my_comp.arrayvar = zeros(3, dtype=numpy_float32)
        my_comp.arrayvar[2] = 3.7
        my_comp.single = array(['a', 'b', 'c'])
        my_comp.singleint = array([1, 2, 3])
        my_comp.singlebool = array([False, True, False])
        
        sb.set_filename(self.filename)
        sb.add_group('Test')
        sb.add_var("arrayvar")
        # This should be ignored because it is zero-D
        sb.add_var("arrayvarzerod")
        sb.add_var("single")
        sb.add_var("singleint")
        sb.add_var("singlebool")
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        compare = "\n" + \
                  "&Test\n" + \
                  "  arrayvar = 0.0, 0.0, 3.700000047683716\n" + \
                  "  single = 'a', 'b', 'c'\n" + \
                  "  singleint = 1, 2, 3\n" + \
                  "  singlebool = F, T, F\n" + \
                  "/\n"

        self.assertEqual(contents, compare)

    def test_2Darray_write(self):
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        
        my_comp.arrayvar = zeros([3,2], dtype=numpy_float32)
        my_comp.arrayvar[0,1] = 3.7
        my_comp.arrayvar[2,0] = 7.88
        
        sb.set_filename(self.filename)
        sb.add_group('Test')
        sb.add_var("arrayvar")
        
        sb.generate()
        
        f = open(self.filename, 'r')
        contents = f.read()
        
        compare = "\n" + \
                  "&Test\n" + \
                  "  arrayvar(1,1) = 0.0,  3.700000047683716, \n" + \
                  "arrayvar(1,2) = 0.0,  0.0, \n" + \
                  "arrayvar(1,3) = 7.880000114440918,  0.0, \n" + \
                  "/\n"

        self.assertEqual(contents, compare)

    def test_unsupported_array(self):
        
        my_comp = VarComponent()
        sb = Namelist(my_comp)
        
        my_comp.arrayvar = zeros([2,2,2], dtype=numpy_float32)
        
        sb.set_filename(self.filename)
        sb.add_group('Test')
        sb.add_var("arrayvar")
        
        try:
            sb.generate()
        except RuntimeError, err:
            self.assertEqual(str(err),
                             "Don't know how to handle array of" + \
                                           " 3 dimensions")
        else:
            self.fail('RuntimeError expected')        

    def test_unsupported_traits(self):
        
        my_comp = VarComponent()
        my_comp.add('unsupported', File(iotype='in'))
        sb = Namelist(my_comp)
        
        sb.set_filename(self.filename)
        sb.add_group('Test')
        sb.add_var("unsupported")
        
        try:
            sb.generate()
        except RuntimeError, err:
            self.assertEqual(str(err),
                             "Error generating input file. Don't" + \
                             "know how to handle data in variable" + \
                             "unsupported in group Test.")
        else:
            self.fail('RuntimeError expected')        

    def test_bool_token_error(self):
            
        try:
            token = ToBool('Junk')
            token.postParse(0, 0, ["Junk"])
        except RuntimeError, err:
            msg = "Unexpected error while trying to identify a Boolean value in the namelist."
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')        
            
if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

