"""
Testing the file wrapping utilities.
"""

import unittest, os

from numpy import array, isnan, isinf

from openmdao.lib.datatypes.api import Float, Bool, Int, Str, Array, File, List, Enum
from openmdao.main.api import Container, Component
from openmdao.util.filewrap import InputFileGenerator, FileParser


class TestCase(unittest.TestCase):
    """ Test namelist writer functions. """

    def setUp(self):
        self.templatename = 'template.dat'
        self.filename = 'filename.dat'

    def tearDown(self):
        if os.path.exists(self.filename):
            os.remove(self.filename)
        if os.path.exists(self.templatename):
            os.remove(self.templatename)
    
    def test_templated_input(self):
        
        template = "Junk\n" + \
                   "Anchor\n" + \
                   " A 1, 2 34, Test 1e65\n" + \
                   " B 4 Stuff\n" + \
                   "Anchor\n" + \
                   " C 77 False Inf 333.444\n"
        
        outfile = open(self.templatename, 'w')
        outfile.write(template)
        outfile.close()
        
        gen = InputFileGenerator()
        gen.set_template_file(self.templatename)
        gen.set_generated_file(self.filename)
        gen.set_delimiters(', ')
        
        gen.mark_anchor('Anchor')
        gen.transfer_var('CC', 2, 0)
        gen.transfer_var(3.0, 1, 3)
        gen.reset_anchor()
        gen.mark_anchor('Anchor', 2)
        gen.transfer_var('NaN', 1, 4)
        gen.reset_anchor()
        gen.transfer_var('55', 3, 2)
        gen.mark_anchor('C 77')
        gen.transfer_var(1.3e-37, -3, 6)
        gen.clearline(-5)
        gen.mark_anchor('Anchor', -1)
        gen.transfer_var('8.7', 1, 5)
        
        gen.generate()
        
        infile = open(self.filename, 'r')
        result = infile.read()
        infile.close()
        
        answer = "\n" + \
                   "Anchor\n" + \
                   " A 1, 3.0 34, Test 1.3e-37\n" + \
                   " B 55 Stuff\n" + \
                   "Anchor\n" + \
                   " C 77 False NaN 8.7\n"
        
        self.assertEqual(answer, result)
        
        # Test some errors
        try:
            gen.mark_anchor('C 77', 3.14)
        except ValueError, err:
            msg = "The value for occurrence must be an integer"
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')        

        try:
            gen.mark_anchor('C 77', 0)
        except ValueError, err:
            msg = "0 is not valid for an anchor occurrence."
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')  
            
        try:
            gen.mark_anchor('ZZZ')
        except RuntimeError, err:
            msg = "Could not find pattern ZZZ in template file template.dat"
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')  
            
            
    def test_templated_input_same_anchors(self):
        
        template = "CQUAD4 1 3.456\n" + \
                   "CQUAD4 2 4.123\n" + \
                   "CQUAD4 3 7.222\n" + \
                   "CQUAD4 4\n"

        outfile = open(self.templatename, 'w')
        outfile.write(template)
        outfile.close()
        
        gen = InputFileGenerator()
        gen.set_template_file(self.templatename)
        gen.set_generated_file(self.filename)
        gen.set_delimiters(', ')

        gen.mark_anchor('CQUAD4')
        gen.transfer_var('x', 0, 2)
        gen.mark_anchor('CQUAD4')
        gen.transfer_var('y', 0, 3)
        gen.mark_anchor('CQUAD4', 2)
        gen.transfer_var('z', 0, 2)

        gen.generate()
        
        infile = open(self.filename, 'r')
        result = infile.read()
        infile.close()
        
        answer =   "CQUAD4 x 3.456\n" + \
                   "CQUAD4 2 y\n" + \
                   "CQUAD4 3 7.222\n" + \
                   "CQUAD4 z\n"

        self.assertEqual(answer, result)
        print result


    def test_templated_input_arrays(self):
        
        template = "Anchor\n" + \
                   "0 0 0 0 0\n"
        
        outfile = open(self.templatename, 'w')
        outfile.write(template)
        outfile.close()
        
        gen = InputFileGenerator()
        gen.set_template_file(self.templatename)
        gen.set_generated_file(self.filename)
        
        gen.mark_anchor('Anchor')
        gen.transfer_array(array([1, 2, 3, 4.75, 5.0]), 1, 3, 5, sep=' ')
        
        gen.generate()
        
        infile = open(self.filename, 'r')
        result = infile.read()
        infile.close()
        
        answer = "Anchor\n" + \
                   "0 0 1.0 2.0 3.0 4.75 5.0\n"
    
        self.assertEqual(answer, result)

    def test_templated_input_2Darrays(self):
        
        template = "Anchor\n" + \
                   "0 0 0 0 0\n" + \
                   "0 0 0 0 0\n"
        
        outfile = open(self.templatename, 'w')
        outfile.write(template)
        outfile.close()
        
        gen = InputFileGenerator()
        gen.set_template_file(self.templatename)
        gen.set_generated_file(self.filename)
        
        gen.mark_anchor('Anchor')
        var = array([[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]])
        gen.transfer_2Darray(var, 1, 2, 1, 5)
        
        gen.generate()
        
        infile = open(self.filename, 'r')
        result = infile.read()
        infile.close()
        
        answer = "Anchor\n" + \
                   "1 2 3 4 5\n" + \
                   "6 7 8 9 10\n"
    
        self.assertEqual(answer, result)

    def test_output_parse(self):
        
        data = "Junk\n" + \
                   "Anchor\n" + \
                   " A 1, 2 34, Test 1e65\n" + \
                   " B 4 Stuff\n" + \
                   "Anchor\n" + \
                   " C 77 False NaN 333.444\n" + \
                   " 1,2,3,4,5\n" + \
                   " Inf 1.#QNAN -1.#IND\n"
        
        outfile = open(self.filename, 'w')
        outfile.write(data)
        outfile.close()
        
        gen = FileParser()
        gen.set_file(self.filename)
        gen.set_delimiters(' ')
        
        gen.mark_anchor('Anchor')
        val = gen.transfer_var(1, 1)
        self.assertEqual(val, 'A')
        gen.reset_anchor()
        val = gen.transfer_var(3, 2)
        self.assertEqual(val, 4)
        self.assertEqual(type(val), int)
        gen.mark_anchor('Anchor',2)
        val = gen.transfer_var(1, 4)
        self.assertEqual(isnan(val), True)
        val = gen.transfer_var(3, 1)
        self.assertEqual(isinf(val), True)
        val = gen.transfer_var(3, 2)
        self.assertEqual(isnan(val), True)
        val = gen.transfer_var(3, 3)
        self.assertEqual(isnan(val), True)
        val = gen.transfer_line(-1)
        self.assertEqual(val, ' B 4 Stuff')

        # Now, let's try column delimiters
        gen.set_delimiters('columns')
        gen.mark_anchor('Anchor',-1)
        val = gen.transfer_var(1, 8, 10)
        self.assertEqual(val, 'als')
        val = gen.transfer_var(1, 17)
        self.assertEqual(val, 333.444)

        # Test some errors
        try:
            gen.mark_anchor('C 77', 3.14)
        except ValueError, err:
            msg = "The value for occurrence must be an integer"
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')        

        try:
            gen.mark_anchor('C 77', 0)
        except ValueError, err:
            msg = "0 is not valid for an anchor occurrence."
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')  
            
        try:
            gen.mark_anchor('ZZZ')
        except RuntimeError, err:
            msg = "Could not find pattern ZZZ in output file filename.dat"
            self.assertEqual(str(err), msg)
        else:
            self.fail('RuntimeError expected')  

    def test_output_parse_same_anchors(self):
        
        data = "CQUAD4 1 3.456\n" + \
               "CQUAD4 2 4.123\n" + \
               "CQUAD4 3 7.222\n" + \
               "CQUAD4 4\n"
        
        outfile = open(self.filename, 'w')
        outfile.write(data)
        outfile.close()
        
        gen = FileParser()
        gen.set_file(self.filename)
        gen.set_delimiters(' ')
        
        gen.mark_anchor('CQUAD4')
        val = gen.transfer_var(0, 3)
        self.assertEqual(val, 3.456)
        
        gen.mark_anchor('CQUAD4')
        val = gen.transfer_var(0, 3)
        self.assertEqual(val, 4.123)

        gen.mark_anchor('CQUAD4', 2)
        val = gen.transfer_var(0, 2)
        self.assertEqual(val, 4)

        gen.reset_anchor()
        
        gen.mark_anchor('CQUAD4', -1)
        val = gen.transfer_var(0, 2)
        self.assertEqual(val, 4)

        gen.mark_anchor('CQUAD4', -1)
        val = gen.transfer_var(0, 3)
        self.assertEqual(val, 7.222)

        gen.mark_anchor('CQUAD4', -2)
        val = gen.transfer_var(0, 3)
        self.assertEqual(val, 4.123)
        
    def test_output_parse_keyvar(self):
        
        data = "Anchor\n" + \
               " Key1 1 2 3.7 Test 1e65\n" + \
               " Key1 3 4 3.2 ibg 0.0003\n" + \
               " Key1 5 6 6.7 Tst xxx\n"
        
        outfile = open(self.filename, 'w')
        outfile.write(data)
        outfile.close()
        
        gen = FileParser()
        gen.set_file(self.filename)
        gen.set_delimiters(' ')
        
        gen.mark_anchor('Anchor')
        val = gen.transfer_keyvar('Key1', 3)
        self.assertEqual(val, 3.7)
        val = gen.transfer_keyvar('Key1', 4, -2)
        self.assertEqual(val, 'ibg')
        val = gen.transfer_keyvar('Key1', 4, -2, -1)
        self.assertEqual(val, 'Test')
        
        try:
            gen.transfer_keyvar('Key1', 4, 0)
        except ValueError, err:
            msg = "The value for occurrence must be a nonzero integer"
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')  

        try:
            gen.transfer_keyvar('Key1', 4, -3.4)
        except ValueError, err:
            msg = "The value for occurrence must be a nonzero integer"
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')  


    def test_output_parse_array(self):
        
        data = "Anchor\n" + \
               "10 20 30 40 50 60 70 80\n" + \
               "11 21 31 41 51 61 71 81\n" + \
               "Key a b c d e\n"
        
        outfile = open(self.filename, 'w')
        outfile.write(data)
        outfile.close()
        
        gen = FileParser()
        gen.set_file(self.filename)
        gen.set_delimiters(' ')
        
        gen.mark_anchor('Anchor')
        val = gen.transfer_array(1, 1, 1, 8)
        self.assertEqual(val[0], 10)
        self.assertEqual(val[7], 80)
        val = gen.transfer_array(1, 5, 2, 6)
        self.assertEqual(val[0], 50)
        self.assertEqual(val[9], 61)
        gen.mark_anchor('Key')
        val = gen.transfer_array(0, 2, 0, 6)
        self.assertEqual(val[4], 'e')
        val = gen.transfer_array(0, 2, fieldend=6)
        self.assertEqual(val[4], 'e')
        
        # Now, let's try column delimiters
        gen.reset_anchor()
        gen.mark_anchor('Anchor')
        gen.set_delimiters('columns')
        val = gen.transfer_array(1, 7, 1, 15)
        self.assertEqual(val[0], 30)
        self.assertEqual(val[2], 50)
        val = gen.transfer_array(1, 10, 2, 18)
        self.assertEqual(val[0], 40)
        self.assertEqual(val[5], 61)
        val = gen.transfer_array(3, 5, 3, 10)
        self.assertEqual(val[0], 'a b c')

        try:
            gen.transfer_array(1, 7, 1)
        except ValueError, err:
            msg = "fieldend is missing, currently required"
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')  

            
if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

