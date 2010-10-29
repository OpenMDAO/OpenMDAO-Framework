"""
Testing the file wrapping utilities.
"""

import unittest

from numpy import array, isnan

from openmdao.lib.api import Float, Bool, Int, Str, Array, File, List, Enum
from openmdao.main.api import Container, Component
from openmdao.util.filewrap import InputFileGenerator, FileParser


class TestCase(unittest.TestCase):
    """ Test namelist writer functions. """

    def setUp(self):
        self.templatename = 'template.dat'
        self.filename = 'filename.dat'

    def tearDown(self):
        pass
    
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
        gen.mark_anchor('Anchor', 2)
        gen.transfer_var('NaN', 1, 4)
        gen.reset_anchor()
        gen.transfer_var('55', 3, 2)
        gen.mark_anchor('C 77')
        gen.transfer_var('1.3e-37', -3, 6)
        gen.clearline(-5)
        gen.mark_anchor('Anchor', -1)
        gen.transfer_var('8.7', 1, 5)
        
        gen.generate()
        
        infile = open(self.filename, 'r')
        result = infile.read()
        infile.close()
        
        answer = "\n" + \
                   "Anchor\n" + \
                   " A 1, 2 34, Test 1.3e-37\n" + \
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
            
            
    def test_templated_input_arrays(self):
        
        template = "Anchor\n" + \
                   "0 0 0 0 0"
        
        outfile = open(self.templatename, 'w')
        outfile.write(template)
        outfile.close()
        
        gen = InputFileGenerator()
        gen.set_template_file(self.templatename)
        gen.set_generated_file(self.filename)
        
        gen.mark_anchor('Anchor')
        gen.transfer_array(array([1, 2, 3, 4, 5]), 1, 3, 5, sep=' ')
        
        gen.generate()
        
        infile = open(self.filename, 'r')
        result = infile.read()
        infile.close()
        
        answer = "Anchor\n" + \
                   "0 0 1 2 3 4 5"
    
        self.assertEqual(answer, result)

    def test_output_parse(self):
        
        data = "Junk\n" + \
                   "Anchor\n" + \
                   " A 1, 2 34, Test 1e65\n" + \
                   " B 4 Stuff\n" + \
                   "Anchor\n" + \
                   " C 77 False NaN 333.444\n" + \
                   " 1,2,3,4,5"
        
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
        val = gen.transfer_line(-1)
        self.assertEqual(val, ' B 4 Stuff')

        # Now, let's try column delimiters
        gen.set_delimiters('columns')
        gen.mark_anchor('Anchor',-1)
        val = gen.transfer_var(1, 8, 10)
        self.assertEqual(val, 'als')

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
            msg = "0 is not valid for an keyvar occurrence."
            self.assertEqual(str(err), msg)
        else:
            self.fail('ValueError expected')  


    def test_output_parse_array(self):
        
        data = "Anchor\n" + \
               "1 2 3 4 5 6 7 8\n" + \
               "11 22 33 44 55 66 77 88\n"
        
        outfile = open(self.filename, 'w')
        outfile.write(data)
        outfile.close()
        
        gen = FileParser()
        gen.set_file(self.filename)
        gen.set_delimiters(' ')
        
        gen.mark_anchor('Anchor')
        val = gen.transfer_array(1, 1, 1, 8)
        self.assertEqual(val[0], 1)
        self.assertEqual(val[7], 8)
        val = gen.transfer_array(1, 5, 2, 6)
        self.assertEqual(val[0], 5)
        self.assertEqual(val[3], 66)
        
        # Now, let's try column delimiters
        gen.set_delimiters('columns')
        val = gen.transfer_array(1, 7, 1, 11)
        self.assertEqual(val[0], 4)
        self.assertEqual(val[2], 6)
        
            
if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

