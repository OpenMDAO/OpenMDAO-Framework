# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.exceptions import ConstraintError
from openmdao.main import Assembly, Component, RefVariable, Float
from openmdao.main.variable import INPUT, OUTPUT

class RefVariableTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.top = Assembly('top', None)
        self.comp1 = Component('comp1', self.top)
        self.comp2 = Component('comp2', self.top)
        RefVariable('objective', self.comp1, INPUT)
        RefVariable('desvar', self.comp1, OUTPUT)
        Float('x',self.comp2, OUTPUT, default=99.9)
        Float('y',self.comp2, INPUT, default=99.9)
        Float('x',self.comp1, OUTPUT, default=99.9)
        Float('y',self.comp1, INPUT, default=99.9)
        
    def test_assignment(self):
        self.comp1.objective.value = 'comp2.x'
        self.comp1.desvar.value = 'comp2.y'
        rval = self.comp1.objective.refvalue
        self.assertEqual(rval, 99.9)
        self.comp1.desvar.refvalue = 3.14
        self.assertEqual(self.comp2.y, 3.14)

    def test_ref_comps(self):
        self.comp1.objective.value = 'comp2.x+3*comp1.x'
        comps = self.comp1.objective.get_referenced_compnames()
        self.assertEqual(set(['comp2','comp1']), comps)
        
    def test_bad_assign(self):
        try:
            self.comp1.objective.value = None
        except TypeError, err:
            self.assertEqual(str(err), "top.comp1.objective: reference must be a string")
        else:
            self.fail('expected TypeError')
            
    def test_bad_IO(self):
        try:
            self.comp1.objective.value = 'comp2.x'
            self.comp1.objective.refvalue = None
        except ValueError, err:
            self.assertEqual(str(err), "trying to set an input expression")
        else:
            self.fail('expected ValueError')
        
    def test_bad_type(self):
        try:
            self.comp1.desvar.value = 'comp2.y'
            self.comp1.desvar.refvalue = None
        except ValueError, err:
            self.assertEqual(str(err), 
                "top.comp2.y: incompatible type <type 'NoneType'> is not one of ['float', 'int', 'long', 'float64']")
        else:
            self.fail('expected ValueError')
        
    def test_bad_direction(self):
        try:
            self.comp1.desvar.value = 'comp2.x'
            self.comp1.desvar.refvalue = 3.2
        except RuntimeError, err:
            self.assertEqual(str(err), "top.comp2.x is an OUTPUT Variable and cannot be set.")
        else:
            self.fail('expected RuntimeError')
        

if __name__ == "__main__":
    unittest.main()

