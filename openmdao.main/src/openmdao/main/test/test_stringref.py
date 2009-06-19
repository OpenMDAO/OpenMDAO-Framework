# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import Float, TraitError

from openmdao.main.exceptions import ConstraintError
from openmdao.main.api import Assembly, Component, StringRef, StringRefArray

class RefComp(Component):   
    desvar = StringRef(iostatus='out')
    desvars = StringRefArray(iostatus='out')
    objective = StringRef(iostatus='in')
    z = Float(99.9, iostatus='out')
            
class SimpleComp(Component):
    x = Float(99.9, iostatus='out')
    y = Float(99.9, iostatus='in')
    d1 = Float(42., iostatus='in')
    d1out = Float(11., iostatus='out')
        
class StringRefTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.top = Assembly('top', None)
        self.comp1 = RefComp('comp1', parent=self.top)
        self.comp2 = SimpleComp('comp2', parent=self.top)
        self.comp3 = SimpleComp('comp3', parent=self.top)
        self.comp4 = SimpleComp('comp4', parent=self.top)
        
    def test_assignment(self):
        self.comp1.objective = 'comp2.x'
        self.comp1.desvar = 'comp2.y'
        rval = self.comp1.objective.refvalue
        self.assertEqual(rval, 99.9)
        self.comp1.desvar.refvalue = 3.14
        self.assertEqual(self.comp2.y, 3.14)

    def test_array_assignment(self):
        self.comp1.desvars = ['comp2.y', 'comp2.d1', 'comp3.d1']
        for dv,val in zip(self.comp1.desvars, [3.14, -3.14, -.01]):
            dv.refvalue = val
        self.assertEqual(self.comp2.y, 3.14)
        self.assertEqual(self.comp2.d1, -3.14)
        self.assertEqual(self.comp3.d1, -0.01)

    def test_ref_comps(self):
        self.comp1.objective = 'comp2.x+3*comp1.z'
        comps = self.comp1.objective.get_referenced_compnames()
        self.assertEqual(set(['comp2','comp1']), comps)
        
    def test_array_ref_comps(self):
        self.comp1.desvars = ['comp2.x', 'comp4.y', 'comp4.d1']
        comps = reduce(lambda x,y: x.union(y.get_referenced_compnames()), 
                       self.comp1.desvars, set())
        self.assertEqual(set(['comp2','comp4']), comps)
        
    def test_bad_assign(self):
        try:
            self.comp1.objective = None
        except TraitError, err:
            self.assertEqual(str(err), 
                "The 'objective' trait of a RefComp instance must be a string, but a value of None <type 'NoneType'> was specified.")
        else:
            self.fail('expected TraitError')
            
    def test_bad_type(self):
        try:
            self.comp1.desvar = 'comp2.y'
            self.comp1.desvar.refvalue = None
        except TraitError, err:
            self.assertEqual(str(err), 
                "The 'y' trait of a SimpleComp instance must be a float, but a value of None <type 'NoneType'> was specified.")
        else:
            self.fail('expected TraitError')
        
    def test_bad_direction(self):
        try:
            self.comp1.objective = 'comp2.x'
            self.comp1.objective.refvalue = 3.2
        except ValueError, err:
            self.assertEqual(str(err), "trying to set input expression 'comp2.x'")
        else:
            self.fail('expected RuntimeError')

    def test_novar_expr(self):
        asm = Assembly('top')
        asm.add_trait('ref', StringRef(iostatus='in'))
        asm.ref = '1+2'
        self.assertEqual(asm.ref.refvalue, 3)
        
        asm.add_trait('refout', StringRef(iostatus='out'))
        try:
            asm.refout = '2'
        except TraitError, err:
            self.assertEqual(str(err), "invalid output ref variable value '2'")
        else:
            self.fail('TraitError expected')
        

if __name__ == "__main__":
    unittest.main()

