# pylint: disable-msg=C0111,C0103

import unittest

from enthought.traits.api import Float, Array, TraitError

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
    array = Array(value=[1., 2., 3.], iostatus='in')
        
class StringRefTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.top = Assembly()
        self.comp1 = self.top.add_container('comp1', RefComp())
        self.comp2 = self.top.add_container('comp2', SimpleComp())
        self.comp3 = self.top.add_container('comp3', SimpleComp())
        self.comp4 = self.top.add_container('comp4', SimpleComp())
        
    def test_assignment(self):
        self.comp1.objective = 'comp2.x'
        self.comp1.desvar = 'comp2.y'
        rval = self.comp1.objective.evaluate()
        self.assertEqual(rval, 99.9)
        self.comp1.desvar.set(3.14)
        self.assertEqual(self.comp2.y, 3.14)
        
    def test_change_ref(self):
        self.comp1.objective = 'comp2.x'
        self.assertEqual(self.comp1.objective.evaluate(), 99.9)
        self.comp1.objective = 'comp2.d1'
        self.assertEqual(self.comp1.objective.evaluate(), 42.)
        
        self.comp1.desvars = ['comp2.y', 'comp2.d1', 'comp2.d1out']
        self.assertEqual([v.evaluate() for v in self.comp1.desvars],
                         [99.9,42.,11.])
        

    def test_array_assignment(self):
        self.comp1.desvars = ['comp2.y', 'comp2.d1', 'comp3.d1']
        for dv,val in zip(self.comp1.desvars, [3.14, -3.14, -.01]):
            dv.set(val)
        self.assertEqual(self.comp2.y, 3.14)
        self.assertEqual(self.comp2.d1, -3.14)
        self.assertEqual(self.comp3.d1, -0.01)
        
    def test_array_access(self):
        self.comp1.objective = 'comp2.array[1]'
        self.assertEqual(2., self.comp1.objective.evaluate())
        self.comp1.desvars = ['comp2.array[0]', 'comp2.array[2]']
        self.assertEqual([1.,3.], 
                         [self.comp1.desvars[0].evaluate(),
                          self.comp1.desvars[1].evaluate()])
        self.comp1.desvars[0].set('-22.')
        self.comp1.desvars[1].set('-777.')
        self.assertEqual(-22., self.comp2.array[0])
        self.assertEqual(-777., self.comp2.array[2])
        
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
            self.comp1.desvar.set(None)
        except TraitError, err:
            self.assertEqual(str(err), 
                "The 'y' trait of a SimpleComp instance must be a float, but a value of None <type 'NoneType'> was specified.")
        else:
            self.fail('expected TraitError')
        
    def test_bad_direction(self):
        try:
            self.comp1.objective = 'comp2.x'
            self.comp1.objective.set(3.2)
        except ValueError, err:
            self.assertEqual(str(err), "trying to set input expression 'comp2.x'")
        else:
            self.fail('expected RuntimeError')

    def test_novar_expr(self):
        asm = Assembly()
        asm.add_trait('ref', StringRef(iostatus='in'))
        asm.ref = '1+2'
        self.assertEqual(asm.ref.evaluate(), 3)
        
        asm.add_trait('refout', StringRef(iostatus='out'))
        try:
            asm.refout = '2'
        except TraitError, err:
            self.assertEqual(str(err), "invalid output ref variable value '2'")
        else:
            self.fail('TraitError expected')
        

if __name__ == "__main__":
    unittest.main()

