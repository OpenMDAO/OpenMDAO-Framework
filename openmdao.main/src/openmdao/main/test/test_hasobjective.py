# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Driver, set_as_top
from openmdao.util.decorators import add_delegate
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.main.expreval import ExprEvaluator
from openmdao.test.execcomp import ExecComp

@add_delegate(HasObjective)
class MySingleDriver(Driver):
    pass

class HasObjectiveTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add('comp1', ExecComp(exprs=['c=a+b', 'd=a-b']))
        self.asm.add('driver', MySingleDriver())
        self.asm.comp1.a = 1
        self.asm.comp1.b = 2
        self.asm.comp1.c = 3
        self.asm.comp1.d = -1
                
    def test_add_objective(self):
        try:
            self.asm.driver.add_objective('blah.foo')
        except Exception as err:
            self.assertEqual(str(err), 
                             "driver: Can't add objective because I can't evaluate 'blah.foo'.")
        else:
            self.fail('Exception expected')
        
    def test_eval_objective(self):
        self.asm.driver.add_objective('comp1.a-comp1.b')
        self.assertEqual(self.asm.driver.eval_objective(), -1)


@add_delegate(HasObjectives)
class MyMultiDriver(Driver):
    pass

class HasObjectivesTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add('comp1', ExecComp(exprs=['c=a+b', 'd=a-b']))
        self.asm.add('driver', MyMultiDriver())
        self.asm.comp1.a = 1
        self.asm.comp1.b = 2
        self.asm.comp1.c = 3
        self.asm.comp1.d = -1
        
    def test_add_objective(self):
        try:
            self.asm.driver.add_objective('blah.foo')
        except Exception as err:
            self.assertEqual(str(err), 
                             "driver: Can't add objective because I can't evaluate 'blah.foo'.")
        else:
            self.fail('Exception expected')
                
    def test_remove_objective(self):
        self.asm.driver.add_objective('comp1.a-comp1.b')
        self.asm.driver.add_objective('comp1.c-comp1.d')
        self.asm.driver.remove_objective('comp1.a-comp1.b')
        self.assertEqual(set(self.asm.driver.get_objectives().keys()), 
                         set(['comp1.c-comp1.d']))
        
    def test_objective_names(self):
        self.asm.driver.add_objective('comp1.a-comp1.b', name='foobar')
        self.asm.driver.add_objective('comp1.c-comp1.d')
        self.assertEqual(set(self.asm.driver.get_objectives().keys()), 
                         set(['comp1.c-comp1.d', 'foobar']))
        self.asm.driver.remove_objective('foobar')
        self.assertEqual(set(self.asm.driver.get_objectives().keys()), 
                         set(['comp1.c-comp1.d']))
        
    def test_add_objectives(self):
        self.asm.driver.add_objectives(['comp1.a-comp1.b', 'comp1.c-comp1.d'])
        self.assertEqual(set(self.asm.driver.get_objectives().keys()), 
                         set(['comp1.a-comp1.b', 'comp1.c-comp1.d']))
        try:
            self.asm.driver.add_objectives('comp1.d+comp1.a')
        except Exception as err:
            self.assertEqual(str(err), "driver: add_objectives requires an iterator of expression strings.")
        else:
            self.fail("Exception expected")
    
    def test_eval_objectives(self):
        self.asm.driver.add_objectives(['comp1.a-comp1.b', 'comp1.c-comp1.d'])
        vals = self.asm.driver.eval_objectives()
        self.assertEqual(vals, [-1, 4])

    def test_clear_objectives(self):
        self.asm.driver.add_objective('comp1.a-comp1.b')
        self.asm.driver.add_objective('comp1.c-comp1.d')
        self.asm.driver.clear_objectives()
        self.assertEqual(len(self.asm.driver.get_objectives()), 0)

if __name__ == "__main__":
    unittest.main()


