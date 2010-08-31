# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.lib.api import Float, Str, Instance
from openmdao.util.decorators import add_delegate
from openmdao.main.hasconstraints import HasConstraints, Constraint
from openmdao.test.execcomp import ExecComp

@add_delegate(HasConstraints)
class MyDriver(Driver):
    pass

class HasConstraintsTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add('driver', MyDriver())
        self.asm.add('comp1', ExecComp(exprs=['c=a+b', 'd=a-b']))
        self.asm.driver.workflow.add(self.asm.comp1)
        
    def test_add_constraint(self):
        drv = self.asm.driver
        self.assertEqual(len(drv.get_eq_constraints()), 0)
        self.assertEqual(len(drv.get_ineq_constraints()), 0)
        
        drv.add_constraint(' comp1.a > comp1.b')
        self.assertEqual(len(drv.get_eq_constraints()), 0)
        self.assertEqual(len(drv.get_ineq_constraints()), 1)
        
        drv.add_constraint('comp1.c =      comp1.d ')
        self.assertEqual(len(drv.get_eq_constraints()), 1)
        self.assertEqual(len(drv.get_ineq_constraints()), 1)
        
        drv.remove_constraint(' comp1.c=comp1.d')
        self.assertEqual(len(drv.get_eq_constraints()), 0)
        self.assertEqual(len(drv.get_ineq_constraints()), 1)
        
        drv.remove_constraint(' comp1.a>  comp1.b  ')
        self.assertEqual(len(drv.get_eq_constraints()), 0)
        self.assertEqual(len(drv.get_ineq_constraints()), 0)
        
        drv.add_constraint(' comp1.a > comp1.b')
        drv.add_constraint('comp1.c =comp1.d ')
        self.assertEqual(len(drv.get_eq_constraints()), 1)
        self.assertEqual(len(drv.get_ineq_constraints()), 1)
        
        drv.clear_constraints()
        self.assertEqual(len(drv.get_eq_constraints()), 0)
        self.assertEqual(len(drv.get_ineq_constraints()), 0)
        
        try:
            drv.add_constraint('comp1.qq < comp1.b')
        except ValueError as err:
            self.assertEqual(str(err), 
                "Invalid expression 'comp1.qq': comp1: cannot get valid flag of 'qq' because it's not an io trait.")
        else:
            self.fail('expected ValueError')
        
    def test_eval_constraints(self):
        drv = self.asm.driver
        vals = drv.eval_eq_constraints()
        self.assertEqual(len(vals), 0)
        
        drv.add_constraint(' comp1.a > comp1.b')
        drv.add_constraint('comp1.c = comp1.d ')
        self.asm.comp1.a = 4
        self.asm.comp1.b = 5
        self.asm.comp1.c = 9
        self.asm.comp1.d = 9
        vals = drv.eval_eq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertEqual(vals[0][0], 9)
        self.assertEqual(vals[0][1], 9)
        self.assertEqual(vals[0][2], '=')
        self.assertEqual(vals[0][3], False)
        vals = drv.eval_ineq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertEqual(vals[0][0], 4)
        self.assertEqual(vals[0][1], 5)
        self.assertEqual(vals[0][2], '>')
        self.assertEqual(vals[0][3], True)
        
        vals = drv.get_eq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertTrue(isinstance(vals['comp1.c=comp1.d'], Constraint))
        vals = drv.get_ineq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertTrue(isinstance(vals['comp1.a>comp1.b'], Constraint))

if __name__ == "__main__":
    unittest.main()


