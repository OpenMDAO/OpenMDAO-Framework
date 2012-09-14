# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Driver, set_as_top
from openmdao.util.decorators import add_delegate
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, HasIneqConstraints, Constraint
from openmdao.test.execcomp import ExecComp

@add_delegate(HasConstraints)
class MyDriver(Driver):
    pass

@add_delegate(HasEqConstraints)
class MyEqDriver(Driver):
    pass

@add_delegate(HasIneqConstraints)
class MyInEqDriver(Driver):
    pass

class HasConstraintsTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add('comp1', ExecComp(exprs=['c=a+b', 'd=a-b']))
        
    def test_list_constraints(self):
        drv = self.asm.add('driver', MyDriver())
        self.asm.run()
        self.assertEqual(self.asm.driver.is_valid(), True)
        self.assertEqual(self.asm.driver._exec_state, 'VALID')
        drv.add_constraint('comp1.a < comp1.b')
        drv.add_constraint('comp1.c = comp1.d')
        self.assertEqual(self.asm.driver.is_valid(), False)
        self.assertEqual(self.asm.driver._exec_state, 'INVALID')
        self.assertEqual(drv.list_constraints(), ['comp1.a<comp1.b','comp1.c=comp1.d'])
        
    def test_list_eq_constraints(self):
        drv = self.asm.add('driver', MyEqDriver())
        drv.add_constraint('comp1.a = comp1.b')
        drv.add_constraint('comp1.c = comp1.d')
        self.assertEqual(drv.list_constraints(), ['comp1.a=comp1.b','comp1.c=comp1.d'])
        
    def test_list_ineq_constraints(self):
        drv = self.asm.add('driver', MyDriver())
        drv.add_constraint('comp1.a < comp1.b')
        drv.add_constraint('comp1.c >= comp1.d')
        self.assertEqual(drv.list_constraints(), ['comp1.a<comp1.b','comp1.c>=comp1.d'])
        
    def _check_ineq_add_constraint(self, drv):
        self.asm.add('driver', drv)

        try:
            drv.add_constraint('comp1.b==comp1.a')
        except Exception as err:
            self.assertEqual(str(err), "driver: Constraints require an explicit comparator (=, <, >, <=, or >=)")
        else:
            self.fail("Exception expected")
    
        self.assertEqual(len(drv.get_ineq_constraints()), 0)
        drv.add_constraint(' comp1.a > comp1.b')
        
        try: 
            drv.add_constraint('comp1.a>comp1.b')
        except Exception as err: 
            self.assertEqual(str(err),
                             'driver: A constraint of the form "comp1.a>comp1.b" already exists '
                             'in the driver. Add failed.')
        else: 
            self.fail("Exception Expected")
        
        self.assertEqual(len(drv.get_ineq_constraints()), 1)
        drv.remove_constraint(' comp1.a>  comp1.b  ')
        self.assertEqual(len(drv.get_ineq_constraints()), 0)
        try:
            drv.remove_constraint('comp1.bogus < comp1.d')
        except Exception as err:
            self.assertEqual(str(err), 
                "driver: Constraint 'comp1.bogus < comp1.d' was not found. Remove failed.")
        else:
            self.fail("Exception expected")
        drv.add_constraint(' comp1.a > comp1.b')
        self.assertEqual(len(drv.get_ineq_constraints()), 1)
        
        drv.add_constraint('comp1.b < comp1.c', name='foobar')
        self.assertEqual(len(drv.get_ineq_constraints()), 2)
        
        try:
            drv.add_constraint('comp1.b < comp1.a', name='foobar')
        except Exception as err:
            self.assertEqual(str(err), 'driver: A constraint named "foobar" already exists in the driver. Add failed.')
        else:
            self.fail("Exception expected")

        self.assertEqual(len(drv.get_ineq_constraints()), 2)
        
        drv.remove_constraint('foobar')
        self.assertEqual(len(drv.get_ineq_constraints()), 1)
        
        drv.clear_constraints()

        self.assertEqual(len(drv.get_ineq_constraints()), 0)
    
        try:
            drv.add_constraint('comp1.b < comp1.qq')
        except ValueError as err:
            self.assertEqual(str(err), 
                "Constraint 'comp1.b < comp1.qq' has an invalid right-hand-side.")
        else:
            self.fail('expected ValueError')
                
    def _check_eq_add_constraint(self, drv):
        self.asm.add('driver', drv)

        self.assertEqual(len(drv.get_eq_constraints()), 0)
        self.assertEqual(len(drv.get_eq_constraints()), 0)
        drv.add_constraint('comp1.c =      comp1.d ')
        self.assertEqual(len(drv.get_eq_constraints()), 1)
        
        try: 
            drv.add_constraint('comp1.c=comp1.d')
        except Exception as err: 
            self.assertEqual(str(err),
                             'driver: A constraint of the form "comp1.c=comp1.d" already exists '
                             'in the driver. Add failed.')
        else: 
            self.fail("Exception Expected")

        drv.remove_constraint(' comp1.c=comp1.d')
        self.assertEqual(len(drv.get_eq_constraints()), 0)
        try:
            drv.remove_constraint('comp1.bogus = comp1.d')
        except Exception as err:
            self.assertEqual(str(err), 
                "driver: Constraint 'comp1.bogus = comp1.d' was not found. Remove failed.")
        else:
            self.fail("Exception expected")
        self.assertEqual(len(drv.get_eq_constraints()), 0)
    
        drv.add_constraint('comp1.c =comp1.d ')
        self.assertEqual(len(drv.get_eq_constraints()), 1)
        
        drv.add_constraint('comp1.b = comp1.c', name='foobar')
        self.assertEqual(len(drv.get_eq_constraints()), 2)
        
        try:
            drv.add_constraint('comp1.b = comp1.a', name='foobar')
        except Exception as err:
            self.assertEqual(str(err), 'driver: A constraint named "foobar" already exists in the driver. Add failed.')
        else:
            self.fail("Exception expected")

        drv.remove_constraint('foobar')
        self.assertEqual(len(drv.get_eq_constraints()), 1)
        
        drv.clear_constraints()

        self.assertEqual(len(drv.get_eq_constraints()), 0)
        try:
            drv.add_constraint('comp1.qq = comp1.b')
        except ValueError as err:
            self.assertEqual(str(err), 
               "Constraint 'comp1.qq = comp1.b' has an invalid left-hand-side.")
        else:
            self.fail('expected ValueError')
                
    def _check_eq_eval_constraints(self, drv):
        self.asm.add('driver', drv)
        
        vals = drv.eval_eq_constraints()
        self.assertEqual(len(vals), 0)
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
        
        vals = drv.get_eq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertTrue(isinstance(vals['comp1.c=comp1.d'], Constraint))

    def _check_ineq_eval_constraints(self, drv):
        self.asm.add('driver', drv)
        
        vals = drv.eval_ineq_constraints()
        self.assertEqual(len(vals), 0)
    
        drv.add_constraint(' comp1.a > comp1.b')
        
        self.asm.comp1.a = 4
        self.asm.comp1.b = 5
        self.asm.comp1.c = 9
        self.asm.comp1.d = 9
        
        vals = drv.eval_ineq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertEqual(vals[0][0], 4)
        self.assertEqual(vals[0][1], 5)
        self.assertEqual(vals[0][2], '>')
        self.assertEqual(vals[0][3], True)

        vals = drv.get_ineq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertTrue(isinstance(vals['comp1.a>comp1.b'], Constraint))

    def test_constraint_scaler_adder(self):
        drv = self.asm.add('driver', MyDriver())
        self.asm.comp1.a = 3000
        self.asm.comp1.b = 5000
        drv.add_constraint('comp1.a < comp1.b', scaler=1.0/1000.0, adder=-4000.0)
        result = drv.eval_ineq_constraints()
        
        self.assertEqual(result[0][0], -1.0)
        self.assertEqual(result[0][1], 1.0)
        
        drv.remove_constraint('comp1.a < comp1.b') #cant add constraints that are already there
        try:
            drv.add_constraint('comp1.a < comp1.b', scaler=-5.0)
        except ValueError as err:
            self.assertEqual(str(err), 
               "Scaler parameter should be a float > 0")
        else:
            self.fail('expected ValueError')
        
        try:
            drv.add_constraint('comp1.a < comp1.b', scaler=2)
        except ValueError as err:
            self.assertEqual(str(err), 
               "Scaler parameter should be a float")
        else:
            self.fail('expected ValueError')
    
        try:
            drv.add_constraint('comp1.a < comp1.b', adder=2)
        except ValueError as err:
            self.assertEqual(str(err), 
               "Adder parameter should be a float")
        else:
            self.fail('expected ValueError')
    
    def test_add_constraint_eq_eq(self):
        drv = MyDriver()
        self.asm.add('driver', drv)
        try:
            drv.add_constraint('comp1.b==comp1.a')
        except Exception as err:
            self.assertEqual(str(err), "driver: Constraints require an explicit comparator (=, <, >, <=, or >=)")
        else:
            self.fail("Exception expected")

    def test_add_constraint(self):
        drv = MyDriver()
        self._check_eq_add_constraint(drv)
        self._check_ineq_add_constraint(drv)
    
    def test_add_eq_constraint(self):
        self._check_eq_add_constraint(MyEqDriver())
    
    def test_add_ineq_constraint(self):
        self._check_ineq_add_constraint(MyInEqDriver())
    
    def test_implicit_constraint(self):
        drv = self.asm.add('driver', MyEqDriver())
        try:
            drv.add_constraint('comp1.a + comp1.b')
        except ValueError, err:
            self.assertEqual(str(err),
                             "driver: Constraints require an explicit comparator (=, <, >, <=, or >=)")
        else:
            self.fail('ValueError expected')
            
    def test_eval_constraint(self):
        self._check_eq_eval_constraints(MyDriver())
        self._check_ineq_eval_constraints(MyDriver())

    def test_eval_eq_constraint(self):
        self._check_eq_eval_constraints(MyEqDriver())

    def test_eval_ineq_constraint(self):
        self._check_ineq_eval_constraints(MyInEqDriver())

if __name__ == "__main__":
    unittest.main()


