# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.util.decorators import add_delegate
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, HasIneqConstraints, Constraint
from openmdao.test.execcomp import ExecComp
from openmdao.units.units import PhysicalQuantity
import openmdao.main.pseudocomp as pcompmod

@add_delegate(HasConstraints)
class MyDriver(Driver):
    pass

@add_delegate(HasEqConstraints)
class MyEqDriver(Driver):
    pass

@add_delegate(HasIneqConstraints)
class MyInEqDriver(Driver):
    pass

class SimpleUnits(Component):
    a = Float(iotype='in', units='inch')
    b = Float(iotype='in', units='inch')
    c = Float(iotype='out', units='ft')
    d = Float(iotype='out', units='ft')
    arr = Array([1.,2.,3.], iotype='in', units='inch')
    arr_out = Array([1.,2.,3.], iotype='out', units='ft')

    def __init__(self):
        super(SimpleUnits, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = PhysicalQuantity(self.a + self.b, 'inch').in_units_of('ft').value
        self.d = PhysicalQuantity(self.a - self.b, 'inch').in_units_of('ft').value

class Simple(Component):
    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')

    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b


class HasConstraintsTestCase(unittest.TestCase):

    def setUp(self):
        pcompmod._count = 0  # keeps names of pseudocomps consistent
        self.asm = set_as_top(Assembly())
        self.asm.add('comp1', Simple())
        self.asm.add('comp2', Simple())
        self.asm.add('comp3', SimpleUnits())
        self.asm.add('comp4', SimpleUnits())

    def test_list_constraints(self):
        drv = self.asm.add('driver', MyDriver())
        self.asm.run()
        self.assertEqual(self.asm.driver.is_valid(), True)
        self.assertEqual(self.asm.driver._exec_state, 'VALID')
        drv.add_constraint('comp1.a < comp1.b')
        drv.add_constraint('comp1.c = comp1.d')
        self.assertEqual(self.asm.driver.is_valid(), False)
        self.assertEqual(self.asm.driver._exec_state, 'INVALID')
        self.assertEqual(drv.list_constraints(),
            ['comp1.c=comp1.d', 'comp1.a<comp1.b'])

    def test_list_eq_constraints(self):
        drv = self.asm.add('driver', MyEqDriver())
        drv.add_constraint('comp1.a = comp1.b')
        drv.add_constraint('comp1.c = comp1.d')
        self.assertEqual(drv.list_constraints(),
            ['comp1.a=comp1.b','comp1.c=comp1.d'])

    def test_list_ineq_constraints(self):
        drv = self.asm.add('driver', MyDriver())
        drv.add_constraint('comp1.a < comp1.b')
        drv.add_constraint('comp1.c >= comp1.d')
        self.assertEqual(drv.list_constraints(),
            ['comp1.a<comp1.b','comp1.c>=comp1.d'])

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
                "driver: Constraint 'comp1.bogus<comp1.d' was not found. Remove failed.")
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
                "Right hand side of constraint 'comp1.b < comp1.qq' has invalid variables 'comp1.qq'")
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
                "driver: Constraint 'comp1.bogus=comp1.d' was not found. Remove failed.")
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
               "Left hand side of constraint 'comp1.qq = comp1.b' has invalid variables 'comp1.qq'")
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
        self.asm.comp1.d = -1

        vals = drv.eval_eq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertEqual(vals[0], 10.)

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
        self.asm.comp1.d = -1

        vals = drv.eval_ineq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertEqual(vals[0], 1)

        vals = drv.get_ineq_constraints()
        self.assertEqual(len(vals), 1)
        self.assertTrue(isinstance(vals['comp1.a>comp1.b'], Constraint))

    def test_constraint_scaler_adder(self):
        drv = self.asm.add('driver', MyDriver())
        self.asm.comp1.a = 3000
        self.asm.comp1.b = 5000
        drv.add_constraint('(comp1.a-4000.)/1000.0 < comp1.b')
        result = drv.eval_ineq_constraints()

        self.assertEqual(result[0], -5001.0)

        drv.remove_constraint('(comp1.a-4000.)/1000.0 < comp1.b') #cant add constraints that are already there
        result = drv.eval_ineq_constraints()
        self.assertEqual(result, [])

        #try:
            #drv.add_constraint('-comp1.a*5.0 < -comp1.b*5.0')
        #except ValueError as err:
            #self.assertEqual(str(err),
               #"Scaler parameter should be a float > 0")
        #else:
            #self.fail('expected ValueError')

        #try:
            #drv.add_constraint('comp1.a < comp1.b', scaler=2)
        #except ValueError as err:
            #self.assertEqual(str(err),
               #"Scaler parameter should be a float")
        #else:
            #self.fail('expected ValueError')

        #try:
            #drv.add_constraint('comp1.a < comp1.b', adder=2)
        #except ValueError as err:
            #self.assertEqual(str(err),
               #"Adder parameter should be a float")
        #else:
            #self.fail('expected ValueError')

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

    def test_pseudocomps(self):
        self.asm.add('driver', MyDriver())
        self.asm.driver.workflow.add(['comp1','comp2','comp3','comp4'])
        self.assertEqual(self.asm._depgraph.list_connections(),
                         [])
        self.asm.driver.add_constraint('comp1.c-comp2.a>5.')
        self.assertEqual(self.asm._pseudo_0._expr_conn, ('5.0-(comp1.c-comp2.a)', 'out0'))
        self.assertEqual(set(self.asm._depgraph.list_connections()),
                         set([('comp2.a', '_pseudo_0.in1'), ('comp1.c', '_pseudo_0.in0')]))
        self.assertEqual(set(self.asm._exprmapper.list_connections()),
                         set([('comp2.a', '_pseudo_0.in1'), ('comp1.c', '_pseudo_0.in0')]))

        self.asm.driver.remove_constraint('comp1.c-comp2.a>5.')
        self.assertEqual(self.asm._depgraph.list_connections(), [])
        self.assertEqual(self.asm._exprmapper.list_connections(), [])

        self.asm.driver.add_constraint('comp1.c > 0.')
        self.assertEqual(set(self.asm._depgraph.list_connections()),
                         set([('comp1.c', '_pseudo_1.in0')]))
        self.assertEqual(set(self.asm._exprmapper.list_connections()),
                         set([('comp1.c', '_pseudo_1.in0')]))
        self.assertEqual(self.asm._pseudo_1._expr_conn, ('-comp1.c', 'out0'))

        self.asm.driver.add_constraint('comp1.c-comp2.a<5.')
        self.assertEqual(self.asm._pseudo_2._expr_conn, ('comp1.c-comp2.a-5.0', 'out0'))

        self.asm.driver.add_constraint('comp1.c < 0.')
        self.assertEqual(self.asm._pseudo_3._expr_conn, ('comp1.c', 'out0'))

        # unit conversions don't show up in constraints or objectives
        self.asm.driver.add_constraint('comp3.c-comp4.a>5.')
        self.assertEqual(self.asm._pseudo_4._expr_conn, ('5.0-(comp3.c-comp4.a)', 'out0'))

        self.asm.driver.clear_constraints()

        self.asm.comp1.a = 2
        self.asm.comp1.b = 1
        self.asm.comp2.a = 4
        self.asm.comp2.b = 2

        # comp1.c = 3
        # comp1.d = 1
        # comp2.c = 6
        # comp2.d = 2
        self.asm.driver.add_constraint('comp2.c - 2*comp1.d > 5')
        self.asm.driver.add_constraint('comp2.c - 2*comp1.d < 5')
        self.asm.driver.add_constraint('comp2.d < 0')

        self.asm.run()

        self.assertEqual(self.asm._pseudo_5.out0, 1.0)
        self.assertEqual(self.asm._pseudo_6.out0, -1.0)
        self.assertEqual(self.asm._pseudo_7.out0, 2.0)

if __name__ == "__main__":
    unittest.main()


