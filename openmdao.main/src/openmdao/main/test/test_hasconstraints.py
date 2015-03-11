# pylint: disable-msg=C0111,C0103

import numpy as np

import unittest

from openmdao.main.api import Assembly, Component, Driver, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, \
     HasIneqConstraints, Constraint, Has2SidedConstraints
from openmdao.main.interfaces import IHas2SidedConstraints, implements
from openmdao.main.pseudocomp import SimpleEQConPComp, SimpleEQ0PComp
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecComp
from openmdao.units.units import PhysicalQuantity
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_rel_error

@add_delegate(HasConstraints)
class MyDriver(Driver):
    pass

@add_delegate(HasEqConstraints)
class MyEqDriver(Driver):
    pass

@add_delegate(HasIneqConstraints)
class MyInEqDriver(Driver):
    pass

@add_delegate(HasConstraints, Has2SidedConstraints)
class My2SDriver(Driver):

    implements(IHas2SidedConstraints)

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

    def list_deriv_vars(self):
        return ('a', 'b'), ('c', 'd')

    def provideJ(self):
        der = 1.0
        return np.array([[der, der], [der, der]])


class HasConstraintsTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add('comp1', Simple())
        self.asm.add('comp2', Simple())
        self.asm.add('comp3', SimpleUnits())
        self.asm.add('comp4', SimpleUnits())

    def test_list_constraints(self):
        drv = self.asm.add('driver', MyDriver())
        self.asm.run()
        drv.add_constraint('comp1.a < comp1.b')
        drv.add_constraint('comp1.c = comp1.d')
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

        self.asm.run()
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

        self.asm.run()
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
        self.asm.run()
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
        self.asm.driver.workflow.add(['comp1','comp2'])
        self.asm._setup()
        self.assertEqual(self.asm._depgraph.list_connections(),
                         [])
        self.asm.driver.add_constraint('comp1.c-comp2.a>5.')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_0._orig_expr, '5.-(comp1.c-comp2.a)')
        self.assertEqual(set(self.asm._depgraph.list_connections(drivers=False)),
                         set([('comp2.a', '_pseudo_0.in1'), ('comp1.c', '_pseudo_0.in0')]))

        self.asm.driver.remove_constraint('comp1.c-comp2.a>5.')
        self.asm._setup()
        self.assertEqual(self.asm._depgraph.list_connections(drivers=False), [])

        self.asm.driver.add_constraint('comp1.c > 0.')
        self.asm._setup()
        self.assertEqual(set(self.asm._depgraph.list_connections(drivers=False)),
                         set([('comp1.c', '_pseudo_1.in0')]))

        self.asm._setup()
        self.assertEqual(self.asm._pseudo_1._orig_expr, '-(comp1.c)')

        self.asm.driver.add_constraint('comp1.c-comp2.a<5.')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_2._orig_expr, 'comp1.c-comp2.a-(5.)')

        self.asm.driver.add_constraint('comp1.c < 0.')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_3._orig_expr, 'comp1.c')

        # unit conversions don't show up in constraints or objectives
        self.asm.driver.add_constraint('comp3.c-comp4.a>5.')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_4._orig_expr, '5.-(comp3.c-comp4.a)')

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

    def test_custom_pseudocomp_creation(self):
        self.asm.add('driver', MyDriver())
        arg = {}
        result = {}

        self.asm.driver.add_constraint('comp1.c = 0')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_0.__class__, SimpleEQ0PComp)
        self.asm.run()
        arg['in0'] = np.array([3.3])
        result['out0'] = np.array([0.0])
        self.asm._pseudo_0.apply_deriv(arg, result)
        self.assertEqual(result['out0'][0], 3.3)

        self.asm.driver.add_constraint('comp1.d = 5.4')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_1.__class__, SimpleEQ0PComp)
        self.asm.run()
        arg['in0'] = np.array([3.3])
        result['out0'] = np.array([0.0])
        self.asm._pseudo_1.apply_deriv(arg, result)
        self.assertEqual(result['out0'][0], 3.3)

        self.asm.driver.add_constraint('comp2.c = comp3.a')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_2.__class__, SimpleEQConPComp)
        self.asm.run()
        arg['in0'] = np.array([7.2])
        arg['in1'] = np.array([3.1])
        result['out0'] = np.array([0.0])
        self.asm._pseudo_2.apply_deriv(arg, result)
        self.assertEqual(result['out0'][0], 4.1)

        self.asm.driver.clear_constraints()
        self.asm.driver.add_constraint('comp2.c - comp3.a=0.0')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_3.__class__, SimpleEQConPComp)
        self.asm.run()
        arg['in0'] = np.array([7.2])
        arg['in1'] = np.array([3.1])
        result['out0'] = np.array([0.0])
        self.asm._pseudo_3.apply_deriv(arg, result)
        self.assertEqual(result['out0'][0], 4.1)

        self.asm.driver.clear_constraints()
        self.asm.driver.add_constraint('0=comp2.c - comp3.a')
        self.asm._setup()
        self.assertEqual(self.asm._pseudo_4.__class__, SimpleEQConPComp)
        self.asm.run()
        arg['in0'] = np.array([7.2])
        arg['in1'] = np.array([3.1])
        result['out0'] = np.array([0.0])
        self.asm._pseudo_4.apply_deriv(arg, result)
        self.assertEqual(result['out0'][0], 4.1)
        
    def test_custom_jacobian(self):
        
        class AComp(Component):
            
            x = Array([[1.0, 3.0], [-2.0, 4.0]], iotype='in')
            y = Array(np.zeros((2, 2)), iotype='out')

            def __init__(self):
                super(AComp, self).__init__()
                self.J = np.array([[3.5, -2.5, 1.5, 4.0],
                                   [4.0, 2.0, -1.1, 3.4],
                                   [7.7, 6.6, 4.4, 1.1],
                                   [0.1, 3.3, 6.8, -5.5]])
            
            def execute(self):
                """ Run arraycomp"""
                y = self.J.dot(self.x.flatten())
                self.y = y.reshape((2,2))
                
            def list_deriv_vars(self):
                """ x and y """
                input_keys = ('x',)
                output_keys = ('y',)
                return input_keys, output_keys

            def provideJ(self):
                """Analytical first derivatives"""
                return self.J
            
            
        def fake_jac():
            """ Returns a User-defined Jacobian. The values are
            totally wrong to facilitate testing. """
            jacs = {}
            jacs['comp.x'] = np.array([[100.0, 101, 102, 103],
                                       [104, 105, 106, 107],
                                       [108, 109, 110, 111],
                                       [112, 113, 114, 115]])
            
            return jacs
        
        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())
        top.add('comp', AComp())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('comp.x', low=10, high=10)
        top.driver.add_constraint('comp.y < 1', jacs=fake_jac)
        
        # Scipy gmres, inequality constraints
        top._setup()
        top.run()
        
        J = top.driver.calc_gradient(mode='forward', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - top.comp.J)
        assert_rel_error(self, diff.max(), 0.0, 1e-4)
        
        J = top.driver.calc_gradient(mode='adjoint', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - fake_jac()['comp.x'])
        assert_rel_error(self, diff.max(), 0.0, 1e-4)

        # Scipy gmres, equality constraints
        top.driver.clear_constraints()
        top._pseudo_count = 0
        top.driver.add_constraint('comp.y = 1', jacs=fake_jac)
        top._setup()
        top.run()
        
        J = top.driver.calc_gradient(mode='forward', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - top.comp.J)
        assert_rel_error(self, diff.max(), 0.0, 1e-4)
        
        J = top.driver.calc_gradient(mode='adjoint', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - fake_jac()['comp.x'])
        assert_rel_error(self, diff.max(), 0.0, 1e-4)

        # Scipy gmres, double-sided constraints
        top.driver.clear_constraints()
        top._pseudo_count = 0
        top.driver.add_constraint('0 < comp.y < 1', jacs=fake_jac)
        top._setup()
        top.run()
        
        J = top.driver.calc_gradient(mode='forward', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - top.comp.J)
        assert_rel_error(self, diff.max(), 0.0, 1e-4)
        
        J = top.driver.calc_gradient(mode='adjoint', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - fake_jac()['comp.x'])
        assert_rel_error(self, diff.max(), 0.0, 1e-4)

        # Linear GS, equality constraints
        top.driver.clear_constraints()
        top._pseudo_count = 0
        top.driver.add_constraint('comp.y = 1', jacs=fake_jac)
        top.driver.gradient_options.lin_solver = 'linear_gs'
        top._setup()
        top.run()
        
        J = top.driver.calc_gradient(mode='forward', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - top.comp.J)
        assert_rel_error(self, diff.max(), 0.0, 1e-4)
        
        J = top.driver.calc_gradient(mode='adjoint', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - fake_jac()['comp.x'])
        assert_rel_error(self, diff.max(), 0.0, 1e-4)

        # Test behavoir

        def fake_jac2():
            """ Returns a User-defined Jacobian. The values are
            totally wrong to facilitate testing. """
            jacs = {}
            jacs['Junk'] = np.array([[100.0, 101, 102, 103],
                                     [104, 105, 106, 107],
                                     [108, 109, 110, 111],
                                     [112, 113, 114, 115]])
    
            return jacs

        top.driver.clear_constraints()
        top._pseudo_count = 0
        top.driver.add_constraint('comp.y = 1', jacs=fake_jac2)
        top._setup()
        top.run()
    
        J = top.driver.calc_gradient(mode='forward', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        diff = np.abs(J - top.comp.J)
        assert_rel_error(self, diff.max(), 0.0, 1e-4)
    
        J = top.driver.calc_gradient(mode='adjoint', return_format='dict')
        J = J['_pseudo_0.out0']['comp.x']
        J_abs = np.abs(J)
        assert_rel_error(self, J_abs.max(), 0.0, 1e-4)



class Has2SidedConstraintsTestCase(unittest.TestCase):

    def setUp(self):
        self.asm = set_as_top(Assembly())
        self.asm.add('comp1', Simple())
        self.asm.add('comp2', Simple())
        self.asm.add('comp3', SimpleUnits())
        self.asm.add('comp4', SimpleUnits())

    def test_unsupported(self):
        drv = self.asm.add('driver', MyDriver())
        self.asm.run()
        try:
            drv.add_constraint('-98 < comp1.a < 101')
        except AttributeError as err:
            self.assertEqual(str(err), "driver: Double-sided constraints are not supported on this driver.")
        else:
            self.fail("Exception expected")

    def test_get_2sided_constraints(self):
        drv = self.asm.add('driver', My2SDriver())
        drv.add_constraint('-44.1 < comp1.a < 13.0')
        drv.add_constraint('77.0 < comp1.c < 79.0')
        self.asm.run()

        cons = drv.get_2sided_constraints()
        self.assertTrue(len(cons) == 2)
        con1 = cons['-44.1<comp1.a<13.0']
        self.assertEqual(self.asm.comp1.a, con1.evaluate(self.asm)[0])
        self.assertEqual(con1.low, -44.1)
        self.assertEqual(con1.high, 13.0)
        con1 = cons['77.0<comp1.c<79.0']
        self.assertEqual(self.asm.comp1.c, con1.evaluate(self.asm)[0])
        self.assertEqual(con1.low, 77.0)
        self.assertEqual(con1.high, 79.0)

        cons = drv.get_constraints()
        self.assertTrue(len(cons) == 0)

    def test_list_constraints(self):
        drv = self.asm.add('driver', My2SDriver())
        drv.add_constraint('-44.1 < comp1.a < 13.0')
        drv.add_constraint('77.0 < comp1.c')
        self.asm.run()

        cons = drv.list_constraints()
        self.assertTrue('-44.1<comp1.a<13.0' in cons)
        self.assertTrue('77.0<comp1.c' in cons)

    def test_gradient(self):
        drv = self.asm.add('driver', My2SDriver())
        drv.add_constraint('-44.1 < comp1.a < 13.0')
        drv.add_constraint('77.0 < -2.5*comp1.a')
        drv.add_constraint('55.0 > comp1.a > 52.0')
        drv.add_constraint('0.1 < 3.0*comp1.a < 1.5')
        self.asm.run()

        J = drv.calc_gradient(inputs=['comp1.a'])

        # ineq
        assert_rel_error(self, J[0][0], 2.5, 1e-5)

        # double sided (in order)
        assert_rel_error(self, J[1][0], 1.0, 1e-5)
        assert_rel_error(self, J[2][0], 1.0, 1e-5)
        assert_rel_error(self, J[3][0], 3.0, 1e-5)

    def test_replace(self):
        drv = self.asm.add('driver', My2SDriver())
        drv.add_constraint('-44.1 < comp1.a < 13.0')
        drv.add_constraint('77.0 < comp1.c < 79.0')
        self.asm.run()

        self.asm.replace('driver', My2SDriver())

if __name__ == "__main__":
    unittest.main()


