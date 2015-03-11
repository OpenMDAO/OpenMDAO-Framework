import unittest
import math
import ast

from numpy import array, eye, arange, roll, tile
from openmdao.main.datatypes.array import Array
from openmdao.main.expreval import ExprEvaluator, ConnectedExprEvaluator, \
                                   ExprExaminer
from openmdao.main.printexpr import ExprPrinter, print_node, transform_expression
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, List, Slot, Dict
from openmdao.util.testutil import assert_rel_error

from math import sqrt, sin, cos

try:
    # as of python2.7, gamma is in the math module
    # (even though docs say it's new as of 3.2)
    from math import gamma
except ImportError as err:
    from scipy.special import gamma

class A(Component):
    f = Float(iotype='in')
    a1d = Array(array([1.0, 1.0, 2.0, 3.0]), iotype='in')
    a2d = Array(array([[1.0, 1.0], [2.0, 3.0]]), iotype='in')
    b1d = Array(array([1.0, 1.0, 2.0, 3.0]), iotype='out')
    b2d = Array(array([[1.0, 1.0], [2.0, 3.0]]), iotype='out')
    c1d = Array(array([0.0, 1.0, 2.0, 3.0]), iotype='in')
    c2d = Array(array([[0.0, 1.0], [2.0, 3.0]]), iotype='in')

    ext1 = Array(array([eye(3), eye(3), eye(3)]))

    def execute(self):
        pass

    def some_funct(self, a, b, op='add'):
        if op == 'add':
            return a+b
        elif op == 'mult':
            return a*b
        elif op == 'sub':
            return a-b
        raise RuntimeError("bad input to some_funct")

    @property
    def some_prop(self):
        return 7

class B(Component):

    in1 = Float(iotype="in")
    in11 = Float(iotype="in")

class Comp(Component):
    x = Float(iotype='in')
    y = Float(iotype='in')
    indct = Dict(iotype='in')
    outdct = Dict(iotype='out')
    cont = Slot(A)
    contlist = List(Slot(A), iotype='in')

    def get_cont(self, i):
        return self.contlist[i]

    def get_attrib(self, name):
        return getattr(self, name)


class Simple(Component):

    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')
    x_array = Array([0, 0, 0], iotype='in')
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b

        self.x_array[1] = self.a*self.b


def new_text(expr):
    ep = ExprPrinter()
    ep.visit(expr._parse_get()[0])
    return ep.get_text()

class ExprPrinterTestCase(unittest.TestCase):
    def test_print_node(self):
        checks = [
            'a',
            'a+b',
            'a-b',
            'a*b',
            'a/b',
            'a-(b-c)',
            'a+b*c',
            'a[0]',
            'a[0::]',
            'a[:0:]',
            'a[::0]',
            'a[(0,0)]',
            'a[::,0]',
            'a[0,::]'
            ]
        for check in checks:
            node = ast.parse(check, mode='eval')
            self.assertEqual(check, print_node(node))

class ExprEvalTestCase(unittest.TestCase):
    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('a', A())
        self.top.a.a1d = [1., 2, 3, 4, 5, 6]
        self.top.add('comp', Comp())
        self.top.comp.x = 3.14
        self.top.comp.y = 42.

    def _do_tests(self, tests, top):
        # each test is a tuple of the form (input, expected output)
        for tst in tests:
            ex = ExprEvaluator(tst[0], top)
            self.assertEqual(new_text(ex), tst[1])

    def test_eq(self):
        ex1 = ExprEvaluator('comp.x', self.top)
        ex2 = ExprEvaluator('comp.x', self.top)
        ex3_bad = "test"


        self.assertTrue(ex1 == ex2)
        self.assertTrue(ex2 != ex3_bad)


    def test_simple(self):
        tests = [
            ('a.f', "scope.get('a.f')"),
            ('a.f**2', "scope.get('a.f')**2"),
            ('a.f/a.a1d[int(a.f)]',
             "scope.get('a.f')/scope.get('a.a1d[int(a.f)]')"),
            ('a.f = a.a1d[int(a.f)]',
             "scope.set('a.f',scope.get('a.a1d[int(a.f)]'))"),
        ]
        self._do_tests(tests, self.top)

    def test_containers(self):
        tests = [
            ('comp.cont.f', "scope.get('comp.cont.f')"),
            ('comp.contlist[2].a1d[3]',
             "scope.get('comp.contlist[2].a1d[3]')"),
        ]
        self._do_tests(tests, self.top)

    def test_dicts(self):
        tests = [
            ("comp.indct['foo.bar']", "scope.get('comp.indct['foo.bar']')"),
            ("comp.indct['foo.bar']=comp.cont.f",
             "scope.set('comp.indct['foo.bar']',scope.get('comp.cont.f'))"),
        ]
        self._do_tests(tests, self.top)

    def test_arrays(self):
        tests = [
            ('a.a1d', "scope.get('a.a1d')"),
            ('-a.a1d', "-scope.get('a.a1d')"),
            ('+a.a1d', "+scope.get('a.a1d')"),
            ('a.a1d[0]', "scope.get('a.a1d[0]')"),
            ('a.a2d[0][0]', "scope.get('a.a2d[0][0]')"),
            ('a.a2d[:,0]', "scope.get('a.a2d[::,0]')"),
            ('a.a2d[-a.a1d[2]]', "scope.get('a.a2d[-a.a1d[2]]')"),
            ('a.a2d[-a.a1d[2]][foo.bar]', "scope.get('a.a2d[-a.a1d[2]][foo.bar]')"),
            ('a.a2d[-a.a1d[2]]=a.f', "scope.set('a.a2d[-a.a1d[2]]',scope.get('a.f'))"),
            ('a.f/a.a1d[int(a.f)]', "scope.get('a.f')/scope.get('a.a1d[int(a.f)]')"),
            ('a.f = a.a1d[int(a.f)]',
             "scope.set('a.f',scope.get('a.a1d[int(a.f)]'))"),
            ('a.b.cde[1+3**4*1]', "scope.get('a.b.cde[1+3**4*1]')"),
            ('a.b[1][2]', "scope.get('a.b[1][2]')"),
            ('abs(a.b[1][2])', "abs(scope.get('a.b[1][2]'))"),
            ('a.b[1][x.y]', "scope.get('a.b[1][x.y]')"),
            ('comp.x=a.b[1]',
             "scope.set('comp.x',scope.get('a.b[1]'))"),
            ('comp.cont.a1d[-3]', "scope.get('comp.cont.a1d[-3]')"),
        ]
        self._do_tests(tests, self.top)

    def test_calls(self):
        tests = [
            ('a.b()', "scope.get('a.b')()"),
            ('a.b(5)', "scope.get('a.b')(5)"),
            ('a.b(5,9)', "scope.get('a.b')(5,9)"),
            ('a.b(5,z.y)', "scope.get('a.b')(5,scope.get('z.y'))"),
            ('a.b(5, z.y(2,3))', "scope.get('a.b')(5,scope.get('z.y')(2,3))"),
            ('a.b(5, z.y[3])', "scope.get('a.b')(5,scope.get('z.y[3]'))"),
            ('a.b(1,23,foo=9)', "scope.get('a.b')(1,23,foo=9)"),
            ('a.b(1,23)[1]', "scope.get('a.b(1,23)[1]')"),
            ('a.b(1).somefunct(2)[1]', "scope.get('a.b(1).somefunct(2)[1]')"),
        ]

        self._do_tests(tests, self.top)

    def test_set_evaluate(self):
        ex = ExprEvaluator('comp.x', self.top)
        self.assertEqual(3.14, ex.evaluate())

        ex.set(75.4)
        self.assertEqual(75.4, self.top.comp.x)

        self.top.comp.contlist = [A(), A(), A()]
        self.top.comp.contlist[1].a1d = [4]*5
        ex = ExprEvaluator('comp.contlist[1].a1d[3]', self.top)
        self.assertEqual(ex.evaluate(), 4)

        ex.set(123)
        self.assertEqual(ex.evaluate(), 123)

        ex = ExprEvaluator("comp.contlist[1].some_funct(3,5,'sub')", self.top)
        self.assertEqual(ex.evaluate(), -2)

        ex = ExprEvaluator("comp.get_cont(1).some_funct(3,5,'add')", self.top)
        self.assertEqual(ex.evaluate(), 8)

        ex = ExprEvaluator("comp.get_cont(1).a1d[2]", self.top)
        self.assertEqual(ex.evaluate(), 4)

        ex = ExprEvaluator("a2d[1][0]", self.top.a)
        self.assertEqual(ex.evaluate(), 2.)
        ex.set(7.)
        self.assertEqual(self.top.a.a2d[1][0], 7.)

        ex = ExprEvaluator("a2d[1,0]", self.top.a)
        self.assertEqual(ex.evaluate(), 7.)
        ex.set(11.)
        self.assertEqual(self.top.a.a2d[1][0], 11.)

        ex = ExprEvaluator("a2d[1]", self.top.a)

        self.assertTrue(all(ex.evaluate() == array([11., 3.])))
        ex.set([0.1, 0.2])
        self.assertTrue(all(self.top.a.a2d[1] == array([0.1, 0.2])))


        self.top.comp.cont = A()

        ex = ExprEvaluator("comp.cont.a2d[1][0]", self.top)
        self.assertEqual(ex.evaluate(), 2.)
        ex.set(7.)
        self.assertEqual(self.top.comp.cont.a2d[1][0], 7.)

        ex = ExprEvaluator("comp.cont.a2d[1,0]", self.top)
        self.assertEqual(ex.evaluate(), 7.)
        ex.set(11.)
        self.assertEqual(self.top.comp.cont.a2d[1][0], 11.)

        # try a numpy function
        try:
            import numpy
        except ImportError:
            pass
        else:
            ex = ExprEvaluator("numpy.eye(2)", self.top.a)
            val = ex.evaluate()

            self.assertTrue((val == numpy.eye(2)).all())

        ex = ExprEvaluator("comp.get_cont(1).a1d", self.top)
        self.assertEqual(list(ex.evaluate()), [4, 4, 4, 123, 4])

        ex = ExprEvaluator("comp.get_attrib('get_cont')(1).a1d", self.top)
        self.assertEqual(list(ex.evaluate()), [4, 4, 4, 123, 4])


    def test_reparse_on_scope_change(self):
        self.top.comp.x = 99.5
        self.top.comp.y = -3.14

        ex = ExprEvaluator('comp.x', self.top)
        self.assertEqual(99.5, ex.evaluate())
        self.assertEqual(new_text(ex), "scope.get('comp.x')")

        ex.scope = self.top.a
        try:
            ex.set(0.5)
        except AttributeError as err:
            self.assertEqual(str(err), "a: 'A' object has no attribute 'comp'")
        else:
            self.fail("AttributeError expected")
        self.assertEqual(new_text(ex), "scope.get('comp.x')")
        self.assertEqual(99.5, ex.evaluate(self.top)) # set scope back to self.top
        self.assertEqual(new_text(ex), "scope.get('comp.x')")

        ex.text = 'comp.y'
        try:
            ex.evaluate(self.top.a)
        except AttributeError as err:
            self.assertEqual(str(err), "can't evaluate expression 'comp.y':"
                             " a: name 'comp' is not defined")
        else:
            self.fail("AttributeError expected")
        ex.scope = self.top
        ex.set(11.1)
        self.assertEqual(11.1, self.top.comp.y)
        self.assertEqual(new_text(ex), "scope.get('comp.y')")

    def test_no_scope(self):
        ex = ExprEvaluator('abs(-3)+int(2.3)+math.floor(5.4)')
        self.assertEqual(ex.evaluate(), 10.0)

        ex.text = 'comp.x'
        try:
            ex.evaluate()
        except Exception, err:
            self.assertEqual(str(err), "can't evaluate expression 'comp.x':"
                             " 'NoneType' object has no attribute 'get'")
        else:
            self.fail("Exception expected")

    def test_property(self):
        ex = ExprEvaluator('some_prop', self.top.a)
        self.assertEqual(ex.evaluate(), 7)

    def test_assignee(self):
        ex = ExprEvaluator('a1d[3]*a1d[2 ]', self.top.a)
        self.assertEqual(ex.is_valid_assignee(), False)
        ex.text = 'comp.contlist[1].a2d[2][1]'
        self.assertEqual(ex.is_valid_assignee(), True)

    def test_resolve(self):
        ex = ExprEvaluator('comp.x[0] = 10*(3.2+ a.a1d[3]* 1.1*a.a1d[2 ])', self.top)
        self.assertEqual(ex.check_resolve(), True)
        ex.text = 'comp.contlist[1].a2d[2][1]'
        self.assertEqual(ex.check_resolve(), True)
        ex.scope = self.top.comp
        ex.text = 'contlist[1]'
        self.assertEqual(ex.check_resolve(), True)
        ex.text = 'contlist[1]-foo.flambe'
        self.assertEqual(ex.check_resolve(), False)

    def test_get_referenced_varpaths(self):
        ex = ExprEvaluator('comp.x[0] = 10*(3.2+ a.a1d[3]* 1.1*a.a1d[2 ].foobar)', self.top.a)
        self.assertEqual(ex.get_referenced_varpaths(), set(['comp.x', 'a.a1d']))
        ex.text = 'comp.contlist[1].a2d[2][1]'
        self.assertEqual(ex.get_referenced_varpaths(), set(['comp.contlist']))
        ex.scope = self.top.comp
        ex.text = 'comp.contlist[1]'
        self.assertEqual(ex.get_referenced_varpaths(), set(['comp.contlist']))
        ex.text = 'comp.contlist[1].foo'
        self.assertEqual(ex.get_referenced_varpaths(), set(['comp.contlist']))
        ex.text = 'contlist[1].foo'
        self.assertEqual(ex.get_referenced_varpaths(), set(['contlist']))
        ex.text = 'asm2.comp3.contlist[1].foo'
        self.assertEqual(ex.get_referenced_varpaths(), set(['asm2.comp3.contlist']))

    def test_get_referenced_compnames(self):
        ex = ExprEvaluator('comp.x[0] = 10*(3.2+ a.a1d[3]* 1.1*a.a1d[2 ].foobar)', self.top)
        self.assertEqual(ex.get_referenced_compnames(), set(['comp', 'a']))
        ex.text = 'comp.contlist[1].a2d[2][1]'
        self.assertEqual(ex.get_referenced_compnames(), set(['comp']))
        ex.scope = self.top
        ex.text = 'comp.contlist[1]'
        self.assertEqual(ex.get_referenced_compnames(), set(['comp']))
        ex.text = 'comp.contlist[1].foo'
        self.assertEqual(ex.get_referenced_compnames(), set(['comp']))
        ex.text = 'contlist[1].foo'
        self.assertEqual(ex.get_referenced_compnames(), set())
        ex.text = 'a.comp3.contlist[1].foo'
        self.assertEqual(ex.get_referenced_compnames(), set(['a']))

    def test_slice(self):
        ex = ExprEvaluator('a1d[1::2]', self.top.a)
        self.assertTrue(all(array([2., 4., 6.]) == ex.evaluate()))
        ex.text = 'a1d[2:4]'
        self.assertTrue(all(array([3., 4.]) == ex.evaluate()))
        ex.text = 'a1d[2:]'
        self.assertTrue(all(array([3., 4., 5., 6.]) == ex.evaluate()))
        ex.text = 'a1d[::-1]'
        self.assertTrue(all(array([6., 5., 4., 3., 2., 1.]) == ex.evaluate()))
        ex.text = 'a1d[:2]'
        self.assertTrue(all(array([1., 2.]) == ex.evaluate()))
        ex.text = 'a2d[:,0]'
        self.assertTrue(all(array([1., 2.]) == ex.evaluate()))
        ex.text = 'a2d[:,1]'
        self.assertTrue(all(array([1., 3.]) == ex.evaluate()))
        ex.text = 'a2d[:,-1]'
        self.assertTrue(all(array([1., 3.]) == ex.evaluate()))
        ex.text = 'a2d[:,-2]'
        self.assertTrue(all(array([1., 2.]) == ex.evaluate()))

    def test_ext_slice(self):
        #Convoluted mess to test all cases of 3 x 3 x 3 array
        #where inner arrays are 2D identity matrices
        #Should cover all cases
        for k in xrange(3):
            expr = 'ext1[{0}, :, :]'.format(k)
            expr = ExprEvaluator(expr, self.top.a)
            comp = (eye(3) == expr.evaluate()).all()
            self.assertTrue(comp)

            expr = 'ext1[:, {0}, :]'.format(k)
            expr = ExprEvaluator(expr, self.top.a)
            comp = (tile(roll(array([1., 0., 0.]), k), (3, 1)) == expr.evaluate()).all()
            self.assertTrue(comp)

            expr = 'ext1[:, :, {0}]'.format(k)
            expr = ExprEvaluator(expr, self.top.a)
            comp = (tile(roll(array([1., 0., 0.]), k), (3, 1)) == expr.evaluate()).all()
            self.assertTrue(comp)

            for j in xrange(3):
                expr = 'ext1[:, {0}, {1}]'.format(j, k)
                expr = ExprEvaluator(expr, self.top.a)

                if j == k:
                    comp = all(array([1., 1., 1.]) == expr.evaluate())
                    self.assertTrue(comp)
                else:
                    comp = all(array([0., 0., 0.]) == expr.evaluate())
                    self.assertTrue(comp)

                expr = 'ext1[{0}, :, {1}]'.format(j, k)
                expr = ExprEvaluator(expr, self.top.a)
                arr = array([1., 0., 0.])
                comp = all(roll(arr, k) == expr.evaluate())
                self.assertTrue(comp)

                expr = 'ext1[{0}, {1}, :]'.format(j, k)
                expr = ExprEvaluator(expr, self.top.a)
                arr = array([1., 0., 0.])
                comp = all(roll(arr, k) == expr.evaluate())
                self.assertTrue(comp)

                for i in xrange(3):
                    expr = 'ext1[{0}, {1}, {2}]'.format(i, j, k)
                    expr = ExprEvaluator(expr, self.top.a)

                    if j == k:
                        comp = all(array([1.]) == expr.evaluate())
                        self.assertTrue(comp)
                    else:
                        comp = all(array([0.]) == expr.evaluate())
                        self.assertTrue(comp)

    def test_boolean(self):
        comp = self.top.comp
        comp.x = 1.
        comp.y = 3.
        self.assertEqual(True, ExprEvaluator('comp.x < comp.y', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('comp.x <= comp.y', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('comp.x != comp.y', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('comp.x == comp.y', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('comp.x > comp.y', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('comp.x >= comp.y', self.top).evaluate())

        self.assertEqual(True, ExprEvaluator('1< comp.y', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('1<= comp.y', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('1!= comp.y', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('1== comp.y', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('1> comp.y', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('1>= comp.y', self.top).evaluate())

        self.assertEqual(True, ExprEvaluator('comp.x < 3', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('comp.x <= 3', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('comp.x != 3', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('comp.x == 3', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('comp.x > 3', self.top).evaluate())
        self.assertEqual(False, ExprEvaluator('comp.x >= 3', self.top).evaluate())

        comp.x = 3.
        self.assertEqual(False, ExprEvaluator('comp.x != comp.y', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('comp.x == comp.y', self.top).evaluate())


        self.top.a.b = [1, 1, 1, 1]

        self.assertEqual(True, ExprEvaluator('all(a.b)', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('any(a.b)', self.top).evaluate())
        self.top.a.b = [1, 1, 0, 1]
        self.assertEqual(False, ExprEvaluator('all(a.b)', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('any(a.b)', self.top).evaluate())

    def test_builtins(self):
        comp = self.top.comp
        comp.x = 1.
        comp.y = -3.
        self.assertEqual(3., ExprEvaluator('abs(comp.y)', self.top).evaluate())
        self.assertAlmostEqual(0., ExprEvaluator('sin(pi)', self.top).evaluate())
        comp.x = 1.35
        self.assertEqual(1., ExprEvaluator('floor(comp.x)', self.top).evaluate())
        self.assertEqual(2., ExprEvaluator('ceil(comp.x)', self.top).evaluate())
        comp.x = 0.
        self.assertEqual(True, ExprEvaluator('sin(comp.x)<math.cos(comp.x)', self.top).evaluate())
        comp.x = math.pi/2.
        self.assertEqual(False, ExprEvaluator('sin(comp.x)<cos(comp.x)', self.top).evaluate())

    def test_multi_object(self):
        # verify that expressions with multiple objects raise a reasonable error message
        # when a set is attempted.
        try:
            ex = ExprEvaluator('comp.x+comp.x', self.top)
            ex.set(1)
        except ValueError, err:
            self.assertEqual(str(err),
                "expression 'comp.x+comp.x' can't be set to a value")
        else:
            raise AssertionError('ValueError expected')

    def test_bogus(self):
        # now try some bogus expressions
        try:
            ex = ExprEvaluator('abcd.efg', self.top)
            ex.evaluate()
        except AttributeError, err:

            self.assertEqual(str(err), "can't evaluate expression 'abcd.efg':"
                             " : name 'abcd' is not defined")

        else:
            raise AssertionError('AttributeError expected')

    def test_get_required_comps(self):
        top = set_as_top(Assembly())
        top.add('comp1', Simple())
        top.add('comp2', Simple())
        top.add('comp3', Simple())
        top.add('comp4', Simple())
        top.add('comp5', Simple())
        top.add('comp6', Simple())
        top.add('comp7', Simple())
        top.add('comp8', Simple())
        top.add('comp9', Simple())

        top.connect('comp1.c', 'comp3.a')
        top.connect('comp2.c', 'comp3.b')
        top.connect('comp3.c', 'comp5.a')
        top.connect('comp3.d', 'comp9.a')
        top.connect('comp3.d', 'comp4.a')
        top.connect('comp4.c', 'comp7.a')
        top.connect('comp3.c', 'comp6.a')
        top.connect('comp6.c', 'comp7.b')
        top.connect('comp8.c', 'comp9.b')

    def test_eval_gradient(self):
        top = set_as_top(Assembly())
        top.add('comp1', Simple())
        top.comp1.a = 5.2
        top.comp1.b = 1.8

        top.run()

        exp = ExprEvaluator('3.0*comp1.c', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        assert_rel_error(self, grad['comp1.c'], 3.0, 0.00001)
        self.assertEqual(top.comp1.c, 7.0)

        # Commented out this test, until we find a case that can't be
        # handled analytically
        # interface test: step size
        # (for linear slope, larger stepsize more accurate because of
        # python's rounding)
        #grad2 = exp.evaluate_gradient(scope=top, stepsize=0.1)
        #assert( abs(grad['comp1.c'] - 3.0) > abs(grad2['comp1.c'] - 3.0) )

        # More complicated, multiple comps
        top.add('comp2', Simple())

        exp = ExprEvaluator('comp2.b*comp1.c**2', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        self.assertEqual(len(grad), 2)
        assert_rel_error(self, grad['comp1.c'], 70.0, 0.00001)
        assert_rel_error(self, grad['comp2.b'], 49.0, 0.00001)

        # test limited varset
        grad = exp.evaluate_gradient(scope=top, wrt=['comp2.b'])
        self.assertEqual(len(grad), 1)

        exp = ExprEvaluator('pow(comp2.b,2)', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        assert_rel_error(self, grad['comp2.b'], 10.0, 0.00001)

        exp = ExprEvaluator('pow(comp2.b,3)', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        assert_rel_error(self, grad['comp2.b'], 75.0, 0.00001)

        exp = ExprEvaluator('log(comp2.a)', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        assert_rel_error(self, grad['comp2.a'], 1./top.comp2.a, 0.00001)

        exp = ExprEvaluator('sin(cos(comp2.b))+sqrt(comp2.a)/comp1.c', top.driver)
        grad = exp.evaluate_gradient(scope=top)

        g1 = -sin(top.comp2.b)*cos(cos(top.comp2.b)) #true gradient components
        g2 = (2*sqrt(top.comp2.a)*top.comp1.c)**-1
        g3 = -sqrt(top.comp2.a)/top.comp1.c**2


        assert_rel_error(self, grad['comp2.b'], g1, 0.00001)
        assert_rel_error(self, grad['comp2.a'], g2, 0.00001)
        assert_rel_error(self, grad['comp1.c'], g3, 0.00001)

        exp = ExprEvaluator('gamma(comp2.a)', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        from scipy.special import polygamma

        g1 = gamma(top.comp2.a)*polygamma(0, top.comp2.a) #true partial derivative

        assert_rel_error(self, grad['comp2.a'], g1, 0.001)

        exp = ExprEvaluator('abs(comp2.a)', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        assert_rel_error(self, grad['comp2.a'], 1.0, 0.0001)


    def test_eval_gradient_array(self):
        top = set_as_top(Assembly())
        top.add('comp1', A())
        top.run()

        # Uncomment these when arrays work
        exp = ExprEvaluator('4.0*comp1.b2d[0][1]*comp1.b2d[1][1]', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        assert_rel_error(self, grad['comp1.b2d[0][1]'], 12.0, 0.00001)
        assert_rel_error(self, grad['comp1.b2d[1][1]'], 4.0, 0.00001)

        exp = ExprEvaluator('comp1.c2d**2', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        assert_rel_error(self, grad['comp1.c2d'][0,0], 0.0, 0.00001)
        assert_rel_error(self, grad['comp1.c2d'][1,1], 2.0, 0.00001)
        assert_rel_error(self, grad['comp1.c2d'][2,2], 4.0, 0.00001)
        assert_rel_error(self, grad['comp1.c2d'][3,3], 6.0, 0.00001)

        exp = ExprEvaluator('comp1.c1d**2', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        assert_rel_error(self, grad['comp1.c1d'][0,0], 0.0, 0.00001)
        assert_rel_error(self, grad['comp1.c1d'][1,1], 2.0, 0.00001)
        assert_rel_error(self, grad['comp1.c1d'][2,2], 4.0, 0.00001)
        assert_rel_error(self, grad['comp1.c1d'][3,3], 6.0, 0.00001)

        exp = ExprEvaluator('comp1.a2d + comp1.c2d**2', top.driver)
        grad = exp.evaluate_gradient(scope=top)
        a2d_grad, c2d_grad = grad['comp1.a2d'], grad['comp1.c2d']
        assert_rel_error(self, a2d_grad[0,0], 1.0, 0.00001)
        assert_rel_error(self, a2d_grad[1,1], 1.0, 0.00001)
        assert_rel_error(self, a2d_grad[2,2], 1.0, 0.00001)
        assert_rel_error(self, a2d_grad[3,3], 1.0, 0.00001)

        assert_rel_error(self, c2d_grad[0,0], 0.0, 0.00001)
        assert_rel_error(self, c2d_grad[1,1], 2.0, 0.00001)
        assert_rel_error(self, c2d_grad[2,2], 4.0, 0.00001)
        assert_rel_error(self, c2d_grad[3,3], 6.0, 0.00001)

    def test_eval_gradient_lots_of_vars(self):
        top = set_as_top(Assembly())
        top.add('comp1', B())
        #build expr
        expr = "2*comp1.in1 + 3*comp1.in11"

        exp = ExprEvaluator(expr, top.driver)
        grad = exp.evaluate_gradient(scope=top)

        assert_rel_error(self, grad['comp1.in1'], 2.0, 0.00001)
        assert_rel_error(self, grad['comp1.in11'], 3.0, 0.00001)

        expr = "asin(comp1.in1)"
        exp = ExprEvaluator(expr, top.driver)
        grad = exp.evaluate_gradient(scope=top)

        assert_rel_error(self, grad['comp1.in1'], 1.0, 0.00001)

    def test_connected_expr(self):
        ConnectedExprEvaluator("var1[x]", self.top)._parse()
        try:
            ConnectedExprEvaluator("var1[x]", self.top, is_dest=True)._parse()
        except Exception as err:
            self.assertEqual(str(err), "bad destination expression 'var1[x]':"
                     " only constant indices are allowed for arrays and slices")
        else:
            self.fail("Exception expected")

        ConnectedExprEvaluator("var1(2.3)", self.top)._parse()
        try:
            ConnectedExprEvaluator("var1(2.3)", self.top, is_dest=True)._parse()
        except Exception as err:
            self.assertEqual(str(err), "bad destination expression 'var1(2.3)':"
                             " not assignable")
        else:
            self.fail("Exception expected")

        ConnectedExprEvaluator("var1[1:5:2]", self.top)._parse()
        ConnectedExprEvaluator("var1[1:5:2]", self.top, is_dest=True)._parse()

        ConnectedExprEvaluator("var1[1:x:2]", self.top)._parse()
        try:
            ConnectedExprEvaluator("var1[1:x:2]", self.top, is_dest=True)._parse()
        except Exception as err:
            self.assertEqual(str(err), "bad destination expression"
                             " 'var1[1:x:2]': only constant indices are allowed"
                             " for arrays and slices")
        else:
            self.fail("Exception expected")

class ExprExaminerTestCase(unittest.TestCase):

    def _examine(self, text, simplevar=True, assignable=True,
                 const_indices=True, refs=None, const=False):

        ee = ExprExaminer(ast.parse(text, mode='eval'))
        self.assertEqual(ee.simplevar, simplevar)
        self.assertEqual(ee.assignable, assignable)
        self.assertEqual(ee.const_indices, const_indices)
        self.assertEqual(ee.const, const)
        if refs is None:
            refs = set()
        self.assertEqual(refs, ee.refs)
        return ee

    def test_exprs(self):
        self._examine("7", simplevar=False, assignable=False, const=True)
        self._examine("7+6", simplevar=False, assignable=False, const=True)
        self._examine("7+6/(8-4)*13", simplevar=False, assignable=False, const=True)
        self._examine("x", refs=set(['x']))
        self._examine("x[2]", simplevar=False, refs=set(['x[2]']))
        self._examine("x[2]+x", simplevar=False, assignable=False, refs=set(['x[2]', 'x']))
        self._examine("x[2]*y[4]", simplevar=False, assignable=False,
                      refs=set(['x[2]', 'y[4]']))
        self._examine("x[y]", simplevar=False, const_indices=False, refs=set(['x[y]']))
        self._examine("x[y[5]]", simplevar=False, const_indices=False,
                      refs=set(['x[y[5]]']))
        self._examine("x[1:4:2]", simplevar=False, refs=set(['x[1:4:2]']))
        self._examine("x[1:4:y]", simplevar=False, const_indices=False, refs=set(['x[1:4:y]']))
        self._examine("x[::,0,0]", simplevar=False, refs=set(['x[::,0,0]']))
        self._examine("x[0,::,0]", simplevar=False, refs=set(['x[0,::,0]']))
        self._examine("x[0,0,::]", simplevar=False, refs=set(['x[0,0,::]']))
        #self._examine("x[:,0,0]", simplevar=False, const_indices=False, refs=set(['x[:,0,0]']))
        self._examine("x+y", simplevar=False, assignable=False, refs=set(['x', 'y']))
        self._examine("x*y", simplevar=False, assignable=False, refs=set(['x', 'y']))
        self._examine("x()", simplevar=False, assignable=False, refs=set(['x']))
        self._examine("x(7)", simplevar=False, assignable=False, refs=set(['x']))
        self._examine("x==6", simplevar=False, assignable=False, refs=set(['x']))


class TransformTestCase(unittest.TestCase):

    def test_xforms(self):
        tests = [
            ('abc.de.g.abc', {'abc':'ABC'}, 'ABC.de.g.abc'),
        ]

        for orig, mapping, expected in tests:
            xformed = transform_expression(orig, mapping)
            self.assertEqual(expected, xformed)




if __name__ == "__main__":
    unittest.main()
