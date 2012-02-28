import unittest
import math
import ast

from openmdao.main.numpy_fallback import array
from openmdao.main.datatypes.array import Array
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.printexpr import ExprPrinter, transform_expression
from openmdao.main.api import Assembly, Container, Component, set_as_top
from openmdao.main.datatypes.api import Float, List, Slot, Dict


class A(Component):
    f = Float(iotype='in')
    a1d = Array(array([1.0, 1.0, 2.0, 3.0]), iotype='in')
    a2d = Array(array([[1.0, 1.0], [2.0, 3.0]]), iotype='in')
    b1d = Array(array([1.0, 1.0, 2.0, 3.0]), iotype='out')
    b2d = Array(array([[1.0, 1.0], [2.0, 3.0]]), iotype='out')
        
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
    
class Comp(Component):
    x = Float(iotype='in')
    y = Float(iotype='in')
    indct = Dict(iotype='in')
    outdct = Dict(iotype='out')
    cont = Slot(A, iotype='in')
    contlist = List(Slot(A), iotype='in')
    
    def get_cont(self, i):
        return self.contlist[i]
    
    def get_attr(self, name):
        return getattr(self, name)
    

class Simple(Component):
    
    a = Float(iotype='in')
    b = Float(iotype='in')
    c = Float(iotype='out')
    d = Float(iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 4.
        self.b = 5.
        self.c = 7.
        self.d = 1.5

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
        
        
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
            self.assertEqual(ex.new_text, tst[1])
            
    def test_eq(self): 
        ex1 = ExprEvaluator('comp.x', self.top)
        ex2 = ExprEvaluator('comp.x', self.top)
        ex3_bad = "test"
        
        self.assertTrue(ex1==ex2)
        self.assertTrue(ex2!=ex3_bad)
        
    def test_simple(self):
        tests = [
            ('a.f', "scope.get('a.f')"),
            ('a.f**2', "scope.get('a.f')**2"),
            ('a.f/a.a1d[int(a.f)]', "scope.get('a.f')/scope.get('a.a1d',[(0,int(scope.get('a.f')))])"),
            ('a.f = a.a1d[int(a.f)]', "scope.set('a.f',scope.get('a.a1d',[(0,int(scope.get('a.f')))]),src=_local_src_)"),
        ]
        self._do_tests(tests, self.top)
        
    def test_containers(self):
        tests = [
            ('comp.cont.f',"scope.get('comp.cont.f')"),
            ('comp.contlist[2].a1d[3]',"scope.get('comp.contlist',[(0,2),(1,'a1d'),(0,3)])"),
        ]
        self._do_tests(tests, self.top)
        
    def test_dicts(self):
        tests = [
            ("comp.indct['foo.bar']","scope.get('comp.indct',[(0,'foo.bar')])"),
            ("comp.indct['foo.bar']=comp.cont.f","scope.set('comp.indct',scope.get('comp.cont.f'),[(0,'foo.bar')],src=_local_src_)"),
        ]
        self._do_tests(tests, self.top)
        
    def test_arrays(self):
        tests = [
            ('a.a1d', "scope.get('a.a1d')"),
            ('-a.a1d', "-scope.get('a.a1d')"),
            ('+a.a1d', "+scope.get('a.a1d')"),
            ('a.a1d[0]', "scope.get('a.a1d',[(0,0)])"),
            ('a.a2d[0][0]', "scope.get('a.a2d',[(0,0),(0,0)])"),
            ('a.a2d[-a.a1d[2]]', "scope.get('a.a2d',[(0,-scope.get('a.a1d',[(0,2)]))])"),
            ('a.a2d[-a.a1d[2]][foo.bar]', 
             "scope.get('a.a2d',[(0,-scope.get('a.a1d',[(0,2)])),(0,scope.get('foo.bar'))])"),
            ('a.a2d[-a.a1d[2]]=a.f', 
             "scope.set('a.a2d',scope.get('a.f'),[(0,-scope.get('a.a1d',[(0,2)]))],src=_local_src_)"),
            ('a.f/a.a1d[int(a.f)]', "scope.get('a.f')/scope.get('a.a1d',[(0,int(scope.get('a.f')))])"),
            ('a.f = a.a1d[int(a.f)]', "scope.set('a.f',scope.get('a.a1d',[(0,int(scope.get('a.f')))]),src=_local_src_)"),
            ('a.b.cde[1+3**4*1]', "scope.get('a.b.cde',[(0,1+3**4*1)])"),
            ('a.b[1][2]', "scope.get('a.b',[(0,1),(0,2)])"),
            ('abs(a.b[1][2])', "abs(scope.get('a.b',[(0,1),(0,2)]))"),
            ('a.b[1][x.y]', "scope.get('a.b',[(0,1),(0,scope.get('x.y'))])"),  
            ('comp.x=a.b[1]',"scope.set('comp.x',scope.get('a.b',[(0,1)]),src=_local_src_)"),
            ('comp.cont.a1d[-3]', "scope.get('comp.cont.a1d',[(0,-3)])"),
        ]
        self._do_tests(tests, self.top)
        
    def test_calls(self):
        tests = [
        ('a.b()', "scope.get('a.b',[(2,)])"),
        ('a.b(5)', "scope.get('a.b',[(2,[5])])"),
        ('a.b(5,9)', "scope.get('a.b',[(2,[5,9])])"),
        ('a.b(5,z.y)', "scope.get('a.b',[(2,[5,scope.get('z.y')])])"),
        ('a.b(5, z.y(2,3))', 
         "scope.get('a.b',[(2,[5,scope.get('z.y',[(2,[2,3])])])])"),
        ('a.b(5, z.y[3])', 
         "scope.get('a.b',[(2,[5,scope.get('z.y',[(0,3)])])])"),
         ('a.b(1,23,foo=9)', 
          "scope.get('a.b',[(2,[1,23],[('foo',9)])])"),
         ('a.b(1,23)[1]', "scope.get('a.b',[(2,[1,23]),(0,1)])"),
         ('a.b(1).somefunct(2)[1]', "scope.get('a.b',[(2,[1]),(1,'somefunct'),(2,[2]),(0,1)])"),
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
        self.assertTrue(all(ex.evaluate() == array([11.,3.])))
        ex.set([0.1,0.2])
        self.assertTrue(all(self.top.a.a2d[1] == array([0.1,0.2])))
    
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
            self.assertTrue((val==numpy.eye(2)).all())
        
        ex = ExprEvaluator("comp.get_cont(1).a1d", self.top)
        self.assertEqual(list(ex.evaluate()), [4,4,4,123,4])
        
        ex = ExprEvaluator("comp.get_attr('get_cont')(1).a1d", self.top)
        self.assertEqual(list(ex.evaluate()), [4,4,4,123,4])
        
        
    def test_reparse_on_scope_change(self):
        self.top.comp.x = 99.5
        self.top.comp.y = -3.14
        
        ex = ExprEvaluator('comp.x', self.top)
        self.assertEqual(99.5, ex.evaluate())
        self.assertEqual(ex.new_text, "scope.get('comp.x')")
        
        ex.scope = self.top.a
        try:
            ex.set(0.5)
        except AttributeError as err:
            self.assertEqual(str(err), "a: object has no attribute 'comp.x'")
        else:
            self.fail("AttributeError expected")
        self.assertEqual(ex.new_text, "scope.get('comp.x')")
        self.assertEqual(99.5, ex.evaluate(self.top)) # set scope back to self.top
        self.assertEqual(ex.new_text, "scope.get('comp.x')")
        
        ex.text = 'comp.y'
        try:
            ex.evaluate(self.top.a)
        except AttributeError as err:
            self.assertEqual(str(err), "can't evaluate expression 'comp.y': a: 'A' object has no attribute 'comp'")
        else:
            self.fail("AttributeError expected")
        ex.scope = self.top
        ex.set(11.1)
        self.assertEqual(11.1, self.top.comp.y)
        self.assertEqual(ex.new_text, "scope.get('comp.y')")
        
    def test_no_scope(self):
        ex = ExprEvaluator('abs(-3)+int(2.3)+math.floor(5.4)')
        self.assertEqual(ex.evaluate(), 10.0)
        
        ex.text = 'comp.x'
        try:
            ex.evaluate()
        except Exception, err:
            self.assertEqual(str(err), "can't evaluate expression 'comp.x': expression has no scope")
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
        self.assertEqual(ex.get_referenced_varpaths(), set(['comp.x','a.a1d']))
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
        ex = ExprEvaluator('comp.x[0] = 10*(3.2+ a.a1d[3]* 1.1*a.a1d[2 ].foobar)', self.top.a)
        self.assertEqual(ex.get_referenced_compnames(), set(['comp','a']))
        ex.text = 'comp.contlist[1].a2d[2][1]'
        self.assertEqual(ex.get_referenced_compnames(), set(['comp']))
        ex.scope = self.top.comp
        ex.text = 'comp.contlist[1]'
        self.assertEqual(ex.get_referenced_compnames(), set(['comp']))
        ex.text = 'comp.contlist[1].foo'
        self.assertEqual(ex.get_referenced_compnames(), set(['comp']))
        ex.text = 'contlist[1].foo'
        self.assertEqual(ex.get_referenced_compnames(), set())
        ex.text = 'asm2.comp3.contlist[1].foo'
        self.assertEqual(ex.get_referenced_compnames(), set(['asm2']))
        
    def test_slice(self):
        ex = ExprEvaluator('a1d[1::2]', self.top.a)
        self.assertTrue(all(array([2.,4.,6.]) == ex.evaluate()))
        ex.text = 'a1d[2:4]'
        self.assertTrue(all(array([3.,4.]) == ex.evaluate()))
        ex.text = 'a1d[2:]'
        self.assertTrue(all(array([3.,4.,5.,6.]) == ex.evaluate()))
        ex.text = 'a1d[::-1]'
        self.assertTrue(all(array([6.,5.,4.,3.,2.,1.]) == ex.evaluate()))
        ex.text = 'a1d[:2]'
        self.assertTrue(all(array([1.,2.]) == ex.evaluate()))

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
        
        self.top.a.b = [1,1,1,1]
        self.assertEqual(True, ExprEvaluator('all(a.b)', self.top).evaluate())
        self.assertEqual(True, ExprEvaluator('any(a.b)', self.top).evaluate())
        self.top.a.b = [1,1,0,1]
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
            self.assertEqual(str(err), 
                "can't evaluate expression 'abcd.efg': : 'Assembly' object has no attribute 'abcd'")
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
        
        top.connect('comp1.c','comp3.a')
        top.connect('comp2.c','comp3.b')
        top.connect('comp3.c','comp5.a')
        top.connect('comp3.d','comp9.a')
        top.connect('comp3.d','comp4.a')
        top.connect('comp4.c','comp7.a')
        top.connect('comp3.c','comp6.a')
        top.connect('comp6.c','comp7.b')
        top.connect('comp8.c','comp9.b')
        
        exp = ExprEvaluator('comp9.c+comp5.d', top.driver)
        self.assertEqual(exp.get_required_compnames(top),
                         set(['comp1','comp2','comp3','comp5','comp8','comp9']))
        exp = ExprEvaluator('comp7.a', top.driver)
        self.assertEqual(exp.get_required_compnames(top),
                         set(['comp1','comp2','comp3','comp4','comp6','comp7']))
        exp = ExprEvaluator('comp8.a', top.driver)
        self.assertEqual(exp.get_required_compnames(top),
                         set(['comp8']))
        exp = ExprEvaluator('comp9.c+comp7.d', top.driver)
        self.assertEqual(exp.get_required_compnames(top),
                         set(['comp1','comp2','comp3','comp4','comp6',
                              'comp7','comp8','comp9']))
        exp = ExprEvaluator('sin(0.3)', top.driver)
        self.assertEqual(exp.get_required_compnames(top),
                         set())
        
    def test_scope_transform(self):
        exp = ExprEvaluator('var+abs(comp.x)*a.a1d[2]', self.top)
        self.assertEqual(exp.new_text, "scope.get('var')+abs(scope.get('comp.x'))*scope.get('a.a1d',[(0,2)])")
        xformed = exp.scope_transform(self.top, self.top.comp)
        self.assertEqual(xformed, 'parent.var+abs(x)*parent.a.a1d[2]')
        
        exp = ExprEvaluator('parent.var+abs(x)*parent.a.a1d[2]', self.top.comp)
        xformed = exp.scope_transform(self.top.comp, self.top)
        self.assertEqual(xformed, 'var+abs(comp.x)*a.a1d[2]')
        
        
if __name__ == "__main__":
    unittest.main()
    
    
