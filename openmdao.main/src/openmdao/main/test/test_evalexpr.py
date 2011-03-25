import unittest
import math

import numpy

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.api import Assembly, Container, Component, set_as_top
from openmdao.lib.datatypes.api import Float, Array, List, Instance, Dict

class A(Component):
    f = Float(iotype='in')
    a1d = Array(numpy.array([1.0, 1.0, 2.0, 3.0]), iotype='in')
    a2d = Array(numpy.array([[1.0, 1.0], [2.0, 3.0]]), iotype='in')
    b1d = Array(numpy.array([1.0, 1.0, 2.0, 3.0]), iotype='out')
    b2d = Array(numpy.array([[1.0, 1.0], [2.0, 3.0]]), iotype='out')
    
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
    cont = Instance(A, iotype='in')
    contlist = List(Instance(A), iotype='in')
    
    def get_cont(self, i):
        return self.contlist[i]
    
    def get_attr(self, name):
        return getattr(self, name)
    
class ExprEvalTestCase(unittest.TestCase):
    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('a', A())
        self.top.a.a1d = numpy.array([1., 2, 3, 4, 5, 6])
        self.top.add('comp', Comp())
        self.top.comp.x = 3.14
        self.top.comp.y = 42.

    def _do_tests(self, tests, top):
        # each test is a tuple of the form (input, expected output)
        for tst in tests:
            ex = ExprEvaluator(tst[0], top)
            ex._parse()
            self.assertEqual(ex.transformed_text, tst[1])
        
    def test_simple(self):
        tests = [
            ('a.f', "scope.get('a.f')"),
            ('a.f**2', "scope.get('a.f')**2"),
            ('a.f/a.a1d[int(a.f)]', "scope.get('a.f')/scope.get('a.a1d',[(0,int(scope.get('a.f')))])"),
            ('a.f = a.a1d[int(a.f)]', "scope.set('a.f',scope.get('a.a1d',[(0,int(scope.get('a.f')))]))"),
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
            ("comp.indct['foo.bar']=comp.cont.f","scope.set('comp.indct',scope.get('comp.cont.f'),[(0,'foo.bar')])"),
        ]
        self._do_tests(tests, self.top)
        
    def test_arrays(self):
        tests = [
            ('a.a1d', "scope.get('a.a1d')"),
            ('-a.a1d', "-scope.get('a.a1d')"),
            ('+a.a1d', "+scope.get('a.a1d')"),
            ('a.a1d[0]', "scope.get('a.a1d',[(0,0)])"),
            ('a.a2d[-a.a1d[2]]', "scope.get('a.a2d',[(0,-scope.get('a.a1d',[(0,2)]))])"),
            ('a.a2d[-a.a1d[2]][foo.bar]', 
             "scope.get('a.a2d',[(0,-scope.get('a.a1d',[(0,2)])),(0,scope.get('foo.bar'))])"),
            ('a.a2d[-a.a1d[2]]=a.f', 
             "scope.set('a.a2d',scope.get('a.f'),[(0,-scope.get('a.a1d',[(0,2)]))])"),
            ('a.f/a.a1d[int(a.f)]', "scope.get('a.f')/scope.get('a.a1d',[(0,int(scope.get('a.f')))])"),
            ('a.f = a.a1d[int(a.f)]', "scope.set('a.f',scope.get('a.a1d',[(0,int(scope.get('a.f')))]))"),
            ('a.b.cde[1+3**4*1]', "scope.get('a.b.cde',[(0,1+3**4*1)])"),
            ('a.b[1][2]', "scope.get('a.b',[(0,1),(0,2)])"),
            ('abs(a.b[1][2])', "abs(scope.get('a.b',[(0,1),(0,2)]))"),
            ('a.b[1][x.y]', "scope.get('a.b',[(0,1),(0,scope.get('x.y'))])"),  
            ('comp.x=a.b[1]',"scope.set('comp.x',scope.get('a.b',[(0,1)]))"),
            ('comp.cont.a1d[-3]', "scope.get('comp.cont.a1d',[(0,-3)])"),
        ]
        self._do_tests(tests, self.top)
        
    def test_mixed_scope(self):
        tests = [
            ('comp.x < a1d', "scope.parent.get('comp.x')<scope.a1d"),
            ('math.sin(f)+math.cos(f+math.pi)', 'math.sin(scope.f)+math.cos(scope.f+math.pi)'),
            ('comp.x[0]', "scope.parent.get('comp.x',[(0,0)])"),
            ('comp.x[0] = 10*(3.2+ a1d[3]* 1.1*a1d[2 ])', 
             "scope.parent.set('comp.x',10*(3.2+scope.a1d[3]*1.1*scope.a1d[2]),[(0,0)])"),
            ('comp.x[0] = some_funct(1,foo.bar)', 
             "scope.parent.set('comp.x',scope.some_funct(1,scope.parent.get('foo.bar')),[(0,0)])"),
            ('a.b[2] = -comp.x',
             "scope.parent.set('a.b',-scope.parent.get('comp.x'),[(0,2)])"),
            ('a1d[foo]', "scope.a1d[scope.parent.get('foo')]"),
        ]

        self._do_tests(tests, self.top.a)

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
         ('a.b(1,23,foo=9,*args,**kwargs)', 
          "scope.get('a.b',[(2,[1,23],{'foo':9},args,kwargs)])"),
         ('a.b(1,23)[1]', "scope.get('a.b',[(2,[1,23]),(0,1)])"),
        ]

        self._do_tests(tests, self.top)
    
    def test_set_evaluate(self):
        ex = ExprEvaluator('comp.x', self.top)
        self.assertEqual(3.14, ex.evaluate())

        # test setting the value of a referenced variable
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
        
        ex = ExprEvaluator("comp.get_cont(1).a1d", self.top)
        self.assertTrue(all(ex.evaluate() == numpy.array([4,4,4,123,4])))
        
        ex = ExprEvaluator("comp.get_attr('get_cont')(1).a1d", self.top)
        self.assertTrue(all(ex.evaluate() == numpy.array([4,4,4,123,4])))
        
    def test_reparse_on_scope_change(self):
        self.top.comp.x = 99.5
        self.top.comp.y = -3.14
        
        ex = ExprEvaluator('comp.x', self.top)
        self.assertEqual(99.5, ex.evaluate())
        self.assertEqual(ex.transformed_text, "scope.get('comp.x')")
        
        ex.scope = self.top.a
        ex.set(0.5)
        self.assertEqual(0.5, self.top.comp.x)
        self.assertEqual(ex.transformed_text, "scope.parent.get('comp.x')")
        self.assertEqual(0.5, ex.evaluate(self.top)) # set scope back to self.top
        self.assertEqual(ex.transformed_text, "scope.get('comp.x')")
        
        ex.text = 'comp.y'
        self.assertEqual(-3.14, ex.evaluate(self.top.a))
        ex.set(11.1)
        self.assertEqual(11.1, self.top.comp.y)
        self.assertEqual(ex.transformed_text, "scope.parent.get('comp.y')")
        
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
        
    def test_resolve(self):
        ex = ExprEvaluator('comp.x[0] = 10*(3.2+ a1d[3]* 1.1*a1d[2 ])', self.top.a)
        self.assertEqual(ex.check_resolve(), True)
        ex.text = 'comp.contlist[1].a2d[2][1]'
        self.assertEqual(ex.check_resolve(), True)
        ex.scope = self.top.comp
        ex.text = 'contlist[1]'
        self.assertEqual(ex.check_resolve(), True)
        ex.text = 'contlist[1]-foo.flambe'
        self.assertEqual(ex.check_resolve(), False)
        
    def test_get_referenced_varpaths(self):
        ex = ExprEvaluator('comp.x[0] = 10*(3.2+ a1d[3]* 1.1*a1d[2 ])', self.top.a)
        self.assertEqual(ex.get_referenced_varpaths(), set(['comp.x','a.a1d']))
        ex.text = 'comp.contlist[1].a2d[2][1]'
        self.assertEqual(ex.get_referenced_varpaths(), set(['comp.contlist']))
        ex.scope = self.top.comp
        ex.text = 'contlist[1]'
        self.assertEqual(ex.get_referenced_varpaths(), set(['comp.contlist']))
        
    def test_slice(self):
        ex = ExprEvaluator('a1d[1::2]', self.top.a)
        self.assertTrue(all(numpy.array([2.,4.,6.]) == ex.evaluate()))
        ex.text = 'a1d[2:4]'
        self.assertTrue(all(numpy.array([3.,4.]) == ex.evaluate()))
        ex.text = 'a1d[2:]'
        self.assertTrue(all(numpy.array([3.,4.,5.,6.]) == ex.evaluate()))
        ex.text = 'a1d[::-1]'
        self.assertTrue(all(numpy.array([6.,5.,4.,3.,2.,1.]) == ex.evaluate()))
        ex.text = 'a1d[:2]'
        self.assertTrue(all(numpy.array([1.,2.]) == ex.evaluate()))

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
            self.assertEqual(str(err), "can't evaluate expression 'abcd.efg': : object has no attribute 'abcd.efg'")
        else:
            raise AssertionError('AttributeError expected')
        
if __name__ == "__main__":
    unittest.main()
    
    
