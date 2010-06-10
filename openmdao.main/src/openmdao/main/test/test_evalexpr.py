"""
Test scenario:
    Container hierarchy    top
                          /   \
                         a   comp
                        /       \
                       b         x
                      /
                    funct
                       
b is an array variable
funct is a callable
comp is a Component
x is an float variable
"""
import unittest
import math

import numpy
import logging

from openmdao.main.expreval import translate_expr, ExprEvaluator
from openmdao.main.api import Assembly, Container, Component, set_as_top
from openmdao.lib.api import Float, Array

class A(Container):
    b = Array(iotype='in')
    
class Comp(Component):
    x = Float(iotype='in')
    y = Float(iotype='in')
    
class ExprEvalTestCase(unittest.TestCase):
    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('a', A())
        self.top.a.b = numpy.array([1., 2, 3, 4, 5, 6])
        self.top.add('comp', Comp())
        self.top.comp.x = 3.14
        self.top.comp.y = 42.

    def test_set1(self):
        # each test is a tuple of the form (input, expected output)
        tests = [
        ('b', "b"),
        ('-b', "-b"),
        ('- b', "-b"),
        ('b[0]', "b[0]"),
        ('b[-3]', "b[-3]"),
        ('b [ -3 ]', "b[-3]"),
        ('abs(b)', 'abs(b)'),
        ('abs( b)', 'abs(b)'),
        ('comp.x < b', "scope.parent.get('comp.x')<b"),
        ('math.sin(b)+math.cos(b+math.pi)', 'math.sin(b)+math.cos(b+math.pi)'),
        ('comp.x[0]', "scope.parent.get('comp.x',[0])"),
        ('comp.x[0] = 10.-(3.2* b[3]+ 1.1*b[2 ])', 
             "scope.parent.set('comp.x',10.-(3.2*b[3]+1.1*b[2]),[0])"),
        ('c.b[2] = -comp.x',
             "scope.parent.set('c.b',-scope.parent.get('comp.x'),[2])"),
        ]

        for tst in tests:
            ex = ExprEvaluator(tst[0], self.top.a, lazy_check=True)
            ex._parse()
            self.assertEqual(ex.scoped_text, tst[1])

    def test_set2(self):
        tests = [
        ('a.b[1+3**4*1]', "scope.get('a.b',[1+3**4*1])"),
        ('a.b[1][2]', "scope.get('a.b',[1,2])"),
        ('abs(a.b[1][2])', "abs(scope.get('a.b',[1,2]))"),
        ('a.b[1,2]', "scope.get('a.b',[1,2])"),
        ('a.b[1][x.y]', "scope.get('a.b',[1,scope.parent.get('x.y')])"),  
        ('a.b()', "scope.invoke('a.b')"),
        ('comp.x=a.b[1]',"scope.set('comp.x',scope.get('a.b',[1]))"),
        ('a.b(5)', "scope.invoke('a.b',5)"),
        ('a.b(5,9)', "scope.invoke('a.b',5,9)"),
        ('a.b(5,z.y)', "scope.invoke('a.b',5,scope.parent.get('z.y'))"),
        ('a.b(5,-z.y)', "scope.invoke('a.b',5,-scope.parent.get('z.y'))"),
        ('a.b(5, z.y(2,3))', "scope.invoke('a.b',5,scope.parent.invoke('z.y',2,3))"),
        ('a.b(5, z.y[3])', "scope.invoke('a.b',5,scope.parent.get('z.y',[3]))"),
        ('0+a.b(5, -z.y[3])**2-z.z[4]',
         "0+scope.invoke('a.b',5,-scope.parent.get('z.y',[3]))**2-scope.parent.get('z.z',[4])"),
        # compound stuff (arglist followed by array index or vice versa) doesn't work
        #('a.b(1,23)[1]', "scope.parent.get(scope.parent.invoke('a.b',1,23),[1])"),
        ]

        for tst in tests:
            ex = ExprEvaluator(tst[0], self.top)
            ex._parse()
            self.assertEqual(ex.scoped_text, tst[1])
    
    def test_set_evaluate(self):
        ex = ExprEvaluator('comp.x', self.top, single_name=True)
        self.assertEqual(3.14, ex.evaluate())

        # test setting the value of a referenced variable
        ex.set(75.4)
        self.assertEqual(75.4, self.top.comp.x)

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
        self.assertAlmostEqual(0., ExprEvaluator('math.sin(math.pi)', self.top).evaluate())
        comp.x = 1.35
        self.assertEqual(1., ExprEvaluator('math.floor(comp.x)', self.top).evaluate())
        self.assertEqual(2., ExprEvaluator('math.ceil(comp.x)', self.top).evaluate())
        comp.x = 0.
        self.assertEqual(True, ExprEvaluator('math.sin(comp.x)<math.cos(comp.x)', self.top).evaluate())
        comp.x = math.pi/2.
        self.assertEqual(False, ExprEvaluator('math.sin(comp.x)<math.cos(comp.x)', self.top).evaluate())
        
    def test_multi_object(self):
        # verify that single_name will not allow expressions with multiple objects
        try:
            ex = ExprEvaluator('comp.x+comp.x', self.top, single_name=True, lazy_check=False)
        except RuntimeError, err:
            self.assertEqual(str(err),
                "Expected end of text (at char 6), (line:1, col:7) - comp.x>!<+comp.x")
        else:
            raise AssertionError('RuntimeError expected')
    
    def test_bogus(self):        
        # now try some bogus expressions
        try:
            ex = ExprEvaluator('abcd.efg', self.top, lazy_check=False)
        except RuntimeError, err:
            self.assertEqual(str(err), "ExprEvaluator: cannot find variable 'abcd.efg'")
        else:
            raise AssertionError('RuntimeError expected')

if __name__ == "__main__":
    unittest.main()
