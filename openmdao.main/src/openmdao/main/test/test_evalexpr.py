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

import numpy
import logging

from openmdao.main.expreval import translate_expr, ExprEvaluator
from openmdao.main import Container, Component
from openmdao.main import ArrayVariable, Float
from openmdao.main.variable import INPUT


top = Container('top', None)
top.a = Container('a', top)
top.a.b = numpy.array([1., 2, 3, 4, 5, 6])
b = ArrayVariable('b', top.a, INPUT)
top.comp = Component('comp', top)
top.comp.x = 3.14
x = Float('x', top.comp, INPUT)
top.make_public(['a', 'comp'])

# each test is a tuple of the form (input, expected output)
tests = [
('b', "b"),
('-b', "-b"),
('b[0]', "b[0]"),
('b[-3]', "b[-3]"),
('comp.x[0]', "scope.parent.get('comp.x',[0])"),
('comp.x[0] = 10.-(3.2*b[3]+1.1*b[2])', 
     "scope.parent.set('comp.x',10.-(3.2*b[3]+1.1*b[2]),[0])"),
('c.b[2] = -comp.x',
     "scope.parent.set('c.b',-scope.parent.get('comp.x'),[2])"),
]

for tst in tests:
    ex = ExprEvaluator(tst[0], top.a, lazy_check=True)
    if ex.scoped_text != tst[1]:
        raise AssertionError('for input of '+tst[0]+', '+ex.scoped_text+" == "+tst[1])  

tests = [
('a.b[1+3**4*1]', "scope.get('a.b',[1+3**4*1])"),
('a.b[1][2]', "scope.get('a.b',[1,2])"),
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
    ex = ExprEvaluator(tst[0], top, lazy_check=True)
    if ex.scoped_text != tst[1]:
        raise AssertionError('for input of '+tst[0]+', '+ex.scoped_text+" == "+tst[1])  
    

ex = ExprEvaluator('comp.x', top, single_name=True)
assert(3.14 == ex.evaluate())

# test setting the value of a referenced variable
ex.set(75.4)
assert(top.comp.x == 75.4)

# verify that single_name will not allow expressions with multiple objects
try:
    ex = ExprEvaluator('comp.x+comp.x', top, single_name=True)
except RuntimeError, err:
    expected = "Expected end of text (at char 6), (line:1, col:7) - comp.x>!<+comp.x"
    if str(err) != expected:
        raise AssertionError(str(err)+'=='+expected)
else:
    raise AssertionError('RuntimeError expected')
    

# now try some bogus expressions
try:
    ex = ExprEvaluator('abcd.efg', top)
except RuntimeError, err:
    expected = "cannot find variable 'abcd.efg'"
    if str(err) != expected:
        raise AssertionError(str(err)+'=='+expected)
else:
    raise AssertionError('RuntimeError expected')

