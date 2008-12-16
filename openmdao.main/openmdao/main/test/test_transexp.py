
import numpy

from openmdao.main.transexpr import translate_expr
from openmdao.main.container import Container
from openmdao.main.component import Component
from openmdao.main.variable import INPUT
from openmdao.main.arrayvar import ArrayVariable
from openmdao.main.float import Float
from openmdao.main.containervar import ContainerVariable

"""
Test scenario:
    Container hierarchy    foo
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

foo = Container('foo', None)
foo.a = Container('a',foo)
foo.a.b = numpy.array([1.,2,3,4,5,6])
b = ArrayVariable('b', foo.a, INPUT)
foo.comp = Component('comp',foo)
foo.comp.x = 3.14
x = Float('x', foo.comp, INPUT)
foo.make_public(['a','comp'])

# each test is a tuple of the form (input, expected output)
tests = [
('b',"b"),
('-b',"-b"),
('b[0]',"b[0]"),
('b[-3]',"b[-3]"),
('comp.x[0]',"self.parent.get('comp.x',[0])"),
('comp.x[0] = 10.-(3.2*b[3]+1.1*b[2])', "self.parent.set('comp.x',10.-(3.2*b[3]+1.1*b[2]),[0])"),
('c.b[2] = -comp.x',"self.parent.set('c.b',-self.parent.get('comp.x'),[2])"),
]


for tst in tests:
    trans = translate_expr(tst[0],scope=foo.a)
    if trans != tst[1]:
        raise AssertionError(trans+" == "+tst[1])  

tests = [
('a.b[1+3**4*1]',"self.get('a.b',[1+3**4*1])"),
('a.b[1][2]',"self.get('a.b',[1,2])"),
('a.b[1,2]',"self.get('a.b',[1,2])"),
('a.b[1][x.y]',"self.get('a.b',[1,self.parent.get('x.y')])"),
('a.b()',"self.invoke('a.b')"),
('a.b(5)',"self.invoke('a.b',5)"),
('a.b(5,9)',"self.invoke('a.b',5,9)"),
('a.b(5,z.y)',"self.invoke('a.b',5,self.parent.get('z.y'))"),
('a.b(5,-z.y)',"self.invoke('a.b',5,-self.parent.get('z.y'))"),
('a.b(5, z.y(2,3))',"self.invoke('a.b',5,self.parent.invoke('z.y',2,3))"),
('a.b(5, z.y[3])',"self.invoke('a.b',5,self.parent.get('z.y',[3]))"),
('0+a.b(5, -z.y[3])**2-z.z[4]',
 "0+self.invoke('a.b',5,-self.parent.get('z.y',[3]))**2-self.parent.get('z.z',[4])"),
# compound stuff (arglist followed by array index or vice versa) doesn't work
#('a.b(1,23)[1]',"self.parent.invoke('a.b',1,23)"),
]


for tst in tests:
    trans = translate_expr(tst[0],scope=foo)
    if trans != tst[1]:
        raise AssertionError('for input of '+tst[0]+', '+trans+" == "+tst[1])  
