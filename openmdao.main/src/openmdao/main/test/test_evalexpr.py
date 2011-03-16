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

from openmdao.main.expreval import ExprEvaluator #,translate_expr
from openmdao.main.api import Assembly, Container, Component, set_as_top
from openmdao.lib.datatypes.api import Float, Array

#import ast

##_allowed_nodes = (ast.BoolOp, ast.BinOp, ast.UnaryOp, ast.Attribute, ast.Assign, 
                  ##ast.Name, ast.IfExp, ast.Call, ast.Num, 
                  ##ast.Str, ast.Subscript, ast.Index, ast.Slice, ast.List, ast.Tuple, 
                  ##ast.USub, ast.UAdd, ast.And, ast.Or,
                  ##ast.Add, ast.Sub, ast.Mult, ast.Div, ast.Mod, ast.Pow, 
                  ##ast.LShift, ast.RShift, ast.BitOr, ast.BitXor, ast.BitAnd, 
                  ##ast.FloorDiv, ast.Eq, ast.NotEq, ast.Lt, ast.LtE, ast.Gt, ast.GtE, 
                  ##ast.Is, ast.IsNot, ast.In, ast.NotIn
                  ##)
                  
                  
#class ExprPrinter(ast.NodeVisitor):
    #def __init__(self):
        #super(ExprPrinter, self).__init__()
        #self.txtlist = []
        
    #def write(self, txt):
        #self.txtlist.append(txt)
        
    #def get_text(self):
        #return ''.join(self.txtlist)

    ##def generic_visit(self, node):
        ##if not isinstance(node, _allowed_nodes):
            ##raise TypeError("Expression AST contains a node of type %s which is not allowed." %
                            ##node.__class__.__name__)
        ##super(ExprPrinter, self).generic_visit(node)
        
    #def visit_Attribute(self, node):
        #self.visit(node.value)
        #self.write(".%s" % node.attr)
        
    #def visit_Assign(self, node):
        #for i,t in enumerate(node.targets):
            #if i>0: self.write(',')
            #self.visit(t)
        #self.write(' = ')
        #self.visit(node.value)
        
    #def visit_Name(self, node):
        #self.write(node.id)

    #def visit_IfExp(self, node):
        #self.visit(node.body)
        #self.write(' if ')
        #self.visit(node.test)
        #self.write(' else ')
        #self.visit(node.orelse)
        
    #def visit_Call(self, node):
        #self.visit(node.func)
        #self.write('(')
        #total_args = 0
        #for arg in node.args:
            #if total_args>0: self.write(',')
            #self.visit(arg)
            #total_args += 1
            
        #if hasattr(node, 'keywords'):
            #for kw in node.keywords:
                #if total_args>0: self.write(',')
                #self.visit(kw)
                #total_args += 1
            
        #if hasattr(node, 'starargs'):
            #if node.starargs:
                #if total_args>0: self.write(',')
                #self.write('*%s'%node.starargs)
                #total_args += 1
            
        #if hasattr(node, 'kwargs'):
            #if node.kwargs:
                #if total_args>0: self.write(',')
                #self.write('**%s'%node.kwargs)
    
        #self.write(')')
        
    #def visit_Num(self, node):
        #self.write(str(node.n))
        
    #def visit_Str(self, node):
        #self.write("'%s'" % node.s)
        
    #def visit_Index(self, node):
        #self.write('[')
        #self.visit(node.value)
        #self.write(']')

    #def visit_Slice(self, node):
        #self.visit(node.lower)
        #self.write(':')
        #self.visit(node.upper)
        #self.write(':')
        #if node.step is not None:
            #self.visit(node.step)
        
    #def visit_List(self, node):
        #self.write('[')
        #for i,e in enumerate(node.elts):
            #if i>0:
                #self.write(',')
            #self.visit(e)
        #self.write(']')
        
    #def visit_Tuple(self, node):
        #self.write('(')
        #for i,e in enumerate(node.elts):
            #if i>0:
                #self.write(',')
            #self.visit(e)
        #if len(node.elts) == 1:
            #self.write(',')
        #self.write(')')
        
    #def visit_USub(self, node):
        #self.write('-')
        
    #def visit_UAdd(self, node):
        #self.write('+')
        
    #def visit_And(self, node):
        #self.write(' and ')
        
    #def visit_Or(self, node):
        #self.write(' or ')
        
    ## operators
    ##Add | Sub | Mult | Div | Mod | Pow | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv
    #def visit_Add(self, node):
        #self.write('+')
        
    #def visit_Sub(self, node):
        #self.write('-')
        
    #def visit_Mult(self, node):
        #self.write('*')
        
    #def visit_Div(self, node):
        #self.write('/')
        
    #def visit_Mod(self, node):
        #self.write('%')
        
    #def visit_Pow(self, node):
        #self.write('**')
        
    #def visit_LShift(self, node):
        #self.write('<<')
        
    #def visit_Rshift(self, node):
        #self.write('>>')
        
    #def visit_BitOr(self, node):
        #self.write('|')
        
    #def visit_BitXor(self, node):
        #self.write('^')
        
    #def visit_BitAnd(self, node):
        #self.write('&')
        
    #def visit_FloorDiv(self, node):
        #self.write('//')
        
    ## cmp operators
    ## Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
    #def visit_Eq(self, node):
        #self.write('==')

    #def visit_NotEq(self, node):
        #self.write('!=')

    #def visit_Lt(self, node):
        #self.write('<')

    #def visit_LtE(self, node):
        #self.write('<=')

    #def visit_Gt(self, node):
        #self.write('>')

    #def visit_GtE(self, node):
        #self.write('>=')

    #def visit_Is(self, node):
        #self.write(' is ')

    #def visit_IsNot(self, node):
        #self.write(' is not ')

    #def visit_In(self, node):
        #self.write(' in ')

    #def visit_NotIn(self, node):
        #self.write(' not in ')

        
#class ExprTransformer(ast.NodeTransformer):
    #def __init__(self, expreval, mode='get'):
        #self.expreval = expreval
        #self.mode = mode
        #super(ExprTransformer, self).__init__()
        
    #def _get_name_node(self, names):
        #node = ast.Name(id=names[0], ctx=ast.Load())
        #for name in names[1:]:
            #node = ast.Attribute(value=node, attr=name, ctx=ast.Load())
        #return node

    #def visit_Attribute(self, node, subs=None):
        #val = node.value
        #parts = [node.attr]
        #while True:
            #if isinstance(val, ast.Attribute):
                #parts.append(val.attr)
                #val = val.value
            #elif isinstance(val, ast.Name):
                #parts.append(val.id)
                #break
            #else:  # it's more than just a simple dotted name, so don't transform it
                #return node

        #long_name = '.'.join(parts[::-1])
        #args = [ast.Str(s=long_name)]
        #scope = self.expreval.scope
        #names = []
        #if scope:
            #if scope.contains(parts[-1]):
                #names = ['scope']
            #elif long_name.startswith('math.') and hasattr(math, long_name.split('.',1)[-1]):
                #return node
            #else:
                #names = ['scope', 'parent']
        #else:
            #raise RuntimeError("expression '%s' can't be evaluated because it has no scope" % str(self.expreval))

        #names.append(self.mode)
        #if self.mode == 'set':
            #args.append(ast.Name(id='_local_setter', ctx=ast.Load()))

        #called_obj = self._get_name_node(names)
        #if subs:
            #args.append(ast.List(elts=subs))

        #return ast.copy_location(ast.Call(func=called_obj, args=args,
                                          #ctx=node.ctx), node)

    #def visit_Subscript(self, node, sublist=None):
        #if sublist:
            #subs = [self.visit(node.slice.value)] + sublist
        #else:
            #subs = [self.visit(node.slice.value)]
            
        #if isinstance(node.value, ast.Attribute):
            #node = self.visit_Attribute(node.value, subs)
        #elif isinstance(node.value, ast.Subscript):
            #return self.visit_Subscript(node.value, subs)
        #return node

    
#import weakref
#class ExprEvaluator(str):
    #"""A class that translates an expression string into a new string containing
    #any necessary framework access functions, e.g., set, get. The compiled
    #bytecode is stored within the object so that it doesn't have to be reparsed
    #during later evaluations.  A scoping object is required at construction time,
    #and that object determines the form of the  translated expression based on scope. 
    #Variables that are local to the scoping object do not need to be translated,
    #whereas variables from other objects must  be accessed using the appropriate
    #*set()* or *get()* call.  Array entry access and function invocation are also
    #translated in a similar way.  For example, the expression "a+b[2]-comp.y(x)"
    #for a scoping object that contains variables a and b, but not comp,x or y,
    #would translate to 
    #"a+b[2]-self.parent.invoke('comp.y',self.parent.get('x'))".
    
    #If *lazy_check* is False, any objects referenced in the expression must exist
    #at creation time (or any time later that text is set to a different value)
    #or a RuntimeError will be raised.  If *lazy_check* is True, error reporting will
    #be delayed until the expression is evaluated.
    
    #If *single_name* is True, the expression can only be the name of one object, with
    #optional array indexing, but general expressions are not allowed because the
    #expression is intended to be on the LHS of an assignment statement.
    #"""
    
    #def __new__(cls, text, scope=None, single_name=False, lazy_check=True):
        #s = super(ExprEvaluator, cls).__new__(ExprEvaluator, text)
        #s.scope = scope
        #s.lazy_check = lazy_check
        #s.single_name = single_name
        #s._text = None  # used to detect change in str
        #s.var_names = set()
        #s._has_globals = False
        #if lazy_check is False:
            #s._parse()
        #return s
    
    #@property
    #def scope(self):
        #if self._scope:
            #return self._scope()
        #return None
        
    #@scope.setter
    #def scope(self, value):
        #if value is not None:
            #self._scope = weakref.ref(value)
        #else:
            #self._scope = None

    #def _parse(self):
        #self.var_names = set()
        #root = ast.parse(self, mode='eval')
        
        ## transform attribute accesses to 'get' calls if necessary
        #new_ast = ExprTransformer(self).visit(root)
        
        ### now take the new AST and save it to a string (for debugging purposes)
        #ep = ExprPrinter()
        #ep.visit(new_ast)
        #self.scoped_text = ep.get_text()
        
        ## compile the transformed AST
        #self._code = compile(self.scoped_text, '<string>', 'eval')
        
        #if self.single_name: # set up a compiled assignment statement
            #old_lazy_check = self.lazy_check
            #try:
                #self.lazy_check = True
                ## transform into a 'set' call to set the specified variable
                #new_ast = ExprTransformer(self, mode='set').visit(root)
                #self._assignment_code = compile(new_ast,'<string>','eval')
            #finally:
                #self.lazy_check = old_lazy_check

    
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
        ('b[0]', "b[0]"),
        ('b[-3]', "b[-3]"),
        ('abs(b)', 'abs(b)'),
        ('comp.x < b', "scope.parent.get('comp.x')<b"),
        ('math.sin(b)+math.cos(b+math.pi)', 'math.sin(b)+math.cos(b+math.pi)'),
        ('comp.x[0]', "scope.parent.get('comp.x',[0])"),
        #('comp.x[0] = 10.-(3.2* b[3]+ 1.1*b[2 ])', 
        #     "scope.parent.set('comp.x',10.-(3.2*b[3]+1.1*b[2]),[0])"),
        #('c.b[2] = -comp.x',
        #     "scope.parent.set('c.b',-scope.parent.get('comp.x'),[2])"),
        ]

        for tst in tests:
            ex = ExprEvaluator(tst[0], self.top.a, lazy_check=True)
            ex._parse()
            self.assertEqual(ex.scoped_text, tst[1])

    def test_set2(self):
        tests = [
        ('a.b.cde[1+3**4*1]', "scope.get('a.b.cde',[1+3**4*1])"),
        ('a.b[1][2]', "scope.get('a.b',[1,2])"),
        ('abs(a.b[1][2])', "abs(scope.get('a.b',[1,2]))"),
        ('a.b[1][x.y]', "scope.get('a.b',[1,scope.parent.get('x.y')])"),  
        #('a.b()', "scope.invoke('a.b')"),
        #('comp.x=a.b[1]',"scope.set('comp.x',scope.get('a.b',[1]))"),
        #('a.b(5)', "scope.invoke('a.b',5)"),
        #('a.b(5,9)', "scope.invoke('a.b',5,9)"),
        #('a.b(5,z.y)', "scope.invoke('a.b',5,scope.parent.get('z.y'))"),
        #('a.b(5,-z.y)', "scope.invoke('a.b',5,-scope.parent.get('z.y'))"),
        #('a.b(5, z.y(2,3))', "scope.invoke('a.b',5,scope.parent.invoke('z.y',2,3))"),
        #('a.b(5, z.y[3])', "scope.invoke('a.b',5,scope.parent.get('z.y',[3]))"),
        #('0+a.b(5, -z.y[3])**2-z.z[4]',
         #"0+scope.invoke('a.b',5,-scope.parent.get('z.y',[3]))**2-scope.parent.get('z.z',[4])"),
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

    #def test_boolean(self):
        #comp = self.top.comp
        #comp.x = 1.
        #comp.y = 3.
        #self.assertEqual(True, ExprEvaluator('comp.x < comp.y', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('comp.x <= comp.y', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('comp.x != comp.y', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('comp.x == comp.y', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('comp.x > comp.y', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('comp.x >= comp.y', self.top).evaluate())
        
        #self.assertEqual(True, ExprEvaluator('1< comp.y', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('1<= comp.y', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('1!= comp.y', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('1== comp.y', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('1> comp.y', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('1>= comp.y', self.top).evaluate())
        
        #self.assertEqual(True, ExprEvaluator('comp.x < 3', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('comp.x <= 3', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('comp.x != 3', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('comp.x == 3', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('comp.x > 3', self.top).evaluate())
        #self.assertEqual(False, ExprEvaluator('comp.x >= 3', self.top).evaluate())
        
        #comp.x = 3.
        #self.assertEqual(False, ExprEvaluator('comp.x != comp.y', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('comp.x == comp.y', self.top).evaluate())
        
        #self.top.a.b = [1,1,1,1]
        #self.assertEqual(True, ExprEvaluator('all(a.b)', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('any(a.b)', self.top).evaluate())
        #self.top.a.b = [1,1,0,1]
        #self.assertEqual(False, ExprEvaluator('all(a.b)', self.top).evaluate())
        #self.assertEqual(True, ExprEvaluator('any(a.b)', self.top).evaluate())
        
    #def test_builtins(self):
        #comp = self.top.comp
        #comp.x = 1.
        #comp.y = -3.
        #self.assertEqual(3., ExprEvaluator('abs(comp.y)', self.top).evaluate())
        #self.assertAlmostEqual(0., ExprEvaluator('math.sin(math.pi)', self.top).evaluate())
        #comp.x = 1.35
        #self.assertEqual(1., ExprEvaluator('math.floor(comp.x)', self.top).evaluate())
        #self.assertEqual(2., ExprEvaluator('math.ceil(comp.x)', self.top).evaluate())
        #comp.x = 0.
        #self.assertEqual(True, ExprEvaluator('math.sin(comp.x)<math.cos(comp.x)', self.top).evaluate())
        #comp.x = math.pi/2.
        #self.assertEqual(False, ExprEvaluator('math.sin(comp.x)<math.cos(comp.x)', self.top).evaluate())
        
    #def test_multi_object(self):
        ## verify that single_name will not allow expressions with multiple objects
        #try:
            #ex = ExprEvaluator('comp.x+comp.x', self.top, single_name=True, lazy_check=False)
        #except RuntimeError, err:
            #self.assertEqual(str(err),
                #"Expected end of text (at char 6), (line:1, col:7) - comp.x>!<+comp.x")
        #else:
            #raise AssertionError('RuntimeError expected')
    
    #def test_bogus(self):        
        ## now try some bogus expressions
        #try:
            #ex = ExprEvaluator('abcd.efg', self.top, lazy_check=False)
        #except RuntimeError, err:
            #self.assertEqual(str(err), "ExprEvaluator: cannot find variable 'abcd.efg'")
        #else:
            #raise AssertionError('RuntimeError expected')

if __name__ == "__main__":
    unittest.main()