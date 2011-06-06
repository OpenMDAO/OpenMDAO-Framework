import unittest
import math
import ast

import numpy

from openmdao.main.expreval import ExprEvaluator
from openmdao.main.api import Assembly, Container, Component, set_as_top
from openmdao.lib.datatypes.api import Float, Array, List, Slot, Dict

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
    cont = Slot(A, iotype='in')
    contlist = List(Slot(A), iotype='in')
    
    def get_cont(self, i):
        return self.contlist[i]
    
    def get_attr(self, name):
        return getattr(self, name)
    

# dict with operator precedence.  We need this because otherwise we can't
# tell where to put parens when we print out an expression with mixed operators.  
# We could just put them around every operation, but that's a little ugly...
_op_preds = {}
_prec = 0

_op_preds[ast.Lambda] = _prec
_prec += 1
_op_preds[ast.If] = _prec
_prec += 1
_op_preds[ast.Or] = _prec
_prec += 1
_op_preds[ast.And] = _prec
_prec += 1
_op_preds[ast.Not] = _prec
_prec += 1
_op_preds[ast.In] = _prec
_op_preds[ast.NotIn] = _prec
_op_preds[ast.Is] = _prec
_op_preds[ast.IsNot] = _prec
_op_preds[ast.Lt] = _prec
_op_preds[ast.LtE] = _prec
_op_preds[ast.Gt] = _prec
_op_preds[ast.GtE] = _prec
_op_preds[ast.NotEq] = _prec
_op_preds[ast.Eq] = _prec
_prec += 1
_op_preds[ast.BitOr] = _prec
_prec += 1
_op_preds[ast.BitXor] = _prec
_prec += 1
_op_preds[ast.BitAnd] = _prec
_prec += 1
_op_preds[ast.LShift] = _prec
_op_preds[ast.RShift] = _prec
_prec += 1
_op_preds[ast.Add] = _prec
_op_preds[ast.Sub] = _prec
_prec += 1
_op_preds[ast.Mult] = _prec
_op_preds[ast.Div] = _prec
_op_preds[ast.FloorDiv] = _prec
_op_preds[ast.Mod] = _prec
_prec += 1
_op_preds[ast.UAdd] = _prec
_op_preds[ast.USub] = _prec
_op_preds[ast.Invert] = _prec
_prec += 1
_op_preds[ast.Pow] = _prec

def _pred_cmp(op1, op2):
    """Used to determine operator precedence."""
    return _op_preds[op1.__class__] - _op_preds[op2.__class__]

class ExprPrinter(ast.NodeVisitor):
    """A NodeVisitor that gets the python text of an expression or assignment
    statement defined by an AST.
    """
    def __init__(self):
        super(ExprPrinter, self).__init__()
        self.txtlist = []
        
    def write(self, txt):
        self.txtlist.append(txt)
        
    def get_text(self):
        return ''.join(self.txtlist)

    def visit_Attribute(self, node):
        self.visit(node.value)
        self.write(".%s" % node.attr)
        
    def visit_Assign(self, node):
        for i,t in enumerate(node.targets):
            if i>0: self.write(',')
            self.visit(t)
        self.write(' = ')
        self.visit(node.value)
        
    def visit_Name(self, node):
        self.write(node.id)
        
    def visit_BinOp(self, node):
        # we have to add parens around any immediate BinOp child
        # that has a lower precedence operation than we do
        if isinstance(node.left, ast.BinOp) and _pred_cmp(node.left.op, node.op) < 0:
            self.write('(')
            self.visit(node.left)
            self.write(')')
        else:
            self.visit(node.left)
        self.visit(node.op)
        if isinstance(node.right, ast.BinOp) and _pred_cmp(node.right.op, node.op) < 0:
            self.write('(')
            self.visit(node.right)
            self.write(')')
        else:
            self.visit(node.right)

    def visit_IfExp(self, node):
        self.visit(node.body)
        self.write(' if ')
        self.visit(node.test)
        self.write(' else ')
        self.visit(node.orelse)
        
    def visit_Call(self, node):
        self.visit(node.func)
        self.write('(')
        total_args = 0
        for arg in node.args:
            if total_args>0: self.write(',')
            self.visit(arg)
            total_args += 1
            
        if hasattr(node, 'keywords'):
            for kw in node.keywords:
                if total_args>0: self.write(',')
                self.visit(kw)
                total_args += 1
            
        if hasattr(node, 'starargs'):
            if node.starargs:
                if total_args>0: self.write(',')
                self.write('*%s'%node.starargs)
                total_args += 1
            
        if hasattr(node, 'kwargs'):
            if node.kwargs:
                if total_args>0: self.write(',')
                self.write('**%s'%node.kwargs)
    
        self.write(')')
        
    def visit_Num(self, node):
        self.write(str(node.n))
        
    def visit_Str(self, node):
        self.write("'%s'" % node.s)
        
    def visit_Index(self, node):
        self.write('[')
        self.visit(node.value)
        self.write(']')

    def visit_Slice(self, node):
        self.write('[')
        if node.lower is not None:
            self.visit(node.lower)
        self.write(':')
        if node.upper is not None:
            self.visit(node.upper)
        self.write(':')
        if node.step is not None:
            self.visit(node.step)
        self.write(']')
        
    def visit_List(self, node):  
        self.write('[')
        for i,e in enumerate(node.elts):
            if i>0: self.write(',')
            self.visit(e)
        self.write(']')
        
    def visit_Dict(self, node):  
        self.write('{')
        for i,tup in enumerate(zip(node.keys,node.values)):
            if i>0: self.write(',')
            self.write("'%s':" % tup[0].s)
            self.visit(tup[1])
        self.write('}')
        
    def visit_Tuple(self, node): 
        self.write('(')
        length = len(node.elts)
        for i,e in enumerate(node.elts):
            if i>0: self.write(',')
            self.visit(e)
        if length==1: self.write(',')
        self.write(')')
        
    def visit_USub(self, node): self.write('-')
    def visit_UAdd(self, node): self.write('+')
    def visit_And(self, node):  self.write(' and ')
    def visit_Or(self, node):   self.write(' or ')
        
    # operators
    def visit_Add(self, node):      self.write('+')
    def visit_Sub(self, node):      self.write('-')
    def visit_Mult(self, node):     self.write('*')
    def visit_Div(self, node):      self.write('/')
    def visit_Mod(self, node):      self.write('%')
    def visit_Pow(self, node):      self.write('**')
    def visit_LShift(self, node):   self.write('<<')
    def visit_Rshift(self, node):   self.write('>>')
    def visit_BitOr(self, node):    self.write('|')
    def visit_BitXor(self, node):   self.write('^')
    def visit_BitAnd(self, node):   self.write('&')
    def visit_FloorDiv(self, node): self.write('//')
        
    # cmp operators
    def visit_Eq(self, node):    self.write('==')
    def visit_NotEq(self, node): self.write('!=')
    def visit_Lt(self, node):    self.write('<')
    def visit_LtE(self, node):   self.write('<=')
    def visit_Gt(self, node):    self.write('>')
    def visit_GtE(self, node):   self.write('>=')
    def visit_Is(self, node):    self.write(' is ')
    def visit_IsNot(self, node): self.write(' is not ')
    def visit_In(self, node):    self.write(' in ')
    def visit_NotIn(self, node): self.write(' not in ')
    
    def _ignore(self, node):
        super(ExprPrinter, self).generic_visit(node)

    visit_Module     = _ignore
    visit_Expr       = _ignore
    visit_Expression = _ignore
    visit_Compare    = _ignore
    visit_UnaryOp    = _ignore
    visit_Subscript  = _ignore
    visit_Load       = _ignore
    visit_Store      = _ignore
    
    def generic_visit(self, node):
        # We want to fail if we see any nodes we don't know about rather than
        # generating code that isn't correct.
        raise RuntimeError("ExprPrinter can't handle a node of type %s" % node.__class__.__name__)


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
        self.top.a.a1d = numpy.array([1., 2, 3, 4, 5, 6])
        self.top.add('comp', Comp())
        self.top.comp.x = 3.14
        self.top.comp.y = 42.

    def _ast_to_text(self, node):
        ep = ExprPrinter()
        ep.visit(node)
        return ep.get_text()
        
    def _do_tests(self, tests, top):
        # each test is a tuple of the form (input, expected output)
        for tst in tests:
            ex = ExprEvaluator(tst[0], top)
            self.assertEqual(self._ast_to_text(ex._parse()), tst[1])
            
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
            ('a.a2d[0][0]', "scope.get('a.a2d',[(0,0),(0,0)])"),
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
            ('a1d[1].value', "scope.a1d[1].value"),
            ('a1d(2).value', "scope.a1d(2).value"),
            ('a.a1d(2).value', "scope.parent.get('a.a1d',[(2,[2]),(1,'value')])"),
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
        self.assertTrue(all(ex.evaluate() == numpy.array([11.,3.])))
        ex.set([0.1,0.2])
        self.assertTrue(all(self.top.a.a2d[1] == numpy.array([0.1,0.2])))
        
        self.top.comp.cont = A()
        
        ex = ExprEvaluator("comp.cont.a2d[1][0]", self.top)
        self.assertEqual(ex.evaluate(), 2.)
        ex.set(7.)
        self.assertEqual(self.top.comp.cont.a2d[1][0], 7.)
        
        ex = ExprEvaluator("comp.cont.a2d[1,0]", self.top)
        self.assertEqual(ex.evaluate(), 7.)
        ex.set(11.)
        self.assertEqual(self.top.comp.cont.a2d[1][0], 11.)
        
        ex = ExprEvaluator("comp.get_cont(1).a1d", self.top)
        self.assertTrue(all(ex.evaluate() == numpy.array([4,4,4,123,4])))
        
        ex = ExprEvaluator("comp.get_attr('get_cont')(1).a1d", self.top)
        self.assertTrue(all(ex.evaluate() == numpy.array([4,4,4,123,4])))
        
        # try an expression that's a simple assignment
        ex = ExprEvaluator("f = 10.333", self.top.a)
        self.assertEqual(ex.evaluate(), None)
        self.assertEqual(self.top.a.f, 10.333)
        
    def test_reparse_on_scope_change(self):
        self.top.comp.x = 99.5
        self.top.comp.y = -3.14
        
        ex = ExprEvaluator('comp.x', self.top)
        self.assertEqual(99.5, ex.evaluate())
        self.assertEqual(self._ast_to_text(ex._parse()), "scope.get('comp.x')")
        
        ex.scope = self.top.a
        ex.set(0.5)
        self.assertEqual(self._ast_to_text(ex._parse()), "scope.parent.get('comp.x')")
        self.assertEqual(0.5, self.top.comp.x)
        self.assertEqual(0.5, ex.evaluate(self.top)) # set scope back to self.top
        self.assertEqual(self._ast_to_text(ex._parse()), "scope.get('comp.x')")
        
        ex.text = 'comp.y'
        self.assertEqual(-3.14, ex.evaluate(self.top.a))
        ex.set(11.1)
        self.assertEqual(11.1, self.top.comp.y)
        self.assertEqual(self._ast_to_text(ex._parse()), "scope.parent.get('comp.y')")
        
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
        
if __name__ == "__main__":
    unittest.main()
    
    
