import ast

from sympy import Symbol, diff, sympify
from sympy.core.function import Derivative
from sympy.functions import *

from openmdao.main.printexpr import print_node
from openmdao.main.expreval import _get_long_name
from openmdao.main.printexpr import transform_expression

class SymbolicDerivativeError(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return repr(self.value)


#class SymTransformer(ast.NodeTransformer):
    #"""Transforms an expression into a sympy'd version.
    #It requires Symbol and sympify from sympy to be defined
    #in globals() in order to evaluate the transformed expression.
    #"""
    #def visit_Name(self, node):
        #name = node.id
        #args = [ast.Str(s=name)]
        #called_obj = ast.Name(id='Symbol', ctx=ast.Load())

        #return ast.copy_location(ast.Call(func=called_obj, args=args,
                                          #ctx=node.ctx, keywords=[]), node)

    #def visit_Num(self, node):
        #args = [node]
        #called_obj = ast.Name(id='sympify', ctx=ast.Load())

        #return ast.copy_location(ast.Call(func=called_obj, args=args,
                                          #ctx=ast.Load(), keywords=[]), node)

def SymGrad(ex, vars):
    """Symbolic gradient."""

    s=[]
    var_map = {}
    for i,var in enumerate(vars):
        s_var = Symbol(var)
        s.append(s_var)
        var_map[var] = "s[%d]"%i

    newex=ex
    newex = transform_expression(ex, var_map)
    
    exec "newex="+newex.replace('math.', '')
    grad=[]
    for i,var in enumerate(vars):
        d = diff(newex, Symbol(var)).evalf()
        diff_str=d.__str__()
        if isinstance(d, Derivative) or 'Derivative' in diff_str \
                                     or 'im(' in diff_str:
            raise SymbolicDerivativeError('Could not symbolically differentiate expression')
        grad.append(diff_str)
    return grad
  
#not used anywhere anymore    
# def SymHess(ex, vars):
#     """ Symbolic Hessian."""
#     s = [Symbol(v) for v in vars]

#     newex=ex
#     for i,v in enumerate(vars):
#         newex = newex.replace(v, "s["+str(i)+"]") 
#     exec "newex="+newex

#     hess=[]
#     for i in xrange(len(vars)):
#         row = []
#         for k in xrange(len(vars)):
#             d=diff(newex, s[i], s[k])
#             diff_str = d.__str__()
#             if isinstance(d, Derivative) or 'Derivative' in diff_str:
#                 raise SymbolicDerivativeError('Could not symbolically differentiate expression')
#             row.append(d.__str__())
#         hess.append(row)
#     return hess

