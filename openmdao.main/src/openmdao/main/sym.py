
from sympy import Symbol, diff
from sympy.core.function import Derivative
from sympy.functions import *

class SymbolicDerivativeError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)


def SymGrad(ex,vars):
    """Symbolic gradient."""
    s=[]
    for var in vars:
        s.append(Symbol(var))

    newex=ex
    for i in xrange(len(vars)):
        newex = newex.replace(vars[i],"s["+str(i)+"]") 
    exec "newex="+newex
    grad=[]
    for i in xrange(len(vars)):
        d = diff(newex,s[i]).evalf()
        diff_str=d.__str__()
        if isinstance(d, Derivative) or 'Derivative' in diff_str:
            raise SymbolicDerivativeError('Could not symbolically differentiate expression')
        grad.append(diff_str)
    return grad
    
def SymHess(ex, vars):
    """ Symbolic Hessian."""
    s = [Symbol(v) for v in vars]

    newex=ex
    for i,v in enumerate(vars):
        newex = newex.replace(v, "s["+str(i)+"]") 
    exec "newex="+newex

    hess=[]
    for i in xrange(len(vars)):
        row = []
        for k in xrange(len(vars)):
            d=diff(newex, s[i], s[k])
            diff_str = d.__str__()
            if isinstance(d, Derivative) or 'Derivative' in diff_str:
                raise SymbolicDerivativeError('Could not symbolically differentiate expression')
            row.append(d.__str__())
        hess.append(row)
    return hess

