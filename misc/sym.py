from sympy import Symbol,diff
from sympy.core.function import Derivative
from sympy.functions import *
import numpy

class SymbolicDerivativeError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)


def SymGrad(ex,vars):
    """Symbolic gradient"""
    s=[]
    for var in vars:
        s.append(Symbol(var))

    newex=ex
    for i in xrange(len(vars)):
        newex=newex.replace(vars[i],"s["+str(i)+"]") 
    exec "newex="+newex
        
    grad=[]
    for i in xrange(len(vars)):
        d=diff(newex,s[i])
        diff_str=d.__str__()
        if isinstance(d, Derivative) or 'Derivative' in diff_str:
            raise SymbolicDerivativeError('Could not symbolically differentiate expression')
        grad.append(diff_str)
    return grad
    
def SymHess(ex,vars):
    """ symbolic hessian"""
    s=[]
    for var in vars:
        s.append(Symbol(var))

    newex=ex
    for i in xrange(len(vars)):
        newex=newex.replace(vars[i],"s["+str(i)+"]") 
    exec "newex="+newex

    hess=[]
    for i in xrange(len(vars)):
        row=[]
        for k in xrange(len(vars)):
            d=diff(newex,s[i],s[k])
            diff_str=d.__str__()
            if isinstance(d, Derivative) or 'Derivative' in diff_str:
                raise SymbolicDerivativeError('Could not symbolically differentiate expression')
            row.append(d.__str__())
        hess.append(row)
    return hess

    
#ex="x.c1[0]**2+sqrt(y.c2**3)+exp(y.c2)+x.c1[0]*exp(-abs((y.c2)))"
#vars=["x.c1[0]","y.c2"]
ex='x+factorial(x)**2'
vars=['x']
print
#print ex
print
print SymGrad(ex,vars)
#print
#print SymHess(ex,vars)
