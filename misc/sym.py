from sympy import Symbol,diff,exp,sin,sqrt
import numpy

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
        d=diff(newex,s[i]).simplify()
        grad.append(d.__str__())
    
    return numpy.array(grad)
    
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
            d=diff(newex,s[i],s[k]).simplify()
            row.append(d.__str__())
        hess.append(numpy.array(row))
    return numpy.array(hess)

    
ex="x.c1[0]**2+sqrt(y.c2**3)+exp(y.c2)+x.c1[0]*sin(y.c2)"

vars=["x.c1[0]","y.c2"]
print
print ex
print
print SymGrad(ex,vars)
print
print SymHess(ex,vars)
