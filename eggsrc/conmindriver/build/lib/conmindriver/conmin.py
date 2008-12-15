import conmin_1 
import numpy.numarray as numarray

def wrap_function(function, args):
    ncalls = [0]
    def function_wrapper(x):
        ncalls[0] += 1
        return function(x, *args)
    return ncalls, function_wrapper

def conmin(func,x0,xmin=None,xmax=None,args=(),iprint=0):
    conmin_1.cnmn1.ndv = len(x0)
    fcalls, func = wrap_function(func, args)
    (f0,g0) = func(x0)
    conmin_1.cnmn1.ncon = len(g0)
    if not xmin==None or not xmax==None:
        conmin_1.cnmn1.nside = 2*conmin_1.cnmn1.ndv
    else:
        conmin_1.cnmn1.nside = 0
    conmin_1.cnmn1.nacmx1 = max(conmin_1.cnmn1.ndv,
                                conmin_1.cnmn1.ncon+conmin_1.cnmn1.nside)+1
    n1 = conmin_1.cnmn1.ndv+2
    n2 = conmin_1.cnmn1.ncon+2*conmin_1.cnmn1.ndv
    n3 = conmin_1.cnmn1.nacmx1
    n4 = max(n3,conmin_1.cnmn1.ndv)
    n5 = 2*n4
    
    x = numarray.zeros(n1,'d')
    vlb = numarray.zeros(n1,'d')
    if xmin:
        for i in range(conmin_1.cnmn1.ndv):
            vlb[i] = xmin[i]
    vub = numarray.zeros(n1,'d')
    if xmax:
        for i in range(conmin_1.cnmn1.ndv):
            vub[i] = xmax[i]
    g = numarray.zeros(n2,'d')
    scal = numarray.ones(n1,'d')
    df = numarray.zeros(n1,'d')
    a = numarray.zeros((int(n1),int(n3)),'d')
    s = numarray.zeros(n1,'d')
    g1 = numarray.zeros(n2,'d')
    g2 = numarray.zeros(n2,'d')
    b = numarray.zeros((int(n3),int(n3)),'d')
    c = numarray.zeros(n4,'d')
    isc = numarray.zeros(n2,'i')
    ic = numarray.zeros(n3,'i')
    ms1 = numarray.zeros(n5,'i')
    
    conmin_1.cnmn1.infog = 0
    conmin_1.cnmn1.info = 0
    conmin_1.cnmn1.nfdg = 0
    conmin_1.cnmn1.iprint = 4
    conmin_1.cnmn1.itmax = 40
    conmin_1.cnmn1.icndir = 0
    conmin_1.cnmn1.nscal = 0
    conmin_1.cnmn1.fdch = 0.0
    conmin_1.cnmn1.fdchm = 0.0
    conmin_1.cnmn1.ct = 0.
    conmin_1.cnmn1.ctmin = 0.
    conmin_1.cnmn1.ctlmin = 0.
    conmin_1.cnmn1.theta = 0.
    conmin_1.cnmn1.phi = 0.
    conmin_1.cnmn1.delfun = 0.
    conmin_1.cnmn1.dabfun = 0.
    conmin_1.cnmn1.linobj = 0
    conmin_1.cnmn1.itrm = 0
    conmin_1.cnmn1.alphax = 0.
    conmin_1.cnmn1.abobj1 = 0.
    conmin_1.cnmn1.ctl = 0.

    conmin_1.cnmn1.obj = f0
    conmin_1.cnmn1.igoto = 0
    conmin_1.cnmn1.nac = 0
    conmin_1.cnmn1.iter = 0
    
    first = 1
    obj = f0
    while conmin_1.cnmn1.igoto or first:
        first = 0
        conmin_1.cnmn1.obj = obj
        zz=conmin_1.conmin(x,vlb,vub,g,scal,df,a,s,g1,g2,b,c,isc,ic,ms1)
        (x,scal,a,s,g1,g2,b,c,isc,ic,ms1) = zz
        if conmin_1.cnmn1.info==1:
            (obj,gg) = func(x)
            for i in range(conmin_1.cnmn1.ncon):
                g[i] = gg[i]
        if conmin_1.cnmn1.info==2:
            (obj,gg) = func(x)
            for i in range(conmin_1.cnmn1.ncon):
                g[i] = gg[i]
    return x,obj


def ff(x):
  obj = x[0]**2+x[1]**2
  g = [x[0]+x[1]+1]
  #g = array(g)
  print "gg-----", g
  return obj,g


if __name__ == "__main__":    
  print "calling conmin"
  x0 = [5.,9.]
  ndv = len(x0)
  xmin = [-100.,-100.]
  xmax = [ 100., 100.]

  x = conmin(ff,x0,xmin,xmax,args=())
  xx = x[:ndv]
  print "back from conmin"
  print xx
  print type(x)
