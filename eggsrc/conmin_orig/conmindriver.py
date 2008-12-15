"""
A CONMIN based driver.
"""

#public symbols
__all__ = []

__version__ = "0.1"

import conmin_1 
import numpy.numarray as numarray

from openmdao.main.driver import Driver
from openmdao.main.component import STATE_WAITING, RUN_UNKNOWN
from openmdao.main.float import Float
from openmdao.main.string import String
from openmdao.main.stringlist import StringList
from openmdao.main.int import Int
from openmdao.main.variable import INPUT
from openmdao.main.transext import translate_expr


def wrap_function(function, args):
    ncalls = [0]  # an array is used here so a reference will be returned
    def function_wrapper(x):
        ncalls[0] += 1
        return function(x, *args)
    return ncalls, function_wrapper


class CONMINdriver(Driver):
    """ Driver wrapper of CONMIN. """
    
    def __init__(self, name, parent=None, desc=None):
        Driver.__init__(self, name, parent, desc)
        self._first = True
        self._design_vars = []
        self.design_vals = []
        self.lower_bounds = None
        self.upper_bounds = None
        self._constraints = []
        self.constraint_vals = []
        self._objective = None
        self.objective_val = None
        self.objective_funct = None
        self.iprint = 1
        self.maxiters = 40
        
        
    def _set_constraints(self, cons):
        self._first = True
        self._constraints_code = []
        self._constraints = []
        for constraint in cons:
            try:
                text = translate_expr(constraint, self)
            except RuntimeError, err:
                raise RuntimeError("constraint '"+str(constraint)+"' is invalid")
            code = compile(text,'<string>','eval')
            self._constraints_code.append(code)
        self._constraints = cons
        
    def _get_constraints(self):
        return self._constraints
        
    constraints = property(_get_constraints, _set_constraints)
    
    def _set_desvars(self, dv):
        self._first = True
        self._design_vars_code = []
        self._design_vars = []
        for desvar in dv:
            try:
                text = translate_expr(desvar, self)
            except RuntimeError, err:
                raise RuntimeError("design variable '"+str(desvar)+"' is invalid")
            code = compile(text,'<string>','eval')
            self._design_vars_code.append(code)
        self._design_vars = dv
        
    def _get_desvars(self):
        return self._design_vars
        
    design_vars = property(_get_desvars, _set_desvars)
    
    def _set_objective(self, obj):
        self._first = True
        self._objective_code = None
        self._objective = None
        try:
            text = translate_expr(obj, self)
        except RuntimeError, err:
            raise RuntimeError("objective '"+str(obj)+"' is invalid")
        code = compile(text,'<string>','eval')
        self._objective_funct = lambda x: eval(code)
        self._objective = obj
        
    def _get_objective(self):
        return self._objective
        
    objective = property(_get_desvars, _set_desvars)
    
        
        
    def execute(self):
            
        # run the other components
        self.parent.workflow.run()
            
        # gather values for the objective and the constraints
        self.conmin()
        

    def _config_conmin(self):
        conmin_1.cnmn1.ndv = len(self._design_vars)
        conmin_1.cnmn1.ncon = len(self._constraints)
        if not self.lower_bounds==None or not self.upper_bounds==None:
            conmin_1.cnmn1.nside = 2*conmin_1.cnmn1.ndv
        else:
            conmin_1.cnmn1.nside = 0
        self.design_vals = 
        conmin_1.cnmn1.nacmx1 = max(conmin_1.cnmn1.ndv,
                                    conmin_1.cnmn1.ncon+conmin_1.cnmn1.nside)+1
        n1 = conmin_1.cnmn1.ndv+2
        n2 = conmin_1.cnmn1.ncon+2*conmin_1.cnmn1.ndv
        n3 = conmin_1.cnmn1.nacmx1
        n4 = max(n3,conmin_1.cnmn1.ndv)
        n5 = 2*n4

        self.x = numarray.zeros(n1,'d')
        self.vlb = numarray.zeros(n1,'d')
        if self.lower_bounds is not None:
            for i in range(conmin_1.cnmn1.ndv):
                self.vlb[i] = self.lower_bounds[i]
        self.vub = numarray.zeros(n1,'d')
        if self.upper_bounds is not None:
            for i in range(conmin_1.cnmn1.ndv):
                self.vub[i] = self.upper_bounds[i]
                
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
        conmin_1.cnmn1.igoto = 0
        conmin_1.cnmn1.nac = 0
        conmin_1.cnmn1.iter = 0

        conmin_1.cnmn1.iprint = self.iprint
        conmin_1.cnmn1.itmax = self.maxiters


    def conmin(self):
        func = self._objective_funct
        
        # set conmin array sizes and such
        if self._first is True:
            self.objective_val,self.constraint_vals = func(x0)
            self._config_conmin()
                        
#        fcalls, func = wrap_function(func, args)

        while conmin_1.cnmn1.igoto or self._first is True:
            self._first = False
            conmin_1.cnmn1.obj = self.objective_val
            zz=conmin_1.conmin(self.x,vlb,vub,g,scal,df,a,s,g1,g2,b,c,isc,ic,ms1)
            (self.x,scal,a,s,g1,g2,b,c,isc,ic,ms1) = zz
            
            # I don't understand what's going on in the following lines...
            # Why are they calling the objective function twice when info==2? The inputs are identical...
            if conmin_1.cnmn1.info==1:
                (self.objective_val,gg) = func(x)
                for i in range(conmin_1.cnmn1.ncon):
                    g[i] = gg[i]
            if conmin_1.cnmn1.info==2:
                (self.objective_val,gg) = func(x)
                for i in range(conmin_1.cnmn1.ncon):
                    g[i] = gg[i]


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

  x = conmin(ff,x0,xmin,xmax)
  xx = x[:ndv]
  print "back from conmin"
  print xx
  print type(x)
