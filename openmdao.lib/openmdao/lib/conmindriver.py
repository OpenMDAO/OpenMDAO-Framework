"""
A CONMIN based driver.
"""

#public symbols
__all__ = []

__version__ = "0.1"

import copy

import conmin.conmin as conmin
import numpy.numarray as numarray

from openmdao.main.driver import Driver
from openmdao.main.component import STATE_WAITING, RUN_UNKNOWN
from openmdao.main.float import Float
from openmdao.main.arrayvar import ArrayVariable
from openmdao.main.string import String
from openmdao.main.stringlist import StringList
from openmdao.main.int import Int
from openmdao.main.variable import INPUT
from openmdao.main.transexpr import ExprEvaluator


class CONMINdriver(Driver):
    """ Driver wrapper of CONMIN. 
    NOTE: This implementation does not support multiple instances of CONMINdriver within
    the same process because the common block information used by conmin is not copied
    and restored per instance at this time.
    
    TODO: add copy/restore of common block info per instance
    TODO: make CONMIN's handling of user calculated gradients accessible through CONMINdriver
    """
    
    def __init__(self, name, parent=None, desc=None):
        Driver.__init__(self, name, parent, desc)
        self._first = True
        self._design_vars = []
        self._design_vars_code = []
        self._design_vars_set_code = []
        self.design_vals = numarray.zeros(0,'d')
        self._lower_bounds = numarray.zeros(0,'d')
        self._upper_bounds = numarray.zeros(0,'d')
        self._constraints = []
        self._constraints_code = []
        self.constraint_vals = numarray.zeros(0,'d')
        self._objective = None
        self.objective_code = None
        self.objective_val = 0.
        self.iprint = 0
        self.maxiters = 40
        self.gradients = None
        # ???this really needs to be a copy
        self.cnmn1 = conmin.cnmn1
        
        StringList('design_vars', self, INPUT)
        StringList('constraints', self, INPUT)
        String('objective', self, INPUT)
        ArrayVariable('upper_bounds', self, INPUT)
        ArrayVariable('lower_bounds', self, INPUT)
        self.make_public(['design_vals',
                          'constraint_vals','objective_val',
                          'iprint','maxiters'])
        
    def _set_desvars(self, dv):
        self._first = True
        self._design_vars_code = []
        self._design_vars = []
        for i,desvar in enumerate(dv):
            try:
                text = translate_expr(desvar, self)
            except RuntimeError, err:
                self.raise_exception("design variable '"+str(desvar)+
                                     "' is invalid", RuntimeError)
            code = compile(text,'<string>','eval')
            self._design_vars_code.append(code)
            
            # now create an expression to set each design variable to
            # the corresponding value in self.design_vals
            setter = desvar+'=design_vals['+str(i)+']'
            try:
                text = translate_expr(setter, self)
            except RuntimeError, err:
                self.raise_exception("expression '"+str(setter)+
                                     "' is invalid", RuntimeError)
            code = compile(text,'<string>','eval')
            self._design_vars_set_code.append(code)
            
        self._design_vars = dv
        self.design_vals = numarray.zeros(len(dv)+2,'d')
        if self.upper_bounds.size != self.design_vals.size:
            self._upper_bounds = numarray.array([1.e99 for x in self.design_vals])
        if self.lower_bounds.size != self.design_vals.size:
            self._lower_bounds = numarray.array([-1.e99 for x in self.design_vals])
            
        # vector of scaling parameters
        self.scal = numarray.ones(len(dv)+2,'d')
        # gradient of objective w.r.t x[i]
        self.df = numarray.zeros(len(dv)+2,'d')
        # move direction in the optimization space
        self.s = numarray.zeros(len(dv)+2,'d')      
        
    def _get_desvars(self):
        return self._design_vars
        
    design_vars = property(_get_desvars, _set_desvars)
        
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
        length = len(cons)+2*len(self.design_vals)
        self.constraint_vals = numarray.zeros(length,'d')
        self.g1 = numarray.zeros(length,'d') # temp storage of constraint and des vals
        self.g2 = numarray.zeros(length,'d') # temp storage of constraint vals
        # if constraint i is known to be a linear function of des vals, 
        # set cons_is_linear[i] to 1, otherwise set it to 0. This is 
        # not essential is is for efficiency only.
        self.cons_is_linear = numarray.zeros(length,'i') 
        
    def _get_constraints(self):
        return self._constraints
        
    constraints = property(_get_constraints, _set_constraints)
    
    
    def _set_objective(self, obj):
        self._first = True
        self._objective_code = None
        self._objective = None
        try:
            text = translate_expr(obj, self)
        except RuntimeError, err:
            raise RuntimeError("objective '"+str(obj)+"' is invalid")
        self._objective_code = compile(text,'<string>','eval')
        self._objective = obj
        
    def _get_objective(self):
        return self._objective
        
    objective = property(_get_objective, _set_objective)

    def _set_lower_bounds(self, val):
        vv = numarray.zeros(len(val)+2)
        if len(val) != len(self._design_vars):
            self.raise_exception('size of new lower bound array ('+str(len(val))+
                                 ') does not match number of design vars ('+
                                 str(len(self._design_vars)), ValueError)
        for i,lb in enumerate(val):
            vv[i] = lb
        self._lower_bounds = vv
            
    def _get_lower_bounds(self):
        return self._lower_bounds
    
    lower_bounds = property(_get_lower_bounds, _set_lower_bounds)

    def _set_upper_bounds(self, val):
        vv = numarray.zeros(len(val)+2)
        if len(val) != len(self._design_vars):
            self.raise_exception('size of new upper bound array ('+str(len(val))+
                                 ') does not match number of design vars ('+
                                 str(len(self._design_vars))+')', ValueError)
        
        for i,ub in enumerate(val):
            vv[i] = ub
        self._upper_bounds = vv
            
    def _get_upper_bounds(self):
        return self._upper_bounds
    
    upper_bounds = property(_get_upper_bounds, _set_upper_bounds)
    
    def execute(self):
        # set conmin array sizes and such
        if self._first is True:
            self._config_conmin()
        self.cnmn1.igoto = 0
        
        # perform an initial run for self-consistency
        self.parent.workflow.run()
        
        # get the initial values of the design variables
        for i,code in enumerate(self._design_vars_code):
            self.design_vals[i] = eval(code)
        self.objective_val = eval(self._objective_code)
            
        # loop until optimized
        while conmin.cnmn1.igoto or self._first is True:
            self._first = False            
            
            conmin.cnmn1.obj = numarray.array(self.objective_val)
            zz=conmin.conmin(self.design_vals,self.lower_bounds,self.upper_bounds,
                               self.constraint_vals,self.scal,self.df,
                               self.gradients,self.s,self.g1,self.g2,self._b,self._c,
                               self.cons_is_linear,self.cons_active_or_violated,self._ms1)
            (self.design_vals,self.scal,self.gradients,self.s,self.g1,self.g2,self._b,self._c,
             self.cons_is_linear,self.cons_active_or_violated,self._ms1) = zz
            
            self.update_design_variables()

            # update the model
            self.parent.workflow.run()
            
            # calculate objective and constraints
            if conmin.cnmn1.info==1:
                self.update_objective_val()
                self.update_constraint_vals()
            # calculate gradients
            elif conmin.cnmn1.info==2:
                self.raise_exception('user defined gradients not yet supported',
                                     NotImplementedError)
        
    def update_objective_val(self):
        if self._objective_code is None:
            self.raise_exception('No objective has been set',RuntimeError)
        else:
            self.objective_val = eval(self._objective_code, self.__dict__, locals())
               
    def update_constraint_vals(self):
        for i,code in enumerate(self._constraints_code):
            self.constraint_vals[i] = eval(code, self.__dict__, locals())
            
    def update_design_variables(self):
        for code in self._design_vars_set_code:
            eval(code, self.__dict__, locals())

    def _config_conmin(self):
        self.cnmn1.ndv = len(self._design_vars)
        self.cnmn1.ncon = len(self._constraints)
        
        if not self._lower_bounds.size==0 or not self._upper_bounds.size==0:
            self.cnmn1.nside = 2*len(self._design_vars)
        else:
            self.cnmn1.nside = 0

        self.cnmn1.nacmx1 = max(len(self._design_vars),
                                    len(self._constraints)+conmin.cnmn1.nside)+1
        n1 = len(self._design_vars)+2
        n2 = len(self._constraints)+2*len(self._design_vars)
        n3 = self.cnmn1.nacmx1
        n4 = max(n3,len(self._design_vars))
        n5 = 2*n4
                
        # array of active or violated constraints (ic in CONMIN)
        self.cons_active_or_violated = numarray.zeros(n3,'i')
        self.gradients = numarray.zeros((int(n1),int(n3)),'d')
        # temp storage
        self._b = numarray.zeros((int(n3),int(n3)),'d')
        # temp storage
        self._c = numarray.zeros(n4,'d')
        # temp storage
        self._ms1 = numarray.zeros(n5,'i')

        self.cnmn1.infog = 0
        self.cnmn1.info = 0
        self.cnmn1.nfdg = 0
        self.cnmn1.icndir = 0
        self.cnmn1.nscal = 0
        self.cnmn1.fdch = 0.0
        self.cnmn1.fdchm = 0.0
        self.cnmn1.ct = 0.
        self.cnmn1.ctmin = 0.
        self.cnmn1.ctlmin = 0.
        self.cnmn1.theta = 0.
        self.cnmn1.phi = 0.
        self.cnmn1.delfun = 0.
        self.cnmn1.dabfun = 1.e-8
        self.cnmn1.linobj = 0
        self.cnmn1.itrm = 0
        self.cnmn1.alphax = 0.
        self.cnmn1.abobj1 = 0.
        self.cnmn1.ctl = 0.
        self.cnmn1.igoto = 0
        self.cnmn1.nac = 0
        self.cnmn1.iter = 0

        self.cnmn1.iprint = self.iprint
        self.cnmn1.itmax = self.maxiters
