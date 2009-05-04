# pylint: disable-msg=C0103

#public symbols
__all__ = ['CONMINdriver']

__version__ = "0.1"

import conmin.conmin as conmin
import numpy.numarray as numarray

from openmdao.main import Driver, ArrayVariable, String, StringList, \
                          ExprEvaluator
from openmdao.main.exceptions import RunStopped
from openmdao.main.variable import INPUT


class CONMINdriver(Driver):
    """ Driver wrapper of Fortran version of CONMIN. 
    
    NOTE: This implementation does not support multiple instances of
    CONMINdriver within the same process because the common block information
    used by conmin is not copied and restored per instance at this time.
    
    .. parsed-literal::
    
       TODO: add copy/restore of common block info per instance    
       TODO: make CONMIN's handling of user calculated gradients 
             accessible through CONMINdriver
            
    """
    
    def __init__(self, name, parent=None, doc=None):
        super(CONMINdriver, self).__init__(name, parent, doc)
        
        # TODO: this really needs to be a copy 
        # (see conmin user's guide for how to save state)
        self.cnmn1 = conmin.cnmn1

        self._first = True
        self._design_vars = []
        self.design_vals = numarray.zeros(0,'d')
        self._lower_bounds = numarray.zeros(0,'d')
        self._upper_bounds = numarray.zeros(0,'d')
        self._constraints = []
        self.constraint_vals = numarray.zeros(0,'d')
        self.cons_active_or_violated  = numarray.zeros(0, 'i')
        self._objective = None
        self.objective_val = 0.
        self.iprint = 0
        self.maxiters = 40
        self.gradients = None
        # vector of scaling parameters
        self.scal = numarray.ones(2, 'd')
        # gradient of objective w.r.t x[i]
        self.df = numarray.zeros(2, 'd')
        # move direction in the optimization space
        self.s = numarray.zeros(2, 'd')
        self.gradients = numarray.zeros((2, 1), 'd')
        # temp storage
        self._b = numarray.zeros((1, 1), 'd')
        # temp storage
        self._c = numarray.zeros(1, 'd')
        # temp storage
        self._ms1 = numarray.zeros(2, 'i')
        
        # temp storage for constraints
        self.g1 = numarray.zeros(0,'d')
        self.g2 = numarray.zeros(0,'d')
        self.cons_is_linear = numarray.zeros(0, 'i') 
                
        StringList('design_vars', self, INPUT)
        StringList('constraints', self, INPUT)
        String('objective', self, INPUT, default=None)
        ArrayVariable('upper_bounds', self, INPUT)
        ArrayVariable('lower_bounds', self, INPUT)
        self.make_public(['design_vals',
                          'constraint_vals', 'objective_val',
                          'iprint', 'maxiters'])
        
    def __getstate__(self):
        """Return dict representing this container's state."""
        state = super(CONMINdriver, self).__getstate__()
        state['cnmn1'] = None
        return state

    def __setstate__(self, state):
        """Restore this component's state."""
        super(CONMINdriver, self).__setstate__(state)
        self.cnmn1 = conmin.cnmn1
        self._first = True

    def _set_desvars(self, dv):
        self._first = True
        self._design_vars = []
        self._design_var_setters = []
        for i, desvar in enumerate(dv):
            try:
                self._design_vars.append(ExprEvaluator(desvar, self))
            except RuntimeError:
                self.raise_exception("design variable '"+str(desvar)+
                                     "' is invalid", RuntimeError)
            
            # now create an expression to set each design variable to
            # the corresponding value in self.design_vals
            self._design_var_setters.append(
                ExprEvaluator(desvar+'=design_vals['+str(i)+']', self))
            
        self.design_vals = numarray.zeros(len(dv)+2, 'd')
        
        # FIXME: we're probably causing CONMIN to do some unnecessary 
        #        bounds checking by making these the same size as design_vars
        #        even if the user doesn't set them.
        if self.upper_bounds.size != self.design_vals.size:
            self._upper_bounds = numarray.array([1.e99]*len(self.design_vals))
        if self.lower_bounds.size != self.design_vals.size:
            self._lower_bounds = numarray.array([-1.e99]*len(self.design_vals))
            
        self.scal = numarray.ones(len(dv)+2, 'd')
        self.df = numarray.zeros(len(dv)+2, 'd')
        self.s = numarray.zeros(len(dv)+2, 'd')      
        
    def _get_desvars(self):
        return [x.text for x in self._design_vars]
        
    design_vars = property(_get_desvars, _set_desvars, None,
        'An array of design variable names. These names can include '+
        'array indexing.')
        
    def _set_constraints(self, cons):
        self._first = True
        self._constraints = []
        for constraint in cons:
            try:
                self._constraints.append(ExprEvaluator(constraint, self))
            except RuntimeError:
                self.raise_exception("constraint '"+str(constraint)+
                                     "' is invalid", RuntimeError)
            
        length = len(cons)+2*len(self.design_vals)
        self.constraint_vals = numarray.zeros(length, 'd')
        # temp storage of constraint and des vals
        self.g1 = numarray.zeros(length, 'd') 
        # temp storage of constraint vals
        self.g2 = numarray.zeros(length, 'd') 
        # if constraint i is known to be a linear function of des vals, 
        # set cons_is_linear[i] to 1, otherwise set it to 0. This is 
        # not essential is is for efficiency only.
        self.cons_is_linear = numarray.zeros(length, 'i') 
        
    def _get_constraints(self):
        return [x.text for x in self._constraints]
        
    constraints = property(_get_constraints, _set_constraints, None,
        'An array of expression strings indicating constraints.'+
        ' A value of < 0 for the expression indicates that the constraint '+
        'is violated.')
    
    
    def _set_objective(self, obj):
        self._first = True
        self._objective = None
        try:
            self._objective = ExprEvaluator(obj, self)
        except RuntimeError:
            self.raise_exception("objective '"+str(obj)+"' is invalid",
                                 RuntimeError)
        
    def _get_objective(self):
        if self._objective is None:
            return ''
        else:
            return self._objective.text
        
    objective = property(_get_objective, _set_objective, None, 
       'An string containing the objective function expression.')

    def _set_lower_bounds(self, val):
        vv = numarray.zeros(len(val)+2)
        if len(val) != len(self._design_vars):
            self.raise_exception('size of new lower bound array ('+
                                 str(len(val))+
                                 ') does not match number of design vars ('+
                                 str(len(self._design_vars))+')', ValueError)
        for i, lb in enumerate(val):
            vv[i] = lb
        self._lower_bounds = vv
            
    def _get_lower_bounds(self):
        return self._lower_bounds
    
    lower_bounds = property(_get_lower_bounds, _set_lower_bounds, None,
          'Array of constraints on the minimum value of each design variable.')

    def _set_upper_bounds(self, val):
        vv = numarray.zeros(len(val)+2)
        if len(val) != len(self._design_vars):
            self.raise_exception('size of new upper bound array ('+
                                 str(len(val))+
                                 ') does not match number of design vars ('+
                                 str(len(self._design_vars))+')', ValueError)
        
        for i, ub in enumerate(val):
            vv[i] = ub
        self._upper_bounds = vv
            
    def _get_upper_bounds(self):
        return self._upper_bounds
    
    upper_bounds = property(_get_upper_bounds, _set_upper_bounds, None,
          'Array of constraints on the maximum value of each design variable.')
    
    def execute(self):
        """Perform the optimization."""
        
        # set conmin array sizes and such
        if self._first is True:
            self._config_conmin()
        self.cnmn1.igoto = 0
        
        # perform an initial run for self-consistency
        self.parent.workflow.run()
        
        # get the initial values of the design variables
        for i, dv in enumerate(self._design_vars):
            self.design_vals[i] = dv.evaluate()
        self.objective_val = self._objective.evaluate()
            
        # loop until optimized
        while conmin.cnmn1.igoto or self._first is True:
            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            self._first = False            
            
            conmin.cnmn1.obj = numarray.array(self.objective_val)
            (self.design_vals,
             self.scal, self.gradients, self.s,
             self.g1, self.g2, self._b, self._c,
             self.cons_is_linear,
             self.cons_active_or_violated, self._ms1) = \
                 conmin.conmin(self.design_vals,
                               self.lower_bounds, self.upper_bounds,
                               self.constraint_vals,
                               self.scal, self.df,
                               self.gradients,
                               self.s, self.g1, self.g2, self._b, self._c,
                               self.cons_is_linear,
                               self.cons_active_or_violated, self._ms1)
            
            self._update_design_variables()

            # update the model
            self.parent.workflow.run()
# TODO: 'step around' ill-behaved cases.

            # calculate objective and constraints
            if conmin.cnmn1.info == 1:
                self._update_objective_val()
                self._update_constraint_vals()
            # calculate gradients
            elif conmin.cnmn1.info == 2:
                self.raise_exception('user defined gradients not yet supported',
                                     NotImplementedError)

    def _update_objective_val(self):
        """Evaluate the new objective."""
        if self._objective is None:
            self.raise_exception('No objective has been set', RuntimeError)
        else:
            self.objective_val = self._objective.evaluate()
               
    def _update_constraint_vals(self):
        """Calculate new constraint values."""
        for i, con in enumerate(self._constraints):
            self.constraint_vals[i] = con.evaluate()
            
    def _update_design_variables(self):
        """Set the new values of the design variables into the model."""
        for dv in self._design_var_setters:
            dv.evaluate()

    def _config_conmin(self):
        """Set up arrays for the FORTRAN conmin routine, and perform some
        basic validation.
        """
        self.cnmn1.ndv = len(self._design_vars)
        
        if self.cnmn1.ndv < 1:
            self.raise_exception('no design variables specified', RuntimeError)
            
        if self._objective is None:
            self.raise_exception('no objective specified', RuntimeError)
        
        self.cnmn1.ncon = len(self._constraints)
        
        if not self._lower_bounds.size == 0 or not self._upper_bounds.size == 0:
            self.cnmn1.nside = 2*len(self._design_vars)
        else:
            self.cnmn1.nside = 0

        self.cnmn1.nacmx1 = max(len(self._design_vars),
                                len(self._constraints)+self.cnmn1.nside) + 1

        n1 = len(self._design_vars)+2
        #n2 = len(self._constraints)+2*len(self._design_vars)
        n3 = self.cnmn1.nacmx1
        n4 = max(n3, len(self._design_vars))
        n5 = 2*n4
                
        # array of active or violated constraints (ic in CONMIN)
        self.cons_active_or_violated = numarray.zeros(n3, 'i')
        self.gradients = numarray.zeros((int(n1), int(n3)), 'd')
        # temp storage
        self._b = numarray.zeros((int(n3), int(n3)), 'd')
        # temp storage
        self._c = numarray.zeros(n4, 'd')
        # temp storage
        self._ms1 = numarray.zeros(n5, 'i')

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

