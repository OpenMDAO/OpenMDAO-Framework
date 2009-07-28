# pylint: disable-msg=C0103

#public symbols
__all__ = ['CONMINdriver']

__version__ = "0.1"

from copy import copy
import numpy.numarray as numarray
import numpy

from enthought.traits.api import Int, Array, List, on_trait_change, TraitError
import conmin.conmin as conmin

from openmdao.main.api import Driver, StringRef, StringRefArray
from openmdao.main.exceptions import RunStopped


class _cnmn1(object):
    """Just a primitive data structure for storing common block data"""
    def __init__(self):
        self.clear()
        
    def clear(self):
        self.ndv = 0
        self.ncon = 0
        self.nside = 0
        self.nacmx1 = 0
        self.infog = 0
        self.info = 0
        self.nfdg = 0
        self.icndir = 0
        self.nscal = 0
        self.fdch = 0.0
        self.fdchm = 0.0
        self.ct = 0.
        self.ctmin = 0.
        self.ctlmin = 0.
        self.theta = 0.
        self.phi = 0.
        self.delfun = 0.
        self.dabfun = 1.e-8
        self.linobj = 0
        self.itrm = 0
        self.alphax = 0.
        self.abobj1 = 0.
        self.ctl = 0.
        self.igoto = 0
        self.nac = 0
        self.iter = 0
        self.iprint = 0
        self.itmax = 0
 
class _consav(object):
    def __init__(self):
        self.clear()
    
    def clear(self):
        self.dm1 = 0.0
        self.dm2 = 0.0
        self.dm3 = 0.0
        self.dm4 = 0.0
        self.dm5 = 0.0
        self.dm6 = 0.0
        self.dm7 = 0.0
        self.dm8 = 0.0
        self.dm9 = 0.0
        self.dm10 = 0.0
        self.dm11 = 0.0
        self.dm12 = 0.0
        self.dct = 0.0
        self.dctl = 0.0
        self.phi = 0.0
        self.abobj = 0.0
        self.cta = 0.0
        self.ctam = 0.0
        self.ctbm = 0.0
        self.obj1 = 0.0
        self.slope = 0.0
        self.dx = 0.0
        self.dx1 = 0.0
        self.fi = 0.0
        self.xi = 0.0
        self.dftdf1 = 0.0
        self.alp = 0.0
        self.fff = 0.0
        self.a1 = 0.0
        self.a2 = 0.0
        self.a3 = 0.0
        self.a4 = 0.0
        self.f1 = 0.0
        self.f2 = 0.0
        self.f3 = 0.0
        self.f4 = 0.0
        self.cv1 = 0.0
        self.cv2 = 0.0
        self.cv3 = 0.0
        self.cv4 = 0.0
        self.app = 0.0
        self.alpca = 0.0
        self.alpfes = 0.0
        self.alpln = 0.0
        self.alpmin = 0.0
        self.alpnc = 0.0
        self.alpsav = 0.0
        self.alpsid = 0.0
        self.alptot = 0.0
        self.rspace = 0.0
        self.idm1 = 0
        self.idm2 = 0
        self.idm3 = 0
        self.jdir = 0
        self.iobj = 0
        self.kobj = 0
        self.kcount = 0
        self.ncal = [0, 0]
        self.nfeas = 0
        self.mscal = 0
        self.ncobj = 0
        self.nvc = 0
        self.kount = 0
        self.icount = 0
        self.igood1 = 0
        self.igood2 = 0
        self.igood3 = 0
        self.igood4 = 0
        self.ibest = 0
        self.iii = 0
        self.nlnc = 0
        self.jgoto = 0
        self.ispace = [0, 0]
        
class CONMINdriver(Driver):
    """ Driver wrapper of Fortran version of CONMIN. 
    
    NOTE: This implementation does not support multiple instances of
    CONMINdriver within the same process because the common block information
    used by conmin is not copied and restored per instance at this time.
    
    .. parsed-literal::
    
       TODO: make CONMIN's handling of user calculated gradients 
             accessible through CONMINdriver
            
    """
    
    design_vars = StringRefArray(iostatus='out',
       desc='An array of design variable names. These names can include array indexing.')
    
    constraints = StringRefArray(iostatus='in',
            desc= 'An array of expression strings indicating constraints.'+
            ' A value of < 0 for the expression indicates that the constraint '+
            'is violated.')
    
    objective = StringRef(iostatus='in',
                      desc= 'A string containing the objective function expression.')
    
    upper_bounds = Array(dtype=numpy.float, iostatus='in',
        desc='Array of constraints on the maximum value of each design variable.')
    
    lower_bounds = Array(dtype=numpy.float, iostatus='in', 
        desc='Array of constraints on the minimum value of each design variable.')
        
    iprint = Int(0)
    maxiters = Int(40)
        
    def __init__(self, name, parent=None, doc=None):
        super(CONMINdriver, self).__init__(name, parent, doc)
        
        # Save data from common blocks into our CONMINdriver object
        self.cnmn1 = _cnmn1()
        self.consav = _consav()
        
        self.iter_count = 0
        self.design_vals = numarray.zeros(0,'d')
        self.lower_bounds = numarray.zeros(0,'d')
        self.upper_bounds = numarray.zeros(0,'d')
        self.cons_active_or_violated  = numarray.zeros(0, 'i')
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
        self._c = numarray.zeros(1, 'd')
        self._ms1 = numarray.zeros(2, 'i')
        
        # temp storage for constraints
        self.g1 = numarray.zeros(0,'d')
        self.g2 = numarray.zeros(0,'d')
        self.cons_is_linear = numarray.zeros(0, 'i') 
        
        
    def execute(self):
        """Perform the optimization."""
        # set conmin array sizes and such
        self._config_conmin()
        self.cnmn1.igoto = 0
        self.iter_count = 0
        
        # perform an initial run for self-consistency
        self.run_iteration()

        # get the initial values of the design variables
        for i, val in enumerate(self.design_vars):
            self.design_vals[i] = val.evaluate()

        # update constraint value array
        for i,v in enumerate(self.constraints):
            self.constraint_vals[i] = v.evaluate()
        #self.debug('%s: new iteration' % self.get_pathname())
        #self.debug('objective = %s' % self.objective)
        #self.debug('design vars = %s' % self.design_vars)
        
        # loop until optimized
        while self.cnmn1.igoto or self.iter_count == 0:
            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            self.iter_count += 1 
                        
            # calculate objective
            self.cnmn1.obj = numarray.array(self.objective.evaluate())
            #self.debug('iter_count = %d' % self.iter_count)
            #self.debug('objective = %f' % self.cnmn1.obj)
            #self.debug('design vals = %s' % self.design_vals[:-2])
            
# TODO: 'step around' ill-behaved cases.
            
            self._load_common_blocks()
            
            try:
                (self.design_vals,
                 self.scal, self.gradients, self.s,
                 self.g1, self.g2, self._b, self._c,
                 self.cons_is_linear,
                 self.cons_active_or_violated, self._ms1) = \
                     conmin.conmin(self.design_vals,
                                   self._lower_bounds, self._upper_bounds,
                                   self.constraint_vals,
                                   self.scal, self.df,
                                   self.gradients,
                                   self.s, self.g1, self.g2, self._b, self._c,
                                   self.cons_is_linear,
                                   self.cons_active_or_violated, self._ms1)
            except Exception, err:
                self.error(str(err))
                raise
            
            # common blocks are saved before, and loaded after execution
            self._save_common_blocks()
            
            # update the design variables in the model
            dvals = [float(val) for val in self.design_vals[:-2]]
            for var,val in zip(self.design_vars, dvals):
                var.set(val)
            
            # update the model
            self.run_iteration()
            
            # calculate constraints
            if self.cnmn1.info == 1:
                # update constraint value array
                for i,v in enumerate(self.constraints):
                    self.constraint_vals[i] = v.evaluate()
                #self.debug('constraints = %s'%self.constraint_vals)
                    
            # calculate gradients
            elif self.cnmn1.info == 2:
                self.raise_exception('user defined gradients not yet supported',
                                     NotImplementedError)


    def _config_conmin(self):
        """Set up arrays for the FORTRAN conmin routine, and perform some
        validation and make sure that array sizes are consistent.
        """
        self.cnmn1.clear()
        self.consav.clear()
        
        # size arrays based on number of design variables
        num_dvs = len(self.design_vars)
        self.design_vals = numarray.zeros(num_dvs+2, 'd')
        
        if num_dvs < 1:
            self.raise_exception('no design variables specified', RuntimeError)
            
        if self.objective is None:
            self.raise_exception('no objective specified', RuntimeError)
         
        # create lower_bounds numarray
        if len(self.lower_bounds) > 0:
            self._lower_bounds = numarray.zeros(len(self.lower_bounds)+2)
            if len(self.lower_bounds) != num_dvs:
                self.raise_exception('size of new lower bound array (%d) does not match number of design vars (%d)'%
                                     (len(self.lower_bounds),num_dvs), ValueError)
            for i, lb in enumerate(self.lower_bounds):
                self._lower_bounds[i] = lb
        else:
            self._lower_bounds = numarray.array(([-1.e99]*num_dvs)+[0.,0.])
            
            
        # create upper bounds numarray
        if len(self.upper_bounds) > 0:
            self._upper_bounds = numarray.zeros(len(self.upper_bounds)+2)
            if len(self.upper_bounds) != num_dvs:
                self.raise_exception('size of new upper bound array (%d) does not match number of design vars (%d)'%
                                     (len(self.upper_bounds),num_dvs), ValueError)
            
            for i, ub in enumerate(self.upper_bounds):
                self._upper_bounds[i] = ub
        else:
            self._upper_bounds = numarray.array(([1.e99]*num_dvs)+[0.,0.])
            
        self.scal = numarray.ones(num_dvs+2, 'd')
        self.df = numarray.zeros(num_dvs+2, 'd')
        self.s = numarray.zeros(num_dvs+2, 'd')
        
        # size constraint related arrays
        length = len(self.constraints)+2*num_dvs
        self.constraint_vals = numarray.zeros(length, 'd')
        # temp storage of constraint and des vals
        self.g1 = numarray.zeros(length, 'd') 
        # temp storage of constraint vals
        self.g2 = numarray.zeros(length, 'd') 
        # if constraint i is known to be a linear function of des vals, 
        # set cons_is_linear[i] to 1, otherwise set it to 0. This is 
        # not essential is is for efficiency only.
        self.cons_is_linear = numarray.zeros(length, 'i') 
        
        self.cnmn1.ndv = num_dvs
        self.cnmn1.ncon = len(self.constraints)
        
        if not self._lower_bounds.size == 0 or not self._upper_bounds.size == 0:
            self.cnmn1.nside = 2*num_dvs
        else:
            self.cnmn1.nside = 0

        self.cnmn1.nacmx1 = max(num_dvs,
                                len(self.constraints)+self.cnmn1.nside)+1
        n1 = num_dvs+2
        n3 = self.cnmn1.nacmx1
        n4 = max(n3, num_dvs)
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

        self.cnmn1.iprint = self.iprint
        self.cnmn1.itmax = self.maxiters
        

    @on_trait_change('objective') 
    def _refvar_changed(self, obj, name, old, new):
        expr = getattr(obj, name)
        try:
            expr.refs_valid()  # force checking for existence of vars referenced in expression
        except (AttributeError, RuntimeError), err:
            self.raise_exception("invalid value '%s' for input ref variable '%s': %s" % 
                                 (str(expr),name,err), TraitError)
        
    @on_trait_change('constraints, design_vars') 
    def _refvar_array_changed(self, obj, name, old, new):
        exprevals = getattr(obj, name)
        for i,expr in enumerate(exprevals):
            try:
                expr.refs_valid()  # force checking for existence of vars referenced in expression
            except (AttributeError, RuntimeError), err:
                self.raise_exception("invalid value '%s' for input ref variable '%s[%d]': %s" % 
                                     (str(expr),name,i,err), TraitError)
        
    def _load_common_blocks(self):
        """ Reloads the common blocks using the intermediate info saved in the class. """
        
        for name, value in self.cnmn1.__dict__.items():
            setattr( conmin.cnmn1, name, value )
        
        for name, value in self.consav.__dict__.items():
            setattr( conmin.consav, name, value  )
        
        
    def _save_common_blocks(self):
        """" Saves the common block data to the class to prevent trampling by
        other instances of CONMIN.
        """
        common = self.cnmn1
        for name, value in common.__dict__.items():
            if isinstance(value, numpy.ndarray):
                setattr(common, name, getattr(conmin.cnmn1, name).copy())
            else:
                setattr(common, name, type(value)(getattr(conmin.cnmn1, name)))
        
        consav = self.consav
        for name, value in consav.__dict__.items():
            if isinstance(value, numpy.ndarray):
                setattr(consav, name, getattr(conmin.consav, name).copy())
            else:
                setattr(consav, name, type(value)(getattr(conmin.consav, name)))
        
        
