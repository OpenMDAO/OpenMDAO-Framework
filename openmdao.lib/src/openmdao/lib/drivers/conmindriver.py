# pylint: disable-msg=C0103

#public symbols
__all__ = ['CONMINdriver']

__version__ = "0.1"

import conmin.conmin as conmin
import numpy.numarray as numarray

from openmdao.main import Driver, ArrayVariable, String, StringList, \
                          RefVariable, RefVariableArray
from openmdao.main.exceptions import RunStopped
from openmdao.main.variable import INPUT, OUTPUT, VariableChangedEvent


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
        self.design_vals = numarray.zeros(0,'d')
        self.lower_bounds = numarray.zeros(0,'d')
        self.upper_bounds = numarray.zeros(0,'d')
        #self.constraint_vals = numarray.zeros(0,'d')
        self.cons_active_or_violated  = numarray.zeros(0, 'i')
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
                
        RefVariableArray('design_vars', self, OUTPUT, default=[],
                doc='An array of design variable names. These names can include array indexing.')
        self.design_vars.add_observer(self._force_config, VariableChangedEvent)
        
        RefVariableArray('constraints', self, INPUT, default=[],
                doc= 'An array of expression strings indicating constraints.'+
                ' A value of < 0 for the expression indicates that the constraint '+
                'is violated.')
        self.constraints.add_observer(self._force_config, VariableChangedEvent)
        
        RefVariable('objective', self, INPUT,
                          doc= 'A string containing the objective function expression.')
        self.objective.add_observer(self._force_config, VariableChangedEvent)
        
        av = ArrayVariable('upper_bounds', self, INPUT,
            doc='Array of constraints on the maximum value of each design variable.')
        av.add_observer(self._force_config, VariableChangedEvent)
        
        av = ArrayVariable('lower_bounds', self, INPUT,
            doc='Array of constraints on the minimum value of each design variable.')
        av.add_observer(self._force_config, VariableChangedEvent)
        
        self.make_public(['iprint', 'maxiters'])
        
    def _force_config(self, obj):
        """This is called if any of our ref variables change, forcing us to possibly
        resize our arrays.
        """
        self._first = True
        
    
    def execute(self):
        """Perform the optimization."""
        sorted_comps = self.sorted_components()
        
        # set conmin array sizes and such
        if self._first is True:
            self._config_conmin()
        self.cnmn1.igoto = 0
        
        # perform an initial run for self-consistency
        self.run_referenced_comps()
        
        # get the initial values of the design variables
        for i, val in enumerate(self.design_vars.refvalue):
            self.design_vals[i] = val
            
        # loop until optimized
        while conmin.cnmn1.igoto or self._first is True:
            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            self._first = False            
            
            conmin.cnmn1.obj = numarray.array(self.objective.refvalue)
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
            
            # update the design variables in the model
            self.design_vars.refvalue = [float(val) for val in self.design_vals[:-2]]

            # update the model
            self.run_referenced_comps()
# TODO: 'step around' ill-behaved cases.

            # calculate objective and constraints
            if conmin.cnmn1.info == 1:
                # update constraint value array
                for i, con in enumerate(self.constraints.refvalue):
                    self.constraint_vals[i] = con
            # calculate gradients
            elif conmin.cnmn1.info == 2:
                self.raise_exception('user defined gradients not yet supported',
                                     NotImplementedError)


    def _config_conmin(self):
        """Set up arrays for the FORTRAN conmin routine, and perform some
        validation and make sure that array sizes are consistent.
        """
        # size arrays based on number of design variables
        num_dvs = len(self.design_vars.value)
        self.cnmn1.ndv = num_dvs
        self.design_vals = numarray.zeros(num_dvs+2, 'd')
        
        if num_dvs < 1:
            self.raise_exception('no design variables specified', RuntimeError)
            
        if self.objective.value is None:
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
        length = len(self.constraints.value)+2*num_dvs
        self.constraint_vals = numarray.zeros(length, 'd')
        # temp storage of constraint and des vals
        self.g1 = numarray.zeros(length, 'd') 
        # temp storage of constraint vals
        self.g2 = numarray.zeros(length, 'd') 
        # if constraint i is known to be a linear function of des vals, 
        # set cons_is_linear[i] to 1, otherwise set it to 0. This is 
        # not essential is is for efficiency only.
        self.cons_is_linear = numarray.zeros(length, 'i') 
        
        self.cnmn1.ncon = len(self.constraints.value)
        
        if not self._lower_bounds.size == 0 or not self._upper_bounds.size == 0:
            self.cnmn1.nside = 2*num_dvs
        else:
            self.cnmn1.nside = 0

        self.cnmn1.nacmx1 = max(num_dvs,
                                len(self.constraints.value)+conmin.cnmn1.nside)+1
        n1 = num_dvs+2
        #n2 = len(self._constraints)+2*num_dvs
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

