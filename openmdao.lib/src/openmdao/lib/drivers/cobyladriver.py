"""
cobyladriver.py - Contains a driver that wraps the cobyla
optimizer as used in pyOpt:

Minimize a function using the Constrained Optimization BY Linear
Approximation (COBYLA) method.

COBYLA is gradient-free and can handle inequality constraints.
"""

import logging
from math import isnan

try:
    from numpy import zeros, array
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
    # to keep class decl from barfing before being stubbed out
    zeros = lambda *args, **kwargs: None 
    
from cobyla.cobyla import cobyla, closeunit

from openmdao.main.datatypes.api import Enum, Float, Int, Str
from openmdao.main.driver import Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasIneqConstraints
from openmdao.main.hasobjective import HasObjective
from openmdao.main.interfaces import IHasParameters, IHasIneqConstraints, \
                                     IHasObjective, implements, IOptimizer
from openmdao.util.decorators import add_delegate, stub_if_missing_deps

    
@stub_if_missing_deps('numpy')
@add_delegate(HasParameters, HasIneqConstraints, HasObjective)
class COBYLAdriver(Driver):
    """Minimize a function using the Constrained Optimization BY Linear
    Approximation (COBYLA) method.

    COBYLA is gradient-free and can handle inequality constraints.
    
    Note: Constraints should be added using the OpenMDAO convention
    (positive = violated).
    """
    
    implements(IHasParameters, IHasIneqConstraints, IHasObjective, IOptimizer)
    
    # pylint: disable-msg=E1101
    rhobeg = Float(1.0, iotype='in', 
                   desc = 'Reasonable initial changes to the variables.')

    rhoend = Float(1e-4, iotype='in', 
                   desc = 'Final accuracy in the optimization (not precisely guaranteed).')

    iprint = Enum(1, [0, 1, 2, 3], iotype='in',
                  desc = 'Controls the frequency of output: 0 (no output),1,2,3.')
    
    maxfun = Int(1000, iotype='in',
                  desc = 'Maximum number of function evaluations.')
    
    iout = Int(6, iotype='in',
                  desc = 'Fortran output unit. Leave this at 6 for STDOUT.')
    
    output_filename = Str('cobyla.out', iotype='in',
                          desc = 'Name of output file (if iout not 6).')
    
    error_code = Int(0, iotype='out',
                  desc = 'Error code returned from COBYLA.')
    

    def __init__(self):
        
        super(COBYLAdriver, self).__init__()
        
        self.error_messages = {
            1 : 'Max. number of function evaluations reached',
            2 : 'Rounding errors are becoming damaging'
        }
        
        self.x = zeros(0,'d')
        self.work_vector = zeros(0,'d')
        self.gg = zeros(0,'d')
        self.iact = zeros(0,'d')
        
        self.ff = 0
        self.nfvals = 0
        
    def start_iteration(self):
        """Perform initial setup before iteration loop begins."""

        self.nparam = len(self.get_parameters().values())
        self.ncon = len(self.get_ineq_constraints())
        self.ncon += 2*self.nparam
        self.x = zeros(self.nparam,'d')
        self.g = zeros(self.ncon,'d')
        self.work_vector = zeros(self.ncon,'d')
        
        # get the initial values of the parameters
        params = self.get_parameters().values()
        for i, val in enumerate(params):
            self.x[i] = val.evaluate(self.parent)
            
        n = self.nparam
        m = self.ncon
        self.work_vector = zeros([n*(3*n+2*m+11)+4*m+6],'d')
        self.iact = zeros([m+1],'i')
        self.gg = zeros([m],'d')
            
        self._continue = True
        
    def run_iteration(self):
        """ Note: cobyla controls the looping."""
        
        try:
            self.iact, self.error_code, self.nfvals = \
              cobyla(self._func, self.nparam, self.ncon, self.x, \
                   self.rhobeg, self.rhoend, self.iprint, self.maxfun, \
                   self.work_vector, self.iact, self.error_code, self.nfvals, \
                   self.iout, self.output_filename, self.ff, self.gg)
            
        except Exception, err:
            self._logger.error(str(err))
            raise       
        
        if self.iprint > 0 :
            closeunit(self.iout)

        # Log any errors
        if self.error_code != 0 :
            self._logger.warning(self.error_messages[self.error_code])
        
        # Iteration is complete
        self._continue = False

    def _func(self, n, m, xnew, f, g):
        """ Return ndarrays containing the function and constraint 
        evaluations.
        
        Note: n, m, f, and g are unused inputs."""
        
        self.set_parameters(xnew)
        super(COBYLAdriver, self).run_iteration()
        f = self.eval_objective()
        
        if isnan(f):
            msg = "Numerical overflow in the objective"
            self.raise_exception(msg, RuntimeError)
            
        # Constraints (COBYLA defines positive as satisfied)
        con_list = []
        for v in self.get_ineq_constraints().values():
            val = v.evaluate(self.parent)
            if '>' in val[2]:
                con_list.append(val[0]-val[1])
            else:
                con_list.append(val[1]-val[0])
                
        # Side Constraints
        for param in self.get_parameters().values():
            val = param.evaluate(self.parent)
        
            con_list.append(val - param.low)
            con_list.append(param.high - val)
                
        g = array(con_list)
        
        # Write out some relevant information to the recorder
        self.record_case()
            
        return f, g
        
