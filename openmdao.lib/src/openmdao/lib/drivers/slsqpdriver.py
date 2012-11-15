"""
slsqpdriver.py - Contains a driver that wraps the SLSQP
optimizer as used in pyOpt:

Minimize a function using Sequential Least SQuares Programming.

SLSQP is a gradient optimizer that can handle both equality and
inequality constraints.
"""

import logging
from math import isnan

try:
    from numpy import zeros, array
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
    # to keep class decl from barfing before being stubbed out
    zeros = lambda *args, **kwargs: None 
    
from slsqp.slsqp import slsqp, closeunit, pyflush

from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.main.datatypes.api import Enum, Float, Int, Str, List
from openmdao.main.driver_uses_derivatives import DriverUsesDerivatives
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasobjective import HasObjective
from openmdao.main.interfaces import IHasParameters, IHasConstraints, \
                                     IHasObjective, implements, IOptimizer
from openmdao.util.decorators import add_delegate, stub_if_missing_deps

    
@stub_if_missing_deps('numpy')
@add_delegate(HasParameters, HasConstraints, HasObjective)
class SLSQPdriver(DriverUsesDerivatives):
    """Minimize a function using the Sequential Least SQuares Programming
    (SLSQP) method.

    SLSQP is a gradient optimizer that can handle both equality and
    inequality constraints.
    
    Note: Constraints should be added using the OpenMDAO convention
    (positive = violated).
    """
    
    implements(IHasParameters, IHasConstraints, IHasObjective, IOptimizer)
    
    # pylint: disable-msg=E1101
    accuracy = Float(1.0e-6, iotype='in', 
                     desc = 'Convergence accuracy')

    maxiter = Int(50, iotype='in', 
                   desc = 'Maximum number of iterations.')

    iprint = Enum(0, [0, 1, 2, 3], iotype='in',
                  desc = 'Controls the frequency of output: 0 (no output),1,2,3.')
    
    iout = Int(6, iotype='in',
                  desc = 'Fortran output unit. Leave  this at 6 for STDOUT.')
    
    output_filename = Str('slsqp.out', iotype='in',
                          desc = 'Name of output file (if iout not 6).')
    
    error_code = Int(0, iotype='out',
                  desc = 'Error code returned from SLSQP.')
    
    
    def __init__(self):
        
        super(SLSQPdriver, self).__init__()
        
        self.error_messages = {
            -1 : "Gradient evaluation required (g & a)",
             1 : "Function evaluation required (f & c)",
             2 : "More equality constraints than independent variables",
             3 : "More than 3*n iterations in LSQ subproblem",
             4 : "Inequality constraints incompatible",
             5 : "Singular matrix E in LSQ subproblem",
             6 : "Singular matrix C in LSQ subproblem",
             7 : "Rank-deficient equality constraint subproblem HFTI",
             8 : "Positive directional derivative for linesearch",
             9 : "Iteration limit exceeded",        
        }
        
        self.x = zeros(0,'d')
        self.x_lower_bounds = zeros(0,'d')
        self.x_upper_bounds = zeros(0,'d')
        
        # We auto-fill the slot because the gradient is required
        # in this implementation
        self.differentiator = FiniteDifference()
        
    def start_iteration(self):
        """Perform initial setup before iteration loop begins."""
        
        if not self.differentiator:
            msg = 'A differentiator must be socketed for this driver.'
            self.raise_exception(msg, RuntimeError)

        self.nparam = len(self.get_parameters().values())
        self.ncon = len(self.get_constraints())
        self.neqcon = len(self.get_eq_constraints())
        
        # get the initial values of the parameters
        self.x = zeros(self.nparam,'d')
        params = self.get_parameters().values()
        for i, val in enumerate(params):
            self.x[i] = val.evaluate(self.parent)
            
        # create lower and upper bounds arrays
        self.x_lower_bounds = zeros(self.nparam)
        self.x_upper_bounds = zeros(self.nparam)
        for i, param in enumerate(params):
            self.x_lower_bounds[i] = param.low
            self.x_upper_bounds[i] = param.high        
            
        self.ff = 0
        self.nfunc = 0
        self.ngrad = 0

        self._continue = True
        
    def run_iteration(self):
        """ Note: slsqp controls the looping."""
        
        n = self.nparam
        m = self.ncon
        meq = self.neqcon
        
        la = max(m,1)
        self.gg = zeros([la], 'd')
        df = zeros([n+1], 'd')
        dg = zeros([la, n+1], 'd')
        
        mineq = m - meq + 2*(n+1)
        lsq = (n+1)*((n+1)+1) + meq*((n+1)+1) + mineq*((n+1)+1)
        lsi = ((n+1)-meq+1)*(mineq+2) + 2*mineq
        lsei = ((n+1)+mineq)*((n+1)-meq) + 2*meq + (n+1)
        slsqpb = (n+1)*(n/2) + 2*m + 3*n + 3*(n+1) + 1
        lw = lsq + lsi + lsei + slsqpb + n + m
        w = zeros([lw], 'd')
        ljw = max(mineq,(n+1)-meq)
        jw = zeros([ljw], 'i')
        
        try:
            dg, self.error_code, self.nfunc, self.ngrad = \
              slsqp(self.ncon, self.neqcon, la, self.nparam, \
                    self.x, self.x_lower_bounds, self.x_upper_bounds, \
                    self.ff, self.gg, df, dg, self.accuracy, self.maxiter, \
                    self.iprint-1, self.iout, self.output_filename, \
                    self.error_code, w, lw, jw, ljw, \
                    self.nfunc, self.ngrad, \
                    self._func, self._grad)
                    
            #slsqp(m,meq,la,n,xx,xl,xu,ff,gg,df,dg,acc,maxit,iprint,
            #      iout,ifile,mode,w,lw,jw,ljw,nfunc,ngrad,slfunc,slgrad)            
            
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
        
    def _func(self, m, me, la, n, f, g, xnew):
        """ Return ndarrays containing the function and constraint 
        evaluations.
        
        Note: m, me, la, n, f, and g are unused inputs."""
        self.set_parameters(xnew)
        super(SLSQPdriver, self).run_iteration()      
        f = self.eval_objective()

        if isnan(f):
            msg = "Numerical overflow in the objective."
            self.raise_exception(msg, RuntimeError)
            
        # Constraints
        if self.ncon > 0 :
            con_list = []
            for v in self.get_constraints().values():
                val = v.evaluate(self.parent)
                if '>' in val[2]:
                    con_list.append(val[0]-val[1])
                else:
                    con_list.append(val[1]-val[0])
                
            g = array(con_list)
            
            
        if self.iprint > 0:
            pyflush(self.iout)
            
        # Write out some relevant information to the recorder
        self.record_case()

        return f, g
    
    def _grad(self, m, me, la, n, f, g, df, dg, xnew):
        """ Return ndarrays containing the gradients of the objective
        and constraints.
        
        Note: m, me, la, n, f, g, df, and dg are unused inputs."""
        
        self.ffd_order = 1
        self.differentiator.calc_gradient()
        self.ffd_order = 0
            
        df[0:self.nparam] = \
            self.differentiator.get_gradient(self.get_objectives().keys()[0])

        if self.ncon > 0 :
            for i, con in enumerate(self.get_constraints().keys()):
                dg[i][0:self.nparam] = \
                    -self.differentiator.get_gradient(con)
        
        return df, dg
    
