"""
fmin_cobyla_driver.py - Contains a driver that wraps the fmin_cobyla
optimizer from scipy:

Minimize a function using the Constrained Optimization BY Linear
Approximation (COBYLA) method

COBYLA is gradient-free and can handle inequality constraints. The
scipy package must be installed to import and use this driver.
"""

try:
    from scipy.optimize.cobyla import fmin_cobyla
    from numpy import zeros
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
    # to keep class decl from barfing before being stubbed out
    zeros = lambda *args, **kwargs: None 
    
from openmdao.main.datatypes.api import Enum, Float, Int
from openmdao.main.driver import Driver
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasIneqConstraints
from openmdao.main.hasobjective import HasObjective
from openmdao.main.interfaces import IHasParameters, IHasIneqConstraints, \
                                     IHasObjective, implements
from openmdao.util.decorators import add_delegate, stub_if_missing_deps

    
@stub_if_missing_deps('numpy', 'scipy')
@add_delegate(HasParameters, HasIneqConstraints, HasObjective)
class fmin_COBYLA_driver(Driver):
    """Minimize a function using the Constrained Optimization BY Linear
    Approximation (COBYLA) method

    COBYLA is gradient-free and can handle inequality constraints. The
    scipy package must be installed to import and use this driver.
    
    Note: constraints should be added using the OpenMDAO convention
    (positive = violated)
    """
    
    implements(IHasParameters, IHasIneqConstraints, IHasObjective)
    
    # pylint: disable-msg=E1101
    rhobeg = Float(1.0, iotype='in', 
                   desc = 'reasonable initial changes to the variables')

    rhoend = Float(1e-4, iotype='in', 
                   desc = 'final accuracy in the optimization (not precisely guaranteed)')

    iprint = Enum(1, [0, 1, 2, 3], iotype='in',
                  desc = 'controls the frequency of output: 0 (no output),1,2,3')
    
    maxfun = Int(1000, iotype='in',
                  desc = 'maximum number of function evaluations')
    
    def __init__(self, *args, **kwargs):
        
        super(fmin_COBYLA_driver, self).__init__(*args, **kwargs)
        
        self.x = zeros(0,'d')
        self.g = zeros(0,'d')


    def start_iteration(self):
        """Perform initial setup before iteration loop begins."""

        nparam = len(self.get_parameters().values())
        ncon = len(self.get_ineq_constraints()) 
        self.x = zeros(nparam,'d')
        self.g = zeros(ncon,'d')   
        
        # get the initial values of the parameters
        for i, val in enumerate(self.get_parameters().values()):
            self.x[i] = val.evaluate(self.parent)
            
        self._continue = True
        
        # create an evaluation function for each constraint
        self.cons_funcs = []
        for i, v in enumerate(self.get_ineq_constraints().values()):
            
            #func = "def _cf%d(self, x):\n" % i
            #func += "    return self._cons(x, %d)" % i
            #exec func
            
            f = lambda self, x: self._cons(x, i)
            f.__name__ = "_cf%d" % i
            setattr(self,"_cf%d" % i, f)
            exec "self.cons_funcs.append(self._cf%d)" % i in locals()
            
        

    def run_iteration(self):
        """ Note: scipy's fmin_cobyla controls the looping."""

        try:
            self.x = fmin_cobyla(self._func, self.x, self.cons_funcs, 
                                 (), None, self.rhobeg,
                                 self.rhoend, self.iprint, self.maxfun)
        except Exception, err:
            self._logger.error(str(err))
            raise       
        
        # Iteration is complete
        self._continue = False

    def _func(self, xnew):
        """ Return an ndarray containing the function evaluations."""
        
        self.set_parameters(xnew)
        super(fmin_COBYLA_driver, self).run_iteration()
        return self.eval_objective()        
        
    def _cons(self, xnew, index):
        """ Return an evaluation of constraint 'index'."""
        
        self.set_parameters(xnew)
        super(fmin_COBYLA_driver, self).run_iteration()

        # Note, scipy's constraint is the negative of ours
        v = self.get_ineq_constraints().values()[index]
        val = v.evaluate(self.parent)
        if '>' in val[2]:
            self.g[index] = val[1]-val[0]
        else:
            self.g[index] = val[0]-val[1]
                
        return self.g[index]
