"""
A simple iteration driver. Basically runs a workflow, passing the output
to the input for the next iteration. Relative change and number of iterations
are used as termination criteria.
"""

# pylint: disable-msg=E0611,F0401
from numpy import zeros
from numpy.linalg import norm

from openmdao.lib.datatypes.api import Float, Int, Bool, Enum
from openmdao.main.api import Driver
from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions
from openmdao.main.exceptions import RunStopped
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints

@add_delegate(HasParameters, HasEqConstraints)
class FixedPointIterator(Driver):
    """ A simple fixed point iteration driver, which runs a workflow and passes
    the value from the output to the input for the next iteration. Relative
    change and number of iterations are used as termination criterea. This type
    of iteration is also known as Gauss-Seidel."""

    # pylint: disable-msg=E1101
    max_iteration = Int(25, iotype='in', desc='Maximum number of '
                                         'iterations before termination.')
    
    tolerance = Float(1.0e-5, iotype='in', desc='Absolute convergence '
                                            'tolerance between iterations.')
    
    norm_order = Enum('Infinity', ['Infinity', 'Euclidean'], 
                       desc = 'For multivariable iteration, type of norm'
                                   'to use to test convergence.')


    def __init__(self):
        super(FixedPointIterator, self).__init__()
        
        self.history = zeros(0)
        self.current_iteration = 0
        
    def execute(self):
        """Perform the iteration."""
        
        self._check_config()

        nvar = len(self.get_parameters().values())
        history = zeros([self.max_iteration, nvar])
        delta = zeros(nvar)
        
        # Get and save the intial value of the input parameters
        val0 = zeros(nvar)
        for i, val in enumerate(self.get_parameters().values()):
            val0[i] = val.evaluate(self.parent)
            
        # perform an initial run
        self.run_iteration()
        self.current_iteration = 0
        
        for i, val in enumerate(self.get_eq_constraints().values()):
            
            term = val.evaluate(self.parent)
            history[0, i] = term[0] - term[1]

        if self.norm_order == 'Infinity':
            order = float('inf')
        else:
            order = 2

        unconverged = True
        while unconverged:

            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            # check max iteration
            if self.current_iteration >= self.max_iteration-1:
                self.history = history[:self.current_iteration+1, :]
                self.raise_exception('Max iterations exceeded without ' + \
                                     'convergence.', RuntimeError)
                
            # Pass output to input
            val0 += history[self.current_iteration, :]
            self.set_parameters(val0)

            # run the workflow
            self.run_iteration()
            self.current_iteration += 1
        
            # check convergence
            for i, val in enumerate(self.get_eq_constraints().values()):
            
                term = val.evaluate(self.parent)
                delta[i] = term[0] - term[1]
            
            history[self.current_iteration] = delta
            
            if norm(delta, order) < self.tolerance:
                break
            # relative tolerance -- problematic around 0
            #if abs( (val1-val0)/val0 ) < self.tolerance:
            #    break
        self.history = history[:self.current_iteration+1, :]
        
    def _check_config(self):
        """Make sure the problem is set up right."""
        
        ncon = len(self.get_eq_constraints())
        
        if ncon == 0:
            msg = "FixedPointIterator requires a constraint equation."
            self.raise_exception(msg, RuntimeError)
            
        params = self.get_parameters().values()
        nparm = len(params)
        
        if nparm == 0:
            msg = "FixedPointIterator requires an input parameter."
            self.raise_exception(msg, RuntimeError)
            
        if ncon != nparm:
            msg = "The number of input parameters must equal the number of" + \
                  " output constraint equations in FixedPointIterator."
            self.raise_exception(msg, RuntimeError)

@add_delegate(HasStopConditions)
class IterateUntil(Driver):
    """ A simple driver to run a workflow until some stop condition is met """

    max_iterations = Int(10,iotype="in", desc="maximun number of iterations")
    iteration = Int(0,iotype="out",desc="current iteration counter")
    run_at_least_once = Bool(True, iotype="in", desc="If True, driver will ignore stop conditions for the first iteration, and run at least one iteration")
    
    def start_iteration(self):
        """ Code executed before the iteration """
        self.iterations = 0
    
    def continue_iteration(self): 
        if self.iteration<1 and self.run_at_least_once:
            self.iteration += 1
            return True

        if self.should_stop():
            return False
        if self.iteration < self.max_iterations: 
            self.iteration += 1
            return True
        
        return False
    
    
# End iterate.py
