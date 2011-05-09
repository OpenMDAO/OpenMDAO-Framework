"""
A simple iteration driver. Basically runs a workflow, passing the output
to the input for the next iteration. Relative change and number of iterations
are used as termination criteria.
"""

# pylint: disable-msg=E0611,F0401
from numpy import zeros

from openmdao.lib.datatypes.api import Float, Int, Str, Bool
from openmdao.main.api import Driver
from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.exceptions import RunStopped

class FixedPointIterator(Driver):
    """ A simple fixed point iteration driver, which runs a workflow and passes
    the value from the output to the input for the next iteration. Relative
    change and number of iterations are used as termination criterea."""

    # pylint: disable-msg=E1101
    x_out = Str(iotype='in', desc='loop output to pass to input.') 
    x_in = Str(iotype='out', desc='loop input, taken from the input.')
    
    max_iteration = Int(25, iotype='in', desc='Maximum number of '
                                         'iterations before termination.')
    
    tolerance = Float(0.00001, iotype='in', desc='Absolute convergence '
                                            'tolerance between iterations.')


    def __init__(self, doc=None):
        super(FixedPointIterator, self).__init__(doc)
        
        self.history = zeros(0)
        self.current_iteration = 0
        self._x_out_expr = None
        self._x_in_expr = None
        
        
    def _x_out_changed(self, oldval, newval):
        self._x_out_expr = ExprEvaluator(newval, scope=self)
    
    def _x_in_changed(self, oldval, newval):
        self._x_in_expr = ExprEvaluator(newval, scope=self)
    
    def execute(self):
        """Perform the iteration."""

        # perform an initial run for self-consistency
        self.run_iteration()
        
        self.current_iteration = 0
        history = zeros(self.max_iteration)
        history[0] = self._x_out_expr.evaluate()
        unconverged = True

        while unconverged:

            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            # check max iteration
            if self.current_iteration >= self.max_iteration-1:
                self.history = history[:self.current_iteration+1]
                self.raise_exception('Max iterations exceeded without ' + \
                                     'convergence.', RuntimeError)
                
            # Pass output to input
            val0 = history[self.current_iteration]
            self._x_in_expr.set(val0)

            # run the workflow
            self.run_iteration()
            self.current_iteration += 1
        
            # check convergence
            history[self.current_iteration] = self._x_out_expr.evaluate()
            val1 = history[self.current_iteration]
            
            if abs(val1-val0) < self.tolerance:
                break
            # relative tolerance -- problematic around 0
            #if abs( (val1-val0)/val0 ) < self.tolerance:
            #    break
            
        self.history = history[:self.current_iteration+1]
            

@add_delegate(HasStopConditions)
class IterateUntil(Driver):

    max_iterations = Int(10,iotype="in", desc="maximun number of iterations")
    iteration = Int(0,iotype="out",desc="current iteration counter")
    run_at_least_once = Bool(True, iotype="in", desc="If True, driver will ignore stop conditions for the first iteration, and run at least one iteration")
    
    def start_iteration(self):
        self.iterations = 0
    
    def continue_iteration(self): 
        if self.iteration<1 and self.run_at_least_once:
            self.iteration += 1
            return True
        if self.should_stop():
            return False
        if self.iteration < self.max_iterations: 
            self.iteration += 1
            #print "iteration: ",self.iteration
            return True
        
        return False
    
    
# End iterate.py
