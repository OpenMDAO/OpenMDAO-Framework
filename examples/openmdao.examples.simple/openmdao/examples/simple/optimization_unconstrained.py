"""
    optimization_unconstrained.py - Top level assembly for the problem.
"""

# Perform an unconstrained optimization on our paraboloid using CONMIN.

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.api import CONMINdriver

from openmdao.examples.simple.paraboloid import Paraboloid

class OptimizationUnconstrained(Assembly):
    """Unconstrained optimization of the Paraboloid with CONMIN."""
    
    def __init__(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
        # pylint: disable-msg=E1101
        
        super(OptimizationUnconstrained, self).__init__()

        # Create CONMIN Optimizer instance
        self.add_container('driver', CONMINdriver())
        
        # Create Paraboloid component instances
        self.add_container('paraboloid', Paraboloid())

        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .000001
        self.driver.fdchm = .000001
        
        # CONMIN Objective 
        self.driver.objective = 'paraboloid.f_xy'
        
        # CONMIN Design Variables 
        self.driver.design_vars = ['paraboloid.x', 
                                         'paraboloid.y' ]
        
        self.driver.lower_bounds = [-50, -50]
        self.driver.upper_bounds = [50, 50]
        

if __name__ == "__main__": # pragma: no cover         

    import time
    
    opt_problem = OptimizationUnconstrained()
    set_as_top(opt_problem)

    tt = time.time()
    opt_problem.run()

    print "\n"
    print "CONMIN Iterations: ", opt_problem.driver.iter_count
    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                         opt_problem.paraboloid.y)
    print "Elapsed time: ", time.time()-tt, "seconds"
    
# end optimization_unconstrained.py
