"""
    optimization_constrained.py - Top level assembly for the problem.
"""

# Perform an constrained optimization on our paraboloid using CONMIN.

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import CONMINdriver

from openmdao.examples.simple.paraboloid import Paraboloid

class OptimizationConstrained(Assembly):
    """Constrained optimization of the Paraboloid with CONMIN."""
    
    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
        # pylint: disable-msg=E1101

        # Create Paraboloid component instances
        self.add('paraboloid', Paraboloid())

        # Create CONMIN Optimizer instance
        self.add('driver', CONMINdriver())
        
        # Driver process definition
        self.driver.workflow.add('paraboloid')
        
        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .000001
        self.driver.fdchm = .000001
        
        # Objective 
        self.driver.add_objective('paraboloid.f_xy')
        
        # Design Variables 
        self.driver.add_parameter('paraboloid.x', low=-50., high=50.)
        self.driver.add_parameter('paraboloid.y', low=-50., high=50.)
        
        # Constraints
        self.driver.add_constraint('paraboloid.x-paraboloid.y >= 15.0')
        
        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    opt_problem = OptimizationConstrained()
    
    tt = time.time()
    opt_problem.run()

    print "\n"
    print "CONMIN Iterations: ", opt_problem.driver.iter_count
    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                         opt_problem.paraboloid.y)
    print "Elapsed time: ", time.time()-tt, "seconds"
    
# end optimization_unconstrained.py
