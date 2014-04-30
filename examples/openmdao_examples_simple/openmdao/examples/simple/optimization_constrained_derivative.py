"""
    optimization_constrained.py - Top level assembly for the problem.
"""

# Perform an constrained optimization on our paraboloid component.

from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import SLSQPdriver

from openmdao.examples.simple.paraboloid_derivative import ParaboloidDerivative

class OptimizationConstrained(Assembly):
    """Constrained optimization of the Paraboloid Component."""
    
    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
                
        # Create Paraboloid component instances
        self.add('paraboloid', ParaboloidDerivative())

        # Create Optimizer instance
        self.add('driver', SLSQPdriver())
        
        # Driver process definition
        self.driver.workflow.add('paraboloid')
        
        # Optimizer Flags
        self.driver.iprint = 0
        
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
    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                         opt_problem.paraboloid.y)
    print "Elapsed time: ", time.time()-tt, "seconds"
    
# end optimization_constrained.py
