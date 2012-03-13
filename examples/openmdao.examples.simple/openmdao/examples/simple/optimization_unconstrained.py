"""
    optimization_unconstrained.py - Top level assembly for the problem.
"""

# Perform an unconstrained optimization on our paraboloid using and optimizer.

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import SLSQPdriver

from openmdao.examples.simple.paraboloid import Paraboloid

class OptimizationUnconstrained(Assembly):
    """Unconstrained optimization of the Paraboloid Component."""
    
    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
        # pylint: disable-msg=E1101

        # Create Optimizer instance
        self.add('driver', SLSQPdriver())
        
        # Create Paraboloid component instances
        self.add('paraboloid', Paraboloid())

        # Driver process definition
        self.driver.workflow.add('paraboloid')
        
        # SQLSQP Flags
        self.driver.iprint = 0
        
        # Objective 
        self.driver.add_objective('paraboloid.f_xy')
        
        # Design Variables 
        self.driver.add_parameter('paraboloid.x', low=-50., high=50.)
        self.driver.add_parameter('paraboloid.y', low=-50., high=50.)
        

if __name__ == "__main__": # pragma: no cover         

    import time
    
    opt_problem = OptimizationUnconstrained()

    tt = time.time()
    opt_problem.run()

    print "\n"
    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                         opt_problem.paraboloid.y)
    print "Elapsed time: ", time.time()-tt, "seconds"
    
# end optimization_unconstrained.py
