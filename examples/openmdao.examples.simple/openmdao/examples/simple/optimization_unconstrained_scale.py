"""
    optimization_unconstrained.py - Top level assembly for the problem.
"""

# Perform an unconstrained optimization on our paraboloid using and optimizer.

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import SLSQPdriver

from openmdao.examples.simple.paraboloid_scale import Paraboloid_scale,Paraboloid_shift

class OptimizationUnconstrainedScale(Assembly):
    """Unconstrained optimization of the Paraboloid Component."""
    
    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
        # pylint: disable-msg=E1101

        # Create Optimizer instance
        self.add('driver', SLSQPdriver())
        
        # Create Paraboloid component instances
        self.add('paraboloid', Paraboloid_scale())

        # Driver process definition
        self.driver.workflow.add('paraboloid')
        
        # SQLSQP Flags
        self.driver.iprint = 0
        
        # Objective 
        self.driver.add_objective('paraboloid.f_xy')
        
        # Design Variables 
        self.driver.add_parameter('paraboloid.x', low=-1000000., high=1000000.)
        self.driver.add_parameter('paraboloid.y', low=-1000000., high=1000000.)  		
		
        #self.driver.add_parameter('paraboloid.x', low=-1000000., high=1000000.,scaler=0.001)
        #self.driver.add_parameter('paraboloid.y', low=-1000000., high=1000000.,scaler=1000)     

class OptimizationUnconstrainedScaleShift(Assembly):
    """Unconstrained optimization of the Paraboloid Component."""
    
    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
        # pylint: disable-msg=E1101

        # Create Optimizer instance
        self.add('driver', SLSQPdriver())
        
        # Create Paraboloid component instances
        self.add('paraboloid', Paraboloid_shift())

        # Driver process definition
        self.driver.workflow.add('paraboloid')
        
        # SQLSQP Flags
        self.driver.iprint = 0
        
        # Objective 
        self.driver.add_objective('paraboloid.f_xy')
        
        # Design Variables 

        self.driver.add_parameter('paraboloid.x', low=-1000000., high=1000000.)
        self.driver.add_parameter('paraboloid.y', low=-1000000., high=1000000.)  

		#self.driver.add_parameter('paraboloid.x', low=-1000000., high=1000000.,scaler=.001
        #self.driver.add_parameter('paraboloid.y', low=-1000000., high=1000000.,scaler=1000,adder=-1000)   		
		
if __name__ == "__main__": # pragma: no cover         

	import time

	opt_problem = OptimizationUnconstrainedScale()

	tt = time.time()
	opt_problem.run()

	print "\n"
	print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x,opt_problem.paraboloid.y)
	print "Elapsed time: ", time.time()-tt, "seconds"
	print "Execution count: ", opt_problem.paraboloid.exec_count
    
# end optimization_unconstrained.py
