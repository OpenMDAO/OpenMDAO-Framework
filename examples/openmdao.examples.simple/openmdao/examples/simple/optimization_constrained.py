"""
    optimization_constrained.py - Top level assembly for the problem.
"""

# Perform an constrained optimization on our paraboloid using CONMIN.

# pylint: disable-msg=E0611,F0401
from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.api import CONMINdriver

from openmdao.examples.simple.paraboloid import Paraboloid

class OptimizationConstrained(Assembly):
    """ Top level assembly for optimizing a vehicle. """
    
    def __init__(self, directory=''):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
        # pylint: disable-msg=E1101
        
        super(OptimizationConstrained, self).__init__(directory)

        # Create Paraboloid component instances
        self.add_container('paraboloid', Paraboloid())

        # Create CONMIN Optimizer instance
        self.add_container('driver', CONMINdriver())
        
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
        
        # CONMIN Constraints
        self.driver.constraints = ['paraboloid.y-paraboloid.x+15.0']
        
        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    opt_problem = set_as_top(OptimizationConstrained("Top"))
    set_as_top(opt_problem)
    
    tt = time.time()
    opt_problem.run()

    print "CONMIN Iterations: ", opt_problem.driver.get("iter_count")
    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.get("x"), \
                                         opt_problem.paraboloid.get("y"))
    print "Elapsed time: ", time.time()-tt
    
# end optimization_unconstrained.py
