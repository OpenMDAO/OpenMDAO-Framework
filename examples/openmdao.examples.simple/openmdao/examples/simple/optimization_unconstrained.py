"""
    optimization_unconstrained.py - Top level assembly for the problem.
"""

# Perform an unconstrained optimization on our parabaloid using CONMIN.

from openmdao.main.api import Assembly

from openmdao.lib.drivers.conmindriver import CONMINdriver

from openmdao.examples.simple.parabaloid import Parabaloid

class Optimization_Unconstrained(Assembly):
    """ Top level assembly for optimizing a vehicle. """
    
    def __init__(self, directory=''):
        """ Creates a new Assembly containing a Parabaloid and an optimizer"""
        
        super(Optimization_Unconstrained, self).__init__(directory)

        # Create DrivingSim component instances
        self.add_container('parabaloid', Parabaloid())

        # Create CONMIN Optimizer instance
        self.add_container('driver', CONMINdriver())
        
        # CONMIN Flags
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .000001
        self.driver.fdchm = .000001
        
        # CONMIN Objective 
        self.driver.objective = 'parabaloid.f_xy'
        
        # CONMIN Design Variables 
        self.driver.design_vars = ['parabaloid.x', 
                                         'parabaloid.y' ]
        
        self.driver.lower_bounds = [-50, -50]
        self.driver.upper_bounds = [50, 50]
        

if __name__ == "__main__": # pragma: no cover         

    import time
    #import profile
    
    opt_problem = Optimization_Unconstrained("Top")
    
    tt = time.time()
    opt_problem.run()

    print "CONMIN Iterations: ", opt_problem.driver.iter_count
    print "Minimum found at (%f, %f)" % (opt_problem.parabaloid.x, opt_problem.parabaloid.y)
    print "Elapsed time: ", time.time()-tt
    
# end engine_optimization.py
