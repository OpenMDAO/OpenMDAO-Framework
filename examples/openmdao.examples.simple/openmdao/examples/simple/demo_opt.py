''' Demonstration of swapping optimizers on a problem '''

from openmdao.examples.simple.paraboloid_derivative import ParaboloidDerivative
from openmdao.lib.drivers.api import COBYLAdriver, CONMINdriver, \
        NEWSUMTdriver, SLSQPdriver, Genetic
from openmdao.main.api import Assembly

class DemoOpt(Assembly):
    """Constrained optimization of the Paraboloid with whatever optimizer
    we want."""
    
    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
        # Create Paraboloid component instances
        self.add('comp', ParaboloidDerivative())

        # Create Optimizer instance
        self.add('driver', CONMINdriver())
        
        # Driver process definition
        self.driver.workflow.add('comp')

        # Objective 
        self.driver.add_objective('comp.f_xy')
        
        # Design Variables 
        self.driver.add_parameter('comp.x', low=-50., high=50.)
        self.driver.add_parameter('comp.y', low=-50., high=50.)
        
        # Inequality Constraints
        #self.driver.add_constraint('comp.x-comp.y >= 15.0')
        
        # Equality Constraints
        #self.driver.add_constraint('comp.x-comp.y=15.0')
        
        # General flag - suppress output
        self.driver.iprint = 0
        
        # CONMIN-specific Settings
        self.driver.itmax = 30
        self.driver.fdch = 0.00001
        self.driver.fdchm = 0.000001
        self.driver.ctlmin = 0.01
        self.driver.delfun = 0.001
        
        # NEWSUMT-specific Settings
        #self.driver.itmax = 100
        
        # COBYLA-specific Settings
        #self.driver.rhobeg = 1.0
        #self.driver.rhoend = 1.0e-4
        #self.driver.maxfun = 1000
        
        # SLSQP-specific Settings
        #self.driver.accuracy = 1.0e-6
        #self.driver.maxiter = 50
        
        # Genetic-specific Settings
        #self.driver.population_size = 90
        #self.driver.crossover_rate = 0.9
        #self.driver.mutation_rate = 0.02
        #self.selection_method = 'rank'
        
        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    opt_problem = DemoOpt()
    
    t1 = time.time()
    opt_problem.run()
    t2 = time.time()

    print "\n"
    print "Optimizer: %s" % type(opt_problem.driver)
    print "Function executions: ", opt_problem.comp.exec_count
    print "Gradient executions: ", opt_problem.comp.derivative_exec_count
    print "Minimum: %f" % opt_problem.driver.eval_objective()
    print "Minimum found at (%f, %f)" % (opt_problem.comp.x, \
                                         opt_problem.comp.y)
    print "Elapsed time: ", t2-t1, "seconds"