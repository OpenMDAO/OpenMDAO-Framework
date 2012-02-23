.. index:: Optimizers

.. _`Optimizers`:


Choosing an Optimizer
======================

        from openmdao.main.api import Assembly, set_as_top
        from openmdao.lib.drivers.api import CONMINdriver
        
        from openmdao.examples.simple.paraboloid import Paraboloid
        
        class OptimizationConstrained(Assembly):
            """Constrained optimization of the Paraboloid with CONMIN."""
            
            def configure(self):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
                
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
                
                # CONMIN Objective 
                self.driver.add_objective('paraboloid.f_xy')
                
                # CONMIN Design Variables 
                self.driver.add_parameter('paraboloid.x', low=-50., high=50.)
                self.driver.add_parameter('paraboloid.y', low=-50., high=50.)
                
                # CONMIN Constraints
                self.driver.add_constraint('paraboloid.x-paraboloid.y >= 15.0')
                
                
        if __name__ == "__main__": # pragma: no cover         
        
            import time
            
            opt_problem = OptimizationConstrained()
            
            tt = time.time()
            opt_problem.run()
        
            print "\n"
            print "Model evaluations: ", opt_problem.driver.exec_count
            print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                                 opt_problem.paraboloid.y)
            print "Elapsed time: ", time.time()-tt, "seconds"
        
