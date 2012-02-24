.. index:: Optimizers

.. _`Optimizers`:


Choosing an Optimizer
======================

When the initial versions of OpenMDAO were released, the standard library only contained
two optimizers -- the gradient optimizer ``CONMIN``, and the genetic optimizer ``genetic``.
OpenMDAO now includes several optimizers, provides access to a few more optimizers as plugins,
and will continue to benefit from community contributions. This tutorial will discuss the
currently available optimizers as well as mention some of the official plugins. Finally,
the Paraboloid example will be used to demonstrate optimizer swapping.

OpenMDAO Optimizers
~~~~~~~~~~~~~~~~~~~

Optimizers from Plugins
~~~~~~~~~~~~~~~~~~~~~~~

Swapping Optimizers
~~~~~~~~~~~~~~~~~~~

.. testcode:: choose_CONMIN

        from openmdao.main.api import Assembly
        from openmdao.lib.drivers.api import COBYLAdriver, CONMINdriver, \
                NEWSUMTdriver, SLSQPdriver, Genetic
        
        from openmdao.examples.simple.paraboloid import Paraboloid
        
        class OptimizationConstrained(Assembly):
            """Constrained optimization of the Paraboloid with whatever optimizer we want."""
            
            def configure(self):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
                
                # Create Paraboloid component instances
                self.add('paraboloid', Paraboloid())
        
                # Create CONMIN Optimizer instance
                self.add('driver', CONMINdriver())
                
                # Driver process definition
                self.driver.workflow.add('paraboloid')
                
                # Optimizer-specific Flags
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
            print "paraboloid executions: ", opt_problem.driver.exec_count
            print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                                 opt_problem.paraboloid.y)
            print "Elapsed time: ", time.time()-tt, "seconds"
        
