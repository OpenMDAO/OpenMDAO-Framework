.. index:: optimization

Setting up an Optimization Problem
==================================

The final step is to create a top level Assembly that defines the problem
using DrivingSim and the vehicle assembly.

The first problem we would like to solve is a single-objective optimization problem
where we adjust some of the design variables to minimize the 0-60 acceleration time.
The chosen design variables are the *bore* and *spark angle.* The optimal value of the
first variable should be quite intuitive (i.e., larger bore means faster acceleration),
but the second variable cannot be optimized by mere inspection. 

The optimization will be handled by the gradient optimizer CONMIN.

To tackle this problem, let's take a look at the iteration hierarchy. 

.. figure:: ../images/tutorials/Driver_Process_Definition.png
   :align: center
   :alt: Diagram of process model showing the vehicle assembly, some simulation drivers, and the optimizer
   
   Iteration Hierarchy for Vehicle Design Optimization
   
This time, our top level driver is the CONMIN optimizer. Its workflow contains the three
simulations. Note that there is very little difference between this iteration hierarchy
and the one we just built, so it should be pretty easy to change the code.

In ``engine_optimization.py``, we define the class EngineOptimization and
create an instance of CONMINdriver and DrivingSim, which are added to the
driver's workflow. We also create a Vehicle instance and insert it into the
socket in DrivingSim:

.. testcode:: Code9

        from openmdao.main.api import Assembly
        from openmdao.lib.drivers.api import CONMINdriver
        
        from openmdao.examples.enginedesign.driving_sim import SimAcceleration, SimEconomy
        from openmdao.examples.enginedesign.vehicle import Vehicle
        
        class EngineOptimization(Assembly):
            """Optimization of a Vehicle."""
            
            def __init__(self):
                """ Creates a new Assembly for vehicle performance optimization."""
                
                super(EngineOptimization, self).__init__()
        
                # pylint: disable-msg=E1101
                
                # Create CONMIN Optimizer instance
                self.add('driver', CONMINdriver())
                
                # Create Vehicle instance
                self.add('vehicle', Vehicle())
                
                # Create Driving Simulation instances
                self.add('sim_acc', SimAcceleration())
                self.add('sim_EPA_city', SimEconomy())
                self.add('sim_EPA_highway', SimEconomy())
                
                # add Sims to optimizer workflow
                self.driver.workflow.add(['sim_acc', 'sim_EPA_city', 'sim_EPA_highway'])
                
                # Add vehicle to sim workflows.
                self.sim_acc.workflow.add('vehicle')
                self.sim_EPA_city.workflow.add('vehicle')
                self.sim_EPA_highway.workflow.add('vehicle')
            
                # CONMIN Flags
                self.driver.iprint = 0
                self.driver.itmax = 30
                
                # CONMIN Objective 
                self.driver.add_objective('sim_acc.accel_time')
                
                # CONMIN Design Variables 
                self.driver.add_parameter('vehicle.spark_angle', -50., 10.)
                self.driver.add_parameter('vehicle.bore', 65., 100.)
                
                # Acceleration Sim setup
                self.sim_acc.velocity_str = 'vehicle.velocity'
                self.sim_acc.throttle_str = 'vehicle.throttle'
                self.sim_acc.gear_str = 'vehicle.current_gear'
                self.sim_acc.acceleration_str = 'vehicle.acceleration'
                self.sim_acc.overspeed_str = 'vehicle.overspeed'
                
                # EPA City MPG Sim Setup
                self.sim_EPA_city.velocity_str = 'vehicle.velocity'
                self.sim_EPA_city.throttle_str = 'vehicle.throttle'
                self.sim_EPA_city.gear_str = 'vehicle.current_gear'
                self.sim_EPA_city.acceleration_str = 'vehicle.acceleration'
                self.sim_EPA_city.fuel_burn_str = 'vehicle.fuel_burn'
                self.sim_EPA_city.overspeed_str = 'vehicle.overspeed'
                self.sim_EPA_city.underspeed_str = 'vehicle.underspeed'
                self.sim_EPA_city.profilename = 'EPA-city.csv'
                self.sim_EPA_city.force_execute = True
                
                # EPA Highway MPG Sim Setup
                self.sim_EPA_highway.velocity_str = 'vehicle.velocity'
                self.sim_EPA_highway.throttle_str = 'vehicle.throttle'
                self.sim_EPA_highway.gear_str = 'vehicle.current_gear'
                self.sim_EPA_highway.acceleration_str = 'vehicle.acceleration'
                self.sim_EPA_highway.fuel_burn_str = 'vehicle.fuel_burn'
                self.sim_EPA_highway.overspeed_str = 'vehicle.overspeed'
                self.sim_EPA_highway.underspeed_str = 'vehicle.underspeed'
                self.sim_EPA_highway.profilename = 'EPA-highway.csv'        
                self.sim_EPA_highway.force_execute = True

        
        if __name__ == "__main__":

            def prz(title):
                """ Print before and after"""
        
                print '---------------------------------'
                print title
                print '---------------------------------'
                print 'Engine: Bore = ', opt_problem.vehicle.bore
                print 'Engine: Spark Angle = ', opt_problem.vehicle.spark_angle
                print '---------------------------------'
                print '0-60 Accel Time = ', opt_problem.sim_acc.accel_time
                print 'EPA City MPG = ', opt_problem.sim_EPA_city.fuel_economy
                print 'EPA Highway MPG = ', opt_problem.sim_EPA_highway.fuel_economy
                print '\n'
    
            import time
            from openmdao.main.api import set_as_top
    
            opt_problem = EngineOptimization()
            set_as_top(opt_problem)
    
            opt_problem.sim_acc.run()
            opt_problem.sim_EPA_city.run()
            opt_problem.sim_EPA_highway.run()
            prz('Old Design')

            tt = time.time()
            opt_problem.run()
            prz('New Design')
            print "CONMIN Iterations: ", opt_problem.driver.iter_count
            print ""
            print "Elapsed time: ", time.time()-tt
    
Recall that the *iprint* flag enables or disables the printing of diagnostics
internal to CONMIN, while the *itmax* parameter specifies the maximum number
of iterations for the optimization loop.

The optimization objective is to minimize the 0-60 mph acceleration time by
adjusting the design variables *bore* and *spark angle*. In the previous
examples, we learned to use strings to build mathematical expressions with
variables that point to locations in the data hierarchy, so here we do it once
again with our objectives and design variables. The information we need for each
variable is the expression that points to it (e.g., ``vehicle.spark_angle``), and
the minimum and maximum value of the search range for that variable (e.g., ``-.50, 10``).
Once again, if the min and max aren't specified, the `low` and `high` attributes
from the OpenMDAO variable will be used if they have been specified.

We are now ready to solve an optimization problem.
