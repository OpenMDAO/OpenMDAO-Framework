.. index:: optimization

Solving the Optimization Problem
==================================

First, let's run the problem inside the Python shell. Load and create an instance of the
EngineOptimization class, setting it as the top assembly.

.. doctest:: optimization_fun

    >>> from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
    >>> from openmdao.main.api import set_as_top
    >>> prob = EngineOptimization()
    >>> set_as_top(prob)
    <openmdao.examples.enginedesign.engine_optimization.EngineOptimization object at ...>

The problem is set up like above. We could run it now, but first let's find out how
the current design performs. We can do this by running the acceleration simulation driver once
and printing the value of the acceleration time.

.. doctest:: optimization_fun

    >>> prob.vehicle.bore
    82.0
    >>> prob.vehicle.spark_angle
    -37.0
    >>> prob.sim_acc.run()
    >>> prob.sim_acc.accel_time
    7.499...
    
The unit for this variable is `seconds`, so the default vehicle design can
accelerate from 0 to 60 mph in seven and a half seconds. We've also printed
the values of the two decision variables which will be changed by the
optimizer. Next, we solve the optimization problem. This may take a while
but should be finished in less than a minute.

:: 

        >>> prob.run()
        ...
        >>> prob.vehicle.accel_time
        5.399...
        >>> prob.vehicle.bore
        100.0
        >>> prob.vehicle.spark_angle
        -28.965...

The optimizer has found a new solution that has a much faster acceleration
time: 5.4 seconds vs 7.5. This was accomplished by increasing the bore to
its maximum value, which makes the engine larger, and by optimizing the
spark timing.

You can run this same problem at the command prompt by typing:

::

    python engine_optimization.py

This script prints out a little more information than we've shown in this
example. See the :ref:`simple tutorial problem <A-Simple-Tutorial-Problem>` 
for a refresher on how to set up a component to run at the command prompt.

String expressions can be used to pose more sophisticated objective expressions
that are functions of multiple simulation variables. For example, if you want
to maximize ``accel_time`` instead of minimizing it, you can do this by negating
the expression:

.. testsetup:: Code10

        from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
        self = EngineOptimization()
        
.. testcode:: Code10

                # CONMIN Objective = Maximize accel_time 
                self.driver.add_objective('-sim_acc.accel_time')
                
You can build up more complicated expressions from any number of OpenMDAO variables using Python's mathematical syntax:

.. testcode:: Code10

                # CONMIN Objective = Maximize weighted sum of EPA city and highway fuel economy 
                self.driver.add_objective('-(.93*sim_EPA_city.fuel_economy + 1.07*sim_EPA_highway.fuel_economy)')

Here we used a weighted sum of the EPA city and highway fuel economy estimates as the objective in a maximization problem.
Try solving the same optimization problem using this objective.

::

        >>> from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
        >>> from openmdao.main.api import set_as_top
        >>> prob = EngineOptimization()
        >>> set_as_top(prob)
        <openmdao.examples.enginedesign.engine_optimization.EngineOptimization object at 0xe80c3b0>
        >>> prob.driver.add_objective('-(.93*sim_EPA_city.fuel_economy + 1.07*sim_EPA_highway.fuel_economy)')
        >>> prob.vehicle.spark_angle
        -37.0
        >>> prob.vehicle.bore
        82.0
        >>> prob.vehicle.run()
        >>> prob.sim_EPA_city.fuel_economy
        24.807...
        >>> prob.sim_EPA_highway.fuel_economy
        33.454...
        >>> prob.run()
        >>> prob.vehicle.spark_angle
        -7.225...
        >>> prob.vehicle.bore
        90.581...
        >>> prob.sim_EPA_city.fuel_economy
        25.713...
        >>> prob.sim_EPA_highway.fuel_economy
        38.696...

If we only care about optimizing the 0-60 acceleration time, we can be a little smarter with our
iteration hierarchy. In such a case, we don't need to run the EPA fuel economy simulations while
we are optimizing, since their outputs won't be used until the conclusion of the optimization, when
we would like to inspect them. We need a new iteration hierarchy in which we optimize the 
acceleration simulation first and then run the two fuel economy simulations. We can do this with
what we've already learned. We can use the default driver at the top level to sequentially run
the optimizer and the two simulations.

.. figure:: ../images/tutorials/Driver_Process_Definition6.png
   :align: center
   :alt: Diagram of process model showing the vehicle assembly, some simulation drivers, and the optimizer
   
   Iteration Hierarchy for Vehicle Acceleration Optimization Only
   
The code for this looks like this:

.. testcode:: OptimizationSmarter

        # pylint: disable-msg=E0611,F0401
        from openmdao.main.api import Assembly
        from openmdao.lib.drivers.api import CONMINdriver
        
        from openmdao.examples.enginedesign.driving_sim import SimAcceleration, \
                                                               SimEconomy
        from openmdao.examples.enginedesign.vehicle import Vehicle
        
        class EngineOptimization(Assembly):
            """Optimization of a Vehicle."""
            
            def __init__(self):
                """ Creates a new Assembly for vehicle performance optimization."""
                
                super(EngineOptimization, self).__init__()
        
                # pylint: disable-msg=E1101
                
                # Create CONMIN Optimizer instance
                self.add('optimizer', CONMINdriver())
                
                # Create Vehicle instance
                self.add('vehicle', Vehicle())
                
                # Create Driving Simulation instances
                self.add('sim_acc', SimAcceleration())
                self.add('sim_EPA_city', SimEconomy())
                self.add('sim_EPA_highway', SimEconomy())
                
                # add the optimizer and economy sims to driver workflow
                self.driver.workflow.add(['optimizer', 'sim_EPA_city', 'sim_EPA_highway'])
                
                # add the acceleration sim to the optimizer workflow
                self.optimizer.workflow.add('sim_acc')
        
                # Add vehicle to sim workflows.
                self.sim_acc.workflow.add('vehicle')
                self.sim_EPA_city.workflow.add('vehicle')
                self.sim_EPA_highway.workflow.add('vehicle')
            
                # CONMIN Flags
                self.optimizer.iprint = 0
                self.optimizer.itmax = 30
                
                # CONMIN Objective 
                self.optimizer.add_objective('sim_acc.accel_time')
                
                # CONMIN Design Variables 
                self.optimizer.add_parameters([('vehicle.spark_angle', -50., 10.),
                                            ('vehicle.bore', 65., 100.)])
                
                # Acceleration Sim setup
                self.sim_acc.add_parameters([('vehicle.velocity', 0, 99999),
                                           ('vehicle.throttle', 0.01, 1.0),
                                           ('vehicle.current_gear', 0, 5)])
                self.sim_acc.add_objective('vehicle.acceleration')
                self.sim_acc.add_objective('vehicle.overspeed')
                
                # EPA City MPG Sim Setup
                self.sim_EPA_city.add_parameters([('vehicle.velocity', 0, 99999),
                                                 ('vehicle.throttle', 0.01, 1.0),
                                                 ('vehicle.current_gear', 0, 5)])
                self.sim_EPA_city.add_objective('vehicle.acceleration')
                self.sim_EPA_city.add_objective('vehicle.fuel_burn')
                self.sim_EPA_city.add_objective('vehicle.overspeed')
                self.sim_EPA_city.add_objective('vehicle.underspeed')
                self.sim_EPA_city.profilename = 'EPA-city.csv'
                self.sim_EPA_city.force_execute = True
                
                # EPA Highway MPG Sim Setup
                self.sim_EPA_highway.add_parameters([('vehicle.velocity', 0, 99999),
                                                    ('vehicle.throttle', 0.01, 1.0),
                                                    ('vehicle.current_gear', 0, 5)])
                self.sim_EPA_highway.add_objective('vehicle.acceleration')
                self.sim_EPA_highway.add_objective('vehicle.fuel_burn')
                self.sim_EPA_highway.add_objective('vehicle.overspeed')
                self.sim_EPA_highway.add_objective('vehicle.underspeed')
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
                print "CONMIN Iterations: ", opt_problem.optimizer.iter_count
                print ""
                print "Elapsed time: ", time.time()-tt

The code for this example can also be found in the file ``engine_optimization_smarter.py``. You
should notice that this runs considerably faster than ``engine_optimization.py``, which runs
all three sims during every iteration of CONMIN.