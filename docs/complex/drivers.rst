.. index:: drivers

Drivers for Simulation
======================

Now that we have a functional and quick Vehicle assembly, we need to complete
the problem by providing a way to simulate the acceleration and the EPA fuel
economy estimates. The acceleration test requires an integration in time to
match a specified velocity profile. In other words, the vehicle assembly needs 
to be executed at each time step to produce the instantaneous acceleration.
The EPA fuel economy tests are a bit more tricky, though they also require an integration in
time. For these tests, the vehicle assembly must be executed while varying the throttle and
gear position inputs to match a desired acceleration for the integration
segment. Both of these solution procedures were implemented in Python as *drivers.* The
SimAcceleratio driver simulates the acceleration test, and the ``SimEconomy`` driver
simulates the EPA fuel economy test.

Recall that in the :ref:`simple tutorial problem <A-Simple-Tutorial-Problem>`, a Driver
called CONMINdriver was used to optimize the Paraboloid problem. Similarly, the algorithms
that perform these tests have been implemented as OpenMDAO drivers that can be found
in ``driving_sim.py``. These drivers use the ``add_parameter`` and ``add_objective``
interface to specify their connections to the Vehicle assembly. This interface should be
familiar to you from setting up the Optimization problems in earlier examples. The
``add_parameters`` function is used to specify the variables that these drivers will
manipulate during solution. Specifically, in order to drive the Vehicle assembly, you
need to be able to set the velocity, throttle, and gear positions. Likewise, the
``add_objective`` function is used to specify the variables that these drivers need to
obtain as a result of running the vehicles at the input state. For the acceleration test, the
vehicle's instantaneous acceleration is needed, and for the economy test, both the 
acceleration and fuel burn are needed.

You may want to examine the SimAcceleration and the SimEconomy drivers that are
found in ``driving_sim.py``. This tutorial does not cover how to create an OpenMDAO
Driver, but a future tutorial will teach this. Until then, you can gain some understanding
by studying these drivers, as well as the ones included in the standard library.

The simulation drivers have a variable that stores the result of the simulation:

**SimAcceleration - Output:**

=================  ===========================================  ========
Variable           Description                                  Units
=================  ===========================================  ========
``accel_time``     Time for vehicle to accelerate to 60 mph     s
                   from a stop.
=================  ===========================================  ========


**SimEconomy - Output:**

=================  ===========================================  ========
Variable           Description                                  Units
=================  ===========================================  ========
``fuel_economy``   Fuel economy estimate based on EPA           mi/galUS
                   driving profile
=================  ===========================================  ========

These variables can be accessed like any other component output, and can be connected to the 
input of another OpenMDAO component if needed.

Let's set up a model that runs all three of these simulations to calculate
these results: 0-60 acceleration time, EPA city mpg, and EPA highway mpg. To build this model, we need to learn a little more about the :term:`iteration hierarchy`. In
the simple example, we used the iteration hierarchy to define a problem which contained
an optimizer and a simple Python component. The iteration hierarchy was defined by adding
the component to the driver's workflow with the ``add`` method. If we want to run the
SimAcceleration driver on the  Vehicle  component, we can set up a similar iteration
hierarchy where we add the Vehicle component to the SimAcceleration driver's workflow. The
top level assembly would look like this:


.. testcode:: SingleSim

        from openmdao.main.api import Assembly
        from openmdao.examples.enginedesign.driving_sim import SimAcceleration
        from openmdao.examples.enginedesign.vehicle import Vehicle
        
        class VehicleSim(Assembly):
            """Optimization of a Vehicle."""
            
            def __init__(self):
                """ Creates a new Assembly for vehicle performance optimization."""
                
                super(VehicleSim, self).__init__()
        
                # Create Vehicle instance
                self.add('vehicle', Vehicle())
                
                # Create 0-60 Acceleration Simulation instance
                self.add('driver', SimAcceleration())
                
                # Add vehicle to sim workflows.
                self.driver.workflow.add('vehicle')
            
                # Acceleration Sim setup
                self.driver.add_parameters([('vehicle.velocity', 0, 99999),
                                           ('vehicle.throttle', 0.01, 1.0),
                                           ('vehicle.current_gear', 0, 5)])
                self.driver.add_objective('vehicle.acceleration')
                self.driver.add_objective('vehicle.overspeed')
                
        if __name__ == "__main__": 
        
            from openmdao.main.api import set_as_top
            my_sim = VehicleSim()
            set_as_top(my_sim)
    
            my_sim.run()
            
            print "Time (0-60): ", my_sim.driver.accel_time

Here, we add a SimAcceleration instance as our top level driver, and then we add our Vehicle
instance to the driver's workflow. Note also that we add our three simulation inputs from vehicle
-- velocity, throttle, and ``current_gear`` -- as parameters. Our simulation outputs contain
values that are needed to calculate the 0-60 acceleration time as the driver performs that
simulation. We use the ``add_objective`` function to specify the vehicle's acceleration and
overspeed as the output variables. These are not actually objectives, since the driver is
not an optimizer, but under the hood they work the same way (those values are set when the
driver's workflow executes), so it was convenient to keep the same interface that is used
with optimizer drivers.

We also introduce the ``add_parameters`` method, which allows us to define multiple parameters
for the driver (recall that ``add_parameter`` defines a single parameter.) The argument for
``add_parameters`` is a list containing tuples structured like: (variable name, min, max). You
can also use three calls to ``add_parameter`` to accomplish the same thing.

This is a very simple problem, and hence the workflows and iteration hierarchy are also very
simple. In OpenMDAO, you can build models with arbitrary levels of complexity. To
understand how this works, it is beneficial to use a diagram like this:

.. figure:: ../images/tutorials/Driver_Process_Definition3.png
   :align: center
   :alt: Diagram of process model showing the vehicle assembly, some simulation drivers, and the optimizer
   
   Iteration Hierarchy for One Vehicle Simulation

This is the iteration hierarchy for the model we just built. The gray rounded-rectangles represent
drivers, the white rounded-rectangles represent components, and the yellow rectangles represent
workflows. The gray rounded-rectangle in the upper left-hand corner of a yellow rectangle is the driver that
owns that workflow. The remaining items in that rectangle are the components that are contained
within that workflow. Note that a workflow can also contain assemblies and drivers, though in this
case it just contains a component.

The top level driver in an assembly is always called *driver.* If no specific
driver instance (e.g., SimAcceleration in our example) is declared with the
name `driver`, then the assembly's default driver is used. The behavior for
this default driver is to execute the components in its workflow sequentially,
inferring the execution order from the data connections. If there are no data
connections, then the components are executed in the order they were added to
the workflow.

When we created the Vehicle component above, we used this default driver to
create a sequential execution of the Transmission, Engine, and Chassis components
in the order that the data connections required. The iteration hierarchy is
shown in this diagram:

.. figure:: ../images/tutorials/Driver_Process_Definition4.png
   :align: center
   :alt: Diagram of process model showing the vehicle assembly, some simulation drivers, and the optimizer
   
   Iteration Hierarchy for Vehicle Component

Notice that the workflow contains the three components that we used to build the vehicle
assembly. The top level driver of the assembly is just called `driver.`
   
Now, let's see how we can make a new assembly that performs all three simulations. Just
as we did with the Vehicle assembly, we want to run these three simulations
sequentially. In this case, they are drivers, but the mechanics of adding a driver
to another driver's workflow is the same as with a component. An additional `level`
is introduced to this iteration hierarchy because each of the simulation drivers
also has its own workflow. Each of these workflows contains the Vehicle instance. The
iteration hierarchy for a model that performs the 0-60 accelerations test, the EPA
city estimated fuel economy test, and the EPA highway estimated fuel economy test
is shown in this diagram:

.. figure:: ../images/tutorials/Driver_Process_Definition2.png
   :align: center
   :alt: Diagram of process model showing the vehicle assembly, some simulation drivers, and the optimizer
   
   Iteration Hierarchy for All Vehicle Simulations

Again, the top level driver commands a sequential execution of the SimAcceleration instance and
the two SimEconomy instances. Note that since there is no data connection between these
drivers, they could be executed in parallel if the option is available. This will be supported
in a future release. The three simulation drivers contain the same Vehicle instance in each
of their workflows. When the second driver starts its simulation, the vehicle's variables
are just as the previous simulation left them. This is not so important for these drivers,
because both simulation components reset the velocity to `0,` the throttle to `idle,` and the 
gear to `first` before starting. However, this persistence of the variables is what enables
us to perform cascade optimization (i.e., using multiple optimizers sequentially on the
same workflow.)

Now, let's build a new assembly that includes all three simulations run sequentially.

.. testcode:: ThreeSim

        from openmdao.main.api import Assembly
        from openmdao.examples.enginedesign.driving_sim import SimAcceleration, \
                                                               SimEconomy
        from openmdao.examples.enginedesign.vehicle import Vehicle
        
        class VehicleSim2(Assembly):
            """Optimization of a Vehicle."""
            
            def __init__(self):
                """ Creates a new Assembly for vehicle performance optimization."""
                
                super(VehicleSim2, self).__init__()
        
                # Create Vehicle instance
                self.add('vehicle', Vehicle())
                
                # Create Driving Simulation instances
                self.add('sim_acc', SimAcceleration())
                self.add('sim_EPA_city', SimEconomy())
                self.add('sim_EPA_highway', SimEconomy())
                
                # add Sims to default workflow
                self.driver.workflow.add(['sim_acc', 'sim_EPA_city', 'sim_EPA_highway'])
                
                # Add vehicle to sim workflows.
                self.sim_acc.workflow.add('vehicle')
                self.sim_EPA_city.workflow.add('vehicle')
                self.sim_EPA_highway.workflow.add('vehicle')
            
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
        
            from openmdao.main.api import set_as_top
            my_sim = VehicleSim2()
            set_as_top(my_sim)
    
            my_sim.run()
            
            print "Time (0-60): ", my_sim.sim_acc.accel_time
            print "City MPG: ", my_sim.sim_EPA_city.fuel_economy
            print "Highway MPG: ", my_sim.sim_EPA_highway.fuel_economy
            
First, all of the components are instantiated in the assembly, including the Vehicle
instance, the SimAcceleration instance, and the two SimEconomy instances, which are named
``sim_EPA_city`` and ``sim_EPA_highway``. Next, the three simulation component instances
are added to the driver's workflow. Multiple components can be added to a workflow
with a single call to ``add`` by passing a list of the name strings. Since there are no
data connections between them, they will be executed in the order they appear in
this list.

Each simulation driver has a workflow, so the `vehicle` instance is added to each
of their workflows. After that, each of the simulation drivers is connected to the
inputs and output it needs with ``add_parameters`` and ``add_objective``. The variable
`profilename` is the name of the file that contains the EPA driving profile, which
is essentially velocity as a function of time.

Finally, notice that the variable ``force_execute`` is set to True. All drivers have a
``force_execute`` flag, which can be set to True to ensure that a component will always
run when its workflow is executed. Since these drivers are basically independent and
have no data connections, there is no way to automatically determine if they have become
invalidated (as changing an upstream input would do) and hence need to be run. With
``force_execute`` set to True, the driver always runs. Note that our top level driver is
the default sequential execution driver, so this model can run without ``force_execute``.
However, ``force_execute`` is definitely needed if we want to take this model and optimize
it (which we will do next), so it's good to practice using it for cascaded drivers.
