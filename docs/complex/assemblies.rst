
.. index:: Assembly

Assemblies
=============

Now that we've created Python components representing the three vehicle
subsystems, we need to connect them so that they can be executed in sequence.
Recall that an OpenMDAO component that contains a collection of other
components is called an :term:`Assembly`. The Assembly allows a set of
components to be linked together by connecting their inputs and outputs. The
data connections define an execution order based on their dependencies, i.e.,
components that are upstream in the data flow will be executed prior to those
downstream so that input data to a component will always be valid with respect
to the other parts of the workflow. A component will not execute if its inputs
have not changed since its last execution. An Assembly contains a
Driver called *driver* that controls iterations over the workflow.

For the vehicle simulation, a Vehicle assembly is needed that can sequentially execute the Transmission,
Engine, and Chassis components.

.. testcode:: Code5

    from openmdao.main.api import Assembly
    from openmdao.lib.datatypes.api import Float

    from openmdao.examples.enginedesign.transmission import Transmission
    from openmdao.examples.enginedesign.chassis import Chassis
    from openmdao.examples.enginedesign.engine_wrap_c import Engine
    
    class Vehicle(Assembly):
        """ Vehicle assembly. """

        def __init__(self):
            """ Creates a new Vehicle Assembly object """

            super(Vehicle, self).__init__()

            # Create component instances
        
            self.add('transmission', Transmission())
            self.add('engine', Engine())
            self.add('chassis', Chassis())

            # Set up the workflow
            self.driver.workflow.add(['transmission', 'engine', 'chassis'])

The Engine, Transmission, and Chassis components all need to be imported so
their instances can be created; they can be added to the assembly
with ``add``. Please notice that an assembly inherits from Assembly
instead of from Component. We also need to add the instances to the driver's
workflow using the ``add`` method. Note that the order we add these doesn't
matter here, because the  execution order is inferred from the data connections,
which we now need to make.

Now that the components are instantiated in the assembly, they need to be hooked up:

.. testsetup:: Code5

        # Note: This block of code does not display in the documentation.

        from openmdao.main.api import Assembly
        from openmdao.lib.datatypes.api import Float

        from openmdao.examples.enginedesign.engine import Engine
        from openmdao.examples.enginedesign.transmission import Transmission
        from openmdao.examples.enginedesign.chassis import Chassis
        
        class Vehicle(Assembly):
            """ Vehicle assembly. """
    
            def __init__(self):
                """ Creates a new Vehicle Assembly object """

                super(Vehicle, self).__init__()

                # Create component instances
        
                self.add('transmission', Transmission())
                self.add('engine', Engine())
                self.add('chassis', Chassis())

                # Set up the workflow
                self.driver.workflow.add(['transmission', 'engine', 'chassis'])

        # This is a trick to get around a limitation in Sphinx's doctest, where
        # there is no way to preserve the indentation level between code
        # blocks, and the concept of "self" is not defined when we fall out of
        # the class scope.
        
        self = Vehicle()

.. testcode:: Code5

        self.connect('transmission.RPM','engine.RPM')
        self.connect('transmission.torque_ratio','chassis.torque_ratio')
        self.connect('engine.torque','chassis.engine_torque')
        self.connect('engine.engine_weight','chassis.mass_engine')

The first argument in the call to ``self.connect`` is the output variable of
the source component instance, and the second argument is the input variable
of the target component instance. For a connection to be valid, the units of
the output and input must be compatible (e.g., length, speed, etc.) If
they differ within the same class (e.g., meters vs. inches), then the value is
converted from the source unit to the target unit before setting the value at
the input. If the classes are incompatible (e.g., meters vs. seconds), then an
exception is raised during execution.

The Vehicle assembly also has inputs and outputs, and it can be hooked up to
other components and included in other assemblies once its variables
are defined. We would like to promote all of the design and simulation
variables from the Engine, Transmission, and Chassis components to the input
and output of the Vehicle assembly. OpenMDAO includes a shortcut for doing
this quickly by creating *passthroughs*:

.. testcode:: Code5

        self.create_passthrough('engine.stroke')
        self.create_passthrough('engine.bore')
        # ...
        # ...
        self.create_passthrough('transmission.ratio1')
        self.create_passthrough('transmission.ratio2')
        # ...
        # ...
        self.create_passthrough('chassis.mass_vehicle')
        self.create_passthrough('chassis.Cf')

The ``create_passthrough`` function creates an identical variable
in the assembly and connects it to the corresponding component variable. So now, all of the
design variables are available as variables in any simulation that includes an instance
of the vehicle model.

However, the engine tutorial throws you a curve ball here. The Engine
and Chassis components are defined with SI units, but the Transmission
component is defined with English units. We have two inputs -- the tire
circumference and the vehicle velocity -- that are each used by two components
with different units. The ``create_passthrough`` function creates an exact copy
of the variable, so we cannot use it here. Instead, we must connect them manually
by declaring variables in our assembly.

.. testcode:: Code5

        class Vehicle(Assembly):
            """ Vehicle assembly. """
    
            tire_circumference = Float(75.0, iotype='in', units='inch', 
                                desc='Circumference of tire (inches)')
    
            velocity = Float(75.0, iotype='in', units='mi/h', 
                       desc='Vehicle velocity needed to determine engine RPM (mi/h)')

Now these inputs are available to connect to the components, so we connect them manually.

.. testsetup:: Code7b

        from openmdao.main.api import Assembly, implements, Interface
        from openmdao.lib.datatypes.api import Float, Int

        from openmdao.examples.enginedesign.engine import Engine
        from openmdao.examples.enginedesign.transmission import Transmission
        from openmdao.examples.enginedesign.chassis import Chassis
        
        class Vehicle(Assembly):
            """ Vehicle assembly. """
    
            tire_circumference = Float(75.0, iotype='in', units='inch', 
                                    desc='Circumference of tire (inches)')
    
            velocity = Float(75.0, iotype='in', units='mi/h', 
                desc='Vehicle velocity needed to determine engine RPM (mi/h)')
    
            def __init__(self):
                """ Creates a new Vehicle Assembly object. """
        
                super(Vehicle, self).__init__()

                # Create component instances
        
                self.add('transmission', Transmission())
                self.add('engine', Engine())
                self.add('chassis', Chassis())

                # Set up the workflow
                self.driver.workflow.add(['transmission', 'engine', 'chassis'])

        self = Vehicle()

.. testcode:: Code7b

        self.connect('velocity', 'chassis.velocity')
        self.connect('velocity', 'transmission.velocity')
        self.connect('tire_circumference', 'chassis.tire_circ')
        self.connect('tire_circumference', 'transmission.tire_circ')

This ensures that the units for these inputs to the Vehicle are converted properly for use in the Chassis and 
Transmission components. While this might seem redundant, it demonstrates
a way that Assemblies can be used to define a more consistent external interface.

Executing the Vehicle Assembly
==============================

We can manipulate the Vehicle Assembly in the Python shell just like we did with the engine component
above. As inputs, the Vehicle takes a commanded velocity, throttle position, a gear position, and
a set of vehicle design parameters, and outputs the vehicle's instantaneous acceleration and rate of fuel
burn. 

        >>> from openmdao.examples.enginedesign.vehicle import Vehicle
        >>> my_car = Vehicle()
        >>> my_car.velocity = 25.0
        >>> my_car.current_gear = 3
        >>> my_car.throttle = .5
        >>> my_car.run()
        >>> my_car.acceleration
        1.1086409681...
        >>> my_car.fuel_burn
        0.0027991856...

When we run the Vehicle, we are performing a simple multidisciplinary analysis via the
OpenMDAO framework. Try setting the simulation variables to other values, including ones that should
trigger an exception. (One way to do this is to command a high velocity in first gear, which should
violate the maximum RPM that the engine allows.)

