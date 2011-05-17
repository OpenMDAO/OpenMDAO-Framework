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

