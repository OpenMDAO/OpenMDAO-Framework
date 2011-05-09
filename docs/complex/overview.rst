
.. index:: tutorial problem

Problem Overview
==================

This tutorial covers some of the more advanced capabilities of OpenMDAO. You should read and understand
the :ref:`simple tutorial problem <A-Simple-Tutorial-Problem>` before starting this one.


The objective of this tutorial problem is to design an automobile that performs "well" as measured
by these three metrics: 

- Time to accelerate from rest to 60 mph
- Fuel economy as measured by the EPA highway driving test
- Fuel economy as measured by the EPA city driving test

We will be designing a conventional automobile with a gasoline-fueled internal
combustion engine and a 5-speed manual transmission. Our scope is preliminary
conceptual design, so we need to choose simulation models with a compact set
of design inputs and a quick execution time to allow exploration of the design
space via multiple executions. An existing vehicle simulation was not
available in the desired form, so we developed one based on physical first
principles. The mathematical model for the engine came from open literature.

To simulate these performance metrics, we need a model of the
vehicle's power-train, including the engine, the transmission, and the rear
differential. We also need the equations of motion for driving the vehicle. A
logical way to divide the vehicle model is to break it into component models
matching each of these subsystems: engine, transmission, and chassis (which
includes the rear differential ratio). In a typical problem, each of these
component models will be a separate implementation, possibly with different
authors or vendors. The design variables for each subsystem are detailed
below.

The engine, transmission, and chassis subsystems of a vehicle are each
described by some kind of mathematical model that depends on a set of design
variables (e.g., compression ratio.) There are also three  simulation
variables: throttle position, gear, and velocity. These variables are
independent of any design and are used during simulation while the vehicle
model is being "driven" to determine the metrics. There are also a couple of
internal inputs, such as RPM and Power, that are provided by other components
in the vehicle. These inter-dependencies define the connection order for
vehicle components in terms of the Data Flow: Transmission -> Engine ->
Chassis.

.. _`process-model`:

The full process model is shown below.

.. figure:: ../images/tutorials/Process_Diagram.png
   :align: center
   :alt: Diagram of process model showing the vehicle assembly, some simulation drivers, and the optimizer
   
   Process Model for the Optimization Problem


The process model includes three drivers that performs the acceleration and
fuel economy simulations and calculate the performance metrics. A typical
problem might be to close an optimization loop around the vehicle model,
driving one or more design variables to minimize the 0-60 acceleration time
and maximize the EPA city and highway mileage.


The Transmission Model
--------------------------

The transmission model must perform two tasks:

1. Provide a transformation from engine output torque to torque at the wheels
2. Calculate the engine RPM

The transmission modeled here is a 5-speed manual. Shifting occurs
instantaneously when the simulation input *CurrentGear* is given a new value.
When the clutch is engaged, the wheel rotation and the engine rotation are
directly linked via the current gear ratio and the differential ratio, so the
engine RPM can be calculated given the velocity. However, this direct linkage
would cause the engine RPM to go to zero as the vehicle stops, so the clutch
partially disengages for low speed operation (i.e., where the engine speed
would drop below 1000 RPM) and sets the engine speed to 1000 RPM. This only
occurs when the transmission is in first gear.


**Transmission - Design Variables:**

======================  ===========================================  ======
Variable                Description                                  Units
======================  ===========================================  ======
``ratio1``              Gear ratio in first gear
----------------------  -------------------------------------------  ------
``ratio2``              Gear ratio in second gear
----------------------  -------------------------------------------  ------
``ratio3``              Gear ratio in third gear
----------------------  -------------------------------------------  ------
``ratio4``              Gear ratio in fourth gear
----------------------  -------------------------------------------  ------
``ratio5``              Gear ratio in fifth gear
----------------------  -------------------------------------------  ------
``final_drive_ratio``   Gear ratio for vehicle's differential
----------------------  -------------------------------------------  ------
``tire_circumference``  Circumference of the tire                    inch
======================  ===========================================  ======

|

**Transmission - Simulation Inputs:**

=================     ===========================================  ======
Variable              Description                                  Units
=================     ===========================================  ======
``current_gear``      Current gear position
-----------------     -------------------------------------------  ------
``velocity``          Current vehicle velocity                     m/s
=================     ===========================================  ======

|

**Transmission - Outputs:**

=================  ===========================================  ======
Variable           Description                                  Units
=================  ===========================================  ======
``torque_ratio``   Ratio of transmission output power to power 
                   at the wheel
-----------------  -------------------------------------------  ------
``RPM``            Engine rotational speed                      rpm
=================  ===========================================  ======

  
The Engine Model
------------------

The engine model must provide two pieces of information:

1. Torque at engine output
2. Fuel burn under current load

We used a model published in a master's thesis by S. Sitthiracha (`1`_). It is
a physics-based model of the Otto cycle in a 4-stroke spark-ignition internal
combustion engine. This model allows the construction of a parametrized engine
model with 10 design inputs covering the engine mechanical design (cylinder
bore, stroke, connecting rod length, and compression ratio); intake valve
design (diameter and lift); and the cycle timing (for both intake and spark).
In the thesis, the model is implemented in Simulink and simulated using data
from a family of Mercedes-Benz engines designed in 1969. The model includes
the effects of burn duration, heat loss through the cylinder wall, losses due
to friction and charge heating, and intake orifice flow. Some of these effects
were derived from empirical data and are essentially valid over an engine
speed ranging from 1000 RPM to 6000 RPM.

Sitthiracha's model also includes the fuel type as a design variable. This
introduces a half dozen parameters that are dependent on the fuel chemistry.
To keep our model simple, we set these parameters to values appropriate for
gasoline and did not provide them as design inputs for the engine model.
However, it would not be difficult to modify the component code so any of
these could be used as design variables.

Sitthiracha's model contained a couple of errors in the equations, and a couple
of factors needed to be adjusted to obtain good results. His model also assumed
wide-open throttle, so the effect of a throttle was modeled as an additional
restriction on the intake flow into the cylinder. For simulation, relating the
throttle position to an actual physical foot position is not important. All that
is needed is a continuum of throttle settings between closed and wide open. This
model assumes that closed is 1% of open, but the simulation currently drives it
using a minimum of 7%, which gives a more realistic performance.

The design variables in this problem allow for some significant modification to
the engine design. This strongly affects the engine weight, so we need to estimate
this. A report by Shikida (`2`_) contains some empirical data taken from a
sampling of engines present in the Japanese market in 2000. This data maps engine
displacement and weight vs power. Displacement is essentially a measurement of the
engine size and can be calculated from the design parameters, so the engine model
uses a linear fit between engine weight and displacement to estimate the engine
weight and provide it as an output.


**Engine - Design Variables:**

=================  ===========================================  ========
**Variable**       **Description**                              **Units**
=================  ===========================================  ========
``stroke``         Length of compression zone in cylinder       mm
-----------------  -------------------------------------------  --------
``bore``           Bore (cylinder diameter)                     mm
-----------------  -------------------------------------------  --------
``conrod``         Connecting rod length                        mm
-----------------  -------------------------------------------  --------
``comp_ratio``     Volumetric ratio of compression            
-----------------  -------------------------------------------  --------
``spark_angle``    Spark angle with respect to top dead center  deg
-----------------  -------------------------------------------  --------
``n_cyl``          Number of Cylinders    
-----------------  -------------------------------------------  --------
``IVO``            Intake valve open before top dead center     deg
-----------------  -------------------------------------------  --------
``IVC``            Intake valve close after bottom dead center  deg
-----------------  -------------------------------------------  --------
``L_v``            Maximum valve lift                           mm
-----------------  -------------------------------------------  --------
``D_v``            Intake valve diameter                        mm
=================  ===========================================  ========

|

**Engine - Simulation Inputs:**

=================  ===========================================  ======
Variable           Description                                  Units
=================  ===========================================  ======
RPM                Engine rotational speed (1000-6000)          rpm
-----------------  -------------------------------------------  ------
throttle           Throttle position                
=================  ===========================================  ======

|

**Engine - Outputs:**

=================  ===========================================  ======
Variable           Description                                  Units
=================  ===========================================  ======
``power``          Power produced by engine                     kW
-----------------  -------------------------------------------  ------
``torque``         Torque produced by engine                    N*m
-----------------  -------------------------------------------  ------
``fuel_burn``      Fuel burn rate                               l/sec
-----------------  -------------------------------------------  ------
``engine_weight``  Engine weight estimate                       kg
-----------------  -------------------------------------------  ------
``overspeed``      True if engine RPM is over 5000
-----------------  -------------------------------------------  ------
``underspeed``      True if engine RPM is under 1000
=================  ===========================================  ======


**References:**

_`1`. Sitthiracha, Sitthichok, "An Analytical Model of Spark Ignition Engine for Performance Prediction,"
Master's Thesis, King Mongkut's Institute of Technology North Bangkok, 2006.

_`2`. Shikida, Takasuke, Yoshikatsu Nakamura, Tamio Nakakubo, and Hiroyuki Kawase, "Development of the High
Speed 2ZZ-GE Engine," SAE World Congress, March 6-9 2000, SAE 2000-01-0671.

  
The Chassis Model
-------------------

The chassis model must provide the vehicle acceleration given the torque
produced by the engine and scaled by the transmission. The equation used for
the model is the sum of the forces acting on the vehicle in the forward
direction. These forces include both the rolling friction associated with the
tires and the vehicle drag which is proportional to the square of velocity.



**Chassis - Design Variables:**

=================  ===========================================  ======
**Variable**       **Description**                              **Units**
=================  ===========================================  ======
``mass_vehicle``   Vehicle mass                                 kg
-----------------  -------------------------------------------  ------
``Cf``             Rolling friction coefficient            
-----------------  -------------------------------------------  ------
``Cd``             Drag coefficient            
-----------------  -------------------------------------------  ------
``area``           Front profile area                           m*m
=================  ===========================================  ======

|

**Chassis - Simulation Inputs:**

======================  ===========================================  ======
**Variable**            **Description**                              **Units**
======================  ===========================================  ======
``mass_engine``         Engine mass estimate                         kg
----------------------  -------------------------------------------  ------
``velocity``            Current vehicle velocity                     m/s
----------------------  -------------------------------------------  ------
``torque_ratio``        Ratio of transmission output power to power 
                        at the wheel
----------------------  -------------------------------------------  ------
``tire_circumference``  Circumference of the tire                    m
======================  ===========================================  ======

|

**Chassis - Outputs:**

=================  ===========================================  ======
Variable           Description                                  Units
=================  ===========================================  ======
acceleration       Vehicle instantaneous acceleration           m/(s*s)
=================  ===========================================  ======

|

Simulating the Acceleration Test (0-60)
------------------------------------------

.. todo:  I am hiding this quote by using the todo directive without the last colon. When more humor is
   ready to be injected, these lines can be deleted and the quote will show up.

        "I saw this in a movie about a bus that had to **speed** around a city, keeping its **speed** over fifty and if its **speed** dropped, it would explode! I think it was called ... *The Bus That Couldn't Slow Down.*" 
            -- Homer Simpson


The procedure for simulating the maximum acceleration is straightforward. The vehicle is commanded at
wide open throttle, and the resulting acceleration is integrated until the velocity reaches 60 mph. A time
step of 0.1 seconds is used for simulation, which is small enough that a simple (and efficient) trapezoidal
integration was adequate. Gears are shifted at the red line, which is the 6000 RPM limit of the engine model.

Shifting at the red line is not always optimal, though it is optimal for the default
engine given here. The optimal shifting RPMs are dependent on the engine's torque curve as well as the gear
ratios, so creating a generalized yet more optimal shifting procedure would be more numerically intensive. It
would also be possible to use the shift points as variables and let an optimizer solve for their
locations.


Simulating the EPA Mileage Tests
--------------------------------

The EPA mileage tests give an estimate of the fuel consumed while driving a predetermined
velocity profile that represents a particular class of driving, the two most well-known of
which represent typical city driving and highway driving. These tests aren't actually
performed on the open road but are instead done in the EPA testing garage with the tires on
rollers and a hose connected to the exhaust pipe to measure the composition of the
exhaust gases. The test still uses a driver who must follow a velocity profile given on a
computer screen. The actual velocity profiles are available on the EPA website as follows:

.. _`EPA City Driving Profile`:

.. figure:: ../images/tutorials/EPA-city.gif
   :align: center
   :alt: line graph showing EPA city driving profile

   EPA City Driving Profile

.. _`EPA Highway Driving Profile`:

.. figure:: ../images/tutorials/EPA-highway.gif
   :align: center
   :alt: line graph showing EPA highway driving profile 

   EPA Highway Driving Profile


To simulate these tests, the vehicle model must follow the EPA velocity profiles. That is, the time
history of the gear and throttle position must be found that allows the vehicle to follow these profiles. The
fuel consumed is captured over the profile so that the mileage estimate can be calculated. This can be
summarized by the following procedure:

1. Determine acceleration required to reach next velocity point
2. Determine correct gear
3. Solve for throttle position that matches the required acceleration
4. For that gear and throttle setting, calculate fuel burn

The trickiest part of the entire simulation is determining the right gear. The simulation has to test the
acceleration at min and max throttle to determine if the required acceleration is possible in that gear. The
simulation also has to make sure the engine RPM lies within its min and max values. For low speed (under
10 mph), the transmission is always set to first gear.

Once the gear is determined, a bisection method is used to find the throttle position that matches the
required acceleration within a small tolerance. This solution method converges quickly, especially
when applied over a linear range of the torque curve. However, the EPA profiles are long, with many calculation
points, so simulating these driving profiles consumes much more CPU time than the acceleration test.

