Engine design sample problem

This is a simple design example that simulates the 0-60 acceleration time for a vehicle given an engine/transmission/vehicle design. To test it out, type the following command at the unix prompt in the same folder where this readme.txt is found.

../../../../buildout/bin/python engine_optimization.py

This file contains an Openmdao model containing a sim_vehicle component and a simple sequential execution driver. Inputs to this component include a number of design parameters, and the output is the 0-60 mph acceleration time that results from simulation.

sim_vehicle has a "socket" which accepts a vehicle component. Sockets are not yet implemented, so the interface to the vehicle is temporary.

The vehicle component is an assembly that includes three components: Transmission, Engine, and Vehicle Dynamics. See Example_Problem_Process_Diagram.pn g for a process diagram of the whole model.

-----------

This example should help to demonstrate the following:

1. Creating inputs and outputs to a component
2. Hooking up components in a model
3. Using a simple optimizer as a driver (CONMIN)
4. Properties of OpenMDAO variables

There are some features that are not yet implemented in OpenMDAO:

1. Promotion of input and output variables from a subcomponent to the assembly level
2. The concept of "Sockets"

As a workaround, the example problem does contain some code that is not representative of how these features will be invoked once they are implemented. These areas are (hopefully) clearly marked.


