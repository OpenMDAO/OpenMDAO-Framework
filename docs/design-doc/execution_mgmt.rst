
Execution Management (Data Passing, etc.)
-----------------------------------------

The execution of the model is determined by the structure of the
System tree. The top level Assembly contains the root of the System
tree, and when you run the Assembly it just runs the root System node.

The root system node can be either a SerialSystem or a DriverSystem.

When a SerialSystem runs, it runs each of its child systems once, in
order according to data flow.

When a DriverSystem runs, it runs its internal system (the workflow system)
once each time the Driver iterates.

- scattering: primary data passing mechanism for flattened data
