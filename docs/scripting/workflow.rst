
Workflow
=========

The execution order for components in a model is determined by the workflow object
that the components belong to. OpenMDAO current has two available workflow classes that
are described below.  They are Dataflow and SequentialWorkflow.

Dataflow
-----------

The "default" workflow for a model is inferred from the data flow connections.
This means that a component is available to run once its inputs become valid,
which occurs when the components that supply those inputs are valid. Since
direct circular connections (algebraic loops for those familiar with Simulink)
are not permitted, there will always be an execution order that can be
determined from the connections.  In the absence of a connection between two
components, this workflow will attempt to execute them in the order that they 
were added to it.

When any component input is set, all dependent outputs are invalidated. If an input
is connected to an output and that output becomes invalid, then the input
is also invalid. If a component
has any invalid inputs or outputs, it will be executed during the next run. 
When a component's inputs are changed, all downstream variables that depend
on them either directly or indirectly are invalidated. Also,
when a model is instantiated, all outputs are invalid, which ensures that the
whole model always executes the first time it is run.


SequentialWorkflow
-----------------------

This workflow is a simple sequence of components.  The components will be executed
in the order that they were added to the workflow regardless of data dependencies.
Generally, this is a bad idea, but it's here for those rare occasions when the 
exact sequence must be specified.


Geometry in OpenMDAO
=====================

We are currently investigating an API to provide a unified geometry interface. More
information on the notional prototype can be found in
:ref:`Geometry-Interfaces-in-OpenMDAO`.


