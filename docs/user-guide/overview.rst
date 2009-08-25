.. index:: user guide overview

Overview of the OpenMDAO Framework
==================================


What is OpenMDAO?
-----------------

MDAO stands for MultiDisciplinary Analysis and Optimization, and OpenMDAO is
an open source framework to analyze and solve MDAO problems. In OpenMDAO, a
problem is represented by a system of objects called Components. These objects
have input and output attributes and can perform some sort of calculation when
they are executed. They can have their inputs and outputs connected to those
of other Components, allowing data to be passed between them when they perform
their calculations.


The following figure gives a conceptual view of what a simple Component might
look like. This Component has three inputs (a, b, and c) and two outputs (x
and y), and performs a simple calculation indicated by the equations shown in
the center of the component.

.. figure:: ../generated_images/Component.png
   :align: center

   Conceptual View of a Simple Component


Note that Components within OpenMDAO can be as simple or complex as necessary,
and the inputs and outputs to a Component are python objects, so they are not
limited to being simple types like float or int.

An Assembly is a special kind of Component that contains other Components.
When an Assembly is executed, it runs the Components it contains in the order
determined by its Workflow object. A Workflow is simply an object that
determines execution order for a group of Components. The default type of
Workflow in an Assembly is a Dataflow, which orders the Components
according to the direction the data flow between them, i.e., such that any
Component that supplies input values to another Component will always run
*before* that Component.

A Driver is another special kind of Component. Drivers are designed to iterate
over a set of Components until some condition is met. Some examples of Drivers
are optimizers, solvers, and design space explorers.



