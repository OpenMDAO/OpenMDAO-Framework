.. index:: user guide script interface

The OpenMDAO Script Interface
==============================

Executing a Simple Problem - Introduction to the OpenMDAO Tutorial
------------------------------------------------------------------

Creating New Components
-----------------------

*The Component API*
~~~~~~~~~~~~~~~~~~~

*Wrapping an Application*
~~~~~~~~~~~~~~~~~~~~~~~~~

Python
+++++++

F2PY
++++

SWIG
++++

File Wrapping
+++++++++++++
   
*Special Plug-ins*
~~~~~~~~~~~~~~~~~~

The Excel Wrapper
+++++++++++++++++

The Matlab Plug-in
++++++++++++++++++

The Octave Plug-in
++++++++++++++++++
   
*Guidelines for Constructing a Good Component*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The PEP 8 Standard
++++++++++++++++++

Pylint
++++++
  
*Publishing a Component*
~~~~~~~~~~~~~~~~~~~~~~~~

Eggs
++++

Adding a New Component to your Local Library
++++++++++++++++++++++++++++++++++++++++++++

Framework Variables
-------------------

*Traits*
~~~~~~~~

*Built-in Types*
~~~~~~~~~~~~~~~~

*StringRefs*
~~~~~~~~~~~~

A StringRef is a special type of string variable that contains an expression to
be evaluated. The expression can reference variables and functions within the
scope of its containing component, as well as within the scope of the component's
parent Assembly.  A number of builtin functions and math functions may also be
referenced within a StringRef expression.  For example, ``abs(math.sin(angle))``
would be a valid StringRef expression, assuming that *angle* is an attribute of the
containing component. Note that *self* does not appear in the example expression.
This is because the StringRef automatically determines the containing scope of
attributes and functions referenced in an expression. This helps keep expressions
from becoming too verbose by containing a bunch of ``self`` and ``self.parent``
references.

StringRefs can be used in a variety of components. Many optimizer components use 
StringRefs to specify their objective function, design variables, and constraints.
Conditional branching components use StringRefs to specify boolean expressions that
determine if a given branch should be executed.

*Unit Conversion*
~~~~~~~~~~~~~~~~~

*Creating New Variable Types*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Building a Simulation Model
---------------------------

*Connecting Components*
~~~~~~~~~~~~~~~~~~~~~~~

*Assemblies*
~~~~~~~~~~~~

*Sockets & Interfaces*
~~~~~~~~~~~~~~~~~~~~~~

*The Top Level Assembly*
~~~~~~~~~~~~~~~~~~~~~~~~

Drivers
-------

*The Driver Interface*
~~~~~~~~~~~~~~~~~~~~~~

*Case Iterator*
~~~~~~~~~~~~~~~

*Solution Drivers*
~~~~~~~~~~~~~~~~~~

CONMIN
++++++

NEWSUMT
+++++++

PyEvolve
++++++++

Newton Solver
+++++++++++++

*Adding new Optimizers*
~~~~~~~~~~~~~~~~~~~~~~~

Running OpenMDAO
-----------------

*Executing Models*
~~~~~~~~~~~~~~~~~~

*Error Logging & Debugging*
~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Saving & Loading*
~~~~~~~~~~~~~~~~~~

*Sharing Models*
~~~~~~~~~~~~~~~~

Data Flow and WorkFlow
----------------------

*Data Flow & Lazy Evaluation*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Building a WorkFlow*
~~~~~~~~~~~~~~~~~~~~~

Looping
+++++++

Branching
+++++++++

Design Tools
------------

*Design of Experiments*
~~~~~~~~~~~~~~~~~~~~~~~

*Multi-objective Optimization and Pareto Frontiers*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Sensitivity Analysis*
~~~~~~~~~~~~~~~~~~~~~~

Managing Simulation Data
------------------------

Multi-Threaded Computation
--------------------------

Geometry in OpenMDAO
--------------------
 
Advanced MDAO 
-------------

*Multi-Fidelity Optimization*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Surrogate Modeling*
~~~~~~~~~~~~~~~~~~~~~

*Uncertainty*
~~~~~~~~~~~~~
 
