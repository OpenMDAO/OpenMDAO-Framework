.. index:: diagrams; class
.. index:: pair: Model Composition; classes
.. index:: classes; core


.. _Class-Diagrams:


Class Diagrams
--------------

The following sections present Class Diagrams for Model Composition Classes
(core classes), built-in Variable Classes, Factory Classes, and Server Classes.


Model Composition Classes
===========================

The figure `Class Diagram of Core Classes`_ shows the classes that are the building
blocks of a *model*. A model is a hierarchical structure with an :term:`Assembly`
at its root. Within the :term:`Assembly` is a :term:`Driver` and some number of
:term:`Components`. Also within the Assembly is a :term:`Workflow`, which controls the
execution order of the Components. The Components have Variables, and these Variables
can be linked to Variables on other Components. An Assembly is a Component, which means
that it can be contained within another :term:`Assembly`. This allows for the creation
of hierarchical models with many levels of nested Assemblies.


.. _`Class Diagram of Core Classes`:

.. figure:: ../generated_images/ModelClasses.png
   :align: center

   Class Diagram of Core Classes


.. index:: built-in Variable classes
.. index:: classes; built-in Variable 


Types of Built-in Variable Classes
==================================

The built-in Variable classes (see `Class Diagram of Variable Classes`_ below) will be
provided with the framework, but it will also be possible to add others as plug-ins. The
plug-in Variables can have the same level of functionality of the built-in ones, i.e.,
they can perform their own validation and can have custom graphical editors.

.. _`Class Diagram of Variable Classes`:


.. figure:: ../generated_images/VariableClasses.png
   :align: center

   Class Diagram of Variable Classes
   
   
.. index:: pair: Factory; classes   

Factory Classes
===============

It is important to give location transparency to the process of object creation,
and using Factory classes lets users do that in an extensible way. The creation
of an object with a specific type and version will be requested, and the
framework will create the object. This creation process could involve spawning a
remote process, instantiating a remote version of the object, and creating a
local proxy to represent the remote object, or it could be a simple import and a
constructor call. To the caller, it makes no difference. The call returns a
local python object, and the true location of the object requested does not
matter.


.. figure:: ../generated_images/CreatorClasses.png
   :align: center

   Class Diagram of Factory Classes
 
   
.. index:: pair: Server; classes   
.. index:: ServerManager
   
   
Server Classes
==============

Simulations are run in one or more :term:`ObjServer` processes, possibly distributed
among multiple hosts. :term:`ObjServer` processes are created by
:term:`ObjServerFactory`, either dynamically when a particular component type is
needed which is not supported in the main simulation server, by the user when starting
a new simulation via the ServerManager, which acts as a portal, or by the user from
the command line.

The base Server class provides a common mechanism for configuring network
protocols and services, while the Simulation class contains the top-level
component and the ResourceAllocationManager for this simulation object.


.. figure:: ../generated_images/ServerClasses.png
   :align: center

   Class Diagram of Server Classes

|


