.. index:: plugin guide overview

Overview of OpenMDAO Plugin Development
=======================================

Plugins provide a way to extend the functionality of an application without
modifying the application itself.  This is possible because the plugins must
implement a particular interface that the framework knows how to interact with.
This section will describe the types of plugins available to extend
the functionality of OpenMDAO and will explain how to build them and how to make 
them usable by the framework.

Types of Plugins
----------------

OpenMDAO supports a number of different plugin types, but the most common by
far is the Component plugin. Other less common types of OpenMDAO plugins exist
and can be found in the table below.

===========================  =================================================================================================
**Plugin Type**              **Purpose**                                                                                              
===========================  =================================================================================================
:term:`Component`            Add custom computations to an OpenMDAO model 
---------------------------  -------------------------------------------------------------------------------------------------
TraitType                    Add custom data object to pass between components
---------------------------  -------------------------------------------------------------------------------------------------
:term:`Driver`               Add custom iterative executive (optimizer, solver, design space explorer) to an OpenMDAO model
---------------------------  -------------------------------------------------------------------------------------------------
:term:`CaseIterator`         Add custom supplier of Cases
---------------------------  -------------------------------------------------------------------------------------------------
:term:`ResourceAllocator`    Add custom handling of allocation of computing resources
===========================  =================================================================================================


How does OpenMDAO find plugins?
-------------------------------

    - TODO: discuss entry points
    - TODO: should we have a plugins directory, OPENMDAO_PLUGIN_PATH env var, ...
    - TODO: tell how to use easy_install to install plugin 
          (easy_install -f http://...  -d <plugin_dir> where <plugin_dir> must be on python path)


Options for Plugin Creation
===========================

There are three primary ways to create a plugin for OpenMDAO. The simplest and
most common for tasks that are not computationally intensive is to create the
plugin by writing it in pure python. For tasks that are computationally
intensive, often the best choice is to create a python extension using a
shared library that was compiled from FORTRAN, C, or C++ code. Finally, when a
legacy code must be wrapped as a Component or Driver plugin and creating a
python extension for it is not feasible, an ExternalCode component can be
configured to communicate with the legacy code via file I/O.


Pure Python Component Plugin Example
------------------------------------

For this example we'll build a plugin for the component shown in the figure
:ref:`Conceptual-View-of-a-Simple-Component`.  This component simply computes
the value of its single output by adding its two inputs together.

Our first step is to create our class. We want to inherit from
openmdao.main.api.Component, because that provides us with the interface we
need to function properly as an OpenMDAO Component.


.. _Code1: 

::

    from enthought.traits.api import Float
    
    from openmdao.main.api import Component

    class SimpleAdder(Component):
        a = Float(0.0, iostatus='in')
        b = Float(0.0, iostatus='in')
        c = Float(0.0, iostatus='out')
    
        def execute(self):
             self.c = self.a + self.b


At this point, we could import the module containing our SimpleAdder class and
use it within OpenMDAO, but we want more than that. We want to package our
class in a python egg in such a way that we can give the egg to anyone we
choose for use in their own OpenMDAO installation, and we want their OpenMDAO
installation to recognize our plugin automatically without any special
configuration required on their part aside from placing the egg in a
designated plugins directory.

In order to accomplish this, we'll add entry points to the metadata that we
associate with our egg. An entry point gives a plugin a name and tells the
framework how to find a class or factory function inside of the egg that can
be used to create instances of the object type defined by the plugin. Entry
points are also arranged in groups. This is how OpenMDAO determines the type
of a given plugin.  The entry point groups associated with each type of 
plugin are shown in the table below.


===========================  ================================
**Plugin Type**              **Entry Point Group**                                                                                              
===========================  ================================
:term:`Component`            openmado.component 
---------------------------  --------------------------------
TraitType                    openmdao.trait
---------------------------  --------------------------------
:term:`Driver`               openmdao.driver
---------------------------  --------------------------------
:term:`CaseIterator`         openmdao.case_iterator
---------------------------  --------------------------------
:term:`ResourceAllocator`    openmdao.resource_allocator
===========================  ================================


Quick and Dirty Egg Creation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There is a tool called mod2egg.py that can be used to convert a single python
module into an egg...


Python Extension
----------------


File Wrapper
------------


