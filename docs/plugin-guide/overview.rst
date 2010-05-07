.. index:: plugin guide overview
.. index:: plugins

Overview of OpenMDAO Plugin Development
=======================================

Plugins provide a way to extend the functionality of an application without
modifying the application itself.  This is possible because the :term:`plugins` must  implement a particular interface that the framework knows how
to interact with. This section will describe the types of plugins available to
extend the functionality of OpenMDAO and will explain how to build them and how
to make  them usable by the framework.

.. index:: Component plugin

Types of Plugins
----------------

OpenMDAO supports a number of different plugin types, but the most common is the :term:`Component` plugin. The
Component plugin, and other less common types of OpenMDAO plugins, are listed in the following table
along with a description of their purpose.

===========================  =================================================================================================
**Plugin Type**              **Purpose**                                                                                              
===========================  =================================================================================================
:term:`Component`            Add custom computations to an OpenMDAO model 
---------------------------  -------------------------------------------------------------------------------------------------
:term:`TraitType`            Add custom data object to pass between components
---------------------------  -------------------------------------------------------------------------------------------------
:term:`Driver`               Add custom iterative executive (optimizer, solver, design space explorer) to an OpenMDAO model
---------------------------  -------------------------------------------------------------------------------------------------
:term:`CaseIterator`         Add custom supplier of Cases
---------------------------  -------------------------------------------------------------------------------------------------
:term:`ResourceAllocator`    Add custom handling of allocation of computing resources
===========================  =================================================================================================


How Does OpenMDAO Find Plugins?
-------------------------------

When creating a distribution of a Python project, you can 
associate a variety of metadata with that distribution.  A list of
entry points is one piece of metadata that can be associated. An 
entry point is a mapping of a name to some Python object, usually
a class or a function, that exists within the distribution.  Each
entry point must be a member of an entry point group. An application
can look at the entry point groups that are defined to determine if
any applicable plugins exist within a given distribution.


OpenMDAO looks for the following entry point groups in order to find
plugins within a distribution:

====================  ================================
**Plugin Type**       **Entry Point Group**           
====================  ================================
Component             openmdao.component 
--------------------  --------------------------------
TraitType             openmdao.variable
--------------------  --------------------------------
Driver                openmdao.driver
--------------------  --------------------------------
CaseIterator          openmdao.case_iterator
--------------------  --------------------------------
ResourceAllocator     openmdao.resource_allocator
====================  ================================


*Defining Entry Points*
~~~~~~~~~~~~~~~~~~~~~~~

Entry points are defined within the ``setup.py`` file that is
used to build the distribution.  The following code snippet
shows a ``setup.py`` file that defines an entry point for an
OpenMDAO component plugin called *SimpleAdder* in a distribution 
called *simple_adder*:


..  _plugin_overview_Code2:


::


    from setuptools import setup, find_packages
    
    setup(
        name='simple_adder',
        version='1.0',
        packages=find_packages(),
        install_requires=['openmdao.lib', 'Traits>=3.1.0'],
        entry_points={
        'openmdao.component': ['SimpleAdder = simple_adder:SimpleAdder']
        }
    )

The example above shows that an entry point named *SimpleAdder* that maps to
the *SimpleAdder* class within the *simple_adder.py* module is a member of
the *openmdao.component* entry point group.  This tells OpenMDAO that the
SimpleAdder plugin is an OpenMDAO Component.


*Installing an OpenMDAO Plugin*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo:: First, write an 'install_plugin' script, then talk about it here


*Making Your Plugin Available to Others*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
.. todo:: Uploading to a package index
   
   
*Adding Custom Egg Metadata*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo:: Need to work with team to determine standard openmdao metadata

