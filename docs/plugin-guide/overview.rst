.. index:: plugin guide overview
.. index:: plugins

Overview of OpenMDAO Plugin Development
=======================================

Plugins provide a way to extend the functionality of an application without
modifying the application itself.  This is possible because a :term:`plugin` must
implement a particular interface that the framework knows how to interact with.
This section will describe the types of plugins available to extend
the functionality of OpenMDAO and will explain how to build them and how to make 
them usable by the framework.

.. index:: Component plugin

Types of Plugins
----------------

OpenMDAO supports a number of different plugin types, but the most common is the :term:`Component` plugin. The
Component plugin, and other less common types of OpenMDAO plugins, are listed in following table
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

When creating a distribution of a python project, the author can 
associate a variety of metadata with that distribution.  A list of
entry points is one piece of metadata that can be associated. An 
entry point is a mapping of a name to some python object, usually
a class or a function, that exists within the distribution.  Each
entry point must be a member of an entry point group. An application
can look at the entry point groups that are defined to determine if
any applicable plugins exist within a given distribution.

OpenMDAO looks for the following entry point groups in order to find
plugins within a distribution:

====================  ================================
**Plugin Type**       **Entry Point Group**                                                                                              
====================  ================================
Component             openmado.component 
--------------------  --------------------------------
TraitType             openmdao.trait
--------------------  --------------------------------
Driver                openmdao.driver
--------------------  --------------------------------
CaseIterator          openmdao.case_iterator
--------------------  --------------------------------
ResourceAllocator     openmdao.resource_allocator
====================  ================================


Defining Entry Points
~~~~~~~~~~~~~~~~~~~~~

Entry points are defined within the setup.py file that is
used to build the distribution.  The following code snippet
shows a setup.py file that defines an entry point for an
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


Installing an OpenMDAO Plugin
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    - TODO: tell how to use easy_install to install plugin 
          (easy_install -mNq -f http://...  -d <plugin_dir> where <plugin_dir> must be on Python path)
          When we install a plugin, it's dependencies will also be installed and must be on the
          python path to be used by the system.  -We may need to write our own plugin installer in 
          order to put plugins in the expected place and to prevent the installation of dependencies
          in the plugin directory when they are already a part of the distribution.
    - TODO: should we have a plugins directory, OPENMDAO_PLUGIN_PATH env var, ...
    - TODO: It seems like we need some kind of metadata to describe the plugin API version that
            a given plugin is tied to, because over time there could be changes made to the various
            plugin APIs that will break old plugins, and it would be nice if we could detect that and
            either hide them from our library manager or (better) create an adapter for them on the 
            fly and still be able to use them.


*Making Your Plugin Available to Others*
----------------------------------------
   
::

   TODO: uploading to a package index
   
   
*Adding Custom Egg Metadata*
----------------------------

::

   TODO: need to work with team to determine standard openmdao metadata
      

