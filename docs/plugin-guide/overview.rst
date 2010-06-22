.. index:: plugin guide overview
.. index:: plugins

Overview of OpenMDAO Plugin Development
=======================================

Plugins provide a way to extend the functionality of OpenMDAO without modifying
OpenMDAO. This is possible because a :term:`plugin`
implements a particular interface that the framework knows how to interact
with. This section describes the types of plugins available to extend the
functionality of OpenMDAO and explains how to build them and how to make
them usable by the framework.

The rest of this document assumes that you have already installed OpenMDAO. If you
haven't, learn how to do that :ref:`here <Installing-OpenMDAO>`.

.. note:: If you intend to develop a plugin on Windows that requires compilation, you
          will need to have the necessary compiler(s) installed on your system. See the
          *Developer Guide* installation instructions for :ref:`Windows <Windows>` for help on installing
          Visual C++ 2008 and mingw32.


.. index:: Component plugin

Types of Plugins
----------------

OpenMDAO supports a number of different plugin types, but the most common is
the :term:`Component` plugin. The Component plugin and other less common
types of OpenMDAO plugins are listed in the following table along with a
description of their purpose.

===========================  =================================================================================================
**Plugin Type**              **Purpose**                                                                                              
===========================  =================================================================================================
:term:`Component`            To add custom computations to an OpenMDAO model 
---------------------------  -------------------------------------------------------------------------------------------------
:term:`TraitType`            To add custom data object to pass between components
---------------------------  -------------------------------------------------------------------------------------------------
:term:`Driver`               To add custom iterative executive (optimizer, solver, design space explorer) to an OpenMDAO model
---------------------------  -------------------------------------------------------------------------------------------------
:term:`CaseIterator`         To add custom supplier of Cases
---------------------------  -------------------------------------------------------------------------------------------------
:term:`ResourceAllocator`    To add custom handling of allocation of computing resources
===========================  =================================================================================================

.. index:: entry point

How Do I Put My Plugin into OpenMDAO?
-------------------------------------

Plugins within OpenMDAO are just Python classes that provide an expected
interface, so as long as your class provides the necessary interface and can
be imported into your Python script, you'll be able to use it as a plugin.
But what if an OpenMDAO user wants to obtain a listing of all of the 
plugins that are available in the environment?  To allow that to happen, 
a plugin developer must provide metadata that specifies the name,
plugin interface, and location within its package for each plugin that
is intended to be discoverable by the framework.  The rest of this
section describes the form of this metadata and how to add it to 
a distribution.

A list of entry points is one piece of metadata that can be associated with a
distribution. An *entry point* is a mapping of a name to some Python object,
usually a class or a function, that exists within the distribution. Each entry
point must be a member of an entry point group. An application can look at the
entry point groups that are defined to determine if any applicable plugins
exist within a given distribution.

OpenMDAO looks for the following entry point groups to find
plugins within a distribution:

====================  ================================
**Plugin Type**       **Entry Point Group**           
====================  ================================
Component             ``openmdao.component`` 
--------------------  --------------------------------
TraitType             ``openmdao.variable``
--------------------  --------------------------------
Driver                ``openmdao.driver``
--------------------  --------------------------------
CaseIterator          ``openmdao.case_iterator``
--------------------  --------------------------------
ResourceAllocator     ``openmdao.resource_allocator``
====================  ================================


*Defining Entry Points*
~~~~~~~~~~~~~~~~~~~~~~~

Entry points are defined within the ``setup.py`` file that is
used to build the distribution.  The following code snippet
shows a ``setup.py`` file that defines an entry point for an
OpenMDAO component plugin called *SimpleAdder* in a distribution 
called ``simple_adder``:


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
the SimpleAdder class within the ``simple_adder.py`` module is a member of
the ``openmdao.component`` entry point group.  This tells OpenMDAO that the
SimpleAdder plugin is an OpenMDAO Component.


*Installing an OpenMDAO Plugin*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo:: First, write an ``install_plugin`` script, then talk about it here


*Making Your Plugin Available to Others*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
.. todo:: Talk about serving distributions over the web

.. todo:: Look into providing a "contrib" area on ``openmdao.org`` for contributed plugins
   
   
*Adding Custom Distribution Metadata*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo:: Need to work with team to determine standard openmdao metadata

