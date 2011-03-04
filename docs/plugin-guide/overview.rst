.. index:: plugin guide overview
.. index:: plugins

Overview of OpenMDAO Plugin Development
=======================================

Plugins provide a way to extend the functionality of OpenMDAO without modifying
the framework itself. This is possible because a :term:`plugin`
implements a particular interface that the framework knows how to interact
with. This section describes the types of plugins available to extend the
functionality of OpenMDAO and explains how to build them and how to make
them usable by the framework.

The rest of this document assumes that you have already installed OpenMDAO. If you
haven't, learn how to do that :ref:`here <Installing-OpenMDAO>`.

.. note:: If you intend to develop a plugin on Windows that requires compilation, you
          will need to have the necessary compiler(s) installed on your system. See the
          *Developer Guide* installation instructions for :ref:`Windows <Windows>` for help installing
          Visual C++ 2008 and mingw32.


.. index:: Component plugin

Types of Plugins
----------------

OpenMDAO supports a number of different plugin types. The following table
gives a brief description of the purpose of each plugin type:


==============================  =================================================================================================
**Plugin Type**                 **Purpose**                                                                                              
==============================  =================================================================================================
`openmdao.component`             To add custom computations to an OpenMDAO model 
------------------------------  -------------------------------------------------------------------------------------------------
`openmdao.variable`              To add custom data object to pass between components
------------------------------  -------------------------------------------------------------------------------------------------
`openmdao.driver`                To add custom iterative executive (optimizer, solver, design space explorer) to an OpenMDAO model
------------------------------  -------------------------------------------------------------------------------------------------
`openmdao.case_iterator`         To add custom supplier of Cases
------------------------------  -------------------------------------------------------------------------------------------------
`openmdao.resource_allocator`    To add custom handling of allocation of computing resources
==============================  =================================================================================================


The framework provides a base class corresponding to each plugin type in order
to make it easier for developers to create new plugins by simply inheriting
from the base class and modifying a small number of methods and/or
attributes.

The table below shows each base class and the plugin type that it corresponds
to:

====================  ================================
**Base Class**        **Plugin Type**           
====================  ================================
Component             ``openmdao.component`` 
--------------------  --------------------------------
Variable              ``openmdao.variable``
--------------------  --------------------------------
Driver                ``openmdao.driver``
--------------------  --------------------------------
CaseIterator          ``openmdao.case_iterator``
--------------------  --------------------------------
ResourceAllocator     ``openmdao.resource_allocator``
====================  ================================


Note that every ``openmdao.driver`` plugin is also assumed to be an 
``openmdao.component``.


.. index:: entry point

How Do I Put My Plugin into OpenMDAO?
-------------------------------------

Plugins within OpenMDAO are just Python classes that provide an expected
interface, so as long as your class provides the necessary interface and can
be imported into your Python script, you'll be able to use it as a plugin. But
what if an OpenMDAO user wants to obtain a listing of all of the plugins that
are available in the environment? To allow that to happen, a plugin developer
must provide metadata that specifies the name, plugin interface, and location
within its package for each plugin that is intended to be discoverable by the
framework. This metadata is in the form of an *entry point*. An entry point is
a mapping of a name to some Python object, usually a class or a function, that
exists within a distribution. Each entry point must be a member of an entry
point group. An application can look at the entry point groups that are
defined to determine if any applicable plugins exist within a given
distribution.

The good news is that if you use the ``package_plugin`` tool provided with
OpenMDAO to package your plugin, the necessary entry points will be created
for you automatically. The bad news is that there are some cases where
``package_plugin`` cannot be used and so the entry points must be defined
manually. The rest of this section describes how to add entry points and other
metadata to a distribution manually.


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
        'openmdao.component': ['simple_adder.SimpleAdder = simple_adder:SimpleAdder']
        }
    )

The example above shows that an entry point named *simple_adder.SimpleAdder*
that maps to the SimpleAdder class within the ``simple_adder.py`` module is a
member of the ``openmdao.component`` entry point group. This tells OpenMDAO
that the SimpleAdder plugin is an OpenMDAO Component.  The list of entry point
groups that OpenMDAO recognizes is the same as the list of plugin types shown
in the table above. 


.. note:: You should always use the full module dotted name as the name of your entry
   point for consistency with other OpenMDAO plugins.


*Installing an OpenMDAO Plugin*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once you've created a distribution for your plugin either by using ``package_plugin`` 
or by doing it manually, you can install your plugin into an OpenMDAO virtual 
environment in the same way you would install any other distribution into it, e.g., 
using ``easy_install`` or ``pip``.


*Making Your Plugin Available to Others*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
There are a number of ways to do this, from simply emailing your distribution
to them or giving it to them on a thumb drive, CD, etc., or placing your
distribution on a file server that they have access to. Both ``easy_install``
and ``pip`` allow you to download and install python distributions from remote
web servers.  For example, if there were a distribution called 'MyDist' on 
the openmdao.org server, you could ``easy_install`` it into your activated
OpenMDAO virtual environment as follows:

::

    easy_install -f http://openmdao.org/dists MyDist

