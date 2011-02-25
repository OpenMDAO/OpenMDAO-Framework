.. index:: plugin guide overview
.. index:: plugins

Overview of OpenMDAO Plugin Development
=======================================

Plugins provide a way to extend the functionality of OpenMDAO without modifying
the framework itself. This is possible because a :term:`plugin`
implements a particular interface that the framework knows how to interact
with. This section describes the types of plugin types available to extend the
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
TraitType             ``openmdao.variable``
--------------------  --------------------------------
Driver                ``openmdao.driver``
--------------------  --------------------------------
CaseIterator          ``openmdao.case_iterator``
--------------------  --------------------------------
ResourceAllocator     ``openmdao.resource_allocator``
====================  ================================


Note that every ``openmdao.driver`` plugin is also assumed to be an 
``openmdao.component``.


How Do I Put My Plugin into OpenMDAO?
-------------------------------------

OpenMDAO plugins are registered using a class decorator called ``@plugin``. The
following shows a simple example of an ``openmdao.component`` plugin:

::


    from openmdao.main.api import Component, plugin
    
    @plugin('openmdao.component')
    class MyComp(Component):
        def execute(self):
            print 'Hello World'
        
    
Using the ``@plugin`` decorator on your plugin class will register it whenever
the module containing it is imported.  In addition, the framework will use the
information you provide in the ``@plugin`` call to create the appropriate entry
point metadata for your plugin distribution when you package it using the 
``package_plugin`` tool.


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
the openmdao.org server, you could ``easy_install`` it into your current python
environment as follows:

::

    easy_install -f http://openmdao.org/dists MyDist

