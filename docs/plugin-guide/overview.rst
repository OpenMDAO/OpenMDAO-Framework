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
a mapping of a name to some Python object, in our case a class, that
exists within a distribution. Each entry point must be a member of an entry
point group. An application can look at the entry point groups that are
defined to determine if any applicable plugins exist within a given
distribution.


Types of Plugin Interfaces
--------------------------

OpenMDAO supports a number of different plugin interfaces. Each one maps to a
different entry point group. The following table shows the mapping of each
entry point group to the purpose within the framework of plugins in that
group:


==============================  =================================================================================================
**Entry Point Group**           **Purpose**                                                                                              
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


The framework also provides a base class corresponding to most plugin
interfaces in order to make it easier for developers to create new plugins by
simply inheriting from the base class and modifying a small number of methods
and/or attributes.

The table below shows each base class and the entry point group that it
corresponds to:

=========================================  ================================
**Base Class**                             **Entry Point Group**
=========================================  ================================
openmdao.main.api.Component                 ``openmdao.component`` 
-----------------------------------------  --------------------------------
openmdao.main.api.Variable                  ``openmdao.variable``
-----------------------------------------  --------------------------------
openmdao.main.api.Driver                    ``openmdao.driver``
-----------------------------------------  --------------------------------
openmdao.main.resource.ResourceAllocator    ``openmdao.resource_allocator``
=========================================  ================================


Note that every ``openmdao.driver`` plugin is also assumed to be an 
``openmdao.component`` since Driver inherits from Component. 


*Plugin Development Tools*
~~~~~~~~~~~~~~~~~~~~~~~~~~

There is a small set of scripts available to simplify the process of
developing plugin distributions. The scripts, ``plugin_quickstart``,
``plugin_package``, and ``plugin_install`` are described in more detail
in :ref:`build-pure-python-plugin-label`.

The good news is that if you use the ``plugin_package`` tool provided with
OpenMDAO to package your plugin, the necessary entry points will be created
for you automatically. The bad news is that there are some cases where
``plugin_package`` cannot be used and so the entry points must be defined
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

Once you've created a distribution for your plugin either by using ``plugin_package`` 
or by doing it manually, you can install your plugin into an OpenMDAO virtual 
environment by running ``plugin_install``.  If you run 

::

    plugin_install
    
from the top directory of your plugin distribution, it will install your
plugin as a *develop* egg, meaning that it places a link to your distribution
on the python path so that you can make changes to your plugin and test it in
the environment without having to keep reinstalling it.

To install a plugin distribution from a tar file, pass the name of the tar file
to the script, e.g.:

::

    plugin_install myplugin-0.5.tar.gz
    

This command installs the distribution into the ``site-packages`` directory
of your OpenMDAO virtual environment.

Finally, if you want to install a plugin distribution from a remote server, it
would look like:

::

    plugin_install [-f <find_links_url>] <distrib_requirement>
    

where *find_links_url* is the url for a find_links server and *distrib_reqirement* is
a requirement string in the same form as you would pass to ``easy_install`` or ``pip``,
for example, *myplugin*, *myplugin==0.5*, and *myplugin>=0.3* are all valid requirement
strings.  If there is no version specifier in the *distrib_requirement* then the latest
version compatible with the current platform will be installed.


*Making Your Plugin Available to Others*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
There are a number of ways to do this, from simply emailing your distribution
to them or giving it to them on a thumb drive, CD, etc., or placing your
distribution on a file server that they have access to. As mentioned above,
``plugin_install`` allows you to download and install python distributions
from remote web servers. For example, if there were a distribution called
'MyDist' on the openmdao.org server and you wanted to grab the newest version
of it, you could ``plugin_install`` it into your activated OpenMDAO virtual
environment as follows:

::

    plugin_install -f http://openmdao.org/dists MyDist

