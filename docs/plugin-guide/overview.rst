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
just a mapping of a name to some Python object, in our case a class, that
exists within a distribution. Each entry point must be a member of an entry
point group. An application can look at the entry point groups that are
defined to determine if any applicable plugins exist within a given
distribution.  In the case of OpenMDAO, we look for entry point group names
that correspond to the types of plugin interfaces that we support.


Types of Plugin Interfaces
--------------------------

OpenMDAO supports a number of different plugin interfaces. Each one maps to a
different entry point group. The following table shows the mapping of each
entry point group to the purpose within the framework of plugins in that
group:


===============================  =================================================================================================
**Entry Point Group**            **Purpose**                                                                                              
===============================  =================================================================================================
``openmdao.component``           To add custom computations to an OpenMDAO model 
-------------------------------  -------------------------------------------------------------------------------------------------
``openmdao.variable``            To add custom data object to pass between components
-------------------------------  -------------------------------------------------------------------------------------------------
``openmdao.driver``              To add custom iterative executive (optimizer, solver, design space explorer) to an OpenMDAO model
-------------------------------  -------------------------------------------------------------------------------------------------
``openmdao.case_iterator``       To add custom supplier of Cases
-------------------------------  -------------------------------------------------------------------------------------------------
``openmdao.resource_allocator``  To add custom handling of allocation of computing resources
===============================  =================================================================================================


.. note:: The entry point group names look like they could be the names of Python
     modules or packages, but they're not.  They're just strings that a plugin 
     developer uses to indicate to the framework that his plugin supports a particular
     plugin interface.

The framework also provides a base class corresponding to most plugin
interfaces to make it easier for developers to create new plugins by
simply inheriting from the base class and modifying a small number of methods
and/or attributes.

The table below shows each base class and the entry point group that it
belongs to:

============================================  ================================
**Base Class**                                **Entry Point Group**
============================================  ================================
``openmdao.main.api.Component``               ``openmdao.component`` 
--------------------------------------------  --------------------------------
``openmdao.main.api.Variable``                ``openmdao.variable``
--------------------------------------------  --------------------------------
``openmdao.main.api.Driver``                  ``openmdao.driver``
--------------------------------------------  --------------------------------
``openmdao.main.resource.ResourceAllocator``  ``openmdao.resource_allocator``
============================================  ================================


Note that every plugin in ``openmdao.driver`` is also assumed to be a member 
of ``openmdao.component`` since Driver inherits from Component. 


*Plugin Development Tools*
~~~~~~~~~~~~~~~~~~~~~~~~~~

There is a script called ``plugin`` available to simplify the process of
developing and installing plugin distributions. The table below shows each
subcommand of ``plugin`` with a brief description of its purpose. All of the
subcommands are described in more detail in
:ref:`build-pure-python-plugin-label`.


======================  ===========================================================================
**Command**             **Purpose**
======================  ===========================================================================
``plugin build_docs``   To build the html docs for the plugin
----------------------  ---------------------------------------------------------------------------
``plugin docs``         To view the html docs for the plugin
----------------------  ---------------------------------------------------------------------------
``plugin install``      To install the plugin into the active environment
----------------------  ---------------------------------------------------------------------------
``plugin list``         To list installed or available plugins
----------------------  ---------------------------------------------------------------------------
``plugin makedist``     To create a source distribution containing the plugin
----------------------  ---------------------------------------------------------------------------
``plugin quickstart``   To create the directory structure needed to build the plugin distribution
======================  ===========================================================================



*Defining Entry Points*
~~~~~~~~~~~~~~~~~~~~~~~

The good news is that if you use the ``plugin makedist`` tool to package your
plugin, the ``setup.py`` file with all necessary entry points will be created
for you automatically. The bad news is that there are some cases where
``plugin makedist`` cannot be used, so the entry points must be defined
manually. The rest of this section describes how to add entry points and other
metadata to a distribution manually.


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
        install_requires=['openmdao.lib'],
        entry_points={
        'openmdao.component': ['simple_adder.SimpleAdder = simple_adder:SimpleAdder']
        }
    )

The example above shows that an entry point named ``simple_adder.SimpleAdder``
that maps to the SimpleAdder class within the ``simple_adder.py`` module is a
member of the ``openmdao.component`` entry point group. This tells OpenMDAO
that the SimpleAdder plugin is an OpenMDAO Component.  The list of entry point
groups that OpenMDAO recognizes is the same as the list of plugin types shown
in the table above. 


.. note:: You should always use the full module dotted name as the name of your entry
   point to be consistent with other OpenMDAO plugins.
   
   
Once you have your ``setup.py`` file and your plugin class is complete, you're ready
to build a distribution.  If you're not able to use ``plugin makedist`` for some 
reason, you can build your distribution by executing your ``setup.py`` file in the following
way:

::

    python setup.py sdist -d .
    
    
This will create a source distribution and place it in the current directory. If your
distribution is named ``simple_adder``, for example, the source distribution will be named 
``simple_adder-1.0.tar.gz``, or possibly ``simple_adder-1.0.zip`` on Windows.  The version 
of the packaged distribution is *1.0* as was specified in the ``setup.py`` file.


*Installing an OpenMDAO Plugin*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you run ``plugin install`` from the top directory of your plugin
distribution, it will install your plugin as a *develop* egg, meaning that it
places a link to your distribution on the Python path so that you can make
changes to your plugin and test it in the environment without having to keep
reinstalling it.

If you have a distrbution tar or zip file, created either by using ``plugin makedist`` 
or by running ``setup.py`` directly, you can install your plugin into an OpenMDAO virtual 
environment by running ``plugin install`` and passing it the name of the file, for 
example:

::

    plugin install myplugin-0.5.tar.gz
    

which will install the distribution into the ``site-packages`` directory
of your OpenMDAO virtual environment.

Finally, if you want to install a plugin distribution from a remote server, it
would look like:

::

    plugin install [-f <find_links_url>] <distrib_requirement>
    

where ``find_links_url`` is the url for a ``find_links`` server and ``distrib_reqirement`` is
a requirement string in the same form as you would pass to ``easy_install`` or ``pip``.
For example, ``myplugin``, ``myplugin==0.5``, and ``myplugin>=0.3`` are all valid requirement
strings.  If there is no version specifier in the ``distrib_requirement``, then the latest
version compatible with the current platform will be installed.


*Making Your Plugin Available to Others*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
You can make your plugin available to others in a number of ways, from simply emailing your distribution
to others or giving it to them on a thumb drive, CD, etc., or placing your
distribution on a file server that they can access. As mentioned above,
``plugin install`` allows you to download and install Python distributions
from remote web servers. For example, if there were a distribution called
*MyDist* on the ``openmdao.org`` server and you wanted to grab the newest version
of it, you could ``plugin install`` it into your activated OpenMDAO virtual
environment as follows:

::

    plugin install -f http://openmdao.org/dists MyDist


If you want to distribute your plugin to the whole world but don't happen to
have your own public server, you can put your plugin up on the 
`Python Package Index`__ (PyPI), which is also known as the *Cheeseshop*. 
PyPI is the default package index for ``plugin install``, so the command

.. __: https://pypi.python.org/pypi


::

    plugin install MyDist
    
    
will attempt to download the MyDist distribution from PyPI. See this `link`__
for more information about how to register your plugin with PyPI.

.. __: http://docs.python.org/2/distutils/packageindex.html


