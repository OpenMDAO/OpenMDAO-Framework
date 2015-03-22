.. index:: plugin guide overview
.. index:: plugins

Overview of OpenMDAO Plugin Development
=======================================

Plugins provide a way to extend the functionality of OpenMDAO without modifying
the framework itself. This is possible because a :term:`plugin`
implements a particular interface that the framework knows how to interact
with. Users can create plugins in one of two ways: using the ``plugin`` script
or manually creating a distribution.

Plugin Development Tools
-------------------------

There is a script called ``plugin`` available to simplify the process of
developing and installing plugin distributions. The table below shows each
subcommand of ``plugin`` with a brief description of its purpose. All of the
subcommands are described in more detail in the section :ref:`build-pure-python-plugin-label`, 
which is a great resource if you are starting a new plugin and are not familiar 
with creating Python distributions.


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


Manual Plugin Distribution Creation
------------------------------------

While the above commands simplify the process of creating 
plugin distributions, they cannot always be used. Because they are best 
used when starting a plugin distribution from scratch and do not work well with
existing plugin distributions, it's useful to understand how to manually develop
a plugin. 

The following are guidelines describing the types of plugins available to extend the
functionality of OpenMDAO and configurations of files used for creating
Python distributions. These guidelines should lead to creating a plugin distribution 
that OpenMDAO can install successfully.

The rest of this page assumes that you have already installed OpenMDAO. If you
haven't, learn how to do that :ref:`here <Installing-OpenMDAO>`.

.. note:: If you intend to develop a plugin on Windows that requires compilation, you
          will need to have the necessary compiler(s) installed on your system. See the
          *Developer Guide* installation instructions for :ref:`Windows <Windows>` for help installing
          Visual C++ 2008 and mingw32.
          
.. note:: If you are developing a plugin for OpenMDAO, you should be wary of certain
          conventions that will make the end product more available and installable to other
          users.  Taking certain measures (like using ``plugin quickstart`` to begin the process)
          will make things less painful later on.  Never fear, if you have created a plugin
          without using ``plugin quickstart``, we can still help you.  And if you have multiple 
          plugins that you would like to install with one command, we can help
          with setting those up correctly as well.

          Our ``plugin install`` command originally was written to work only with local plugins and 
          with the OpenMDAO-Plugins account on GitHub.  As OpenMDAO's popularity has grown, new users 
          are creating plugins and would like the option to install all of the plugins they have written 
          using our ``plugin install`` command.


.. index:: entry point

Plugin Interface
-----------------

Plugins within OpenMDAO are just Python classes that provide an expected
interface, so as long as your class provides the necessary interface and can
be imported into your Python script, you'll be able to use it as a plugin. But
what if an OpenMDAO user wants to obtain a listing of all of the plugins that
are available in the environment? To allow that to happen, a plugin developer
must provide metadata that specifies the *name, plugin interface,* and *location*
within its package for each plugin that is intended to be discoverable by the
framework. This metadata is in the form of an *entry point*. An entry point is
just a mapping of a name to some Python object, in our case, a class that
exists within a distribution. Each entry point must be a member of an entry
point group. An application can look at the entry point groups that are
defined to determine if any applicable plugins exist within a given
distribution.  In the case of OpenMDAO, we look for entry point group names
that correspond to the types of plugin interfaces that we support.

*Types of Plugin Interface*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

*setup.cfg*
~~~~~~~~~~~

A ``setup.cfg`` file is required for specifying metadata for your distribution.
You should set all metadata fields that are applicable to your plugin.
While *name* and *version* values are the only required fields, you should
also set *requires-dist* if your distribution has dependencies. In general, 
you should fill in as many as possible to better inform potential users about
your plugin. 

.. note::
    Distributions tend to evolve over time, so providing a **version** id for a
    package is extremely important. It is assumed that once a distribution is
    created from a particular version of a package, that distribution will
    never change. People may build things that depend on a particular
    version of your distribution, so changing that version could break their
    code. If, however, you update your distribution's version id, then users
    have the option of either using the updated distribution and modifying
    their own code to make it work or sticking with an older version that
    already works with their code. 


More descriptions of the various metadata values can be found `here`__.

.. __: http://alexis.notmyidea.org/distutils2/setupcfg.html


The values in the *metadata* section are specified by **PEP 345** and 
apply to any Python distribution.  We've added an *openmdao* section to the
file to provide a place to put metadata that isn't mentioned in PEP 345, for
example, the copyright notice for the documentation. Additionally, you can create 
an ``easy_install`` section to list URLs to be used when searching for requirements. 
Doing so is an alternative to using the find-links option from the command line
and makes it easier for users to install your plugin. 
An example of specifying find-links in ``setup.cfg`` can be read `here`__.

.. __: https://pythonhosted.org/setuptools/easy_install.html#configuration-files


Adding Documentation
---------------------

OpenMDAO allows plugins to have their own documentation and has built-in support for building,
distributing, and viewing plugins. If you would like to include documentation with your plugin, you
must follow a few  steps.

First, you'll want to create a ``docs`` directory within the root directory of your
distribution. 

Next, you'll need to add the following files to the ``docs`` directory:

    - ``index.rst`` -- A reStructuredText file containing the index for plugin documentation
                
    - ``usage.rst`` -- A reStructuredText file containing any docs that you want to add to those that are generated automatically.
                
    - ``conf.py`` -- Configuration file used by Sphinx for building documentation
    
    - ``srcdocs.rst`` -- A reStructuredText file containing source documentation for the plugin
                  
    - ``pkgdocs.rst`` -- A reStructuredText file with documentation to support plugins with multiple packages
 
 
*MANIFEST.in*
~~~~~~~~~~~~~

A ``MANIFEST.in`` file is required for directing Python to include files in 
your distribution that were not specified via ``setup.py``. Because
OpenMDAO builds documentation in a specific location, you will need to update
``MANIFEST.in`` to ensure that the built version of your documentation is included
with distributions of your plugin. To do so, you should add a line, similar to the following, to your ``MANIFEST.in``
file:

::

  graft <package-root>/sphinx_build/html



where `<package-root>` is the root directory of your package as specified in your ``setup.py`` file.

More information about writing a ``MANIFEST.in`` file can be read `here`__.

.. __: https://docs.python.org/2/distutils/sourcedist.html#manifest-template
    


Building a Source Distribution
-------------------------------

Once you have your ``setup.py`` file and your plugin class is complete, you're ready
to build a distribution. If you're not able to use ``plugin makedist`` for some 
reason, you can build your distribution by executing your ``setup.py`` file in the following
way:

::

    python setup.py sdist -d .
    
    
This will create a source distribution and place it in the current directory. If your
distribution is named ``simple_adder``, for example, the source distribution will be named 
``simple_adder-1.0.tar.gz`` or possibly ``simple_adder-1.0.zip`` on Windows.  The version 
of the packaged distribution is 1.0 as was specified in the ``setup.py`` file.



Tagging Plugins: How and Why
----------------------------

If you're using Git to develop your plugin, tagging is a way to signify that a certain point in
a repository's history is important. The most common way we tag an OpenMDAO repository is with
a  version number. So when your plugin is stable (e.g., between enhancements), it's a good idea
to tag it with a version number using the ``git tag`` command.  If you tag certain versions of
your plugin, then even when you make changes that break the latest plugin for your users, users
can jump back and access an earlier, unbroken version. Without plugin tagging, users would be
stuck with whatever the most recent version of your repository is.

**Tagging Basics**

To see the tags available in a local repository, switch to the repository and type:

::

    git tag

You should get a list of version tags, for example:    
    
::

    0.1
    0.2
    0.3
    0.4
    0.5
    0.6
    0.7

If you want to see the tags that exist on a plugin but don't have a local copy of a repository,  if
it's public, you can still see what the tags are, but you will need to use the GitHub api in the
following form: 

::

    .. __: https://api.github.com/repos/OWNER/PLUGIN/tags

Going to: https://api.github.com/repos/OpenMDAO-Plugins/CADRE/tags, for example, will return a
page like this (edited for length to show just one tag):

::

    [
      {
        "name": "0.7",
        "zipball_url": "https://api.github.com/repos/OpenMDAO-Plugins/CADRE/zipball/0.8",
        "tarball_url": "https://api.github.com/repos/OpenMDAO-Plugins/CADRE/tarball/0.8",
        "commit": {
          "sha": "00349ff3f07c537a56ba4a049b7c18c8b34dd34a",
          "url": "https://api.github.com/repos/OpenMDAO-Plugins/CADRE/commits/00349ff3f07c537a56ba4a049b7c18c8b34dd34a"
        }
      },
    ]

To create a tag in a plugin repository, just specify the tag number and briefly describe what's been
done in the version. In this example we are in CADRE plugin repository.

::
 
    git tag -a 0.8 -m 'Adding such and such functionality into v 0.8'

Setting this tag will associate the current state of the code with the tag. Later on, this tag will
allow our plugin installer to go back and install version 0.8 of CADRE if, for instance, the latest
version didn't work. To install this version, type:

::

    plugin install CADRE==0.8  --github

If you want the latest version of the plugin, just type 

::

    plugin install CADRE --github

and you will get the latest tagged release. If a repository has never been tagged, however, ``plugin
install`` will simply retrieve a repository's latest version of the default branch, which may not be
stable.  This is why plugins found at OpenMDAO-Plugins are all tagged.

We will discuss :ref:`plugin-install` in more detail later in this document. For further general
discussion of Git tagging, follow this link: http://git-scm.com/book/en/v2/Git-Basics-Tagging.


Making Your Plugin Available to Others
-----------------------------------------

You can make your plugin available to others in a number of ways; you can simply email your distribution
to others or give it to them on a thumb drive, CD, etc. To simplify plugin installation
for users via ``plugin install``,  it's best to place your distributions on a file server.

If you want to distribute your plugin to the whole world but don't happen to
have your own public server, you can put your plugin up on the 
`Python Package Index`_ (PyPI), which is also known as the *Cheeseshop*. 

.. _Python Package Index: https://pypi.python.org/pypi

Another option is to host your plugin source code at `GitHub`_, allowing
users to use a GitHub-specific option with ``plugin install``. 

.. _Github: https://github.com

While you're free to host your plugin where ever you'd like, if your plugin
is not hosted at GitHub or PYPI, users will need to know the URL to pass to 
``plugin install`` so that your distribution can be downloaded. 

.. note::
   Hosting plugins at PyPI requires registration. See this `link`_ for more information about how to register your plugin with PyPI.

.. _link: https://docs.python.org/2/distutils/packageindex.html



