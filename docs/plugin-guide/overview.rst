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

OpenMDAO supports a number of different plugin types, but the most common is
the Component plugin. Other less common types of OpenMDAO plugins exist and
can be found in the table below.

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


The code defines the class *SimpleAdder*, and it specifies that the class has
three traits of type *Float* with the names *a*, *b*, and *c*. All three
attributes have a default value of 0.0. Attributes *a* and *b* are inputs, so
we specify that they have an *iostatus* of *'in'*. Attribute *c* is an output,
so it has an *iostatus* of *'out'*. The *Float* trait is defined in the
package *enthought.traits.api*, so we have to import it from there before we
can use it. The *enthought.traits.api* package defines a wide variety of traits
including basic types like *Int*, *Str*, and *Bool*; containers like *List* and
*Dictionary*, and many others. OpenMDAO also supplies some special-purpose
traits as well, e.g., *UnitsFloat*, a floating point attribute with
units. OpenMDAO traits can be found in *openmdao.lib.traits*. Our *SimpleAdder*
class inherits from the Component class defined in *openmdao.main.api* so we
have to import it from there. The function in our Component that performs a
computation is called *execute()*, and there we define that *c* is simply the
sum of *a* and *b*. The *self* object that is passed as an argument to
*execute()* represents an instance of our *SimpleAdder* class.

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


Egg Creation
~~~~~~~~~~~~

Creating an egg out of a python module is straightforward, but it does
require the creation of a simple directory structure, because eggs are
intended to contain python packages, not just individual modules.

For example, if our SimpleAdder class is in a file called ``simple_adder.py``, 
we need a directory structure that looks like this to make it distributable
as a package in an egg:

::

   simple_adder
      |
      -- simple_adder
      |     |
      |     -- simple_adder.py
      |     -- __init__.py
      |
      -- setup.py
      

The ``__init__.py`` file is empty, and is only there because that is how
python determines that the directory ``simple_adder`` is a python package. The
only other file in the directory structure besides ``simple_adder.py`` is the
``setup.py`` file, which describes how to build an egg containing our module.
In this case, the ``setup.py`` file looks like this:

.. _Code1

::

    from setuptools import setup, find_packages
    
    setup(
        name='simple_adder',
        version='1.0',
        packages=find_packages(),
        install_requires=['openmdao.main', 'Traits>=3.1.0'],
        entry_points="""
        [openmdao.component]
        SimpleAdder = simple_adder:SimpleAdder
        """
    )

    
The ``setup()`` command has *many* options in addition to those shown above,
e.g., **author**, **author_email**, **maintainer**, **maintainer_email**,
**url**, **license**, **description**, **long_description**, **keywords**,
**platforms**, **fullname**, **contact**, **contact_email**, **classifiers**,
and **download_url**. If you supply any of these, their values will be stored
as metadata in the egg. To keep things simple, we won't describe all of the
options in detail, but if you're interested, you can go to 
`<http://docs.python.org/distutils/apiref.html#module-distutils.core>`_ and 
`<http://peak.telecommunity.com/DevCenter/setuptools#new-and-changed-setup-keywords>`_.

The following options are required if you want your egg to function properly
within the OpenMDAO framework:

**name**
    Your package must have a name, and to avoid confusion that name should be the
    name of your module, minus the .py extension, e.g., 'simple_adder'.
    
**version**
    Packages tend to evolve over time, so providing a version id for them 
    is extremely important.  You **must** update the version id of your package prior
    to creating an egg (or any other type of distribution) out of it. The assumption 
    being that once an egg is created from a particular version of a package, that
    egg should **never** change. People may build things that depend on a particular
    version of your egg, so changing that version could break their code. If, however,
    you update your egg's version id, then users of your egg have the option to either
    use the updated egg and make whatever modifications are necessary to their own code
    to make it work, or stick with an older version of your egg that already works with
    their code.  The value of *version* is specified as a string, e.g., '1.0.4'.
    
**packages**
    In this case, where you only have one module, there will only be one package, but
    the egg format allows for the existence of multiple packages. You can specify
    *packages* as an explicit list of strings, but the easiest thing to do is to use
    the *find_packages()* function from setuptools as shown in the example above.
    
**install_requires**
    This specifies the packages that your egg depends upon. Note that you only need to
    include *direct* dependencies in this list, i.e., if your package depends on *package_A*
    which in turn depends on *package_B*, you only need to include *package_A*. Make sure not
    to leave out any direct dependencies here, because doing so will result in failure to
    install needed dependent distributions whenever your egg is installed.  The value
    of *install_requires* should be a list of strings.
    
**entry_points**
    Entry points can be used by OpenMDAO to determine what plugins are
    available within an egg. Entry points are divided into groups, and each
    type of OpenMDAO plugin has a particular group. For example, Component
    plugins are found in the *openmdao.component* group. Each individual entry
    point is specified by its name, followed by an equals sign, followed by
    dotted module path (dotted path you would use to import the module in
    python), followed by a colon and the name of the plugin class. The value
    of *entry_points* should be a string in INI file format. For example:
     """
     [openmdao.components]
     SimpleAdder = simple_adder:SimpleAdder
     
     [openmdao.drivers]
     MyDriver = mydriver:MyDriver
     """

    



Python Extension
----------------


File Wrapper
------------


