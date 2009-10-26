.. index:: SimpleAdder

Building a Plugin from a Python Module
--------------------------------------

For this example we'll build a plugin for the component shown in the figure
:ref:`Conceptual-View-of-a-Simple-Component` (from the User's Guide).  This component
simply computes the value of its single output by adding its two inputs.

Our first step is to create our class. We want to inherit from
``openmdao.main.api.Component``, because that provides us with the interface we
need to function properly as an OpenMDAO Component.


.. _plugin_overview_Code1: 

::

    from enthought.traits.api import Float
    
    from openmdao.main.api import Component

    class SimpleAdder(Component):
        a = Float(0.0, iostatus='in')
        b = Float(0.0, iostatus='in')
        c = Float(0.0, iostatus='out')
    
        def execute(self):
             self.c = self.a + self.b


The code defines the class *SimpleAdder*, which inherits from the
Component class defined in ``openmdao.main.api``, so we have to import it from
there. The function in our Component that performs a computation is called
``execute()``, and there we define *c* as the sum of *a* and *b*.
The *self* object that is passed as an argument to ``execute()`` represents an
instance of our *SimpleAdder* class.

*SimpleAdder* has three traits of type *Float* with the names *a*, *b*, and
*c*. All three attributes have a default value of 0.0. Attributes *a* and *b*
are inputs, so we specify that they have an *iostatus* of *'in'*. Attribute
*c* is an output, so it has an *iostatus* of *'out'*.

The *Float* trait is defined in the package ``enthought.traits.api``, so we have
to import it from there before we can use it. The ``enthought.traits.api``
package defines a wide variety of traits, including basic types like *Int*,
*Str*, and *Bool*; containers like *List* and *Dictionary*; and many others.
To learn more about traits, you may want to look at the 
`Traits User Manual <http://code.enthought.com/projects/traits/docs/html/traits_user_manual/index.html>`_
and the list of 
`available traits <http://code.enthought.com/projects/files/ETS32_API/enthought.traits.api.html>`_.

OpenMDAO also supplies some special-purpose traits as well, e.g.,
*UnitsFloat*, a floating point attribute with units. OpenMDAO traits can be
found in ``openmdao.lib.traits``. 

At this point, our SimpleAdder plugin is usable within OpenMDAO. We could simply
import the module containing it and use it in a model; but we want more than
that. By packaging our plugin in a Python :term:`egg`, we can make it more usable by
others in the OpenMDAO community. We can give our egg a version identifier and
other :term:`metadata` that will help perspective users determine if our egg will meet
their needs. We can also upload our egg to a package index so that others can
install it via ``easy_install`` or ``zc.buildout``.


*Egg Creation*
~~~~~~~~~~~~~~

Creating an egg out of a Python module is straightforward, but it does
require the creation of a simple directory structure, because eggs are
intended to contain Python packages, not just individual modules.

For example, if our SimpleAdder class is in a file called ``simple_adder.py``, 
we need a directory structure that looks like this to make it distributable
as a package in an egg:

::

   simple_adder
      |
      |-- simple_adder
      |     |
      |     |-- simple_adder.py
      |     `-- __init__.py
      |
      `-- setup.py
      

The ``__init__.py`` file is empty, and is only there because that is how
Python determines that the directory ``simple_adder`` is a Python package. The
only other file in the directory structure besides ``simple_adder.py`` is the
``setup.py`` file, which describes how to build an egg containing our module.
In this case, the ``setup.py`` file looks like this:


..  _module_plugin_Code2:


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

    
The ``setup()`` command has *many* options in addition to those shown above,
e.g., author, author_email, maintainer, maintainer_email, url, license,
description, long_description, keywords, platforms, fullname, contact,
contact_email, classifiers, and download_url. If you supply any of these,
their values will be stored as metadata in the egg. To keep things simple, we
won't describe all of the options in detail, but if you're interested, you can
go to  `<http://docs.python.org/distutils/apiref.html#module-distutils.core>`_ and
`<http://peak.telecommunity.com/DevCenter/setuptools#new-and-changed-setup-keywords>`_.

The following options are required for our egg to function properly
within the OpenMDAO framework:

**name**
    The package must have a name, and generally it should be the
    name of the module, minus the .py extension, e.g., 'simple_adder', or the
    name of the class within the module, assuming that the module contains
    only one class.
    
**version**
    Packages tend to evolve over time, so providing a version id for a package
    is extremely important. You **must** update the version id of your package
    prior to creating an egg (or any other type of distribution) out of it.
    The assumption being that once a distribution is created from a particular
    version of a package, that distribution should **never** change. People
    may build things that depend on a particular version of your distribution,
    so changing that version could break their code. If, however, you update
    your distribution's version id, then users of your distribution have the
    option to either use the updated distribution and make whatever
    modifications are necessary to their own code to make it work, or stick
    with an older version that already works with their code. The value of
    *version* is specified as a string, e.g., '1.0.4'.
    
**packages**
    In the case where you only have one module there will only be one package, but
    the egg format allows for the existence of multiple packages. You can specify
    *packages* as an explicit list of strings, but the easiest thing to do is to use
    the ``find_packages()`` function from setuptools as shown in the example above.
    
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
    Python), followed by a colon and the name of the plugin class. The value
    of *entry_points* should be a string in INI file format, or a dictionary. 
    
        
    For example:
    
    ::
    
        """
        [openmdao.components]
        SimpleAdder = simple_adder:SimpleAdder
        
        [openmdao.drivers]
        MyDriver = mydriver:MyDriver
        """
	   
    or
     
    :: 
       
          
        { 'openmdao.components': ['SimpleAdder = simple_adder:SimpleAdder'],
          'openmdao.drivers': ['MyDriver = mydriver:MyDriver']
        }

        
With the ``simple_adder`` directory structure shown above and the ``setup.py`` file shown,
we can now build our egg.  From the ``simple_adder`` directory, typing
``python setup.py bdist_egg -d .`` will create the egg in our current directory. The version
of the egg and the Python version will be included in the filename of the egg. For example,
since the version we specified in our ``setup.py`` file was '1.0', and assuming we're using
Python 2.6, our egg will be named ``simple_adder-1.0-py2.6.egg``.  If our package had contained
compiled code, then our egg name would also include the name of the platform we're on, but
since simple_adder is nothing but pure Python code, that's not necessary.


*Egg Creation for the Lazy*
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A tool called ``mod2egg`` exists for those of us who don't want to create a package
directory structure and a setup.py file manually. It has a number of options that you
can see if you run ``mod2egg -h``.  The only required options are the desired version
of the egg and the module to use to generate the egg.  For example, the command

::

   mod2egg -v 1.0 simple_adder.py
   
   
will generate the same egg that we built manually earlier in this example.

