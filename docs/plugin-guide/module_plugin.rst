.. index:: SimpleAdder

.. index:: pair: plugin; building from a Python Module


Building a Component Plugin from a Python Module
================================================

For this example we'll build a plugin for the component shown in the figure
:ref:`Conceptual-View-of-a-Simple-Component` (from the *User Guide*).  This component
simply computes the value of its single output by adding its two inputs.

Our first step is to create our class. We want to inherit from
``openmdao.main.api.Component``, because that provides us with the interface we
need to function properly as an OpenMDAO component.


.. _plugin_overview_Code1: 

.. testcode::plugin_example

    from openmdao.lib.api import Float
    
    from openmdao.main.api import Component

    class SimpleAdder(Component):
        a = Float(0.0, iotype='in')
        b = Float(0.0, iotype='in')
        c = Float(0.0, iotype='out')
    
        def execute(self):
             self.c = self.a + self.b


The code defines the class *SimpleAdder*, which inherits from the
Component class defined in ``openmdao.main.api``, so we have to import it from
there. The function in our component that performs a computation is called
``execute()``, and there we define *c* as the sum of *a* and *b*.
The *self* object that is passed as an argument to ``execute()`` represents an
instance of our *SimpleAdder* class.

*SimpleAdder* has three Public Variables of type *Float* with the names *a*, *b*, and
*c*. All three attributes have a default value of 0.0. Attributes *a* and *b*
are inputs, so we specify that they have an *iotype* of *'in'*. Attribute
*c* is an output, so it has an *iotype* of *'out'*.

The *Float* variable is defined in the package ``openmdao.lib.api``, so we have
to import it from there before we can use it. This 
package defines a wide variety of traits, including basic types like *Int*,
*Str*, and *Bool*; containers like *List* and *Dictionary*; and many others. Public Variables
are actually based off of Enthought's Traits, and a larger selection of less commonly used
traits are available by importing from the package ``enthought.traits.api``.
To learn more about traits, you may want to look at the 
`Traits User Manual <http://code.enthought.com/projects/traits/docs/html/traits_user_manual/index.html>`_
and the list of 
`available traits <http://code.enthought.com/projects/files/ETS32_API/enthought.traits.api.html>`_.

At this point, our SimpleAdder plugin is usable within OpenMDAO. We could simply
import the module containing it and use it in a model; but we want more than
that. By packaging our plugin as a Python distribution, we can make it more usable by
others in the OpenMDAO community. We can give our distribution a version identifier and
other :term:`metadata` that will help others determine if our plugin will meet
their needs. We can also upload our distribution to a package index so that it can be
installed via ``easy_install``, ``pip``, or ``zc.buildout``.

.. index:: creation

Distribution Creation
---------------------

Creating a distribution out of a Python module is straightforward, but it does
require the creation of a simple directory structure, because distributions are
intended to contain Python packages, not just individual modules.

For example, if our SimpleAdder class is in a file called ``simple_adder.py``, 
we need a directory structure that looks like this to make it distributable
as a package in a distribution:

::

   simple_adder
      |
      |-- simple_adder
      |     |
      |     |-- simple_adder.py
      |     `-- __init__.py
      |
      `-- setup.py
      

The ``__init__.py`` file is empty and is only there because that is how
Python determines that the directory ``simple_adder`` is a Python package. The
only other file in the directory structure besides ``simple_adder.py`` is the
``setup.py`` file, which describes how to build a distribution containing our module.
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

    
The *setup()* command has *many* options in addition to those shown above,
e.g., *author, author_email, maintainer, maintainer_email, url, license,
description, long_description, keywords, platforms, fullname, contact,
contact_email, classifiers,* and *download_url.* If you supply any of these,
their values will be stored as metadata in the distribution. To keep things simple, we
won't describe all of the options in detail, but if you're interested, you can
go to  `<http://docs.python.org/distutils/apiref.html#module-distutils.core>`_ and
`<http://peak.telecommunity.com/DevCenter/setuptools#new-and-changed-setup-keywords>`_.

The following options are required for our distribution to function properly
within the OpenMDAO framework:

**name**
    The package must have a name, and generally it should be the
    name of the module, minus the .py extension, e.g., 'simple_adder', or the
    name of the class within the module, assuming that the module contains
    only one class.
    
**version**
    Packages tend to evolve over time, so providing a version id for a package
    is extremely important. You **must** update the version id of your package
    prior to creating a distribution out of it. It is assumed that once a
    distribution is created from a particular version of a package, that
    distribution will **never** change. People may build things that depend on
    a particular version of your distribution, so changing that version could
    break their code. If, however, you update your distribution's version id,
    then users of your distribution have the option to either use the updated
    distribution and make whatever modifications are necessary to their own
    code to make it work or stick with an older version that already works
    with their code. The value of *version* is specified as a string, e.g.,
    '1.0.4'.
    
**packages**
    In the case where you have only one module, there will be only one
    package, but the distribution format allows for the existence of multiple
    packages. You can specify *packages* as an explicit list of strings, but
    the easiest thing to do is to use the *find_packages()* function from
    setuptools as shown in the example above.
    
**install_requires**  
    This specifies the distributions that your distribution depends upon. Note
    that you need to include only *direct* dependencies in this list, i.e., if
    your package depends on *package_A*, which in turn depends on *package_B*,
    you need to include only *package_A*. Make sure not to leave out any
    direct dependencies here, because doing so will result in failure to
    install needed dependent distributions whenever your distribution is
    installed. The value of *install_requires* should be a list of strings.
    These strings can specify not only the name of a distribution, but also a
    version or a range of versions. For example, 'numpy>=1.3.0', 'numpy<=1.5'
    and 'numpy=='1.4.1' are all valid entries in *install_requires*. However,
    it's usually best not to specify an exact version in *install_requires*
    because it will make it harder to install your distribution in an
    environment with other distributions that depend upon a different version
    of some distribution that your package depends on.

**entry_points**
    Entry points can be used by OpenMDAO to determine which plugins are
    available within a distribution. Entry points are divided into groups, and each
    type of OpenMDAO plugin has a particular group. For example, Component
    plugins are found in the *openmdao.component* group. Each individual entry
    point is specified by its name, followed by an equals sign, followed by
    dotted module path (dotted path you would use to import the module in
    Python), followed by a colon and the name of the plugin class. The value
    of *entry_points* should be a string in INI file format or a dictionary. 
    
        
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

        
With the ``simple_adder`` directory structure shown above and the ``setup.py``
file shown, we can now build our distribution. From the ``simple_adder``
directory, typing ``python setup.py sdist -d .`` will create the distribution
in our current directory. The version of the distribution and the Python
version will be included in the filename. For example, since the version we
specified in our ``setup.py`` file was '1.0', our distribution will be named 
``simple_adder-1.0.tar.gz``. 


.. index:: mod2dist

Egg Creation for the Lazy
--------------------------

A tool called ``mod2dist`` exists for those of us who don't want to create a package
directory structure and a setup.py file manually. It has a number of options that you
can see if you run ``mod2dist -h``.  The only required options are the desired version
of the distribution and the module to use to generate the distribution.  For example, the command

::

   mod2dist -v 1.0 simple_adder.py
   
   
will generate the same distribution that we built manually earlier in this example.

.. _Building-a-Variable-Plugin:

Building a Variable Plugin from a Python Module
===============================================

Sometimes it's necessary to create a new type of variable that can be passed 
between OpenMDAO components.  This section describes how to do this using a 
pure Python OpenMDAO plugin.

Let's assume we want to have a variable that represents a set of Cartesian 
coordinates, with the value of the variable being a 3-tuple of floating point
values representing the x, y, and z position.  We'll start by creating a 
file called ``coord.py`` and placing the following code in it:

::

    from enthought.traits.api import TraitType
    
    class Coordinates(TraitType):
    
        def __init__(self, default_value = (0.,0.,0.), **metadata):
            super(Coordinates, self).__init__(default_value=default_value,
                                             **metadata)
    
        def validate(self, object, name, value):
            if isinstance(value, tuple) and len(value) == 3 and \
               all([isinstance(val,float) or isinstance(val,int) for val in value]):
                return value
            else:
                self.error(object, name, value)


OpenMDAO uses the Traits package from Enthought to implement public
variables. The base class for custom traits is *TraitType*, so that's the
base class for our coordinates variable. If a component or a component class
contains a TraitType object and that object has a metadata attribute called
*iotype*, then that object is exposed to the framework as a variable whose
value can be passed between components.  One thing to note that can be a 
little confusing to people first using Traits is that the Trait object itself
is just a validator and possibly a converter.  The object that actually gets
passed around between components is the *value* that the trait corresponds to
and not the trait itself.  For example, if we had a component named *wheel* that 
contained one of our Coordinates traits named *center_location*, then the value
of *wheel.center_location* would be a 3-tuple, not a Coordinates object.

We override the base class constructor so we can supply a default value of
(0.,0.,0.) if the caller doesn't supply one. After that, the only function we
need to supply is the *validate* function, which will be called with the
following arguments:

    **object**
        The object that contains the value of our coordinates variable
    
    **name**
        The name of our coordinates variable
    
    **value**
        The value that our current value is being replaced with


Our validate function should test that the value we've been called with is
valid. In this particular case, we just need to verify that the value is a
3-tuple and it has float or int entries. If the value is acceptable, then we
just return it. We don't need to do it in this case, but in other custom
traits, we could convert the value before returning it. If the value
is not acceptable, then we call the error function, which will raise an
exception.

That's all of the source code required to make our coordinates variable 
functional.  The next step is to turn our module into a package and define
an entry point for our new class.  This is very similar to what we did in the
section earlier where we made a component plugin, except this time we use
a different entry point group name.


::


    from setuptools import setup, find_packages
    
    setup(
        name='coord',
        version='1.0',
        packages=find_packages(),
        install_requires=['Traits>=3.1.0'],
        entry_points={
          'openmdao.variable': ['Coordinates = coord:Coordinates']
        }
    )

We can create this file by hand or generate it using ``mod2dist`` as we showed in
an earlier section.

