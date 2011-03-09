.. index:: SimpleAdder

.. index:: pair: plugin; building a pure Python plugin


.. _build-pure-python-plugin-label:

Building a Pure Python Component Plugin
=======================================

For this example we'll build a plugin for the component shown in the figure
:ref:`Conceptual-View-of-a-Simple-Component` (from the *User Guide*).  This component
simply computes the value of its single output by adding its two inputs.

Our first step is to run the ``plugin_quickstart`` script as follows:

::

    plugin_quickstart SimpleAdder -v 0.9 -d myplugins
    
.. program:: plugin_quickstart

Where the first argument is the name of your plugin class. By default, the
name of the distribution will be the lower case version of your plugin class
name. To change that, use the ``--dist`` option. All arguments starting with a
dash (-) are optional and are described below:

.. option:: -v

   Version id of the plugin.
   
.. option:: -d

   Destination directory where the plugin subdirectory will be created.

.. option:: -g

   Plugin group. The default
   plugin group is ``openmdao.component``. Other possible values are:
   ``openmdao.driver`` and ``openmdao.variable``.

.. option:: --dist

   Name of the distribution. Use this if you want
   it to be different from the lower case version of the plugin class.

   
After running ``plugin_quickstart``, the ``myplugins`` directory will contain
a directory named ``simpleadder``, which is the lower case version of the
plugin class name we passed to the script. The ``simpleadder`` directory
structure will look something like this::

    simpleadder
    |-- docs
    |   |-- conf.py
    |   |-- index.rst
    |   |-- pkgdocs.rst
    |   |-- srcdocs.rst
    |   `-- usage.rst
    |-- MANIFEST.in
    |-- README.txt
    |-- setup.cfg
    |-- setup.py
    `-- src
        `-- simpleadder
            |-- __init__.py
            `-- simpleadder.py


Customizing our Plugin
----------------------

The ``plugin_quickstart`` automatically generates skeleton files for
our plugin class, distribution metadata, and documentation, but
we'll want to tailor these specifically to our plugin.


Editing our Plugin Class
++++++++++++++++++++++++

The most important file to edit is of course the python file that defines our
plugin class.  The plugin class definition is found in:

::

    src/<dist_name>/<dist_name>.py
    

or in our case:

::

    src/simpleadder/simpleadder.py

    
When we're done modifying the skeleton plugin class in the ``simpleadder.py`` file, 
it should look like this:

.. _plugin_overview_Code1: 

.. testcode:: plugin_example

    from openmdao.lib.datatypes.api import Float
    
    from openmdao.main.api import Component

    class SimpleAdder(Component):
        """A simple component whose output *c* is the sum of
        its inputs *a* and *b*.
        """
        a = Float(0.0, iotype='in', desc='an input to be combined with *b* to make *c*')
        b = Float(0.0, iotype='in', desc='an input to be combined with *a* to make *c*')
        c = Float(0.0, iotype='out', desc='the sum of *a* and *b*')
    
        def execute(self):
             """Calculate c as the sum of a and b."""
             self.c = self.a + self.b


The code defines the class ``SimpleAdder``, which inherits from the
Component class defined in ``openmdao.main.api``, so we have to import it from
there. The function in our component that performs a computation is called
``execute()``, and there we define *c* as the sum of *a* and *b*.
The *self* object that is passed as an argument to ``execute()`` represents an
instance of our ``SimpleAdder`` class.

``SimpleAdder`` has three variables of type Float with the names *a*, *b*, and
*c*. All three variables have a default value of 0.0. Attributes *a* and *b*
are inputs, so we specify that they have an iotype of ``'in'``. Attribute
*c* is an output, so it has an iotype of ``'out'``.

The Float variable is defined in the package ``openmdao.lib.datatypes.api``, so 
we have to import it from there before we can use it. This  package defines a 
wide variety of traits, including basic types like *Int*, *Str*, and *Bool*; 
containers like *List* and *Dict*; and others. Variables are actually 
implemented using Enthought's Traits and to learn more about traits, see the  
`Traits User Manual 
<http://code.enthought.com/projects/traits/docs/html/traits_user_manual/index.html>`_.

Developing a plugin is often an iterative process, so it's convenient to have
a way to install the plugin and hack on it, test it, etc., without having to 
reinstall it each time we change it.  Luckily this is easy to do by just
installing our plugin as a *develop* egg. To do this, we just run the ``plugin_install``
command from the top directory of our plugin distribution.

After that, our plugin can be imported and used in the OpenMDAO environment
just like any other installed plugin.  For example, we could import our
plugin class like this:


::

    from <distrib_name> import <plugin_class>
    
    
or, in this specific case:

::

    from simpleadder import SimpleAdder
    


Adding Documentation
++++++++++++++++++++

Now that our plugin class is fully defined, we should write up some documentation
about how to use it.  The packaging script that we'll run later, ``plugin_package``, 
will automatically generate source documentation for our plugin, but we can add to
that by editing the ``docs/usage.rst`` file, perhaps providing some detailed usage
instructions and maybe a few examples.  The format of the ``usage.rst`` file is 
:term:`reStructuredText` and we use Sphinx to generate our documentation, so any
reST or Sphinx directives may be used there.

.. note:: Make sure to give your plugin class a doc string, because otherwise
   the automatic source documentation for your input and output variables won't
   be generated.

The other documentation file that you may want to edit is the ``README.txt`` file.
A small amount of information is put there automatically but you may want to add
more.


Setting Metadata
++++++++++++++++

The final step in preparing to package our plugin is to define metadata for
our distribution.  You specify that metadata by editing the ``setup.cfg`` file.
The skeleton version of ``setup.cfg`` generated by ``plugin_quickstart`` in our
case looks like this:

::

    [metadata]
    name = simpleadder
    version = 0.9
    summary = 
    description-file = README.txt
    keywords = openmdao
    home-page = 
    download-url = 
    author = 
    author-email = 
    maintainer = 
    maintainer-email = 
    license = 
    classifier = Intended Audience :: Science/Research
        Topic :: Scientific/Engineering
    
    requires-dist = openmdao.main
    provides-dist = 
    obsoletes-dist = 
    requires-python = 
        >=2.6
        <2.7
    requires-externals = 
    project-url = 
    
    [openmdao]
    copyright =
    


You should set whatever of these values you feel are applicable to your plugin.
The **name** and **version** values are the only ones that are mandatory, but
you should fill in as many as possible to better inform potential users about
your plugin. 

.. note::
    Distributions tend to evolve over time, so providing a version id for a
    package is extremely important. It is assumed that once a distribution is
    created from a particular version of a package, that distribution will
    **never** change. People may build things that depend on a particular
    version of your distribution, so changing that version could break their
    code. If, however, you update your distribution's version id, then users
    have the option of either using the updated distribution and modifying
    their own code to make it work or sticking with an older version that
    already works with their code. 


More descriptions of the various metadata values can be found 
`here`__.

.. __: http://readthedocs.org/docs/distutils2/en/latest/setupcfg.html#metadata


The values in the *metadata* section are specified by **PEP 345** and they
apply to any python distribution.  We've added an *openmdao* section to the
file to provide a place to put metadata that isn't mentioned in PEP 345, for
example the copyright notice for the documentation.


Additional Customization
++++++++++++++++++++++++

In some cases, you may want to add multiple plugin classes to your distribution,
either in the *<dist_name>.py* file or in separate Python source files that you
add to the ``src`` directory, possible as part of a nested package directory
structure.  The ``plugin_package`` script knows how to handle this sort of a
situation and will generate the appropriate source documentation and metadata
for whatever plugins you define under the ``src`` tree.

If you plan to use ``plugin_package`` to create your distribution, you should not
modify any of the files listed below because they will be overwritten by the script.

    - **setup.py**
    - **docs/conf.py**
    - **docs/pkgdocs.rst**
    - **docs/srcdocs.rst**


If for some reason you must modify any of the files above, you must build your
distribution using the standard Python packaging procedure, for example:

::

    python setup.py sdist


That will create a source distribution of your plugin, but keep in mind that
in this case you will have to specify entry point metadata in the ``setup.py``
file manually for each of your plugins. In order to specify entry points
manually, you must add an ``entry_points`` keyword argument to the ``setup``
call inside of the ``setup.py`` file.

Entry points are divided into groups, and each
type of OpenMDAO plugin has a particular group. For example, Component
plugins are found in the ``openmdao.component`` group. Each entry
point is specified by its name, followed by an equals (**=**) sign, followed by
dotted module path (dotted path you would use to import the module in
Python), followed by a colon (**:**) and the name of the plugin class. The value
of ``entry_points`` should be a string in INI file format or a dictionary. 


For example:

::

    """
    [openmdao.component]
    simpleadder.SimpleAdder = simpleadder:SimpleAdder
    
    [openmdao.driver]
    mydriver.MyDriver = mydriver:MyDriver
    """

or
 
:: 
   
      
    { 'openmdao.component': ['simpleadder.SimpleAdder = simpleadder:SimpleAdder'],
      'openmdao.driver': ['mydriver.MyDriver = mydriver:MyDriver']
    }



.. index:: creation

Distribution Creation
---------------------

Eventually our hacking will be finished and our plugin will be ready to
package up as a distribution. Packaging our plugin as a 
distribution makes it easier to share it with others in the OpenMDAO
community. To create our distribution, we issue the command:

::

    plugin_package <dist_dir>


where ``dist_dir`` is the name of the directory containing our distribution.
The script will automatically detect plugins within the distribution ``src``
directory and generate any necessary entry points for them in the ``setup.py``
file.  It will also generate the sphinx documentation and place the sphinx
generated files and all other necessary files in a source distribution that
will be named as follows:

::

    <dist_name>-<version>.tar.gz
    
    
In our particular case, the file would be named ``simpleadder-0.9.tar.gz``.

Once we've created our source distribution, it can be installed into an active
OpenMDAO environment by running:

::

    plugin_install simpleadder-0.9.tar.gz
    
    
We could also put the source distribution on a file server so that anyone with
access to the server would be able to download and install it automatically.
For example, if we were to put the file on the *openmdao.org* server, anyone
could install it by typing:

::

    plugin_install -f http://openmdao.org/dists simpleadder



.. _Building-a-Variable-Plugin:

Building a Variable Plugin
==========================

Sometimes it's necessary to create a new type of variable that can be passed 
between OpenMDAO components.  This section describes how to do this using a 
pure Python OpenMDAO plugin.

Let's assume we want to have a variable that represents a set of Cartesian 
coordinates, with the value of the variable being a 3-tuple of floating point
values representing the *x, y,* and *z* position.

As before when we created a component plugin, we'll use ``plugin_quickstart`` to
generate the directory structure for our distribution, but this time we use
the **-g** option to specify the plugin group as ``openmdao.variable``.  
Also, this time around we'll specify the name *coord* for our distribution 
using the **--dist** option.

::


    plugin_quickstart Coordinates -d myplugins -g openmdao.variable --dist=coord 


Since we said our distribution name is going to be *coord*, that means that
``plugin_quickstart`` created a skeleton of our plugin class definition in 
the ``src/coord/coord.py`` file.  After editing that file, it looks like this:

::

    from openmdao.main.variable import Variable
    
    class Coordinates(Variable):
    
        def __init__(self, default_value = (0.,0.,0.), **metadata):
            super(Coordinates, self).__init__(default_value=default_value,
                                             **metadata)
    
        def validate(self, object, name, value):
            if isinstance(value, tuple) and len(value) == 3 and \
               all([isinstance(val,(float,int)) for val in value]):
                return value
            else:
                self.error(object, name, value)


OpenMDAO provides a base class for framework visible inputs and outputs called
``Variable``, so that's the base class for our coordinates variable. If a
class inherits from ``Variable``, then that class is recognized by the
framework as a plugin. If a Component object contains a ``Variable`` instance
that has a metadata attribute named *iotype* then that instance object is
exposed to the framework as a variable whose value can be passed between
components. Valid values for *iotype* are 'in' and 'out'. 

One thing that can be a little confusing to people first using Variables is that
the Variable object itself is just a validator and possibly a converter. The
object that actually gets passed around between components is the *value* that
the variable corresponds to and not the variable itself. For example, if we had a
component named *wheel* that contained one of our Coordinates variables named
``center_location``, then the value of ``wheel.center_location`` would be a
3-tuple, not a Coordinates object.

We override the base class constructor so we can supply a default value of
(``0.,0.,0.``) if the caller doesn't supply one. After that, the only function we
need to supply is the ``validate`` function, which will be called with the
following arguments:

**object**
    The object that contains the value of our coordinates variable

**name**
    The name of our coordinates variable

**value**
    The value that our current value is being replaced with


Our ``validate`` function should test that the value we've been called with is
valid. In this particular case, we just need to verify that the value is a
3-tuple and it has float or int entries. If the value is acceptable, then we
just return it. We don't need to do it in this case, but in other custom
traits, we could convert the value before returning it. If the value
is not acceptable, then we call the error function, which will raise a
TraitError exception.

That's all of the source code required to make our Coordinates variable 
functional.  As in the earlier section where we made a component plugin,
we need to specify the metadata for our distribution by editing the 
``setup.cfg`` file and add any extra documentation that we want to the
``docs/usage.rst`` file and the ``README.txt`` file.  When that's done,
as before, we run ``plugin_package`` and the end result should be a
source distribution named ``coord-0.1.tar.gz``.  The version id of our 
plugin defaulted to **0.1** because we didn't specify it when we ran
``plugin_quickstart``.


