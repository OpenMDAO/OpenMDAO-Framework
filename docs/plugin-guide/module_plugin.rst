.. index:: SimpleAdder

.. index:: pair: plugin; building a pure Python plugin


.. _build-pure-python-plugin-label:

Building a Component Plugin
===========================

For this example we'll build a plugin for the component shown in the figure
:ref:`Conceptual-View-of-a-Simple-Component` in the introduction to the framework. This component
simply computes the value of its single output by adding its two inputs.

These instructions apply to any plugin component distribution that is pure
Python, i.e., not containing any Python extensions.  They will also work with file
wrapped components as long as the distribution includes only the wrapper component
and not the application being file wrapped.


Getting Started Quickly
-----------------------

The script we use to create the initial directory structure for our plugin
distribution is called ``plugin_quickstart``.

.. program:: plugin_quickstart

::

    plugin_quickstart dist_name [-v version [-c class_name [-g plugin_group [-d dest_directory]]]]

.. option:: dist_name

   The name of the distribution we want to create.

.. option:: -v

   Version id of the distribution. Defaults to ``0.1``.  
   
.. option:: -c

   Name of the plugin class.  By default the plugin class has the same name as
   the distribution except the first letter is capitalized.

.. option:: -g

   Plugin group. Defaults to ``openmdao.component``. Other possible values are:
   ``openmdao.driver`` and ``openmdao.variable``.

.. option:: -d

   Directory where the plugin subdirectory will be created. Defaults
   to the current directory.

   
The first thing we'll do is to use ``plugin_quickstart`` to build a
directory structure in our current directory for a distribution named
*simpleadder* containing a component plugin class named *SimpleAdder* and
having a version id of *0.9*.

::

    plugin_quickstart simpleadder -c SimpleAdder -v 0.9
    

After running ``plugin_quickstart``, the current directory will contain
a directory named ``simpleadder``. The ``simpleadder`` directory
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
            `-- test
                `-- test_simpleadder.py



Customizing our Plugin
----------------------

The ``plugin_quickstart`` automatically generates skeleton files for
our plugin class, distribution metadata, and documentation, but
we'll want to tailor these specifically to our plugin by editing the
following files found in our distribution directory:

**src/<dist_name>/<dist_name>.py**
    A Python source file containing our plugin class.
    
**setup.cfg**
    A config file that specifies metadata related to the plugin. This
    is where we would specify license information, contact emails, etc.
    
**MANIFEST.in**
    If we have additional files to include in our distribution beyond
    the standard set of Python source files and setup files, we can 
    specify them here.  For more info about ``MANIFEST.in``, look `here`__.
    
**docs/usage.rst**
    A reStructuredText file containing any docs that we want to add to those
    that are generated automatically.
    
**README.txt**
    A simple text file with miscellaneous instructions about the plugin.  
    
**src/<dist_name>/test/test_<dist_name>.py**
    A Python source file containing a unit test for our plugin class. It
    actually doesn't run any tests by default, but there is a skeletal
    version of a ``unittest.TestCase`` defined here to make it as easy as 
    possible to add some unit tests for our plugin.
    

.. __: http://docs.python.org/distutils/sourcedist.html#the-manifest-in-template


The following sections describe how to edit these files in more detail.


*Editing our Plugin Class*
+++++++++++++++++++++++++++

The most important file to edit is the Python file that defines our
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
        a = Float(0.0, iotype='in', desc='an input added to *b* to make *c*')
        b = Float(0.0, iotype='in', desc='an input added to *a* to make *c*')
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
wide variety of variables, including basic types like *Int*, *Str*, and *Bool*; 
containers like *List* and *Dict*; and others. 

Developing a plugin is often an iterative process, so it's convenient to have
a way to install the plugin and hack on it, test it, etc., without having to
reinstall it each time we change it. Luckily this is easy to do by just
installing our plugin as a *develop* egg. To do this, we just run the
``plugin_install`` command from the top directory of our plugin distribution.
After that, our plugin can be imported and used in the OpenMDAO environment
just like any other installed plugin. For example, we could import our plugin
class like this:


::

    from <package_name>.<module_name> import <plugin_class>
    
    
or, in this specific case:

::

    from simpleadder.simpleadder import SimpleAdder
    


*Adding Documentation*
+++++++++++++++++++++++

Now that our plugin class is fully defined, we should write up some documentation
about how to use it.  The packaging script that we'll run later, ``plugin_makedist``, 
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

Sometimes you want to add a static document like a PDF file to a plugin package.
For example, if you are file wrapping a code and want to include the documentation
for the code, you need to put the file into one of the directories listed in the
``static_path`` variable inside the ``setup.cfg`` file. 

*Setting Metadata*
++++++++++++++++++

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
        <3.0
    requires-externals = 
    project-url = 
    
    [openmdao]
    copyright =
    static_path = [ '_static' ]

    


You should set whatever of these values you feel are applicable to your plugin.
The **name** and **version** values are the only ones that are mandatory, but
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


More descriptions of the various metadata values can be found 
`here`__.

.. __: http://readthedocs.org/docs/distutils2/en/latest/setupcfg.html#metadata


The values in the *metadata* section are specified by **PEP 345** and 
apply to any Python distribution.  We've added an *openmdao* section to the
file to provide a place to put metadata that isn't mentioned in PEP 345, for
example, the copyright notice for the documentation.


*Additional Customization*
++++++++++++++++++++++++++

In some cases, you may want to add multiple plugin classes to your distribution,
either in the ``<dist_name>.py`` file or in separate Python source files that you
add to the ``src`` directory, possibly as part of a nested package directory
structure.  The ``plugin_makedist`` script knows how to handle this sort of a
situation and will generate the appropriate source documentation and metadata
for whatever plugins you define under the ``src`` tree.

If you plan to use ``plugin_makedist`` to create your distribution, you should not
modify any of the files listed below because they will be overwritten by the script.

    - **setup.py**
    - **docs/conf.py**
    - **docs/pkgdocs.rst**
    - **docs/srcdocs.rst**


If for some reason you must modify any of the files above, then you must build your
distribution using the standard Python packaging procedure, for example:

::

    python setup.py sdist


That will create a source distribution of your plugin, but keep in mind that
in this case you will have to specify entry point metadata in the ``setup.py``
file manually for each of your plugins. To specify entry points
manually, you must add an ``entry_points`` keyword argument to the ``setup``
call inside of the ``setup.py`` file.

Entry points are divided into groups, and each 
type of OpenMDAO plugin has a particular group. For example, Component
plugins are found in the ``openmdao.component`` group. Each entry
point is specified by its name followed by an equals (**=**) sign; followed by
dotted module path (dotted path you would use to import the module in
Python); followed by a colon (**:**) and the name of the plugin class. The value
of ``entry_points`` should be a string in INI file format or a dictionary. 


For example:

::

    """
    [openmdao.component]
    simpleadder.simpleadder.SimpleAdder = simpleadder.simpleadder:SimpleAdder
    
    [openmdao.driver]
    mydriver.mydriver.MyDriver = mydriver.mydriver:MyDriver
    """

or
 
:: 
   
      
    { 'openmdao.component': ['simpleadder.simpleadder.SimpleAdder = simpleadder.simpleadder:SimpleAdder'],
      'openmdao.driver': ['mydriver.mydriver.MyDriver = mydriver.mydriver:MyDriver']
    }


Building Just the Plugin Docs
-----------------------------

Sometimes when we're iterating on the plugin documentation it's 
convenient to regenerate just the docs instead of creating a new
distribution every time.  We can do this using the ``plugin_build_docs``
command.

.. program:: plugin_build_docs

::

    plugin_build_docs [dist_directory]

    
.. option:: dist_directory

   Top level directory of the distribution.  Defaults to the current
   directory.
   

We can view the docs for a plugin using the ``plugin_docs`` command.  Note 
that this only works for installed plugin distributions.

.. program:: plugin_docs

::

   plugin_docs plugin_dist_name
   
   
.. option:: plugin_dist_name

   The name of the plugin distribution.
   

If we install our plugin as a *develop* egg by running ``plugin_install`` from
the top level directory of our plugin distribution, we can then edit and view
our docs efficiently by repeating the following sequence:

::

    ... hack, hack, hack
    plugin_build_docs
    plugin_docs <plugin_dist_name>
    
    
.. index:: creation

Creating Our Plugin Distribution
--------------------------------

Eventually our plugin will be ready to package as a distribution. Doing this
makes it easier to share our plugin with others in the OpenMDAO community. To
create our distribution, we issue the command:

::

    plugin_makedist <dist_dir>


where ``dist_dir`` is the name of the directory containing our distribution.
It defaults to the current directory. The script will automatically detect
plugins within the distribution ``src`` directory and generate any necessary
entry points for them in the ``setup.py`` file. It will also generate the
Sphinx documentation and place the Sphinx-generated files and all other
necessary files in a source distribution that will be named as follows:



