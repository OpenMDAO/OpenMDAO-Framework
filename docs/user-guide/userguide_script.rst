.. index:: user guide script interface

.. _The-OpenMDAO-Scripting-Interface:

The OpenMDAO Scripting Interface
================================

OpenMDAO provides a programmatic interface that allows a user to write a python
script that describes the struture of the model and provides the ability to
interact with objects in the framework. Other interfaces are planned, including
both graphical and a command line. Everything that can be done in OpenMDAO, can
be done using this scripting interface.

This goal of this section of the user guide is to explain and demonstrate every
aspect of the OpenMDAO script interface. This is intended primarily as a
reference. Inexperienced users would be best served by reading and understanding
the examples in :ref:`Getting-Started-with-OpenMDAO` and :ref:`The-OpenMDAO-tutorial-problem`.

OpenMDAO Fundamentals
---------------------

*The OpenMDAO Namespace*
~~~~~~~~~~~~~~~~~~~~~~~~

The namespace is a Python concept that provides a structure for organizing
variables and functions in a logical hierarchical fashion. Namespaces allow the
user to import needed functions and class definitions into the Python environment
while allowing those with the same name to co-exist. The branch levels in a
namespace are seperated by a period (".").

The OpenMDAO namespace includes several packages, all of which are prefixed by "openmdao.":

- **openmdao.main** -- Core infrastructure for the framework
- **openmdao.lib** -- OpenMDAO's standard library, containing some important plug-ins (drivers, traits, etc.) that are available to users of the framework
- **openmdao.examples** -- Tutorials and example problems for learning OpenMDAO
- **openmdao.util** -- Utilities used by OpenMDAO, but are not dependent on it
- **openmdao.test** -- Functions and classes used strictly for unit testing

Users are likely to need only the first three of these (main, lib, and examples.)
Importing classes and functions from OpenMDAO's libraries is performed with the
same syntax as loading any other Python module:

.. testcode:: namespace

    from openmdao.main.api import Component, Assembly
    from openmdao.lib.drivers.conmindriver import CONMINdriver
    
Here, the fundamental OpenMDAO component classes Component and Assembly are
loaded from openmdao.main, along with the CONMIN driver from openmdao.lib.

To simplify the imports, a selection of the most commonly used imports was
placed in openmdao.main.api. A complete listing of what is available in
this module can be obtained using the dir() command in Python:

    >>> import openmdao.main.api
    >>> items = dir(openmdao.main.api)
    >>> for item in items:
    ...     print(item)
    Assembly
    Case
    Component
    ConstraintError
    Container
    Dataflow
    Driver
    ExprEvaluator
    Factory
    FileCaseIterator
    FileMetadata
    FileRef
    FileTrait
    ListCaseIterator
    PkgResourcesFactory
    SAVE_CPICKLE
    SAVE_LIBYAML
    SAVE_PICKLE
    SAVE_YAML
    SimulationRoot
    StringRef
    StringRefArray
    Workflow
    __builtins__
    __doc__
    __file__
    __name__
    __package__
    create
    get_available_types
    logger
    plugin_path
    set_as_top

Most of these items are explained elsewhere in the user guide. These can all be
imported from openmdao.main.api.

Note that there is some overhead associated with importing things into the Python
environment. Thus, it is important to only import what will be used in the
module. Never import an entire library when only a subset is needed.

.. testcode:: namespace

    # BAD
    import openmdao.main.api
    
    # BAD
    from openmdao.main.api import *
    
    # GOOD
    from openmdao.main.api import Component, Assembly, StringRef, Driver

Unused imports are one of the problems that pylint can find, so it always pays
to use it.

*The Data Tree*
~~~~~~~~~~~~~~~

Creating New Components
-----------------------

The component is a basic building block of the OpenMDAO model, so the user needs
to be familiar with how to create and execute them. The concept of the component
and the place it holds in the OpenMDAO architecture is given in :ref:`Overview-of-the-OpenMDAO-Framework`.

Presumably the user has his own components to implement in OpenMDAO as part of 
a larger model or process. This implementation will usually require the creation
of an OpenMDAO Python component based on the Component class and conforming to the
Component API.

*The Component API*
~~~~~~~~~~~~~~~~~~~

Every component in the OpenMDAO framework is an object that conforms to a
specific interface. At present, the easiest way to match this interface
is to inherit from the built-in Component class, and then override the
execute() function to give the component some kind of run behavior. Likewise,
the __init__() function can also be overriden to prescribe the component's
behavior when it is instantiated. This is mostly useful for defining any 
internal private variables that need to be saved between runs, but aren't
needed by other components in the framework.

One important note: at present, a component has to be derived from Component
in order to run in openMDAO. However, there has been some discussion recently
about changing the implementation to remove this requirement. In such a case,
a component would merely need to conform to the specified interface. There
are quite a few other functions in the Component API that haven't been mentioned
here, but some effort to tighten this interface would also be needed as part
of this.

A simple component that implements an equation with two inputs is shown below:

.. testcode:: simple_component_Equation

    from enthought.traits.api import Float
    from openmdao.main.api import Component
    
    class Equation(Component):
        """ Evaluates the equation (x-3)^2 + xy + (y+4)^2 = 3 """
    
	# Component Input 
	x = Float(0.0, iostatus='in', desc='The variable y')
        y = Float(0.0, iostatus='in', desc='The variable x')

	# Component Output
        f_xy = Float(0.0, iostatus='out', desc='F(x,y)')        

	# Initialization function (technically not needed here)
	def __init__(self, doc=None, directory=''):
	    super(Equation, self).__init__(doc, directory)        
	
	# Executes when component is run
	def execute(self):
	    """ Solve (x-3)^2 + xy + (y+4)^2 = 3
	        Optimal solution (minimum): x = 6.6667; y = -7.3333
	        """
        
	    x = self.x
	    y = self.y
        
	    self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

In this example, the __init__() function doesn't do anything but call the
equivalent in the base class, so technically it should be removed from this 
class definition. More detail on framework variables is explained in 
:ref:`Public-Variables`.

One additional function that may need to be defined in certain cases is save_to_egg().
Sometimes a wrapped code might require some additional files or directories
to be packed with it. These kinds of things can be taken care of in save_to_egg().
It is important not to forget to call the save_to_egg() for the base
class.

TODO: save_to_egg example

*Special Plug-ins*
~~~~~~~~~~~~~~~~~~

The OpenMDAO Standard Library will ultimately include a number of specialized components
that enable it to interface with commonly used applications. These will
definitely include Excel, Matlab, and Octave, though others are also possible.

The Excel Wrapper
+++++++++++++++++

There are requirements for OpenMDAO to interface with Excel, including the
capability to write output that is readable by Excel, as well as the capability
to execute an Excel component. The implementation is planned in the near future.

The Matlab Plug-in
++++++++++++++++++

A Matlab plug-in is required for OpenMDAO, and will be implemented in the near
future. There is an active project called pymatlab (http://pypi.python.org/pypi/pymatlab/0.1.0)
which is developing a python package to interface with the latest version of 
Matlab, so hopefully this will be able to be used.

The Octave Plug-in
++++++++++++++++++

GNU's Octave is an open source alternative to Matlab that is capable of running 
some (possibly most) programs written in Matlab's m-script. In the interest of
supporting other open-source environments for numerical computation, an Octave
plug-in is desired, though there has at present been no work performed in
integrating one into OpenMDAO. Something like Pytave (https://launchpad.net/pytave) 
may be a possibile candidate.
  
.. _Public-Variables:

Public Variables
----------------


*Traits*
~~~~~~~~

The underlying implementation of Public Variables in OpenMDAO was accomplished
through a Python add-on called :term:`Traits`, which is an open-source extension 
to Python that was developed by a company called Enthought. Traits provide a way to 
apply explicit typing to the normally untyped Python variables. They also provide 
the capability to add some other features to the framework variables, including 
unit checking and conversion, default values, minima and maxima, and a way to create 
callback functions that execute under specified conditions.

Most users won't need to worry about Traits or how Public Variables are implemented,
but those users who want to create custom datatypes will need to essentially 
create a new custom trait. More details on traits can be found on Enthought's 
Trait Project page (http://code.enthought.com/projects/traits/).

*Built-in Variable Types*
~~~~~~~~~~~~~~~~~~~~~~~~~

StringRef
+++++++++

A StringRef is a special type of string variable that contains an expression to
be evaluated. The expression can reference variables and functions within the
scope of its containing component, as well as within the scope of the component's
parent Assembly.  A number of builtin functions and math functions may also be
referenced within a StringRef expression.  For example, ``abs(math.sin(angle))``
would be a valid StringRef expression, assuming that *angle* is an attribute of the
containing component. Note that *self* does not appear in the example expression.
This is because the StringRef automatically determines the containing scope of
attributes and functions referenced in an expression. This helps keep expressions
from becoming too verbose by containing a bunch of ``self`` and ``self.parent``
references.

StringRefs can be used in a variety of components. Many optimizer components use 
StringRefs to specify their objective function, design variables, and constraints.
Conditional branching components use StringRefs to specify boolean expressions that
determine if a given branch should be executed.

Unitsfloat and Unit Conversions
+++++++++++++++++++++++++++++++

OpenMDAO also supports variables with explicitly defined units using the UnitsFloat
variable type. UnitsFloat is a custom Trait that is included as part of the Standard
Library. This variable type provides some specific useful effects when utilized 
in the framework:

- Automatically converts a value passed from an output to an input with compatible units (e.g., 'in' and 'm')
- Raises an exception when attempting to pass a value from an output to an input having incompatible units (e.g., 'kg' and 'm')
- Permits connection between UnitsFloat and Float variables without additional restrictions

A complete list of the available units is given in :ref:`Summary-of-Units`. The
unit conversion code and the base set of units come from the Physical Quantities
package found in Scientific Python (http://dirac.cnrs-orleans.fr/plone/software/scientificpython).
It was necessary to add a few units to the existing one in Physical Quantities
(in particular, a currency unit), so a new Units package was derived and is
included as part of the Standard Library. This package has the same basic
function as that of Physical Quantities, but to make it more extensible, the
unit definitions were moved from the internal dictionary into an externally
readable text file called unitLibdefault.ini. More information on customization
(i.e., adding new units) of the Units package can be found in the OpenMDAO 
Standard Library Guide.


*Creating Custom Variable Types*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Building a Simulation Model
---------------------------

*Connecting Components*
~~~~~~~~~~~~~~~~~~~~~~~

*Assemblies*
~~~~~~~~~~~~

*Sockets & Interfaces*
~~~~~~~~~~~~~~~~~~~~~~

*The Top Level Assembly*
~~~~~~~~~~~~~~~~~~~~~~~~

Drivers
-------

*The Driver Interface*
~~~~~~~~~~~~~~~~~~~~~~

*Case Iterator*
~~~~~~~~~~~~~~~

*Solution Drivers*
~~~~~~~~~~~~~~~~~~

CONMIN
++++++

NEWSUMT
+++++++

PyEvolve
++++++++

Newton Solver
+++++++++++++

*Adding new Optimizers*
~~~~~~~~~~~~~~~~~~~~~~~

Running OpenMDAO
-----------------

*Executing Models*
~~~~~~~~~~~~~~~~~~

*Error Logging & Debugging*
~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Saving & Loading*
~~~~~~~~~~~~~~~~~~

*Sharing Models*
~~~~~~~~~~~~~~~~

Data Flow and WorkFlow
----------------------

*Data Flow & Lazy Evaluation*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Building a WorkFlow*
~~~~~~~~~~~~~~~~~~~~~

Looping
+++++++

Branching
+++++++++

Design Tools
------------

*Design of Experiments*
~~~~~~~~~~~~~~~~~~~~~~~

*Multi-objective Optimization and Pareto Frontiers*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Sensitivity Analysis*
~~~~~~~~~~~~~~~~~~~~~~

Managing Simulation Data
------------------------

Multi-Threaded Computation
--------------------------

Publishing a Component
----------------------

*Eggs*
~~~~~~

*Adding a New Component to your Local Library*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Geometry in OpenMDAO
--------------------
 
Advanced MDAO 
-------------

*Multi-Fidelity Optimization*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Surrogate Modeling*
~~~~~~~~~~~~~~~~~~~~~

*Uncertainty*
~~~~~~~~~~~~~
 
