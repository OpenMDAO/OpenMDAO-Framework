.. index:: user guide script interface

.. _The-OpenMDAO-Scripting-Interface:

The OpenMDAO Scripting Interface
================================

OpenMDAO provides a programmatic interface that allows you to write a Python
script that describes the structure of the model and provides the ability to
interact with objects in the framework. Other interfaces are planned, including
both a graphical and a command line. Everything that can be done in OpenMDAO can
be done using this scripting interface.

The goal of this section of the *User's Guide* is to explain and demonstrate every
aspect of the OpenMDAO script interface. This section is intended primarily as a
reference. If you are an inexperienced user, you would be best served by reading and
understanding the examples in :ref:`Getting-Started-with-OpenMDAO` and
:ref:`The-OpenMDAO-tutorial-problem`.

OpenMDAO Fundamentals
---------------------

.. index:: namespace

*The OpenMDAO Namespace*
~~~~~~~~~~~~~~~~~~~~~~~~

The *namespace* is a Python concept that provides a structure for organizing
variables and functions in a logical hierarchical fashion. Namespaces allow you to
import needed functions and class definitions into the Python environment while
allowing those with the same name to co-exist. The branch levels in a
namespace are separated by a period (".").

The OpenMDAO namespace includes several packages, all of which are by 
"openmdao.":

- ``openmdao.main`` -- Core infrastructure for the framework
- ``openmdao.lib`` -- OpenMDAO's standard library, containing some important plugins (drivers, traits, etc.) that are available to users of the framework
- ``openmdao.examples`` -- Tutorials and example problems for learning OpenMDAO
- ``openmdao.util`` -- Utilities used by OpenMDAO, but are not dependent on it
- ``openmdao.test`` -- Functions and classes used strictly for unit testing

You are likely to need only the first three of these (*main, lib,* and *examples*).
Importing classes and functions from OpenMDAO's libraries is performed with the
same syntax as loading any other Python module:

.. testcode:: namespace

    from openmdao.main.api import Component, Assembly
    from openmdao.lib.api import CONMINdriver
    
Here, the fundamental OpenMDAO component classes *Component* and *Assembly* are
loaded from ``openmdao.main``, along with the CONMIN driver from ``openmdao.lib``.

To simplify the imports, a selection of the most commonly used imports was
placed in the pseudo-package ``openmdao.main.api``. You can obtain a complete listing of what is
available in this module by using the *dir()* command in Python:

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
    set_as_top

Most of these items are explained elsewhere in the *User's Guide.* These can all be
imported from ``openmdao.main.api``.

Note that there is some overhead associated with importing things into the Python
environment. Thus, it is important to import only what will be used in the
module. Never import an entire library when only a subset is needed.

.. testcode:: namespace

    # BAD
    import openmdao.main.api
    
    # BAD
    from openmdao.main.api import *
    
    # GOOD
    from openmdao.main.api import Component, Assembly, StringRef, Driver

Unused imports are one of the problems that Pylint can find, so it always pays
to use it.

A pseudo-package was also created to house some of the most commonly-used imports
from the standard library. In general, it contains Public Variables and Drivers.
Most of these items are also explained elsewhere in the *User's Guide.*

    >>> import openmdao.lib.api
    >>> items = dir(openmdao.lib.api)
    >>> for item in items:
    ...     print(item)
    Array
    Bool
    CBool
    CONMINdriver
    CaseIteratorDriver
    Complex
    Enum
    Float
    Instance
    Int
    List
    Str
    __builtins__
    __doc__
    __file__
    __name__
    __package__
    pyevolvedriver

*The Model Hierarchy*
~~~~~~~~~~~~~~~~~~~~~

.. index:: Component

Creating New Components
-----------------------

The component is a basic building block of the OpenMDAO model, so you need 
to be familiar with how to create and execute them. The concept of the component
and the place it holds in the OpenMDAO architecture is given in
:ref:`Overview-of-the-OpenMDAO-Framework`.

Presumably you have your own components to implement in OpenMDAO as part of 
a larger model or process. This implementation will usually require the creation
of an OpenMDAO Python component based on the Component class and conforming to the
Component API.

*The Component API*
~~~~~~~~~~~~~~~~~~~

Every component in the OpenMDAO framework is an object that conforms to a
specific interface. At present, the easiest way to match this interface
is to inherit from the built-in Component class, and then override the
*execute()* function to give the component some kind of run behavior. Likewise,
the *__init__()* function can also be overridden to prescribe the component's
behavior when it is instantiated. This is mostly useful for defining any 
internal private variables that need to be saved between runs, but aren't
needed by other components in the framework.

One important note: at present, a component has to be derived from Component
to run in openMDAO. However, there has been some discussion recently
about changing the implementation to remove this requirement. In such a case,
a component would merely need to conform to the specified interface. There
are quite a few other functions in the Component API that haven't been mentioned
here, but some effort to tighten this interface would also be needed as part
of this.

A simple component that implements an equation with two inputs is shown below:

.. testcode:: simple_component_Equation

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
    class Equation(Component):
        """ Evaluates the equation (x-3)^2 + xy + (y+4)^2 = 3 """
    
	# Component Input 
	x = Float(0.0, iotype='in', desc='The variable y')
        y = Float(0.0, iotype='in', desc='The variable x')

	# Component Output
        f_xy = Float(0.0, iotype='out', desc='F(x,y)')        

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

In this example, the *__init__()* function doesn't do anything but call the
equivalent in the base class, so technically it should be removed from this 
class definition. More detail on framework variables is explained in 
:ref:`Public-Variables`.

.. index:: save_to_egg()

One additional function that may need to be defined in certain cases is
*save_to_egg().* Sometimes a wrapped code might require some additional files or
directories to be packed with it. These kinds of things can be taken care of in
*save_to_egg().* It is important not to forget to call the *save_to_egg()* for the base
class.

TODO: save_to_egg example

*Special Plugins*
~~~~~~~~~~~~~~~~~~

The OpenMDAO Standard Library will ultimately include a number of specialized
components that enable it to interface with commonly used applications. These will
definitely include Excel, Matlab, and Octave, although others are also possible.

.. index:: Excel wrapper

The Excel Wrapper
+++++++++++++++++

OpenMDAO has requirements to interface with Excel, including the requirement to provide the
capability to write output that is readable by Excel and the capability
to execute an Excel component. The implementation is planned in the near future.

.. index:: pair: Matlab; plugin

The Matlab Plugin
++++++++++++++++++

A Matlab plugin is required for OpenMDAO and will be implemented in the near
future. There is an active project called `pymatlab <http://pypi.python.org/pypi/pymatlab/0.1.0>`_
which is developing a Python package to interface with the latest version of 
Matlab, so it is hoped that this can be used.

.. index:: pair: Octave; plugin

The Octave Plugin
++++++++++++++++++

GNU's Octave is an open source alternative to Matlab that is capable of running 
some (possibly most) programs written in Matlab's m-script. In the interest of
supporting other open-source environments for numerical computation, an Octave
plugin is desired, although at present no work has been done to integrate one into
OpenMDAO. Something like `Pytave <https://launchpad.net/pytave>`_ may be a possible
candidate.
  
.. _Public-Variables:

Public Variables
----------------

In OpenMDAO, a Public Variable is a variable that can be seen or manipulated by
other entities in the framework. Any data that is passed between components in a
model must use Public Variables to declare the inputs and output for each
component.

There are two ways to create a public variable for a component. The first is to
declare it in the component's class definition of the as shown in the example 
given in :ref:`Getting-Started-with-OpenMDAO`. A simple component that takes
a floating point number as an input and provides a floating point number as an
output would look like this:

.. testcode:: creating_public_variables_1

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
    class Simple(Component):
        """ A simple multiplication """
    
	# set up interface to the framework  
	x = Float(1.0, iotype='in', desc='The input x')
        y = Float(0.0, iotype='out', desc='The output y')        

	def execute(self):
	    """ y = 3*x """
	    
	    self.y = 3.0*self.x

The example above shows the way the majority of users will create Public Variables.
An alternative way to declare them is to use the *add_trait* function that is part of the
*Component* public interface.
	    
.. testcode:: creating_public_variables_2

    from openmdao.main.api import Component
    from openmdao.lib.api import Int
    
    class Simple(Component):
        """ A simple multiplication """
    
	def __init__(self, doc=None, directory=''):
	
	    self.add_trait('x',Float(1.0, iotype='in', desc='The input x'))
	    self.add_trait('y',Float(0.0, iotype='out', desc='The output y'))
	    
	    super(Simple, self).__init__(doc, directory)
	    
	def execute(self):
	    """ y = 3*x """
	    
	    self.y = 3.0*self.x
	    
Note that *add_trait* is called in the constructor (i.e, the __init__ function),
so a local copy was created that overloads the one in the parent *Component* 
class. In most of the examples shown so far, we did not need to declare a
constructor because the one in *Component* was adequate. 

There isn't a real advantage to creating a Public Variable in this manner. However,
the primary use of add_trait is to create a Public Variable dynamically at some
point after the component has been created (possibly during execution).

    >>> from openmdao.examples.simple.paraboloid import Paraboloid
    >>> from openmdao.lib.api import Int
    >>> test=Paraboloid()
    >>> test.z
    Traceback (most recent call last):
    ...
    AttributeError: 'Paraboloid' object has no attribute 'z
    >>> test.add_trait('z',Int(7777, iotype='out', desc='An Int'))
    >>> test.z
    7777

There are some more specialized components that will make use of the ability to create
Public Variables on the fly, but it won't be used for most general components.

.. index:: Traits

*Traits*
~~~~~~~~

The underlying implementation of Public Variables in OpenMDAO was accomplished
through a Python add-on called :term:`Traits`, which is an open-source extension 
to Python that was developed by a company called Enthought. Traits provide a way to 
apply explicit typing to the normally untyped Python variables. They also provide 
the capability to add some other features to the framework variables, including 
unit checking and conversion, default values, minima and maxima, and a way to create 
callback functions that execute under specified conditions.

Most of you won't need to worry about Traits or how Public Variables are implemented,
but those of you who want to create custom datatypes will essentially need to
create a new custom trait. More details on traits can be found on `Enthought's 
Traits <http://code.enthought.com/projects/traits/>`_ project page.

*Built-in Variable Types*
~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: Public Variable Types
    
**Summary of Public Variable Types**

+------------------+----------------------------------------------------------+
| Name             | Callable Signature                                       |
+==================+==========================================================+
| Array            | Array( [*dtype* = None, *shape* = None, *value* = None,  |
|                  | *typecode* = None, *iotype* = None, *desc* = None] )     |
+------------------+----------------------------------------------------------+
| Bool             | Bool( [*value* = None, *desc* = None, *iotype* = None] ) | 
+------------------+----------------------------------------------------------+
| Float            | Float( [*default_value* = None, *iotype* = None,         | 
|                  | *desc* = None, *low* = None, *high* = None,              |
|                  | *exclude_low* = False, *exclude_high* = False,           |
|                  | *units* = None] )                                        |
+------------------+----------------------------------------------------------+
| Instance         | Instance( [*klass* = None, *desc* = None, *iotype* =     |
|                  | None, *factory* = None, *args* = None, *kw* = None,      |
|                  | *allow_none* = True, *adapt* = None, *module* = None,    |
|                  | *required* = False] )                                    | 
+------------------+----------------------------------------------------------+
| Int              | Int( [*default_value* = None, *iotype* = None,           |
|                  | *desc* = None, *low* = None, *high* = None,              |
|                  | *exclude_low* = False, *exclude_high* = False] )         |
+------------------+----------------------------------------------------------+
| Str              | Str( [*value* = None, *desc* = None, *iotype* = None] )  |
+------------------+----------------------------------------------------------+
| StringRef        | StringRef( [*desc* = None, *iotype* = None,              |
|                  | *default_value* = NoDefaultSpecified] )                  |
+------------------+----------------------------------------------------------+
| StringRefArray   | StringRefArray( [*desc* = None, *iotype* = None,         |
|                  | *default_value* = NoDefaultSpecified] )                  |
+------------------+----------------------------------------------------------+

Note: a more detailed list of Enthought's Traits is given in their `documentation`__.
These are also available for use as Public Variables in the framework, though
no examples are presented here for some of the more esoteric ones. If you need
to use one, remember that *iotype* and *desc* should be added to the arguements
when one of these is instantiated. The Traits use \*\*metadata to store these
user-definied attributes.

.. __: http://code.enthought.com/projects/traits/docs/html/traits_user_manual/defining.html?highlight=cbool#other-predefined-traits

A Public Variable is declared with a number of arguments, many of which are
optional.

The *iotype* attribute is required for all Public Variables regardless of type.
It's sole function is to tell the framework whether the variable should be
treated as an input or an output. Presently, the only two options for this
attribute are 'in' and 'out'.

**Summary of iotypes**

============  =====================
**iotype**    **Description**
------------  ---------------------
iotype='in'   Component input
------------  ---------------------
iotype='out'  Component output
============  =====================

The *desc* attribute is a concise description of the Public Variable -- one or
two sentences should be fine. While nothing in the framework requires this
description, it would be wise to include one for every input and output of your
components. The GUI will use these descriptions to provide information that will
aid simulation builders in connecting components.

.. index:: Array

Array
+++++

It is possible to use an array as a Public Variable through use of the *Array*
trait. The value for an Array can be expressed as either a Python array or a NumPy
array. NumPy arrays are particularly useful because of the built-in mathematical
capabilities. Either array can be n-dimensional and of potentially any type.

Constructing an Array variable requires a couple of additional parameters that
are illustrated in the following example:

    >>> from openmdao.lib.api import Array
    >>> from numpy import array
    >>> from numpy import float as numpy_float
    >>> z = Array(dtype=numpy_float, shape=(2,2), value=array([[1.0,2.0],[3.0,5.0]]), iotype='in')
    >>> z.default_value
    array([[ 1.,  2.],
           [ 3.,  5.]])
    >>> z.default_value[0][1]
    2.0

Here, we import the *Array* Public Variable, and the NumPy *array*, which is a
general-purpose n-dimensional array class. A 2-dimensional array is assigned as
the default value for the Public Variable named *z*. 

The *dtype* parameter defines the type of variable that is in the array. For
example, using a string (*str*) for a dtype would give an array of strings.
Note that the alternate *typecode* is also supported for non-Numpy arrays 
(e.g., typecode='I' for unsigned integers.)

The *shape* parameter is not a required attribute; the Array will default to
the dimensions of the array that is given as the value. However, it is often
useful to specify the size explicitly, so that an exception is generated if an
array of a different size or shape is passed into it. If the size if an array is not
determined until runtime (e.g., a driver that takes an array of constraint
equations as an input), then the *shape* should be left blank.

Below is an example of a simple component that takes two Arrays as inputs,
and calculates their dot product as an output.

.. testcode:: array_example

    from numpy import array, sum, float   
    
    from openmdao.main.api import Component
    from openmdao.lib.api import Array, Float
    
    class Dot(Component):
        """ A component that outputs a dot product of two arrays"""
    
	# set up interface to the framework  
	x1 = Array(dtype=float, desc = "Input 1", \
	           value=array([1.0,2.0]), iotype='in')
	x2 = Array(dtype=float, desc = "Input 2", \
	           value=array([7.0,8.0]), iotype='in')
		   
	y = Float(0.0, iotype='out', desc = "Dot Product")

	def execute(self):
	    """ calculate dot product """
	    
	    # Note: array multiplication is element by element
	    self.y = sum(self.x1*self.x2)
	    
	    # print the first element of x1
	    print x1[0]

Multiplication of a NumPy array is element by element, so *sum* is used to
complete the calculation of the dot product. Individual elements of the aray
can also be accesssed using brackets.

.. index:: Instance Traits

Instance Traits
+++++++++++++++

.. index:: StringRef

An Instance is a special type of Public Variable that allows an object to be
passed between components. Essentially, any object can be passed through the
use of an Instance. The first argument in the constructor is always the type of
object that is required. Attempting to assign an object that does not match
this type will generate an exception.


.. testcode:: instance_example

    from openmdao.main.api import Component
    from openmdao.lib.api import Instance
    
    class Fred(Component):
        """ A component that takes a class as an input """
	
	recorder = Instance(object, desc='Something to append() to.', \
	                    iotype='in', required=True)
        model = Instance(Component, desc='Model to be executed.', \
	                    iotype='in', required=True)

StringRef
+++++++++

A *StringRef* is a special type of string variable that contains an expression to
be evaluated. The expression can reference variables and functions within the
scope of its containing component, as well as within the scope of the component's
parent Assembly.  A number of built-in functions and math functions may also be
referenced within a StringRef expression.  For example, ``abs(math.sin(angle))``
would be a valid StringRef expression, assuming that *angle* is an attribute of the
containing component. Note that *self* does not appear in the example expression.
This is because the StringRef automatically determines the containing scope of
attributes and functions referenced in an expression. This helps keep expressions
from becoming too verbose by containing a bunch of *self* and *self.parent*
references.

StringRefs can be used in a variety of components. Many optimizer components use 
StringRefs to specify their objective function, design variables, and constraints.
Conditional branching components use StringRefs to specify boolean expressions that
determine if a given branch should be executed.

Here is an example of declaring a StringRef as an input, as it would be used to
create a variable to hold the objective function of an optimizer, which is
inherently a function of variables in the framework.

.. testcode:: StringRef_example

    from openmdao.main.api import Driver, StringRef
    
    class MyDriver(Driver):
        """ A component that outputs a dot product of two arrays"""
	
        objective = StringRef(iotype='in', \
                    desc= 'A string containing the objective function \
                    expression.')
			    
Note that it makes little sense to give a default value to a StringRef, since
its value will usually depend on the component names. Stringrefs are most
likely to be assigned their value in the higher-level container: typically the
top level assembly. Also, note that StringRef is imported from
``openmdao.main.api`` instead of ``openmdao.lib.api``. This is because a
StringRef is a special class of Public Variables that is an integral part of
the framework infrastructure.

There is also a *StringRefArray* variable which can be used to hold multiple
string expressions. For example, an optimizer might take as input a list
containing some number of constraints that are built from these string
expressions.

.. testcode:: StringRefArray_example

    from openmdao.main.api import Driver, StringRefArray
    
    class MyDriver(Driver):
        """ A component that outputs a dot product of two arrays"""
	
	constraints = StringRefArray(iotype='in',
		desc= 'An array of expression strings indicating constraints.'+
		' A value of < 0 for the expression indicates that the constraint '+
		'is violated.')

Again, no default is needed.		
		
.. index:: Float; unit conversion with
.. index:: unit conversion; with Float

Unit Conversions with Float
+++++++++++++++++++++++++++

OpenMDAO also supports variables with explicitly defined units using the Float
variable type, which is included as part of the Standard Library. This variable 
type provides some specific useful effects when utilized in the framework:

- Automatically converts a value passed from an output to an input with compatible units (e.g., 'in' and 'm')
- Raises an exception when attempting to pass a value from an output to an input having incompatible units (e.g., 'kg' and 'm')
- Allows values to be passed between unitless variable and variables with units

A complete list of the available units is given in :ref:`Summary-of-Units`. The unit
conversion code and the base set of units come from the Physical Quantities package found
in `Scientific Python <http://dirac.cnrs-orleans.fr/plone/software/scientificpython>`_. It
was necessary to add a few units to the existing ones in Physical Quantities (in particular,
a currency unit), so a new Units package was derived and is included as part of the
Standard Library. This package has the same basic function as that of Physical Quantities,
but to make it more extensible, the unit definitions were moved from the internal dictionary into an externally
readable text file called ``unitLibdefault.ini``. More information on customization
(i.e., adding new units) of the Units package can be found in the OpenMDAO 
Standard Library Guide.

This units library can also be used to convert internal variables by importing
the function *convert_units*.

    >>> from openmdao.lib.traits.float import convert_units
    >>> convert_units(33,'m','ft')
    108.267...

*Creating Custom Variable Types*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to create new types of Public Variables to use in your models. 
For an example of a user-created Public Variable, see :ref:`Building-a-Variable-Plugin`.

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
 
