.. index:: user guide script interface

.. _`OpenMDAO-scripting-interface`:

OpenMDAO Scripting Interface
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
:ref:`A-More-Complex-Tutorial-Problem`.

OpenMDAO Fundamentals
---------------------

.. index:: package

*OpenMDAO Package Hierarchy*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The *package* is a Python concept that provides a structure for organizing
variables and functions in a logical hierarchical fashion. Packages allow you to
import needed functions and class definitions into the Python environment while
allowing those with the same name to co-exist. The branch levels in a
package are separated by a period (".").

The OpenMDAO package hierarchy includes several subpackages, all of which are prefixed by 
"openmdao.":

- ``openmdao.main`` -- Core infrastructure for the framework
- ``openmdao.lib`` -- OpenMDAO's standard library, containing some important plugins (drivers, traits, etc.) that are available to users of the framework
- ``openmdao.examples`` -- Tutorials and example problems for learning OpenMDAO
- ``openmdao.util`` -- Utilities used by OpenMDAO, but are not dependent on it
- ``openmdao.test`` -- Functions and classes used strictly for unit testing

As a component developer, you are likely to need only the first three of these (*main, lib,* and *examples*).
Importing classes and functions from OpenMDAO's libraries is performed with the
same syntax as loading any other Python module:

.. testcode:: package

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
    Expression
    ExpressionList
    Factory
    FileCaseIterator
    FileMetadata
    FileRef
    ListCaseIterator
    PkgResourcesFactory
    SAVE_CPICKLE
    SAVE_LIBYAML
    SAVE_PICKLE
    SAVE_YAML
    SimulationRoot
    Workflow
    __builtins__
    __doc__
    __file__
    __name__
    __package__
    create
    enable_console
    get_available_types
    logger
    set_as_top

Most of these items are explained elsewhere in the *User's Guide.* These can all be
imported from ``openmdao.main.api``.

Note that there is some overhead associated with importing things into the Python
environment. Thus, it is important to import only what will be used in the
module. Never import an entire library when only a subset is needed.

.. testcode:: package

    # BAD
    import openmdao.main.api
    
    # BAD
    from openmdao.main.api import *
    
    # GOOD
    from openmdao.main.api import Component, Assembly, Expression, Driver

Unused imports are one of the problems that Pylint can find, so it always pays
to use it.

A pseudo-package was also created to house some of the most commonly-used imports
from the standard library. In general, it contains public variables and Drivers.
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
    File
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
    convert_units
    pyevolvedriver

*The Model Hierarchy*
~~~~~~~~~~~~~~~~~~~~~

Every item (Component, Assembly, public variable) that is publicly accessible
to the framework is part of OpenMDAO's model hierarchy.

TODO: Talk about the model hierarchy

*Naming Conventions*
~~~~~~~~~~~~~~~~~~~~

Components and public variables that are instantiated into the OpenMDAO Model 
Hierarchy must follow the same naming syntax as variables in the Python
language. Summarized, this means that they can only include alphanumeric
characters and the underscore, and that the lead character cannot be a number.
Any attempt to create a component or a public variable that does not conform
to Python's syntax should result in an exception. This restriction was required
because these entities essentially exist as Python variables. One unfortunate
side-effect is that names with spaces are not allowed. OpenMDAO checks for
compliance when a public variable or Component instance is created:

    >>> from openmdao.main.api import Assembly
    >>> from openmdao.examples.enginedesign.chassis import Chassis
    >>> top = Assembly('top')
    >>> top.add_container('chassis1',Chassis())
    <openmdao.examples.enginedesign.chassis.Chassis object at ...
    >>> top.add_container('the chassis',Chassis())
    Traceback (most recent call last):
    ...
    NameError: name 'the chassis' contains illegal characters

Additionally, we've tried to follow the `PEP 8 <http://www.python.org/dev/peps/pep-0008/>`_
standard at all levels, including component instance names and public variable 
names. For all variable names, PEP 8 proscribes the use of lower case names 
with words separated by underscores. Naturally, PEP 8 compliance is not a
requirement that will be forced on the user, but merely a good style guideline.

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
to run in OpenMDAO. However, there has been some discussion recently
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
class definition. Framework variables are explained in more detail in the section on
:ref:`public variables <Public-Variables>`.

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

In OpenMDAO, a *public variable* is a variable that can be seen or manipulated by
other entities in the framework. Any data that is passed between components in a
model must use public variables to declare the inputs and output for each
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

The example above shows the way the majority of users will create public variables.
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

There isn't a real advantage to creating a public variable in this manner. However,
the primary use of add_trait is to create a public variable dynamically at some
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
public variables on the fly, but it won't be used for most general components.

.. index:: Traits

*Traits*
~~~~~~~~

The underlying implementation of public variables in OpenMDAO was accomplished
through a Python add-on called :term:`Traits`, which is an open-source extension 
to Python that was developed by a company called Enthought. Traits provide a way to 
apply explicit typing to the normally untyped Python variables. They also provide 
the capability to add some other features to the framework variables, including 
unit checking and conversion, default values, minima and maxima, and a way to create 
callback functions that execute under specified conditions.

Most of you won't need to worry about Traits or how public variables are implemented,
but those of you who want to create custom datatypes will essentially need to
create a new custom trait. More details on traits can be found on `Enthought's 
Traits <http://code.enthought.com/projects/traits/>`_ project page.

*Built-in Variable Types*
~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: public variable yypes
    
**Summary of Public Variable Types**

+------------------+----------------------------------------------------------+
| Name             | Callable Signature                                       |
+==================+==========================================================+
| Array            | Array( [*dtype* = None, *shape* = None, *value* = None,  |
|                  | *typecode* = None, *iotype* = None, *desc* = None] )     |
+------------------+----------------------------------------------------------+
| Bool             | Bool( [*value* = None, *desc* = None, *iotype* = None] ) | 
+------------------+----------------------------------------------------------+
| Complex          | Complex( [*value* = None, *desc* = None,                 |
|                  | *iotype* = None] )                                       | 
+------------------+----------------------------------------------------------+
| Enum             | Enum( [*default_value*, *values* = (),                   |
|                  | *desc* = None, *iotype* = None, *aliases* = ()] )        |
+------------------+----------------------------------------------------------+
| File             | File( [*default_value* = None, *iotype* = None,          | 
|                  | *desc* = None, *low* = None, *high* = None, *path* =     |
|                  | None, *content_type* = None, *binary* = False,           |
|                  | *local_path* = None                                      |
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
| Range            | Deprecated. Use OpenMDAO's Int or Float.                 |
+------------------+----------------------------------------------------------+
| Str              | Str( [*value* = None, *desc* = None, *iotype* = None] )  |
+------------------+----------------------------------------------------------+
| Expression       | Expression( [*desc* = None, *iotype* = None,             |
|                  | *default_value* = NoDefaultSpecified] )                  |
+------------------+----------------------------------------------------------+
| ExpressionList   | ExpressionList( [*desc* = None, *iotype* = None,         |
|                  | *default_value* = NoDefaultSpecified] )                  |
+------------------+----------------------------------------------------------+

Note: a more detailed list of Enthought's `Traits`__ is given in their documentation.
These are also available for use as public variables in the framework, though
no examples are presented here for some of the more esoteric ones. If you need
to use one, remember that *iotype* and *desc* should be added to the arguments
when one of these is instantiated. The Traits use \*\*metadata to store these
user-defined attributes.

.. __: http://code.enthought.com/projects/traits/docs/html/traits_user_manual/defining.html?highlight=cbool#other-predefined-traits

A public variable is declared with a number of arguments, many of which are
optional.

The *iotype* attribute is required for all public variables regardless of type.
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

The *desc* attribute is a concise description of the public variable -- one or
two sentences should be fine. While nothing in the framework requires this
description, it would be wise to include one for every input and output of your
components. The GUI will use these descriptions to provide information that will
aid simulation builders in connecting components.

.. index:: Array

Arrays
++++++

It is possible to use an array as a public variable through use of the *Array*
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

Here, we import the *Array* public variable, and the NumPy *array*, which is a
general-purpose n-dimensional array class. A 2-dimensional array is assigned as
the default value for the public variable named *z*. 

The *dtype* parameter defines the type of variable that is in the array. For
example, using a string (*str*) for a dtype would give an array of strings. Any
of Python's standard types and NumPy's additional types should be valid for the
*dtype parameter. Note that the alternate *typecode* is also supported for 
non-Numpy arrays (e.g., typecode='I' for unsigned integers.)

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
complete the calculation of the dot product. Individual elements of the array
can also be accessed using brackets.

.. index:: Enum

Enums
+++++

It is possible to use an Enum (enumeration) type as a public variable in
OpenMDAO. This is useful for cases where an input has certain fixed values
that are possible. For example, consider a variable that can be one of three
colors:

.. testcode:: enum_example

    from openmdao.lib.api import Enum
    from openmdao.main.api import Component
    
    class TrafficLight(Component):
        color = Enum(0, (0, 1, 2), iotype='in', aliases=("Red", "Yellow", "Green"))

Now, if we create an instance of this component, and try setting the Enum.

.. doctest:: enum_example

    >>> test = TrafficLight()
    >>> test.color=2
    >>> test.color
    2

What if we set to an invalid value?

.. doctest:: enum_example

    >>> test.color=4
    Traceback (most recent call last):
    ...
    TraitError: : Trait 'color' must be in (0, 1, 2), but a value of 4 <type 'int'> was specified.`

We can also access the list of indices and the list of aliases directly from the trait.

.. doctest:: enum_example

    >>> color_trait = test.trait('color')
    >>> color_trait.aliases
    ('Red', 'Yellow', 'Green')
    >>> color_trait.values
    (0, 1, 2)
    >>> color_trait.aliases[test.color]
    'Green'

Note that the alias is not a required attribute. It will mostly be useful for
display in the planned GUI, while the numerical value is probably passed on to
some wrapped code. However, the Enum isn't required to be an integer. We could
simplify this by using the color strings directly. If we define a new trait in
our component above, as:

.. testcode:: enum_example2

    from openmdao.lib.api import Enum
    from openmdao.main.api import Component
    
    class TrafficLight(Component):
	color2 = Enum('Red', ('Red', 'Yellow', 'Green'), iotype='in')

Then we can interact like this:

.. doctest:: enum_example2

    >>> test = TrafficLight()
    >>> test.color2
    'Red'
    >>> test.color2=1
    Traceback (most recent call last):
    ...
    TraitError: : Trait 'color2' must be in ('Red', 'Yellow', 'Green'), but a value of 1 <type 'int'> was specified.
    >>> test.color2="Green"
    >>> test.color2
    'Green'

However, if the Enum is being used to select the input for an old code, then you will
most likely need to feed it integers, not strings, so the aliases will be useful.
    
.. index:: File Variables, File

File Variables
++++++++++++++

The File variable contains a reference to an input or output file on disk. It
is more than simply a text string that contains a path and filename; it is
actually a file object that can be passed into other functions expecting such
an object. As such, it has functions to open it for writing, reading, etc.

.. testcode:: filevar_example

    from openmdao.lib.api import File
    
    text_file = File(path='source.txt', iotype='out', content_type='txt')
    binary_file = File(path='source.bin', iotype='out', binary=True,
                            extra_stuff='Hello world!')

Note that the *path* must be a descendant of the parent component's path, as
explained in :ref:`Files-and-Directories`. The *binary* flag can be used to
mark a file as binary. 

TODO: Provide some examples to demonstrate the options.
			    
.. index:: Instance Traits

Instance Traits
+++++++++++++++

An Instance is a special type of public variable that allows an object to be
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
			    
In this example, we have two inputs that are Instances. The one called model
is of type *Component*, which means that this component actually takes another
Component as input. Similarly, the one called recorder is of type *object*. In
Python, object is the ultimate base class for any object, so this input can
actually take anything. (Note: it is still possible to create a class that doesn't
inherit from *object* as its base class, but this is not considered good form.)

The attribute *required* is used to indicate whether the object that plugs into
this input is required. If *required* is True, then an exception will be raised
if the object is not present.

.. index:: Expression

Expression
++++++++++

A *Expression* is a special type of string variable that contains an expression to
be evaluated. The expression can reference variables and functions within the
scope of its containing component, as well as within the scope of the component's
parent Assembly.  A number of built-in functions and math functions may also be
referenced within an Expression.  For example, ``abs(math.sin(angle))``
would be a valid Expression, assuming that *angle* is an attribute of the
containing component. Note that *self* does not appear in the example expression.
This is because the Expression automatically determines the containing scope of
attributes and functions referenced in an expression. This helps keep expressions
from becoming too verbose by containing a bunch of *self* and *self.parent*
references.

Expressions can be used in a variety of components. Many optimizer components use 
Expressions to specify their objective function, design variables, and constraints.
Conditional branching components use Expressions to specify boolean expressions that
determine if a given branch should be executed.

Here is an example of declaring an Expression as an input, as it would be used to
create a variable to hold the objective function of an optimizer, which is
inherently a function of variables in the framework.

.. testcode:: Expression_example

    from openmdao.main.api import Driver, Expression
    
    class MyDriver(Driver):
        """ A component that outputs a dot product of two arrays"""
	
        objective = Expression(iotype='in', \
                    desc= 'A string containing the objective function \
                    expression.')
			    
Note that it makes little sense to give a default value to an Expression, since
its value will usually depend on the component names. Stringrefs are most
likely to be assigned their value in the higher-level container: typically the
top level assembly. Also, note that Expression is imported from
``openmdao.main.api`` instead of ``openmdao.lib.api``. This is because a
Expression is a special class of public variables that is an integral part of
the framework infrastructure.

There is also a *ExpressionList* variable which can be used to hold multiple
string expressions. For example, an optimizer might take as input a list
containing some number of constraints that are built from these string
expressions.

.. testcode:: ExpressionList_example

    from openmdao.main.api import Driver, ExpressionList
    
    class MyDriver(Driver):
        """ A component that outputs a dot product of two arrays"""
	
	constraints = ExpressionList(iotype='in',
		desc= 'A list of expression strings indicating constraints.'+
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

As an example, consider a component that calculates a pressure (in Pascals) given
a known force (in Newtons) applied to a known area (in square meters). Such a
component would look like this:

.. testcode:: units_declare

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
    class Pressure(Component):
        """Simple component to calculate pressure given force and area"""
    
	# set up interface to the framework  
	force = Float(1.0, iotype='in', desc='force', units='N')
        area = Float(1.0, iotype='in', low=0.0, exclude_low=True, desc='m*m')        

        pressure = Float(1.0, iotype='out', desc='Pa')        

	def execute(self):
	    """calculate pressure"""
	    
	    self.pressure = self.force/self.area

Note that some additional parameters in the declaration of *area* prevent a
value of zero from being assigned (and thus a division error.) Of course you
could still get very large values for *pressure* if *area* is near machine
zero. You could also change the output from 'Pa' to 'atm' (standard atmosphere)
and the result will be converted to this specification.

This units library can also be used to convert internal variables by importing
the function *convert_units*.

    >>> from openmdao.lib.traits.float import convert_units
    >>> convert_units(33,'m','ft')
    108.267...

Coercion and Casting
++++++++++++++++++++

OpenMDAO variables have a certain pre-defined behavior when a value from a
variable of a different type is assigned. Public variables were created
using the Casting traits as opposed to the Coercion traits. This means that
most mis-assignments in variable connections (i.e., a float connected to
a string) should generate a TraitError exception. However, certain widening
coercions seem to be permitted (e.g., Int->Float, Bool->Int, Bool->Float). No
coercion from Str or to Str is allowed. If the user needs to apply different
coercion behavior, it should be fairly simple to create a Python component to
do the type translation.

More details can be found in the `Traits 3 User Manual`__.

.. __: http://code.enthought.com/projects/traits/docs/html/traits_user_manual/defining.html?highlight=cbool#predefined-traits-for-simple-types

*Variable Containers*
~~~~~~~~~~~~~~~~~~~~~

For components with many public variables, it is often useful to compartmentalize
them into a hierarchy of containers to enhance readability and "findability." This
is particularly important when the user is submitting or connecting variables in
a GUI, but it is also useful for the script interface.

Variables in OpenMDAO can be compartmentalized by creating a container from the
*Container* base class. This container merely contains variables or other 
containers.

Normally a variable is accessed in the data hierarchy as:

``...component_name.var_name``

but when it is in a container, it can be accessed as:

``...component_name.container_name(.subcontainer_name.etc).var_name``

Consider an example of an aircraft simulation that requires some values for
three variables that define two flight conditions:

.. testcode:: variable_containers

    from openmdao.main.api import Component, Container
    from openmdao.lib.api import Float

    class FlightCondition(Container):
        """Container of public variables"""
    
        airspeed = Float(120.0, iotype='in', units='nmi/h')
        angle_of_attack = Float(0.0, iotype='in', units='deg')
        sideslip_angle = Float(0.0, iotype='in', units='deg')

    class AircraftSim(Component):
        """This component contains variables in a container"""
    
        weight = Float(5400.0, iotype='in', units='kg')
	# etc.
	
        def __init__(self, directory=''):
            """Constructor"""

            super(AircraftSim, self).__init__(directory)
        
	    # Instantiate our variable containers.
            self.fcc1 = FlightCondition()
            self.fcc2 = FlightCondition()
	    
        def execute(self):
            """Do something."""
	    
	    self.fcc2.angle_of_attack = 2.0
	    
Here, the container FlightCondition was defined, containing 3 public variables.
The component AircraftSim is also defined with a public variable *weight* and
two variable containers *fcc1* and *fcc2*. We can access weight through *self.weight*; 
likewise, we can access the airspeed of the second flight condition through
*self.fcc2.airspeed*. Note that you can have containers in containers. There are
no physical limitations to how deep you can go with containers, but for practical
purposes, intricately deep hierarchies may introduce more overhead.

There is one other interesting thing to note about this example. We've effectively
implemented a kind of data structure with this container, and used it to create
multiple copies of a set of public variables. This can prove useful for blocks of
variables that are repeated in a component. Note that at the framework level, 
connections are still made by connecting individual variables. The next section also 
presents a way to create a custom data structure, but one that the framework 
sees as a single entity for connection purposes.

*Creating Custom Variable Types*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to create new types of public variables to use in your models. 
For an example of a user-created public variable, see :ref:`Building-a-Variable-Plugin`.

Building a Simulation Model
---------------------------

A model is a collection of components (which can include assemblies and drivers)
that can be executed in the framework. The entity that contains this model is
called the top level Assembly, which behaves functionally the same as an
Assembly. There is no way to distinguish it from any other assembly, other
than in how it is used -- it is instantiated on its own instead of adding it
to another assembly. Therefore, it has no parent, and it sits at the top of
the Model Hierarchy. Executing the top level Assembly executes the model.

Consider the top level assembly that was created for :ref:`Getting-Started-with-OpenMDAO`.

.. testcode:: simple_model_Unconstrained_pieces

	from openmdao.main.api import Assembly
	from openmdao.lib.api import CONMINdriver
	from openmdao.examples.simple.paraboloid import Paraboloid

	class OptimizationUnconstrained(Assembly):
    	    """ Top level assembly for optimizing a vehicle. """
    
    	    def __init__(self, directory=''):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
	        super(OptimizationUnconstrained, self).__init__(directory)

	        # Create Paraboloid component instances
	        self.add_container('paraboloid', Paraboloid())

	        # Create CONMIN Optimizer instance
	        self.add_container('driver', CONMINdriver())
		
We can see here that components that comprise the top level of this model are
declared in the constructor. Note that the base class constructor is called
(with the *super* function) before anything is added to the empty assembly. This
is important to ensure that functions that are defined in the base classes are
available for use, such as *add_container*. 

The function *add_container*, takes a valid OpenMDAO name and a constructor as
its arguments. This function call creates a new instance of the Component, and 
adds it to the OpenMDAO model hierarchy using the given name. In this case then,
the CONMIN driver is accessible anywhere in this assembly via *self.driver*.
Likewise, the Paraboloid is accessed via *self.paraboloid*.

Note that in the Graphical Interface, the analog to *add_container* is dragging
a component into some workspace or tableau.

A Component can also be removed from an Assembly using *remove_container*,
though it is not expected to be needed except in rare cases.

*Assemblies*
~~~~~~~~~~~~

An Assembly is a special type of Component with the following characteristics:

- Contains some number of other components (some of which may be assemblies)
- Contains a workflow (essentially an execution order)
- Contains a driver that operates on the workflow

An Assembly retains the Component API (i.e, it can be executed, added to
models, and exists in the Model Hierarchy), but it also extends the API to
include functions that support the above-listed characteristics.

*Connecting Components*
~~~~~~~~~~~~~~~~~~~~~~~

Consider once again the top level assembly that was created for 
:ref:`Getting-Started-with-OpenMDAO`. We would like to create a few
instances of the Paraboloid function, and connect them together in series.

.. testcode:: connect_components

	from openmdao.main.api import Assembly
	from openmdao.examples.simple.paraboloid import Paraboloid

	class ConnectingComponents(Assembly):
    	    """ Top level assembly for optimizing a vehicle. """
    
    	    def __init__(self, directory=''):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
		
		self.add_container("par1",Paraboloid())
		self.add_container("par2",Paraboloid())
		self.add_container("par3",Paraboloid())
		
		self.connect("par1.f_xy","par2.x")
		self.connect("par2.f_xy","par3.y")

Components are connected by using the *connect* function built into the
assembly. Connect takes two arguments, the first of which must be a component
output, and the second of which must be a component input. These are expressed
using their locations in the OpenMDAO model hierarchy with respect to the scope
of the top level assembly. Note that an input can be connected to another input,
but an output cannot be connected to another output. Additionally, only one output can
be connected to any input. The violation of any of these rules generates a
RuntimeError. On the other hand, it is perfectly fine to connect multiple
inputs to an output.
		
A public variable is not required to be connected to anything. Most typical 
components will have numerous inputs, and many of these will contain values
that are set by the user, or are perfectly fine at their defaults.

Variables in an assembly also must be able to be connected to the assembly
boundary, so that outside components can link to them. They can be declared
explicitly, however this does create additional overhead as data is passed 
through an intermediary variable in the Assembly. A more efficient way to
accomplish this is to create a passthrough using the *create_passthrough*
function in an Assembly.

Consider a similar assembly as shown above, except that we want to promote the
remaining unconnected variables to the assembly boundary, so that they can be
linked at that level.

.. testcode:: passthroughs

	from openmdao.main.api import Assembly
	from openmdao.examples.simple.paraboloid import Paraboloid

	class ConnectingComponents(Assembly):
    	    """ Top level assembly for optimizing a vehicle. """
    
    	    def __init__(self, directory=''):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
		
		self.add_container("par1",Paraboloid())
		self.add_container("par2",Paraboloid())
		
		self.connect("par1.f_xy","par2.x")
		
		self.create_passthrough('par1.x')
		self.create_passthrough('par1.y')
		self.create_passthrough('par2.y')
		self.create_passthrough('par2.f_xy')

The *create_passthrough* creates a public variable on the assembly. This new
variable has the same name, iotype, default value, units, description, and range
characteristics as the original variable on the subcomponent. If it is desired
that any of these be different in the interface presented external to the
assembly (and there are valid reasons to change some of these, particularly the
units), then a passthrough cannot be used. Instead, the desired public variables
must be manually created and connected just like the normal ones. However, at
present, this will only work with inputs, because inputs can be connected to
other inputs, but outputs cannot be connected to other outputs. A more
detailed example is given in :ref:`A-More-Complex-Tutorial-Problem`. Fortunately,
the passthroughs are sufficient for most needs.

Assemblies also include a way to break variable connections. The *disconnect*
function can be called to break the connection between an input and an output,
or to break all connections to an input or output.

    >>> from openmdao.examples.enginedesign.vehicle import Vehicle
    >>> my_car = Vehicle("new_car")
    >>>
    >>> # Disconnect all connections to tire_circumference (total:2)
    >>> my_car.disconnect('tire_circumference')
    >>>
    >>> # Disconnect a specific connection
    >>> my_car.disconnect('velocity','transmission.velocity')

The opportunity to use the *disconnect* in the scripting interface should be
fairly uncommon, though it is recognized that some specialized assemblies of
components might need to reconfigure their connections during run-time, so it
is available. 

.. _Files-and-Directories:

*Interacting with Files and Directories*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many components will need to read from and write to files in the file system during
model execution. For example, a component might need to generate input files
for and parse output files from an external application. In order to write
components such as these, it is important to understand how objects in OpenMDAO
interact with the file system.

The top assembly in the OpenMDAO model hierarchy contains the root path. This
path is actually not known until after the assembly is instantiated (to learn
how to set the root path, see :ref:`Setting-the-Top-Level-Assembly`.) All 
components that are part of an assembly with a valid absolute directory have
the same absolute directory.

It is possible to change the absolute path of the working directory for any
component on instantiation by setting the *directory* attribute in the
constructor. For example, given the simple optimization model, we can specify
a new working directory for the Paraboloid component when it is instantiated.
    
.. testcode:: simple_model_component_directory

	from openmdao.main.api import Assembly
	from openmdao.lib.api import CONMINdriver
	from openmdao.examples.simple.paraboloid import Paraboloid

	class OptimizationUnconstrained(Assembly):
    	    """ Top level assembly for optimizing a vehicle. """
    
    	    def __init__(self, directory=''):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
	        super(OptimizationUnconstrained, self).__init__(directory)

	        # Create Paraboloid component instances
	        self.add_container('paraboloid', Paraboloid(directory='folder/subfolder'))

Notice that this is a relative path. **All components in the model hierarchy
must operate in a directory that is a sub-directory of the top level assembly's
absolute path.** An attempt to give a component an absolute path that is not a
descendant of the top assembly's absolute path will result in a ValueError
exception. This is a restriction that may be changed in the future depending
on user feedback, but is accurate at present. If two components need to
operate in directories disparate from the top path in hierarchy (e.g., one
component in the model needs to run on a scratch disc), then this can be
accomplished with by using multiprocessing, wherein each process has its own
top level.
		
*Sockets & Interfaces*
~~~~~~~~~~~~~~~~~~~~~~

TODO: Discuss sockets and interfaces

Drivers
-------

*The Driver Interface*
~~~~~~~~~~~~~~~~~~~~~~

*Solution Drivers*
~~~~~~~~~~~~~~~~~~

Solution drivers are generally iterative solvers that operate on their respective
workflow until some conditions are met. Optimizers and solvers fall under this
classification. OpenMDAO comes with several solution drivers that were 
distributable (i.e., either open-source or public domain.)

CONMIN
++++++

CONMIN, which stands for CONstraint MINimization, is a gradient descent optimization
algorithm based on the Method of Feasible Directions. It was developed at
NASA in the 1970s, and is  currently in the public domain. It has been  included
in OpenMDAO's Standard Library to provide users with a basic gradient algorithm.
The interface for CONMIN is full detailed in :ref:`CONMIN-driver`.


PyEvolve
++++++++

PyEvolve is complete genetic algorithm framework written in pure python. It was
developed and is actively maintained by Christian S. Perone.

Documentation for the PyEvolve package can be found at `<http://pyevolve.sourceforge.net/>`_.

Documentation for the OpenMDAO driver is forthcoming, pending some reworking.

Newton Solver
+++++++++++++

No capability at present, but it is part of our requirements. Scientific Python
includes a Newton solver; this may serve as a starting point for the OpenMDAO
driver.

*The Case Iterator*
~~~~~~~~~~~~~~~~~~~

*Adding new Drivers*
~~~~~~~~~~~~~~~~~~~~

Running OpenMDAO
-----------------

.. _Setting-the-Top-Level-Assembly:

*Setting the Top Level Assembly*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a Component or Assembly is instantiated as a standalone object, it is not
aware of the directory where it resides. Any component added to such an assembly
also does not know its path. The function *set_as_top* is available to denote an
assembly as the top level assembly in the framework. Once an assembly is set
as the top level assembly, it gains an absolute path which can be assessed
through the function *get_abs_directory*.

The path that is set by *set_as_top* is always the current working directory 
in the Python environment.

    >>> from openmdao.main.api import Assembly, set_as_top   
    >>> z1 = Assembly()
    >>> z1.get_abs_directory()
    Traceback (most recent call last):
    ...
    RuntimeError: can't call get_abs_directory before hierarchy is defined
    >>>
    >>> set_as_top(z1)
    <openmdao.main.assembly.Assembly object at ...>
    >>> z1.get_abs_directory()
    '...'

Note that the output in this example depends on your local directory structure.
All components added into this assembly will have this same absolute path. If a 
component or assembly does not have a valid absolute directory, then File 
variables will not be able to read, write, or even open their target files.

*Executing Models*
~~~~~~~~~~~~~~~~~~

TODO: Running a model

TODO: Reset to Defaults

*Error Logging & Debugging*
~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Saving & Loading*
~~~~~~~~~~~~~~~~~~

*Sharing Models*
~~~~~~~~~~~~~~~~

Data Flow and WorkFlow
----------------------

The execution order for components in a model can either be determined 
automatically by OpenMDAO, or specified explicitly  by the user. This
distinction can be made at the assembly level, so for example, a model can have
some assemblies with user-specified workflow, while other assemblies are
left to automatic determination. In addition, a driver workflow can also be
specified by the user. All three of these scenarios are discussed below.

*Data Flow & Lazy Evaluation*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The 'default' workflow for a model is inferred from the data flow connections.
This means that a component is available to run once its inputs become valid,
which occurs when the components that supply those inputs are valid. Since
direct circular connections (algebraic loops for those familiar with Simulink)
are not permitted, there will always be an execution order that can be
determined from the connections. OpenMDAO uses the *networkx* package to find
loops and solve for the execution order. Note that this order isn't always
unique.

A bit more on the technical details: every component contains a dictionary of
its input public variables coupled with a validity flag. When any input is
invalid, the component is essentially invalid and therefore will be executed during the
next run. If the component is valid (i.e., has no invalid inputs), it does
not need to execute when the model is run. This is the principal of Lazy 
Evaluation. It should be noted that when a component's inputs become invalidated,
the effect is propagated downstream to all components that depend on it. Also,
when a model is instantiated, all inputs are invalid, which ensures that
the whole model always executes the first time it is run.

*Custom WorkFlow*
~~~~~~~~~~~~~~~~~

Custom workflow capability is currently under development and should be available soon.

*Custom Driver Workflow*
~~~~~~~~~~~~~~~~~~~~~~~~

Custom driver workflow capability is currently under development and should be
available in the near future.

Design Tools
------------

*Design of Experiments*
~~~~~~~~~~~~~~~~~~~~~~~

No capability at present, but it is part of our requirements.

*Multi-objective Optimization and Pareto Frontiers*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

No capability at present, but it is part of our requirements.

*Sensitivity Analysis*
~~~~~~~~~~~~~~~~~~~~~~

No capability at present, but it is part of our requirements.

Managing Simulation Data
------------------------

There is presently no specific capability to help the user manage simulation
data (in potentially large amounts), but it has been identified as an important
need. More work will be done in this area in the future.

Multi-Threaded Computation
--------------------------

No capability at present, but it is part of our requirements, and is
currently being implemented.

Publishing a Component
----------------------

*Eggs*
~~~~~~

*Adding a New Component to your Local Library*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Geometry in OpenMDAO
--------------------

An API to provided a unified geometry interface is currently being investigated.
More information on the notional prototype can be found in :ref:`Geometry-Interfaces-in-OpenMDAO`.
 
Advanced MDAO 
-------------

*Multi-Fidelity Optimization*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

No capability at present, but it is part of our requirements.

*Surrogate Modeling*
~~~~~~~~~~~~~~~~~~~~~

No capability at present, but it is part of our requirements.

*Uncertainty*
~~~~~~~~~~~~~
 
No capability at present, but it is part of our requirements.

