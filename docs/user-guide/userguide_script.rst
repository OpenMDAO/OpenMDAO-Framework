.. index:: user guide script interface

.. _`OpenMDAO-scripting-interface`:

OpenMDAO Scripting Interface
================================

OpenMDAO provides a programmatic interface that allows you to write a Python
script that describes the structure of the model and provides the ability to
interact with objects in the framework.

The goal of this section of the *User Guide* is to explain and demonstrate
several aspects of the OpenMDAO script interface. This section is intended
primarily as a reference. If you are an inexperienced user, you would best be 
served by reading and understanding the examples in
:ref:`Getting-Started-with-OpenMDAO` and :ref:`A-More-Complex-Tutorial-Problem`.

OpenMDAO Fundamentals
---------------------

The following sections discuss the the basics of OpenMDAO.

.. index:: package

*OpenMDAO Package Hierarchy*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Package* is a Python concept that provides a structure for organizing
variables and functions in a logical hierarchical fashion. Packages allow you
to import needed functions and class definitions into the Python environment
using dotted module names, where the branch levels are separated by a period
(".").

The OpenMDAO package hierarchy includes several subpackages, all of which are prefixed by 
``openmdao.``:

- ``openmdao.main`` -- core infrastructure for the framework
- ``openmdao.lib`` -- OpenMDAO's standard library, containing some important plugins (drivers, traits, etc.) that are available to users of the framework
- ``openmdao.units`` -- unit definition and conversion
- ``openmdao.examples`` -- tutorials and example problems for learning OpenMDAO
- ``openmdao.util`` -- utilities used by OpenMDAO but can also be used standalone
- ``openmdao.test`` -- functions and classes used strictly for unit testing

OpenMDAO users and component developers will likely need only the first three of these (``main,
lib,`` and ``units``). Importing classes and functions from OpenMDAO's libraries is performed with
the same syntax as loading any other Python module:

.. testcode:: package

    from openmdao.main.api import Component, Assembly
    from openmdao.lib.api import CONMINdriver

Here, the fundamental OpenMDAO component classes *Component* and *Assembly* are
loaded from ``openmdao.main.api``, along with the CONMIN driver from ``openmdao.lib.api``.

To simplify the imports, a selection of the most commonly used imports was
placed in the pseudo-package ``openmdao.main.api``. You can obtain a complete
listing of what is available in this module by using the ``dir()`` command in
Python. Likewise, a pseudo-package was also created to house some of the most
commonly used imports from the standard library. In general, it contains
variables and drivers. Most of these items are also explained elsewhere
in the *User Guide.*

Importing more objects into the namespace of your module increases the
likelihood of name collision, so you should import only the objects that you need.
You should avoid using ``from <modname> import *`` because it puts every object
from the given module into the current namespace. 

.. testcode:: package

    # BAD
    from openmdao.main.api import *
    
    # INCONVENIENT
    import openmdao.main.api
    
    # GOOD
    from openmdao.main.api import Component, Assembly, Driver


*Naming Conventions*
~~~~~~~~~~~~~~~~~~~~

Components and variables that are instantiated into the OpenMDAO model 
hierarchy must follow the same naming syntax as attributes in the Python
language. To summarize, they can include only alphanumeric
characters and the underscore, and the lead character cannot be a number.
Any attempt to create a component or a variable that does not conform
to Python's syntax should result in an exception. This restriction was required
because these entities essentially exist as Python variables. One unfortunate
side effect is that names with spaces are not allowed. OpenMDAO checks for
compliance when a variable or Component instance is created:

    >>> from openmdao.main.api import Assembly
    >>> from openmdao.examples.enginedesign.chassis import Chassis
    >>> top = Assembly('top')
    >>> top.add('chassis1',Chassis())
    <openmdao.examples.enginedesign.chassis.Chassis object at ...
    >>> top.add('the chassis',Chassis())
    Traceback (most recent call last):
    ...
    NameError: name 'the chassis' contains illegal characters

In the OpenMDAO source and examples, we've tried to follow the `PEP 8
<http://www.python.org/dev/peps/pep-0008/>`_ standard, which specifies a naming
convention for component instance names and variable names. For all
variable names, PEP 8 prescribes the use of lower case names with words
separated by underscores. Naturally, PEP 8 compliance is not a requirement
that will be forced on users, but it is a good style guideline.

.. index:: Component

Creating New Components
-----------------------

Components are the basic building block of the OpenMDAO model, so you need 
to be familiar with how to create and execute them. The concept of the component
and the place it holds in the OpenMDAO architecture is given in the
:ref:`Introduction-to-the-OpenMDAO-Framework`.

Presumably you have your own components to implement in OpenMDAO as part of 
a larger model or process. This implementation will usually require the creation
of an OpenMDAO Python component based on the Component class and conforming to the
Component API.

**The Component API**

Every component in the OpenMDAO framework is an object that conforms to a
specific interface. At present, the easiest way to match this interface
is to inherit from the built-in Component class and then override the
``execute()`` function to give the component some kind of run behavior. Likewise,
the ``__init__()`` function can also be overridden to prescribe the component's
behavior when it is instantiated. This is mostly useful for defining any 
internal private variables that need to be saved between runs but aren't
needed by other components in the framework.

A simple component that implements an equation with two inputs is shown below:

.. testcode:: simple_component_Equation

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Float
    
    class Equation(Component):
        """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
        # Component Input 
        x = Float(0.0, iotype='in', desc='The variable y')
        y = Float(0.0, iotype='in', desc='The variable x')

        # Component Output
        f_xy = Float(0.0, iotype='out', desc='F(x,y)')        

        # Initialization function (technically not needed here)
        def __init__(self):
            super(Equation, self).__init__()        
    
        # Executes when component is run
        def execute(self):
            """ Solve (x-3)^2 + xy + (y+4)^2 = 3
            Optimal solution (minimum): x = 6.6667; y = -7.3333
            """
        
            x = self.x
            y = self.y
        
            self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

In this example, the ``__init__()`` function doesn't do anything but call the
equivalent in the base class, so technically it should be removed from this 
class definition. 

.. index:: save_to_egg()

One additional function that may need to be defined in certain cases is
``save_to_egg()``. Sometimes a wrapped code might require some additional files or
directories to be packed with it. These kinds of things can be taken care of in
``save_to_egg()``. It is important not to forget to call the ``save_to_egg()`` for the base
class.

.. todo::

    ``save_to_egg`` example


.. _Variables:

Variables
---------

In OpenMDAO, a *variable* is an attribute that can be seen or manipulated by
other entities in the framework. Any data that is passed between components in a
model must use variables to declare the inputs and outputs for each
component.

You can create a variable for a component in two ways. The first is to
declare it in the component's class definition as shown in the example 
given in the :ref:`simple tutorial problem <Getting-Started-with-OpenMDAO>`. A simple component that takes
a floating point number as an input and provides a floating point number as an
output would look like this:

.. testcode:: creating_public_variables_1

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Float
    
    class Simple(Component):
        """ A simple multiplication """
    
        # set up interface to the framework  
        x = Float(1.0, iotype='in', desc='The input x')
        y = Float(iotype='out', desc='The output y')        

        def execute(self):
            """ y = 3*x """
        
            self.y = 3.0*self.x

The example above shows the way the majority of users will create variables.
An alternative way to declare them is to use the ``add_trait`` function that is part of the
Component public interface. First, lets define the same class in the shell but without
the variables *x* and *y*.
  
.. testcode:: creating_public_variables_2

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Float
    class Simple(Component):
        """ A simple multiplication """
        def execute(self):
            """ y = 3*x """
            self.y = 3.0*self.x

Next, the ``add_trait`` function is used to add the input *x* and the output *y* after
an instance of *Simple* has been created:

.. doctest:: creating_public_variables_2

    >>> equation = Simple()
    >>> equation.add_trait('x',Float(1.0, iotype='in', desc='The input x'))
    >>> equation.add_trait('y',Float(iotype='out', desc='The output y'))
    >>> equation.x=7
    >>> equation.run()
    >>> equation.y
    21.0

The primary use of ``add_trait`` is to create a variable dynamically at some
point after the component has been created (possibly during execution).

    >>> from openmdao.examples.simple.paraboloid import Paraboloid
    >>> from openmdao.lib.datatypes.api import Int
    >>> test=Paraboloid()
    >>> test.z
    Traceback (most recent call last):
    ...
    AttributeError: 'Paraboloid' object has no attribute 'z
    >>> test.add_trait('z',Int(7777, iotype='out', desc='An Int'))
    >>> test.z
    7777

Some specialized components will make use of the ability to create
variables on the fly, but most general components won't need this.

The example above shows how to directly access a variable, but there is also an
indirect access using a ``set`` and ``get`` method. ``Set`` and ``get`` are primarily used by the
framework to pass data between variables. In some cases a
model developer may need to use them -- but only for specific cases where
some objects are executing on remote servers.

Here is an example of the ``get`` function:

.. doctest:: var_indirect

    >>> from openmdao.examples.enginedesign.engine import Engine
    >>> my_engine = Engine()
    >>> my_engine.bore
    82.0
    >>> my_engine.get("bore")
    82.0

Here is an example of the ``set`` function:

.. doctest:: var_indirect

    >>> my_engine.RPM = 2500
    >>> my_engine.RPM
    2500.0
    >>> my_engine.set("RPM",3333)
    >>> my_engine.RPM
    3333.0

.. index:: Traits

*Traits*
~~~~~~~~

The underlying implementation of variables in OpenMDAO was accomplished
through a Python add-on called :term:`Traits`. Traits provide a way to 
apply explicit typing to the normally untyped Python attributes. They also provide 
the capability to add some other features to the variables, including 
unit checking and conversion, default values, upper and lower bounds, and a way to create 
callback functions that execute under specified conditions.

In general, you won't need to worry about traits or how variables are
implemented, but those of you who want to create custom datatypes can do so by
defining a new custom trait. More details on traits can be found on
Enthought's Traits `project page <http://code.enthought.com/projects/traits/>`_.

*Built-in Variable Types*
~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: variable types
    
**Summary of Variable Types**

+------------------+----------------------------------------------------------+
| Name             | Callable Signature                                       |
+==================+==========================================================+
| Array            | Array( [default_value = None, shape = None, value = None,|
|                  | dtype = None, units = None, iotype = None, desc = None,  |
|                  | units = None] )                                          |
+------------------+----------------------------------------------------------+
| Bool             | Bool( [value = None, desc = None, iotype = None] )       | 
+------------------+----------------------------------------------------------+
| Complex          | Complex( [value = None, desc = None,                     |
|                  | iotype = None] )                                         |
+------------------+----------------------------------------------------------+
| Enum             | Enum( [default_value, values = (),                       | 
|                  | desc = None, iotype = None, aliases = ()] )              |
+------------------+----------------------------------------------------------+
| File             | File( [default_value = None, iotype = None,              |
|                  | desc = None, low = None, high = None, path = None,       |
|                  | content_type = None, binary = False,                     |
|                  | local_path = None] )                                     |
+------------------+----------------------------------------------------------+
| Float            | Float( [default_value = None, iotype = None,             |
|                  | desc = None, low = None, high = None,                    |
|                  | exclude_low = False, exclude_high = False,               |
|                  | units = None] )                                          |
+------------------+----------------------------------------------------------+
| Instance         | Instance( [klass = None, desc = None, iotype = None,     |
|                  | factory = None, args = None, kw = None,                  |
|                  | allow_none = True, adapt = None, module = None,          |
|                  | required = False] )                                      |
+------------------+----------------------------------------------------------+
| Int              | Int( [default_value = None, iotype = None,               |
|                  | desc = None, low = None, high = None,                    |
|                  | exclude_low = False, exclude_high = False] )             |
+------------------+----------------------------------------------------------+
| Range            | Deprecated. Use OpenMDAO's Int or Float.                 |
+------------------+----------------------------------------------------------+
| Str              | Str( [value = None, desc = None, iotype = None] )        |
+------------------+----------------------------------------------------------+

A more detailed list of Enthought's `Traits`__ is given in their documentation.
Traits are also available for use as variables in the framework, though
we haven't included examples of the more exotic ones. If you need
to use one, remember that *iotype* and *desc* should be added to the arguments
when one of these is instantiated. The traits use \*\*metadata to store these
user-defined attributes.

.. __: http://code.enthought.com/projects/traits/docs/html/traits_user_manual/defining.html?highlight=cbool#other-predefined-traits

A variable is declared with a number of arguments, many of which are
optional.

The *iotype* attribute is required for all variables regardless of type.
Its sole function is to tell the framework whether the variable should be
treated as an input or an output. Presently, the only two options for this
attribute are ``'in'`` and ``'out'``.

**Summary of iotypes**

============  =====================
**iotype**    **Description**
============  =====================
iotype='in'   Component input
------------  ---------------------
iotype='out'  Component output
============  =====================

The *desc* attribute is a concise description of the variable -- one or
two sentences should be fine. While nothing in the framework requires this
description, it would be wise to include one for every input and output of your
components.

It is possible to create new types of variables to use in your models. 
For an example of a user-created variable, see :ref:`Building-a-Variable-Plugin`.

.. index:: Array

Arrays
++++++

It is possible to use an array as a variable through use of the *Array*
trait. The value for an Array can be expressed as either a Python array or a NumPy
array. NumPy arrays are very useful because of NumPy's built-in mathematical
capabilities. Either array can be n-dimensional and of potentially any type.

Constructing an Array variable requires a couple of additional parameters that
are illustrated in the following example:

    >>> from openmdao.lib.datatypes.api import Array
    >>> from numpy import array
    >>> from numpy import float as numpy_float
    >>> z = Array(array([[1.0,2.0],[3.0,5.0]]), dtype=numpy_float, shape=(2,2), iotype='in')
    >>> z.default_value
    array([[ 1.,  2.],
           [ 3.,  5.]])
    >>> z.default_value[0][1]
    2.0

Here, we import the Array variable and the NumPy array, which is a
general-purpose n-dimensional array class. A 2-dimensional array is assigned as
the default value for the variable named *z*. 

The *dtype* parameter defines the type of variable that is in the array. For
example, using a string (*str*) for a dtype would give an array of strings. Any
of Python's standard types and NumPy's additional types should be valid for the
dtype parameter. The alternate *typecode* specification is also supported for 
non-NumPy arrays (e.g., ``typecode='I'`` for unsigned integers.)

The *shape* parameter is not a required attribute; the Array will default to
the dimensions of the array that are given as the value. However, it is often
useful to specify the size explicitly, so an exception is generated if an
array of a different size or shape is passed into it. If the size of an array is not
determined until runtime (e.g., a driver that takes an array of constraint
equations as an input), then the shape should be left blank.

An array can also have a single unit defined with the *units* parameter. This unit
applies to every element in the array, and it enables unit checking and conversion
when connecting an array output to an array input.

Below is an example of a simple component that takes two Arrays as inputs
and calculates their dot product as an output.

.. testcode:: array_example

    from numpy import array, sum, float   
    
    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Array, Float
    
    class Dot(Component):
        """ A component that outputs a dot product of two arrays"""
    
        # set up interface to the framework  
        x1 = Array(array([1.0,2.0]), dtype=float, desc = "Input 1",
                   iotype='in')
        x2 = Array(array([7.0,8.0]), dtype=float, desc = "Input 2",
                   iotype='in')
           
        y = Float(0.0, iotype='out', desc = "Dot Product")

        def execute(self):
            """ calculate dot product """
        
            if len(self.x1) != len(self.x2):
                self.raise_exception('Input vectors must be of equal length',
                          RuntimeError)
        
            # Note: array multiplication is element by element
            self.y = sum(self.x1*self.x2)
        
            # print the first element of x1
            print x1[0]

Multiplication of a NumPy array is element by element, so *sum* is used to
complete the calculation of the dot product. Individual elements of the array
can also be accessed using brackets. An OpenMDAO Array behaves like a NumPy
array, so it can be used as an argument in a NumPy function like `sum`.

Note that this is a horrible way to do a dot product. Numpy has a dot function
which is much faster than sum.

.. index:: Enum

.. _Enums:

Enums
+++++

It is possible to use an *Enum* (enumeration) type as a variable in
OpenMDAO. This is useful for cases where an input has certain fixed values
that are possible. For example, consider a variable that can be one of three
colors:

.. testcode:: enum_example2

    from openmdao.lib.datatypes.api import Enum
    from openmdao.main.api import Component
    
    class TrafficLight(Component):
        color2 = Enum('Red', ('Red', 'Yellow', 'Green'), iotype='in')

Then we can interact like this:

.. doctest:: enum_example2

    >>> test = TrafficLight()
    >>> test.color2
    'Red'
    >>> test.color2="Purple"
    Traceback (most recent call last):
    ...
    TraitError: : Trait 'color2' must be in ('Red', 'Yellow', 'Green'), but a value of Purple <type 'str'> was specified.
    >>> test.color2="Green"
    >>> test.color2
    'Green'

However, if the Enum is being used to select the input for an old code, then you will
most likely need to feed it integers, not strings. To make this more convenient, the
Enum includes an optional parameter *alias* that can be used to provide descriptive
strings to go along with the numbers the code expects.

.. testcode:: enum_example

    from openmdao.lib.datatypes.api import Enum
    from openmdao.main.api import Component
    
    class TrafficLight(Component):
        color = Enum(0, (0, 1, 2), iotype='in', aliases=("Red", "Yellow", "Green"))

Let's create an instance of this component and try setting the Enum.

.. doctest:: enum_example

    >>> test = TrafficLight()
    >>> test.color=2
    >>> test.color
    2

If we set to an invalid value, an exception is raised.

.. doctest:: enum_example

    >>> test.color=4
    Traceback (most recent call last):
    ...
    TraitError: : Trait 'color' must be in (0, 1, 2), but a value of 4 <type 'int'> was specified.`

We can also access the list of indices and the list of aliases directly from the trait.

.. doctest:: enum_example

    >>> color_trait = test.get_trait('color')
    >>> color_trait.aliases
    ('Red', 'Yellow', 'Green')
    >>> color_trait.values
    (0, 1, 2)
    >>> color_trait.aliases[test.color]
    'Green'

If the default value is not given, then the first value of the list is taken as the default.

.. testcode:: enum_example

    color2 = Enum(('Red', 'Yellow', 'Green'), iotype='in')
    
This is the simplest form of the Enum constructor.

It is also possible to produce a simple array that behaves like an Enum where each element of
the array can only contain a value that is in the Enum. This kind of variable can be
defined by creating a *List* of Enums.
    
.. testcode:: enum_list_example

    from openmdao.lib.datatypes.api import Enum, List
    from openmdao.main.api import Component
    
    class Dice(Component):
        roll = List( Enum(1, (1, 2, 3, 4, 5, 6)), iotype='in')
        
This example defines a variable named *roll* that can contain the values for any number
of dice. Instead of giving a List as the default value, we've given it the definition
for an Enum variable that has a default value of 1, and a set of valid values spanning
the integers from 1 to 6. Note that the Enum doesn't need an iotype, but the List does.

.. doctest:: enum_list_example

    >>> my_dice = Dice()
    >>> 
    >>> # Valid
    >>> my_dice.roll = [1, 6, 3, 2, 2]
    >>>
    >>> # Invalid
    >>> my_dice.roll = [1, 6, 3, 2, 7]
    Traceback (most recent call last):
    ...
    TraitError: : Trait 'roll' must be in (1, 2, 3, 4, 5, 6), but a value of 7 <type 'int'> was specified.


.. index:: File Variables, File

File Variables
++++++++++++++

The *File* variable contains a reference to an input or output file on disk. It
is more than just a text string that contains a path and filename; it is
a *FileReference* that can be passed into other functions expecting
such an object. FileReferences have methods for copying the reference and
opening the referenced file for reading. The available "flags" are defined
by `FileMetadata`, which supports arbitrary user metadata.


.. testcode:: filevar_example

    from openmdao.lib.datatypes.api import File
    
    text_file = File(path='source.txt', iotype='out', content_type='txt')
    binary_file = File(path='source.bin', iotype='out', binary=True,
                            extra_stuff='Hello world!')

The *path* must be a descendant of the parent component's path, as
explained in :ref:`Files-and-Directories`. The *binary* flag can be used to
mark a file as binary. 

.. todo::

    Provide some examples to demonstrate the options.
                
.. index:: Instance Traits

Instance Traits
+++++++++++++++

An *Instance* is a trait that requires any value assigned to it to be either an instance of a specific class
or an implementation of a specific Interface. The class or Interface to be matched is the first argument to
the constructor. Failure to match the specified class or Interface will result in an exception being raised.
Instance traits are typically used to implement Sockets, which are placeholders for plugins within a
component, but they may also be used to implement Variables by setting their *iotype* metadata attribute to
``'in'`` or ``'out'``.  In this case, it is important to  also set the *copy* metadata attribute so the
framework knows how to copy the data to connected components.  Allowable values for *copy* are ``'deep'`` (the
default), ``'shallow'``, and None.  A copy value of None indicates that the data will be passed by reference
and no copy will be made.


.. testcode:: instance_example

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Instance
    from openmdao.main.interfaces import ICaseRecorder, ICaseIterator
    
    class Fred(Component):
        """ A component that takes a class as an input """
    
        recorder = Instance(ICaseRecorder, desc='Something to append() to.',
                            required=True)
        caseiter = Instance(ICaseIterator, desc='set of cases to run.',
                            iotype='in')
 
In this example, we have one Socket and one input that are Instances. The
input called *caseiter* requires data objects that implement the ICaseIterator
interface. The Socket called *recorder* is required to implement the
ICaseRecorder Interface.

The attribute *required* is used to indicate whether the object that plugs into
a Socket is required. If *required* is True, then an exception will be raised
if the object is not present.


.. index:: Float; Array; unit conversion with
.. index:: unit conversion; with Float

Unit Conversions with Float and Array
+++++++++++++++++++++++++++++++++++++

OpenMDAO also supports variables with explicitly defined units using the Float and Array
variable types, which are included as part of the Standard Library. Both
types provide the following useful effects when utilized in the framework.

- Automatically convert a value passed from an output to an input with compatible units (e.g., ``'inch'`` and ``'m')``
- Raise an exception when attempting to pass a value from an output to an input having incompatible units (e.g., ``'kg'`` and ``'m'``)
- Allow values to be passed between unitless variables and variables with units; no unit conversion occurs

A complete list of the available units is given in the :ref:`Appendix:-Summary-of-Units`.
The unit conversion code and the base set of units come from the
PhysicalQuantities package found in `Scientific Python
<http://dirac.cnrs-orleans.fr/plone/software/scientificpython>`_. It was
necessary to add a few units to the existing ones in PhysicalQuantities (in
particular, a currency unit), so a new Units package was derived and is
included in OpenMDAO as ``openmdao.units``. This package has the same basic
function as that of PhysicalQuantities, but to make it more extensible, the
unit definitions were moved from the internal dictionary into an externally
readable text file called ``unitLibdefault.ini``. See the source documentation for more information on the
OpenMDAO :ref:`Units package<openmdao.units.units.py>`, including how to add units.

As an example, consider a component that calculates a pressure (in Pascals) given
a known force (in Newtons) applied to a known area (in square meters). Such a
component would look like this:

.. testcode:: units_declare

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Float
    
    class Pressure(Component):
        """Simple component to calculate pressure given force and area"""
    
        # set up interface to the framework  
        force = Float(1.0, iotype='in', desc='force', units='N')
        area = Float(1.0, iotype='in', low=0.0, exclude_low=True, desc='m*m')

        pressure = Float(1.0, iotype='out', desc='Pa')

        def execute(self):
            """calculate pressure"""
        
            self.pressure = self.force/self.area

The ``low`` and ``exclude_low`` parameters are used in the declaration of *area* to prevent a
value of zero from being assigned, resulting in a division error. Of course, you
could still get very large values for *pressure* if area is near machine zero.

This units library can also be used to convert internal variables by importing
the function ``convert_units`` from ``openmdao.lib.api``.

    >>> from openmdao.main.api import convert_units
    >>> convert_units(12.0,'inch','ft')
    1.0

Coercion and Casting
++++++++++++++++++++

OpenMDAO variables have a certain pre-defined behavior when a value from a
variable of a different type is assigned. Variables were created
using the *casting* traits as opposed to the *coercion* traits. This means that
most mis-assignments in variable connections (e.g., a float connected to
a string) should generate a TraitError exception. However, certain widening
coercions are permitted (e.g., ``Int->Float, Bool->Int, Bool->Float``). No
coercion from Str or to Str is allowed. If you need to apply different
coercion behavior, it should be simple to create a Python component to
do the type translation.

More details can be found in the `Traits 3 User Manual`__.

.. __: http://code.enthought.com/projects/traits/docs/html/traits_user_manual/defining.html?highlight=cbool#predefined-traits-for-simple-types

*Variable Containers*
~~~~~~~~~~~~~~~~~~~~~

For components with many variables, it is often useful to compartmentalize
them into a hierarchy of containers to enhance readability and "findability."

Variables in OpenMDAO can be compartmentalized by creating a container from the
Container base class. This container merely contains variables or other 
containers.

Normally a variable is accessed in the data hierarchy as:

``...component_name.var_name``

but when it is in a container, it can be accessed as:

``...component_name.container_name(.subcontainer_name.etc).var_name``

Consider an example of an aircraft simulation that requires values for
three variables that define two flight conditions:

.. testcode:: variable_containers

    from openmdao.main.api import Component, Container
    from openmdao.lib.datatypes.api import Float

    class FlightCondition(Container):
        """Container of variables"""
    
        airspeed = Float(120.0, iotype='in', units='nmi/h')
        angle_of_attack = Float(0.0, iotype='in', units='deg')
        sideslip_angle = Float(0.0, iotype='in', units='deg')

    class AircraftSim(Component):
        """This component contains variables in a container"""
    
        weight = Float(5400.0, iotype='in', units='kg')
        # etc.

        def __init__(self):
            """Instantiate variable containers here"""

            super(AircraftSim, self).__init__()
        
            # Instantiate and add our variable containers.
            self.add('fcc1', FlightCondition())
            self.add('fcc2', FlightCondition())
    
        def execute(self):
            """Do something."""
        
            print "FCC1 angle of attack = ", self.fcc1.angle_of_attack
            print "FCC2 angle of attack = ", self.fcc2.angle_of_attack

Here, the container ``FlightCondition`` was defined, containing three variables.
The component ``AircraftSim`` is also defined with a variable *weight* and
two variable containers *fcc1* and *fcc2*. We can access weight through ``self.weight``; 
likewise, we can access the airspeed of the second flight condition through
``self.fcc2.airspeed``. You can also add containers to containers.

An interesting thing about this example is that we've
implemented a data structure with this container and used it to create
multiple copies of a set of variables. This can prove useful for blocks
of variables that are repeated in a component. At the framework level,
connections are still made by connecting individual variables. It is possible
to create a custom data structure that the framework sees as a single entity
for connection purposes. This is explained in :ref:`Building-a-Variable-Plugin`.

Building a Simulation Model
---------------------------

A *model* is a hierarchical collection of components with an assembly at its root. 
The root assembly is also called the *top level assembly.* 
Executing the top level assembly executes the entire model.

Consider the top level assembly that was created for the 
:ref:`simple tutorial problem <Getting-Started-with-OpenMDAO>`.

.. testcode:: simple_model_Unconstrained_pieces

    from openmdao.main.api import Assembly
    from openmdao.lib.api import CONMINdriver
    from openmdao.examples.simple.paraboloid import Paraboloid

    class OptimizationUnconstrained(Assembly):
        """Unconstrained optimization of the Paraboloid with CONMIN."""
    
        def __init__(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
            super(OptimizationUnconstrained, self).__init__()

            # Create CONMIN Optimizer instance
            self.add('driver', CONMINdriver())
        
            # Create Paraboloid component instances
            self.add('paraboloid', Paraboloid())
    
            # Add to driver's workflow
            self.driver.workflow.add(self.paraboloid)
        

We can see here that components that comprise the top level of this model are
declared in the ``__init__`` function. The base class ``__init__`` function is called
(with the ``super`` function) before anything is added to the empty assembly. This
is important to ensure that internal framework machinery has been properly initialized
before any methods such as ``add`` are called.

The ``add`` method takes a valid OpenMDAO name and a corresponding component
instance as its arguments. This function call adds the instance to the
OpenMDAO model hierarchy using the given name. In this case then, the CONMIN
driver is accessible anywhere in this assembly via ``self.driver``. Likewise,
the Paraboloid is accessed via ``self.paraboloid``.

A Component can also be removed from an Assembly using ``remove``.

*Assemblies*
~~~~~~~~~~~~

An Assembly is a special type of Component with the characteristics below. It contains:

- Some number of other components (some of which may be assemblies)
- At least one Driver with the name *driver*. Each Driver has its own workflow.

An Assembly retains the Component API (i.e., it can be executed, added to
models, and exists in the model hierarchy), but it also extends the API to
include functions that support the above-listed characteristics.

*Connecting Components*
~~~~~~~~~~~~~~~~~~~~~~~

Consider once again the top level assembly that was created for the 
:ref:`simple tutorial <Getting-Started-with-OpenMDAO>`. We would like to create a few
instances of the ``Paraboloid`` function and connect them together in series.

.. testcode:: connect_components

    from openmdao.main.api import Assembly
    from openmdao.examples.simple.paraboloid import Paraboloid

    class ConnectingComponents(Assembly):
        """ Top level assembly for optimizing a vehicle. """
    
        def __init__(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
            self.add("par1",Paraboloid())
            self.add("par2",Paraboloid())
            self.add("par3",Paraboloid())
        
            self.connect("par1.f_xy","par2.x")
            self.connect("par2.f_xy","par3.y")

Components are connected by using the ``connect`` function built into the
assembly. ``Connect`` takes two arguments, the first of which must be a component
output, and the second of which must be a component input. These are expressed
using their locations in the OpenMDAO model hierarchy with respect to the scope
of their parent assembly. Additionally, only one output can
be connected to any input.  On the other hand, it is fine to connect an output to multiple
inputs. The violation of any of these rules raises an exception.

A variable is not required to be connected to anything. Typical 
components will have numerous inputs, and many of these will contain values
that are set by the user or are perfectly fine at their defaults.

Variables can be added to an assembly and used to *promote* internal variables,
making them visible to components outside of the assembly. There is a convenience
function called ``create_passthrough`` that creates a variable in the assembly and
connects it to an internal component variable in one step.

Consider a similar assembly as shown above, except that we want to promote the
remaining unconnected variables to the assembly boundary so that they can be
linked at that level.

.. testcode:: passthroughs

    from openmdao.main.api import Assembly
    from openmdao.examples.simple.paraboloid import Paraboloid

    class ConnectingComponents(Assembly):
        """ Top level assembly for optimizing a vehicle. """
    
        def __init__(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
            super(ConnectingComponents, self).__init__()

            self.add("par1",Paraboloid())
            self.add("par2",Paraboloid())
        
            self.connect("par1.f_xy","par2.x")
        
            self.create_passthrough('par1.x')
            self.create_passthrough('par1.y')
            self.create_passthrough('par2.y')
            self.create_passthrough('par2.f_xy')

The ``create_passthrough`` function creates a variable on the assembly. This new variable has
the same name, iotype, default value, units, description, and range characteristics as the
original variable on the subcomponent. If you would like to present a different interface
external to the assembly (perhaps you would like different units), then a passthrough
cannot be used. Instead, the desired variables must be manually created and
connected. You can find a more detailed example of this in the :ref:`complex tutorial
<A-More-Complex-Tutorial-Problem>`. Most of the time passthroughs are sufficient.

Assemblies also include a way to break variable connections. The ``disconnect``
function can be called to break the connection between an input and an output
or to break all connections to an input or output.

    >>> from openmdao.examples.enginedesign.vehicle import Vehicle
    >>> my_car = Vehicle()
    >>>
    >>> # Disconnect all connections to tire_circumference (total:2)
    >>> my_car.disconnect('tire_circumference')
    >>>
    >>> # Disconnect a specific connection
    >>> my_car.disconnect('velocity','transmission.velocity')

You probably won't need to use ``disconnect`` very often. However, some components may
need to reconfigure their connections during runtime, so it is available.

.. _Files-and-Directories:

*Interacting with Files and Directories*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many components will need to read from and write to files during
model execution. For example, a component might need to generate input files
for and parse output files from an external application. In order to write
components such as these, it is important to understand how objects in OpenMDAO
interact with the file system.

The top assembly in the OpenMDAO model hierarchy contains the root path. This
path is not known until after the assembly is instantiated (to learn
how to set the root path, see :ref:`Setting-the-Top-Level-Assembly`). All 
components that are part of an assembly with a valid absolute directory have
the same absolute directory.

You can change the absolute path of the working directory for any
component on instantiation by setting the *directory* attribute in the
``__init__`` function. For example, given the simple optimization model, we can specify
a new working directory for the Paraboloid component when it is instantiated.

.. testcode:: simple_model_component_directory

    from openmdao.main.api import Assembly
    from openmdao.lib.api import CONMINdriver
    from openmdao.examples.simple.paraboloid import Paraboloid

    class OptimizationUnconstrained(Assembly):
        """Unconstrained optimization of the Paraboloid with CONMIN."""
    
        def __init__(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
            super(OptimizationUnconstrained, self).__init__()

            # Create Paraboloid component instances
            self.add('paraboloid', Paraboloid(directory='folder/subfolder'))

Notice that this is a relative path. **All components in the model hierarchy
must operate in a directory that is a sub-directory of the top level
assembly's absolute path.** If you attempt to give a component an absolute path
that is not a descendant of the top assembly's absolute path, OpenMDAO will terminate
with an exception. If two components need to operate in directories
disparate from the top path in the hierarchy (e.g., one component in the model
needs to run on a scratch disc), then this can be accomplished by using
multiprocessing, wherein each process has its own top level.

Drivers
-------

Drivers are generally iterative solvers, such as optimizers, that operate on
their respective workflow until certain conditions are met. OpenMDAO includes
several drivers that are distributable (i.e., either open source or
public domain.) This section describes the driver interface that is common
to most drivers. A more complete discussion on how to use each of the
drivers can be found in the section on :ref:`Drivers` in Appendix B: Standard Library Reference.

.. _Driver-API: 

The Driver API
~~~~~~~~~~~~~~

Drivers in OpenMDAO share a functional interface for setting up certain common
parts of the problem. There are functions to handle parameters, which are inputs
to a system and are also known as *design variables* for optimizers or *independents*
for solvers. Likewise, there are also functions to handle constraints.

.. index:: parameter, design variable

To illustrate the parameter interface, consider a model in which our goal
is to optimize the design of a vehicle with several design variables using
the CONMINdriver optimizer.

.. testcode:: Parameter_API

    from openmdao.main.api import Assembly
    from openmdao.lib.api import CONMINdriver

    class EngineOpt(Assembly):
        """ Top level assembly for optimizing a vehicle. """
    
        def __init__(self):
            """ Creates a new Assembly containing a DrivingSim and an optimizer"""
        
            super(EngineOptimization, self).__init__()

            # Create DrivingSim component instances
            self.add('driving_sim', DrivingSim())

            # Create CONMIN Optimizer instance
            self.add('driver', CONMINdriver())
        
            # add DrivingSim to workflow
            driver.workflow.add(self.driving_sim)

We add design variables to the driver ``self.driver`` using the ``add_parameter``
function. 

.. testsetup:: Parameter_API
    
    from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
    self = EngineOptimization()
    self.driver.clear_parameters()

.. testcode:: Parameter_API

    # CONMIN Design Variables 
    self.driver.add_parameter('driving_sim.spark_angle', low=-50. , high=10.)
    self.driver.add_parameter('driving_sim.bore', low=65. , high=100.)

Parameters are assigned via a string that contains the pathname of an OpenMDAO
variable. This variable must exist in the scope of the assembly that contains
the driver. In other words, if an assembly contains a driver, the parameters
added to that driver cannot be located outside of that assembly. Also, each
parameter must point to a component input, not a component output. During
driver execution, the parameter values are set, and the relevant portion of
the model is executed to evaluate the new objective.
    
The *low* and *high* arguments can be used to specify an allowable range for a parameter. Using these
parameters is useful for optimization problems where the design variables are constrained. Generally, the
optimizer treats these as a special kind of constraint, so they should be defined using the low and high
parameters rather than the ``add_constraint method``. If low and high values are not given, then they are
pulled from the corresponding low and high parameters that are defined in the variable. If low and high aren't
defined in either place, then an exception is raised. Some drivers (in particular solvers) do not support a
low or high value; in such a case, you can just set each of them to a large number, e.g., ``low=-1e99`` and
``high=1e99``.

Multiple parameters can also be added in a single call to ``add_parameters`` (note the letter
*s*) by passing a list of tuples.

.. testcode:: Parameter_API

    # Some more Design Variables 
    self.driver.add_parameters([ ('driving_sim.conrod', 65.0 , 90.0), 
                                 ('driving_sim.IVC', 0.0, 90.0) ])


The ``IHasParameters`` interface also includes some other functions that are more useful when
used interactively or when writing more advanced components. The functions ``list_parameters``,
``remove_parameters``, and ``clear_parameters`` can be used to respectively list all parameters, delete a
single parameter, and clear all parameters.

.. doctest:: more_parameter_interface

    >>> from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
    >>> top = OptimizationConstrained()
    >>> top.driver.list_parameters()
    ['paraboloid.x', 'paraboloid.y']
    >>> top.driver.remove_parameter('paraboloid.x')
    >>> top.driver.list_parameters()
    ['paraboloid.y']
    >>> top.driver.clear_parameters()
    >>> top.driver.list_parameters()
    []

There are also ``get_parameters`` and ``set_parameters`` methods, but these
methods are typically used by drivers to manage the parameters in their
workflow and are not called directly by users. These will be described in the
section :ref:`Adding-new-Drivers`.

.. index:: constraint

A similar interface is present for interacting with constraints. *Constraints*
are defined using strings containing equations or inequalities that reference
available OpenMDAO variables. Both equality and
inequality constraints are supported via the interface; however, when you use a
driver, you should verify that it supports the desired type of constraint. For
example, the CONMIN driver supports inequality constraints but not equality
constraints.

Constraints are added to a driver using the ``add_constraint`` method.

.. testcode:: Parameter_API

    self.driver.add_constraint('driving_sim.stroke < driving_sim.bore')

Constraints are defined using boolean expressions, so they are considered to
be satisfied when the expressions evaluate to *True* and violated when they
evaluate to *False*. The following constraint declarations are all equivalent:

.. testcode:: Parameter_API

    self.driver.add_constraint('driving_sim.stroke - driving_sim.bore < 0')
    self.driver.add_constraint('driving_sim.stroke < driving_sim.bore')
    self.driver.add_constraint('driving_sim.bore > driving_sim.stroke')
    
Using the ``eval_eq_constraints`` and ``eval_ineq_constraints`` methods,
an optimizer or solver can query for the status and values of its constraints. Both
methods return a list of tuples of the form ``(lhs, rhs, relation, result)``, where
*lhs* is the value of the left hand side of the expression, *rhs* is the value of
the right hand side of the expression, *result* is the boolean result of evaluating
the expression, and *relation* is a string indicating the type of
relation used in the expression, e.g., ``>, <, >=, <=, or =``. The
values of the left- and right-hand sides are needed by gradient optimizers that 
apply the constraint via a penalty function.

The *IHasConstraints* interface also supports equality constraints. At
present, none of the optimizers in OpenMDAO support equality constraints, but
they are used by the BroydenSolver to assign the dependent equation. The
syntax includes an equal sign in the expression.

.. testsetup:: Parameter_API2

    from openmdao.lib.api import BroydenSolver
    from openmdao.main.api import Assembly
    from openmdao.examples.mdao.disciplines import SellarDiscipline1
    
    self = Assembly()
    self.add('dis1', SellarDiscipline1())
    self.add('driver', BroydenSolver())

.. testcode:: Parameter_API2

    self.driver.add_constraint('dis1.y1 = 0.0')

.. note::

    OpenMDAO does not check for duplicate constraints, so be careful when
    adding them.
    
Sometimes it is desirable to change the scaling on constraints, particularly for
cases where the constrained variables are of disparate orders of magnitude. This
can be done conveniently with the optional *scale* argument in the call to
add_constraint.

.. testcode:: Parameter_API

    self.driver.add_constraint('driving_sim.stroke - driving_sim.bore < .00001', scaler=10000.0)
    
Here, the constraint has been scaled up so that its value when passed to the optimizer
is in a similar range (and hence similar weight) to the other constraints in the model. An
optional *adder* argument was also added to shift both the left and right hand sides of
a constraint, though the current OpenMDAO gradient optimizer (CONMINdriver) internally shifts
all constraints to the origin, so this parameter is not needed.


Constraints can be removed using ``remove_constraint``.  The same string used
to add the constraint should be used to remove it. Whitespace within the expression
is ignored.

.. testcode:: Parameter_API2

    self.driver.remove_constraint('dis1.y1 = 0.0')

A list of constraint expression strings can be obtained using ``list_constraints``.

.. testcode:: Parameter_API2

    lst = self.driver.list_constraints()
    
Calling ``clear_constraints`` will remove all constraints from a driver.

.. testcode:: Parameter_API2

    self.driver.clear_constraints()
    

.. index:: objective

Finally, OpenMDAO uses a similar interface for specifying objectives. A single
objective (some future optimizers will handle multiple objectives) can be
added to a driver using the ``add_objective`` method with an argument that is
a string expression built up from available OpenMDAO outputs.

.. testcode:: Parameter_API

    # CONMIN Objective = Maximize weighted sum of EPA city and highway fuel economy 
    self.driver.add_objective('-(.93*driving_sim.EPA_city + 1.07*driving_sim.EPA_highway)')

In this example, the objective is to maximize the weighted sum of two variables.
The equation must be constructed using valid Python operators. All variables in
the function are expressed in the scope of the local assembly that contains the
driver.

For drivers that only operate on a single objective (e.g., CONMIN), you can
replace the current objective by calling add_objective with the new objective as an argument.

.. testcode:: Parameter_API

    self.driver.add_objective('-driving_sim.EPA_city')
    # Replace the objective with EPA_highway
    self.driver.add_objective('-driving_sim.EPA_highway')

The *IHasObjective* interface also includes functions to list the objective and to query
for the objective value.

.. doctest:: more_objective_interface

    >>> from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    >>> model = OptimizationUnconstrained()
    >>> model.driver.list_objective()
    'paraboloid.f_xy'
    >>> model.driver.eval_objective()
    0.0

.. _Adding-new-Drivers:

*Adding new Drivers*
~~~~~~~~~~~~~~~~~~~~

.. todo::

    Show how to add new drivers.

Running OpenMDAO
-----------------

.. _Setting-the-Top-Level-Assembly:

*Setting the Top Level Assembly*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a Component or Assembly is instantiated as a standalone object, it is not
aware of the directory where it resides. Any component added to such an assembly
also does not know its path. The function ``set_as_top`` is available to denote an
assembly as the top level assembly in the framework. Once an assembly is set
as the top level assembly, it gains an absolute path which can be accessed
through the function ``get_abs_directory``.

The path that is set by ``set_as_top`` is always the current working directory 
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

The output in this example depends on your local directory structure.
All components added into this assembly will have this same absolute path. If a 
component or assembly does not have a valid absolute directory, then File 
variables will not be able to read, write, or even open their target files.

*Executing Models*
~~~~~~~~~~~~~~~~~~

.. todo::

    Show how to run a model.

.. todo::

    Discuss Reset to Defaults.

*Error Logging & Debugging*
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo::

    Explain the error logging capability.

*Saving & Loading*
~~~~~~~~~~~~~~~~~~

.. todo::

    Show how to save and load.

*Sharing Models*
~~~~~~~~~~~~~~~~

.. todo::

    Discuss sharing models.

Workflow
--------

The execution order for components in a model is determined by the workflow object
that the components belong to. OpenMDAO current has two available workflow classes that
are described below.  They are Dataflow and SequentialWorkflow.

*Dataflow*
~~~~~~~~~~

The "default" workflow for a model is inferred from the data flow connections.
This means that a component is available to run once its inputs become valid,
which occurs when the components that supply those inputs are valid. Since
direct circular connections (algebraic loops for those familiar with Simulink)
are not permitted, there will always be an execution order that can be
determined from the connections.  In the absence of a connection between two
components, this workflow will attempt to execute them in the order that they 
were added to it.

When any component input is set, all dependent outputs are invalidated. If an input
is connected to an output and that output becomes invalid, then the input
is also invalid. If a component
has any invalid inputs or outputs, it will be executed during the next run. 
When a component's inputs are changed, all downstream variables that depend
on them either directly or indirectly are invalidated. Also,
when a model is instantiated, all outputs are invalid, which ensures that the
whole model always executes the first time it is run.


*SequentialWorkflow*
~~~~~~~~~~~~~~~~~~~~

This workflow is a simple sequence of components.  The components will be executed
in the order that they were added to the workflow regardless of data dependencies.
Generally, this is a bad idea, but it's here for those rare occasions when the 
exact sequence must be specified.


Geometry in OpenMDAO
--------------------

We are currently investigating an API to provide a unified geometry interface. More
information on the notional prototype can be found in
:ref:`Geometry-Interfaces-in-OpenMDAO`.


