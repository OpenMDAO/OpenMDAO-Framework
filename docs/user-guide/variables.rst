
.. _Variables:

Variables
==========

In OpenMDAO, a *variable* is an attribute that can be seen or manipulated by
other entities in the framework. Any data that is passed between components in a
model must use variables to declare the inputs and outputs for each
component.

You can create a variable for a component in two ways. The first is to
declare it in the component's class definition as shown in the example 
given in the :ref:`simple tutorial problem <A-Simple-Tutorial-Problem>`. A simple component that takes
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
An alternative way to declare them is to use the ``add`` function that is part of the
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

Next, the ``add`` function is used to add the input *x* and the output *y* after
an instance of ``Simple`` has been created:

.. doctest:: creating_public_variables_2

    >>> equation = Simple()
    >>> equation.add('x',Float(1.0, iotype='in', desc='The input x'))
    <openmdao.lib.datatypes.float.Float object at ...>
    >>> equation.add('y',Float(iotype='out', desc='The output y'))
    <openmdao.lib.datatypes.float.Float object at ...>
    >>> equation.x=7
    >>> equation.run()
    >>> equation.y
    21.0

Using ``add`` in this way allows you to create a variable dynamically at some
point after the component has been created.

    >>> from openmdao.examples.simple.paraboloid import Paraboloid
    >>> from openmdao.lib.datatypes.api import Int
    >>> test=Paraboloid()
    >>> test.z
    Traceback (most recent call last):
    ...
    AttributeError: 'Paraboloid' object has no attribute 'z
    >>> test.add('z',Int(7777, iotype='out', desc='An Int'))
    <openmdao.lib.datatypes.int.Int object at ...>
    >>> test.z
    7777

Some specialized components will make use of the ability to create
variables on the fly, but most general components won't need this.

The example above shows how to directly access a variable, but there is also an
indirect access using a ``set`` and ``get`` method.  The framework uses ``set`` and ``get`` 
to pass data between variables. In some cases a
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

Traits
--------

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

Built-in Variable Types
------------------------

.. index:: variable types
    
**Summary of Variable Types**

+----------+--------------------------------------------------------------+
| Nam      | Callable Signature                                           |
+==========+==============================================================+
| Arr      | ``Array( [default_value = None, shape = None, value = None,  |
|          | dtype = None, units = None, iotype = None, desc = None,      |
|          | units = None] )``                                            |
+----------+--------------------------------------------------------------+
| Bool     | ``Bool( [value = None, desc = None, iotype = None] )``       | 
+----------+--------------------------------------------------------------+
| Complex  | ``Complex( [value = None, desc = None,                       |
|          | iotype = None] )``                                           |
+----------+--------------------------------------------------------------+
| Enum     | ``Enum( [default_value, values = (),                         | 
|          | desc = None, iotype = None, aliases = ()] )``                |
+----------+--------------------------------------------------------------+
| File     | ``File( [default_value = None, iotype = None,                |
|          | desc = None, low = None, high = None, path = None,           |
|          | content_type = None, binary = False,                         |
|          | local_path = None] )``                                       |
+----------+--------------------------------------------------------------+
| Float    | ``Float( [default_value = None, iotype = None,               |
|          | desc = None, low = None, high = None,                        |
|          | exclude_low = False, exclude_high = False,                   |
|          | units = None] )``                                            |
+----------+--------------------------------------------------------------+
| Int      | ``Int( [default_value = None, iotype = None,                 |
|          | desc = None, low = None, high = None,                        |
|          | exclude_low = False, exclude_high = False] )``               |
+----------+--------------------------------------------------------------+
| Range    | Deprecated. Use OpenMDAO's Int or Float.                     |
+----------+--------------------------------------------------------------+
| Slot     | ``Slot( [klass = None, desc = None, iotype = None,           |
|          | factory = None, args = None, kw = None,                      |
|          | allow_none = True, adapt = None,                             |
|          | required = False] )``                                        |
+----------+--------------------------------------------------------------+
| Str      | ``Str( [value = None, desc = None, iotype = None] )``        |
+----------+--------------------------------------------------------------+

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

================  =====================
**iotype**        **Description**
================  =====================
``iotype='in'``   Component input
----------------  ---------------------
``iotype='out'``  Component output
================  =====================

The *desc* attribute is a concise description of the variable -- one or
two sentences should be fine. While nothing in the framework requires this
description, it would be wise to include one for every input and output of your
components.

It is possible to create new types of variables to use in your models. 
For an example of a user-created variable, see :ref:`Building-a-Variable-Plugin`.

.. index:: Array

*Arrays*
++++++++

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

*Enums*
+++++++

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
    ValueError: : Variable 'color2' must be in ('Red', 'Yellow', 'Green'), but a value of Purple <type 'str'> was specified.
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
    ValueError: : Variable 'color' must be in (0, 1, 2), but a value of 4 <type 'int'> was specified.`

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

It is also possible to produce a simple array that behaves like an Enum, where each element of
the array can contain only a value that is in the Enum. This kind of variable can be
defined by creating a *List* of Enums.
    
.. testcode:: enum_list_example

    from openmdao.lib.datatypes.api import Enum, List
    from openmdao.main.api import Component
    
    
    class Dice(Component):
        roll = List( Enum(1, (1, 2, 3, 4, 5, 6)), iotype='in')
        
This example defines a variable named *roll* that can contain the values for any number
of dice. Instead of giving a List as the default value, we've given it the definition
for an Enum variable that has a default value of 1 and a set of valid values spanning
the integers 1 to 6. Note that the Enum doesn't need an iotype, but the List does.

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
    ValueError: : Variable 'roll' must be in (1, 2, 3, 4, 5, 6), but a value of 7 <type 'int'> was specified.


.. index:: File Variables, File

*File Variables*
++++++++++++++++

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
                
.. index:: Slot Traits

*Slot Traits*
++++++++++++++++++

An *Slot* is a trait that requires any value assigned to it to be either an instance of a
specific class or an implementation of a specific Interface. The class or Interface to be matched is
the first argument to the constructor. Failure to match the specified class or Interface will result
in an exception being raised. Slot traits are typically used to implement 
placeholders for plugins within a component, but they may also be used to implement Variables by
setting their *iotype* metadata attribute to ``'in'`` or ``'out'``.  In this case, it is important
to  also set the *copy* metadata attribute so the framework knows how to copy the data to connected
components.  Allowable values for *copy* are ``'deep'`` (the default), ``'shallow'``, and ``None``. 
A copy value of ``None`` indicates that the data will be passed by reference and no copy will be
made.


.. testcode:: instance_example

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Slot
    from openmdao.main.interfaces import ICaseRecorder, ICaseIterator
    
    
    class Fred(Component):
        """ A component that takes a class as an input """
    
        recorder = Slot(ICaseRecorder, desc='Something to append() to.',
                          required=True)
        caseiter = Slot(ICaseIterator, desc='set of cases to run.',
                          iotype='in')
 
In this example, we have one Slot and one input that is a Slot. The
input called *caseiter* requires data objects that implement the ``ICaseIterator``
interface. The Slot called *recorder* is required to implement the
``ICaseRecorder`` interface.

The attribute *required* is used to indicate whether the object that plugs into
a Slot is required. If ``required`` is True, then an exception will be raised
if the object is not present.


.. index:: Float; Array; unit conversion with
.. index:: unit conversion; with Float

*Unit Conversions with Float and Array*
++++++++++++++++++++++++++++++++++++++++

OpenMDAO also supports variables with explicitly defined units using the Float and Array
variable types, which are included as part of the Standard Library. Both
types provide the following useful effects when utilized in the framework.

- Automatically convert a value passed from an output to an input with compatible units (e.g., ``'inch'`` and ``'m')``
- Raise an exception when attempting to pass a value from an output to an input having incompatible units (e.g., ``'kg'`` and ``'m'``)
- Allow values to be passed between unitless variables and variables with units; no unit conversion occurs

A complete list of the available units is given in the :ref:`units`. The unit conversion code
and the base set of units come from the PhysicalQuantities package found in `Scientific Python
<http://dirac.cnrs-orleans.fr/plone/software/scientificpython>`_. It was necessary to add a few
units to the existing ones in PhysicalQuantities (in particular, a currency unit), so a new
Units package was derived and is included in OpenMDAO as ``openmdao.units``. This package has
the same basic function as that of PhysicalQuantities, but to make it more extensible, the unit
definitions were moved from the internal dictionary into an externally readable text file called
``unitLibdefault.ini``. See the source documentation for more information on the OpenMDAO
:ref:`units package<openmdao.units.units.py>`, including how to add units.

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
the function ``convert_units`` from ``openmdao.main.api``.

    >>> from openmdao.main.api import convert_units
    >>> convert_units(12.0,'inch','ft')
    1.0

*Coercion and Casting*
++++++++++++++++++++++

OpenMDAO variables have a certain pre-defined behavior when a value from a
variable of a different type is assigned. Variables were created
using the *casting* traits as opposed to the *coercion* traits. This means that
most mis-assignments in variable connections (e.g., a float connected to
a string) should generate an exception. However, certain widening
coercions are permitted (e.g., ``Int->Float, Bool->Int, Bool->Float``). No
coercion from Str or to Str is allowed. If you need to apply different
coercion behavior, it should be simple to create a Python component to
do the type translation.

More details can be found in the `Traits 3 User Manual`__.

.. __: http://code.enthought.com/projects/traits/docs/html/traits_user_manual/defining.html?highlight=cbool#predefined-traits-for-simple-types

Variable Trees
--------------

For components with many variables, it is often useful to compartmentalize
them into a hierarchy of containers to enhance readability and "findability."

Variables in OpenMDAO can be compartmentalized by creating a container from the
VariableTree base class. This container merely contains variables or other 
VariableTrees.

Normally a variable is accessed in the data hierarchy as:

``...component_name.var_name``

but when it is in a VariableTree, it can be accessed as:

``...component_name.container_name(.subcontainer_name.etc).var_name``

Consider an example of an aircraft simulation that requires values for
three variables that define two flight conditions:

.. testcode:: variable_containers

    from openmdao.main.api import Component, VariableTree, Slot
    from openmdao.lib.datatypes.api import Float

    class FlightCondition(VariableTree):
        """Container of variables"""
    
        airspeed = Float(120.0, units='nmi/h')
        angle_of_attack = Float(0.0, units='deg')
        sideslip_angle = Float(0.0, units='deg')

    
    class AircraftSim(Component):
        """This component contains variables in a VariableTree"""
    
        fcc1 = Slot(FlightCondition(), iotype='in')
        fcc2 = Slot(FlightCondition(), iotype='in')
        
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

Here, the class ``FlightCondition`` was defined, containing three variables.
The component ``AircraftSim`` is also defined with a variable *weight* and two
FlightConditions *fcc1* and *fcc2*. We can access weight through
``self.weight``; likewise, we can access the airspeed of the second flight
condition through ``self.fcc2.airspeed``. In this example we had only one
level of nesting in our VariableTree class, but a VariableTree can be added to
another VariableTree, so any level of nesting is possible.

An interesting thing about this example is that we've
implemented a data structure with this container and used it to create
multiple copies of a set of variables. This can prove useful for blocks
of variables that are repeated in a component. At the framework level,
connections are still made by connecting individual variables. It is possible
to create a custom data structure that the framework sees as a single entity
for connection purposes. This is explained in :ref:`Building-a-Variable-Plugin`.
