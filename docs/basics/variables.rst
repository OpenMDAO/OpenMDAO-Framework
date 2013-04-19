
.. _Variables:

Working with Variables
======================

In OpenMDAO, a *variable* is an attribute that can be seen or manipulated by
other entities in the framework. Any data that is passed between components in a
model must use variables to declare the inputs and outputs for each
component.

You can create a variable for a component in two ways. The first is to
declare it in the component's class definition. A simple component that takes
a floating point number as an input and provides a floating point number as an
output would look like this:

.. testcode:: creating_public_variables_1

    from openmdao.main.api import Component
    from openmdao.main.datatypes.api import Float
    
    
    class Simple(Component):
        """ A simple multiplication """
    
        # set up interface to the framework  
        x = Float(1.0, iotype='in', desc='The input x')
        y = Float(iotype='out', desc='The output y')        

        def execute(self):
            """ y = 3*x """
        
            self.y = 3.0*self.x


Built-in Variable Types
------------------------

.. index:: variable types
    
**Summary of Variable Types**

+----------+--------------------------------------------------------------+
| Name     | Callable Signature                                           |
+==========+==============================================================+
| Array    | ``Array( [default_value = None, shape = None, value = None,  |
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
| Slot     | ``Slot( [klass = None, desc = None, iotype = None,           |
|          | factory = None, args = None, kw = None,                      |
|          | allow_none = True, adapt = None,                             |
|          | required = False] )``                                        |
+----------+--------------------------------------------------------------+
| Str      | ``Str( [value = None, desc = None, iotype = None] )``        |
+----------+--------------------------------------------------------------+


When a variable is declared, it gets passed a number of arguments, many of which are
optional.

But the *iotype* attribute is required for all variables regardless of type.
Its function is to tell the framework whether the variable should be
treated as an input or an output.

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

It is also possible to create new types of variables to use in your models. 
For an example of a user-created variable, see :ref:`Building-a-Variable-Plugin`.

.. index:: Array

*Arrays*
++++++++

You can use an array as a variable by using the *Array* trait. The value for an Array can be
expressed as either a Python array or a NumPy array. NumPy arrays are very useful because of NumPy's
built-in mathematical capabilities. Either array can be n-dimensional and potentially of any type.

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
dtype parameter. The alternative *typecode* specification is also supported for 
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
opening the referenced file for reading. The available `flags` are defined
by `FileMetadata`, which supports arbitrary user metadata.


.. testcode:: filevar_example

    from openmdao.lib.datatypes.api import File
    
    text_file = File(path='source.txt', iotype='out', content_type='txt')
    binary_file = File(path='source.bin', iotype='out', binary=True,
                            extra_stuff='Hello world!')

The *path* must be a descendant of the parent component's path.
The *binary* flag can be used to mark a file as binary. This can be important
when transferring files between Windows and OS X or Linux.  The default value
is False, signifying a text file which needs newline translation between
different systems.  If newline translation is applied to a binary file it will
corrupt the data.

.. todo::

    Provide some examples to demonstrate the options.
                
.. index:: Slot Variables

*Slot Variables*
++++++++++++++++++

A *Slot* is a variable that requires any value assigned to it to be either an instance of a
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
if the component is executed when that object is not present.

You can also use a class name to define what is permitted in the slot. In this
code sample, we've specified that the ``recorder`` slot can only contain an
object of class ``CSVCaseRecorder```.

.. testcode:: instance_example

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Slot
    from openmdao.lib.casehandlers.api import CSVCaseRecorder
    
    
    class Fred(Component):
        """ A component that takes a class as an input """
    
        recorder = Slot(CSVCaseRecorder, desc='Something to append() to.',
                          required=True)
                          
We can also declare a pre-filled slot by passing an instance instead of the class
name. This is a shortcut for adding it later.

.. testcode:: instance_example

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Slot
    from openmdao.lib.casehandlers.api import CSVCaseRecorder
    
    
    class Fred(Component):
        """ A component that takes a class as an input """
    
        recorder = Slot(CSVCaseRecorder(), desc='Something to append() to.',
                          required=True)
                          
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
``unitLibdefault.ini``. For more information on the OpenMDAO units package, including how to add units, see the 
:ref:`source documentation<openmdao.units.units.py>`.

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
variable of a different type is assigned. Generally, they do not try to
coerce the given value into the type that they expect. This means that
most mis-assignments in variable connections (e.g., a float connected to
a string) will generate an exception. However, certain widening
coercions are permitted (e.g., ``Int->Float, Bool->Int, Bool->Float``). No
coercion from Str or to Str is allowed. If you need to apply different
coercion behavior, just create a new class inherited from Variable and 
perform the coercion in the validate function.

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

    from openmdao.main.api import Component, VariableTree
    from openmdao.lib.datatypes.api import Float, VarTree

    class FlightCondition(VariableTree):
        """Container of variables"""
    
        airspeed = Float(120.0, units='nmi/h')
        angle_of_attack = Float(0.0, units='deg')
        sideslip_angle = Float(0.0, units='deg')

    
    class AircraftSim(Component):
        """This component contains variables in a VariableTree"""
    
        # create VarTrees to handle updates to our FlightCondition attributes
        fcc1 = VarTree(FlightCondition(), iotype='in')
        fcc2 = VarTree(FlightCondition(), iotype='out')
        
        weight = Float(5400.0, iotype='in', units='kg')
        # etc.

        def execute(self):
            """Do something."""
        
            print "FCC1 angle of attack = ", self.fcc1.angle_of_attack
            print "FCC2 angle of attack = ", self.fcc2.angle_of_attack


.. note::

    It's important to create a VarTree variable (which is much like a Slot)
    for each VariableTree object contained in your component if you intend to
    connect it to variables in other components.
    Also make sure to set the *iotype* attribute in the VarTree.  If you don't, changes 
    to variables within the VariableTree object won't properly notify the component.
    If you have a nested VariableTree, it's necessary to create a VarTree in the
    VariableTree that contains it.
    
    
Here, we defined the class ``FlightCondition``, containing three variables.
The component ``AircraftSim`` is also defined with a variable *weight*, the
input FlightCondition *fcc1* and the output FlightCondition *fcc2*. We can 
access weight through ``self.weight``; likewise, we can access the airspeed of the output flight
condition through ``self.fcc2.airspeed``. In this example we had only one
level of nesting in our FlightCondition class, but a VariableTree can be added to
another VariableTree, so any level of nesting is possible.  For example:


.. testsetup:: nested_vartree

    from openmdao.main.api import VariableTree
    from openmdao.lib.datatypes.api import Float, VarTree

    class FlightCondition(VariableTree):
        pass
    
.. testcode:: nested_vartree

    class MyNestedVars(VariableTree):
        """A nested container of variables"""
    
        f1 = Float(120.0)
        f2 = Float(0.0)
        
        sub_vartree = VarTree(FlightCondition())
        
    
An interesting thing about this example is that we've
implemented a data structure with this VariableTree and used it to create
multiple copies of a set of variables. This can prove useful for blocks
of variables that are repeated in a component. At the framework level,
connections can be made either to individual variables within a VariableTree or
to entire VariableTrees. It is also possible
to create custom data objects and validators to use when connecting 
components. This is explained in :ref:`Building-a-Variable-Plugin`.
