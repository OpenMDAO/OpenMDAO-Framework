Building a Simulation Model
===========================

A *model* is a hierarchical collection of components with an assembly at its root. 
The root assembly is also called the *top level assembly.* 
Executing the top level assembly executes the entire model.

Consider the top level assembly that was created for the 
:ref:`simple tutorial problem <A-Simple-Tutorial-Problem>`.

.. testcode:: simple_model_Unconstrained_pieces

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import CONMINdriver
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
            self.driver.workflow.add('paraboloid')
        

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

Assemblies
-----------

An Assembly is a special type of Component with the characteristics below. It contains:

- Some number of other components (some of which may be assemblies)
- At least one Driver with the name *driver*. Each Driver has its own workflow.

An Assembly retains the Component API (i.e., it can be executed, added to
models, and exists in the model hierarchy), but it also extends the API to
include functions that support the above-listed characteristics.

Connecting Components
----------------------

Consider once again the top level assembly that was created for the 
:ref:`simple tutorial <A-Simple-Tutorial-Problem>`. We would like to create a few
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

Interacting with Files and Directories
---------------------------------------

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
    from openmdao.lib.drivers.api import CONMINdriver
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
