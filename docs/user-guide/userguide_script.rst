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

OpenMDAO includes several namespaces, all prefixed by .openmdao:

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

*Special Plug-ins*
~~~~~~~~~~~~~~~~~~

The OpenMDAO Standard Library will include a number of specialized components
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
  
Framework Variables
-------------------

*Traits*
~~~~~~~~

*Built-in Types*
~~~~~~~~~~~~~~~~

*StringRefs*
~~~~~~~~~~~~

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

*Unit Conversion*
~~~~~~~~~~~~~~~~~

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
 
