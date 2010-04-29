.. index:: simple example

.. _Getting-Started-with-OpenMDAO:

Getting Started: A Simple Example Problem
==========================================

The purpose of this section is to teach the novice user how to set up and
execute a simple optimization problem using the OpenMDAO script interface. This
tutorial assumes that you are familiar (though not necessarily proficient)
with the Python language and have a reasonable understanding of the concepts
presented in :ref:`Overview-of-the-OpenMDAO-Framework`, particularly the
concepts of the :term:`Component`, :term:`Assembly`, and :term:`Driver`.

The problem we present here is a paraboloid that is a function of two input 
variables. The optimization goal is to find the minimum values of this function
over the design space. For the simplest form of this problem, there are no
constraints, and we assume that the analytical gradient of the function is
unavailable. The problem can be expressed graphically as follows:


.. _`OpenMDAO_overview`:

.. figure:: ../../examples/openmdao.examples.simple/openmdao/examples/simple/Simple1.png
   :align: center

   A Simple Optimization Problem
   
This graphical view of the problem also helps in understanding how to map the
problem onto the OpenMDAO framework. The optimizer is the :term:`Driver`,
and its job is to manipulate the two design variables (*x* and *y*) to 
minimize the output of the paraboloid function (*f*). The equation fits
into the OpenMDAO process as a :term:`Component`. More specifically, this 
Paraboloid component contains an execute function which takes the inputs 
(*x* and *y*) and returns the value of the function (*f*) evaluated at those 
inputs. Finally, both the driver and the component are contained in an 
:term:`Assembly`, which maintains the connections between the driver and the
component, and knows how to run the system.

The following instructions will help you locate the directory containing
the pieces needed for the model relative to the install directory.

If you have a branch from the source repository:

	``examples/openmdao.examples.simple/openmdao/examples/simple``
	
If you have downloaded the latest release version from the website:

	``openmdao-X.X.X/lib/python2.6/site-packages/openmdao.examples.simple-X.X.X-######.egg/openmdao/examples/simple``
	
where X.X.X is the current OpenMDAO version, and ###### is a string that
contains the Python version and the operating system description. This will
vary depending on your system and version, but there will only be one
*simple* egg.
	
.. index:: Component

Building a Component - Paraboloid
---------------------------------

A component is simply something that takes a set of inputs and operates on
them to produce a set of outputs. In the OpenMDAO architecture, a class called
:term:`Component` provides this behavior. Any :term:`Component` has inputs and outputs
and contains a function called *execute* that calculates the outputs based on
the values of the inputs. The Python code for the Paraboloid component is as
follows:

.. testcode:: simple_component_Paraboloid

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
    class Paraboloid(Component):
	""" Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
	# set up interface to the framework  
	x = Float(0.0, iotype='in', desc='The variable x')
        y = Float(0.0, iotype='in', desc='The variable y')

        f_xy = Float(0.0, iotype='out', desc='F(x,y)')        

        
	def execute(self):
	    """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
            Minimum: x = 6.6667; y = -7.3333
	    """
        
	    x = self.x
	    y = self.y
        
	    self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

We will now explain the details of this. A component is implemented in the
OpenMDAO framework by writing Python code and placing it in a file. In Python,
this file is called a module. Typically, a file will contain one component,
although it is possible to include more than one component in a single file.
The file ``paraboloid.py`` contains the code shown above. Later in this
tutorial we will discuss how to execute a model containing this component.

In Python, a class or function must be imported before it can be used. There are two libraries
that contain most of what you will need in OpenMDAO: *openmdao.main.api* and *openmdao.lib.api*.

The first two lines in the ``paraboloid.py`` module are importing the definitions
of the Component class and the Float class.  We will use these in the definition
of our Paraboloid class. 

.. testcode:: simple_component_Paraboloid_pieces

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
Note that there are many other objects that we could import
from ``openmdao.main.api`` and ``openmdao.lib.api``, but we are only importing the
two classes that we need.  This is a good idea because it helps to prevent any 
namespace collisions in our module. In other words:

.. testcode:: package

    # BAD
    import openmdao.main.api
    
    # BAD
    from openmdao.main.api import *
    
    # GOOD
    from openmdao.main.api import Component

The next line defines a class called Paraboloid:

.. testcode:: simple_component_Paraboloid_pieces

    class Paraboloid(Component):
        """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
	
.. index:: classes, functions

At this point, it is essential that, as a component developer, you have a clear
understanding of the distinction between classes and functions. On the surface
it would appear that we merely need a function that can take two inputs and
return an output, and for such a simple example, this might be the case.
However, object-oriented programming allows much more complicated systems to be
described without increasing the complexity of the framework code. A flexible
framework like OpenMDAO probably could not be implemented without objects, and
even if it could, it would be complex and difficult to maintain.

So, this line of Python code defines the Paraboloid class by deriving it from
the Component class. This means that a Paraboloid is a Component, so it contains
all of the data and functions that a Component contains. This includes a lot of
helper functions that are used by the framework infrastructure to manage things.
You don't have to worry about any of the
framework back-end; typically there are just two functions that you would
provide -- one for initialization (any calculation that needs to be done
before the optimization loop), and one to execute the component (calculate
the outputs from the inputs.)

Note that if we stop here, we have a Paraboloid component with no inputs, no 
outputs, and an execute function that does nothing. The first thing we need
to do is to define the inputs and outputs. We do this in the class definition
by adding these lines:

.. testcode:: simple_component_Paraboloid_pieces

	# set up interface to the framework  
	x = Float(0.0, iotype='in', desc='The variable x')
        y = Float(0.0, iotype='in', desc='The variable y')

        f_xy = Float(0.0, iotype='out', desc='F(x,y)')  


.. index:: Traits

There are two kinds of variables in OpenMDAO: *internal variables* and *public variables*.
Internal variables are variables that are used internally to a component but are
ignored by the framework. Public variables are variables that are 
publicly visible (and manipulatable if they are inputs) in the framework.

All of our inputs and outputs are floating point numbers, so we use a type of
public variable called *Float*. The *Float* constructor contains a default
value (set to 0 for both x and y), an *iotype* (which declares this variable
as an input or an output), and a *desc*, or description (a string of text that
describes this variable). The variable is given a name by which it will be
known. This name is limited to those names that are valid as Python
variables. The only argument that is absoluately required for all public
variables is the *iotype*. If this is omitted (or mispelled) then the
variable won't be visible in the framework.

For the Paraboloid component, we've created two inputs and one output. Later
in this example, an optimizer will set these inputs, but we could just as
easily set them by connecting each of them to an output of another component.

Finally, we need a function to execute this component:

.. testcode:: simple_component_Paraboloid_pieces

	def execute(self):
	    """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
	    Optimal solution (minimum): x = 6.6667; y = -7.3333
	    """
        
	    x = self.x
	    y = self.y
        
	    self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0
	    
The execute function is where you define what a component will do when it is told to run. For
our Paraboloid component, the equation is evaluated here. The input and output public variables
are members of the Parabaloid class, which means that they must be accessed using the *self.*
prefix. For example, *self.x* gives you the value stored in *x*, which originates outside the
component. This *self.* can be cumbersome in a big equation, so a pair of internal variables *x*
and *y* are used in the calculation.

Often, you will already have the code for evaluating your component outputs,
but it will be in some other language, such as Fortran or C/C++. The :ref:`Plugin-Developer's-Guide` 
gives some examples of how to incorporate these kinds of components into OpenMDAO.

The Paraboloid component is now built and ready for inclusion in a model.


Building a Model - Unconstrained Optimization using CONMIN
-----------------------------------------------------------

The next task is to build a model that finds the minimum value for the
Paraboloid component described above. This model will contain the Paraboloid as well as
a public domain gradient optimizer called :term:`CONMIN`, for which a Python-wrapped
driver has been included in OpenMDAO. The model can be found in
the Python file ``optimization_unconstrained.py``:

.. testcode:: simple_model_Unconstrained

	from openmdao.main.api import Assembly
	from openmdao.lib.api import CONMINdriver
	from openmdao.examples.simple.paraboloid import Paraboloid

	class OptimizationUnconstrained(Assembly):
	    """Unconstrained optimization of the Parabaloid with CONMIN."""
    
    	    def __init__(self):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
	        super(OptimizationUnconstrained, self).__init__()

	        # Create Paraboloid component instances
	        self.add_container('paraboloid', Paraboloid())

	        # Create CONMIN Optimizer instance
	        self.add_container('driver', CONMINdriver())
        
	        # CONMIN Flags
	        self.driver.iprint = 0
	        self.driver.itmax = 30
	        self.driver.fdch = .000001
	        self.driver.fdchm = .000001
        
	        # CONMIN Objective 
	        self.driver.objective = 'paraboloid.f_xy'
        
	        # CONMIN Design Variables 
	        self.driver.design_vars = ['paraboloid.x', 
	                                 'paraboloid.y' ]
        
	        self.driver.lower_bounds = [-50, -50]
        	self.driver.upper_bounds = [50, 50]


.. index:: top level Assembly
 		
In OpenMDAO parlance, we usually describe this as the *top level Assembly.* An 
:term:`Assembly` is a container that can hold some number of components, drivers, and 
other assemblies. An Assembly also manages the interconnections between the
components and assemblies that it owns, and it has its own workflow, which it
uses to execute the components and drivers in the correct order. For our
problem, this assembly will include a Paraboloid component and a 
CONMIN driver. It will tell the CONMIN driver when to run and what to run.

Note that this is an Assembly, so the class is derived from Assembly instead
of Component. This gives it access to the management functions mentioned above.

.. testsetup:: simple_model_Unconstrained_pieces

	from openmdao.main.api import Assembly
	from openmdao.lib.api import CONMINdriver
	from openmdao.examples.simple.paraboloid import Paraboloid
	from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
	
	self = OptimizationUnconstrained()
	
.. testcode:: simple_model_Unconstrained_pieces

	class OptimizationUnconstrained(Assembly):
	    """Unconstrained optimization of the Parabaloid with CONMIN."""
    
For the Paraboloid component, we created an execute function to tell it what to
do when the component is run. This is not needed for the 
*OptimizationUnconstrained* assembly because the Assembly class already has an
execution function that should be usable for most cases. However, this assembly
does need an initialize function to set parameters for the optimization. This
is done using the *__init__* function.

.. testcode:: simple_model_Unconstrained_pieces

    	    def __init__(self):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
	        super(OptimizationUnconstrained, self).__init__()

.. index:: StringRef
		
The __init__ function is called by the class constructor on a new
uninitialized instance of the class, so it's a good spot to set up any
parameters that are needed for CONMIN. The double leading and trailing
underscores are a required part of the syntax. The *super* command calls the
__init__ function of the parent (Assembly); this is also required.

Next, the Paraboloid and the CONMIN driver have to be instantiated and added
to OptimizationUnconstrained. The function *add_container* is used to add them
to the assembly.

.. testcode:: simple_model_Unconstrained_pieces

	        # Create Paraboloid component instances
	        self.add_container('paraboloid', Paraboloid())

	        # Create CONMIN Optimizer instance
	        self.add_container('driver', CONMINdriver())
		
Here we make an instance of the *Paraboloid* component we created above and
give it the name parabaloid. Similarly we create an instance of the CONMIN
driver and give it the name *driver.* As with other class members, these are
now accessible in the *OptimizationUnconstrained* assembly via *self.paraboloid*
and *self.driver.*
		
For this problem, we want to minimize *f_xy.* In optimization, this is called
the objective function. In OpenMDAO, we define the objective function using a
*StringRef* variable:
        
.. testcode:: simple_model_Unconstrained_pieces

	        # CONMIN Objective 
	        self.driver.objective = 'paraboloid.f_xy'
		
A *StringRef* is a special kind of public variable that contains a string
expression that combines public variables with Python mathematical syntax.
Every public variable has a unique name in the OpenMDAO data hierarchy. This
name combines public variable name with its parents' names, and is analogous
to the path name in a file system using the "." as a separator. This allows
for two components to have the same variable name while still assuring they'll
be uniquely referable. Here, the *f_xy* output of the Paraboloid component is
selected as the objective for minimization.

StringRefs are also used to define the design variables (decision variables)
for the optimization problem. While CONMIN operates only on a single objective,
it allows multiple design variables. These are assigned in a Python list:
        
.. testcode:: simple_model_Unconstrained_pieces

	        # CONMIN Design Variables 
	        self.driver.design_vars = ['paraboloid.x', 
	                                 'paraboloid.y' ]
					 
Here, both *x* and *y* are chosen as the design variables. We can also add a range
of validity for these variables, which allows an unconstrained optimization to be
performed on what is essentially a bounded problem. For this problem, we have
created a lower and an upper bound, constraining *x* and *y* to lie on [-50, 50].
        
.. testcode:: simple_model_Unconstrained_pieces

	        self.driver.lower_bounds = [-50, -50]
        	self.driver.upper_bounds = [50, 50]

The problem is now essentially ready to execute. CONMIN contains quite a few
additional control parameters, though many of them are fine using the default
values. These parameters are detailed in :ref:`CONMIN-driver`.
		
.. testcode:: simple_model_Unconstrained_pieces

	        # CONMIN Flags
	        self.driver.iprint = 1
	        self.driver.itmax = 30
	        self.driver.fdch = .000001
	        self.driver.fdchm = .000001

The parameters specified here include the debug verbosity (*iprint*) and the number of
iterations (*itmax*). Additionally, the relative and absolute step sizes for the
numerical gradient calculation are adjusted to reduce the step size for this
problem (*fdch* and *fdchm*). If the default values are used, only two places of
accuracy can be obtained in the calculated minimum because the default step
size is too large for this problem.

This model is now finished, and ready to be run. The next section will show how this is done.
		
Executing the Simple Optimization Problem
------------------------------------------

To run our model, we have to instantiate it and tell it to run. One
convenient way to do this is to add some code to the end of the file that contains OptimizationUnconstrained,
so that it can be executed in Python, either at the command line or in the Python shell. Using the check
``if __name__ == "__main__":`` we can include some Python code at the bottom of
``optimization_unconstrained.py``; it will execute only when we call it at the command line or
the shell, and not when another module imports it. So, the final lines in this file are:

.. testsetup:: simple_model_Unconstrained_run

	from openmdao.main.api import set_as_top
	from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
	__name__ = "__main__"

.. testcode:: simple_model_Unconstrained_run

	if __name__ == "__main__": 

	    opt_problem = OptimizationUnconstrained()
	    set_as_top(opt_problem)

	    import time
	    tt = time.time()
	    
	    opt_problem.run()

	    print "\n"
	    print "CONMIN Iterations: ", opt_problem.driver.get("iter_count")
	    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.get("x"), \
                                         opt_problem.paraboloid.get("y"))
	    print "Elapsed time: ", time.time()-tt, "seconds"

.. testoutput:: simple_model_Unconstrained_run
    :hide:

    ...
    CONMIN Iterations:  5
    Minimum found at (6.666309, -7.333026)
    Elapsed time:  ... seconds
	    
						 
This block of code really does just four things. In the first statement, an
instance of the class *OptimizationUnconstrained* is created with the name
*opt_problem.* In the second statement, *opt_problem* is set as the top
assembly in the model hierarchy. This will be explained in a later tutorial.
In the fifth statement, *opt_problem* is told to run, which executes the model
until the optimizer's termination criteria are reached. The rest of the
statements print the results and report the elapsed time.

This script can be executed in the shell by going to the
``examples/openmdao.examples/simple/openmdao/examples/simple`` directory, and typing:

::

        python optimization_unconstrained.py
	
Make sure that you are using OpenMDAO's local Python environment.
	
This should produce the output:

.. :: 

    [ CONMIN output not shown ]
    CONMIN Iterations:  5
    Minimum found at (6.666309, -7.333026)
    Elapsed time:  0.0558300018311 seconds

An *OptimizationUnconstrained* assembly is instantiated and given the
name *opt_problem.* This creates the problem and instantiates a Paraboloid and
a CONMIN driver. The run function is used to run the model, which solves the
optimization problem as set up above. And last, the final design variables are
accessed using the get function on the Paraboloid component, which is
accessible even from outside this assembly.

.. index:: contraints, CONMIN

Building a Model - Constrained Optimization using CONMIN
---------------------------------------------------------

Usually, an optimization problem also contains a number of constraints on the
design space. 

*Constraints* are equations (generally inequalities) that are expressed as functions
of the design variables. In OpenMDAO, they are constructed much like the objective functions
using the available public variables to build an expression with Python
mathematical syntax. For CONMIN, the constraints parameter is a list of inequalities that
are defined to be satisfied when they return a negative value or zero and violated
when they return a positive value.

We want to add the constraint *(y-x+15)<0* to the problem. The unconstrained
minimum actually violates this constraint, so a new minimum must be found by
the optimizer. We can add a constraint to our existing OptimizationUnconstrained
model by adding one line to the init function:

.. testcode:: simple_model_Unconstrained_pieces

        # CONMIN Constraints
        self.driver.constraints = ['paraboloid.y-paraboloid.x+15.0']

This new script should be saved as optimization_constrained.py. It can be executed
in the shell by going to the
``examples/openmdao.examples/simple/openmdao/examples/simple`` directory, and
typing:

::

        python optimization_constrained.py
	
When this is executed, it should produce the output:

.. testoutput:: simple_model_Constrained_run

    [ CONMIN output not shown ]
    CONMIN Iterations:  6
    Minimum found at (7.175775, -7.824225)
    Elapsed time:  0.0295481681824 seconds


This concludes an introduction to a simple problem of component creation and execution in
OpenMDAO. The next tutorial section introduces a problem with more complexity and
presents some more of the features of the framework.
