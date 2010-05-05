.. index:: simple example

.. _Getting-Started-with-OpenMDAO:

Getting Started: A Simple Tutorial Problem
==========================================

In this section, you are going to learn how to execute a simple optimization
problem using the OpenMDAO script interface. To get the most out of this
tutorial, you should be familiar (though you don't have to be proficient) with
the Python language and the concepts presented in
:ref:`Overview-of-the-OpenMDAO-Framework`. You should understand the terms
:term:`Component`, :term:`Assembly`, and :term:`Driver`. If you don't have
much experience with Python, we recommend trying `Dive into Python
<http://diveintopython.org/>`_. It is an excellent introduction to Python that
is licensed under the GNU Free Documentation License, so you can download and
use it as you wish.

The problem we present here is a paraboloid that is a function of two input 
variables. Our goal is to find the minimum value of this function
over a particular range of interest. First, we will solve this problem with no constraints. After
this, we will add constraints and solve the problem again. We will not be providing
analytical gradients. The optimizer will calculate numerical gradients internally.

If we express the problem as a block diagram, we can see how to set it up in OpenMDAO:

.. _`OpenMDAO_overview`:

.. figure:: ../../examples/openmdao.examples.simple/openmdao/examples/simple/Simple1.png
   :align: center

   A Simple Optimization Problem
   
The optimizer is the :term:`Driver`. Its job is to manipulate the two design
variables (*x* and *y*) to minimize the output of the paraboloid function
(*f*). The Paraboloid equation fits into the OpenMDAO process as a
:term:`Component`. This Paraboloid component contains a method that operates
on the inputs (*x* and *y*) and returns the value of the function (*f*)
evaluated at those inputs. Both the driver and the component are contained in
an :term:`Assembly`, which maintains the connections between the driver and
the component, and knows how to run the system.

The following instructions will help you locate the directory containing
the pieces needed for the model.

If you have downloaded the latest release version from the website:

	``openmdao-X.X.X/lib/python2.6/site-packages/openmdao.examples.simple-X.X.X-######.egg/openmdao/examples/simple``
	
where X.X.X is the current OpenMDAO version, and ###### is a string that
contains the Python version and the operating system description. This will
vary depending on your system and version, but there will only be one
*simple* egg.
	
If you are a developer, and have a branch from the source repository:

	``examples/openmdao.examples.simple/openmdao/examples/simple``
	
.. index:: Component

Building a Component - Paraboloid
---------------------------------

A component takes a set of inputs and operates on them to produce a set of
outputs. In the OpenMDAO architecture, a class called :term:`Component`
provides this behavior. Any :term:`Component` has inputs and outputs and
contains a function called *execute* that calculates the outputs based on the
values of the inputs. Let's take a look at how we would implement the
paraboloid as an OpenMDAO component:

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

To implement a component in the OpenMDAO framework, you write some Python
code and place it in a file. This file is called a module in Python.
Typically, a module will contain one component, although you can include more
than one component in a single file. The file ``paraboloid.py`` contains the
code shown above. Later in this tutorial we will discuss how to execute a
model containing this component.

In Python, a class or function must be imported before it can be used. Most of
what you need in OpenMDAO can be imported from: *openmdao.main.api* and
*openmdao.lib.api*.

The first two lines in the ``paraboloid.py`` module import the definitions
of the Component class and the Float class.  We will use these in the definition
of our Paraboloid class. 

.. testcode:: simple_component_Paraboloid_pieces

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
There are many other objects that we could import from ``openmdao.main.api``
and ``openmdao.lib.api``, but we are only importing the two classes that we
need. This is a good idea because it helps to prevent any namespace collisions
in our module. In other words:

.. testcode:: package

    # BAD
    from openmdao.main.api import *
    
    # INCONVENIENT
    import openmdao.main.api
    
    # GOOD
    from openmdao.main.api import Component

The next line defines a class called Paraboloid:

.. testcode:: simple_component_Paraboloid_pieces

    class Paraboloid(Component):
        """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
	
.. index:: classes, functions

We define the Paraboloid class by deriving it from
the Component class. A Paraboloid is a Component, so it
contains all of the data and members that a Component contains. This includes
a lot of helper functions that are used by the framework infrastructure to
manage things. You don't have to worry about any of the framework back-end.
Typically there are just two functions that you provide -- one for
initialization (anything that needs to be set up once), and one to execute the
component (calculate the outputs from the inputs.)

If we stop here, we have a Paraboloid component with no inputs, no 
outputs, and an execute function that does nothing. The next thing we need
to do is to define the inputs and outputs in the class definition
by adding these lines:

.. testcode:: simple_component_Paraboloid_pieces

	# set up interface to the framework  
	x = Float(0.0, iotype='in', desc='The variable x')
        y = Float(0.0, iotype='in', desc='The variable y')

        f_xy = Float(iotype='out', desc='F(x,y)')  

.. index:: Traits

There are two kinds of variables in OpenMDAO: *internal variables* and *public variables*.
Internal variables are variables that are used internally to a component but are
ignored by the framework. Public variables are variables that are 
publicly visible (and manipulable if they are inputs) in the framework. Public
variables are declared in the class definition of a component.

All of our inputs and outputs are floating point numbers, so we use a type of
public variable called *Float*. The *Float* constructor contains a default
value and some arguments. The default value has been set to zero for the *x*
and *y*.

The argument *iotype* declares this variable as an input or an output. This
argument is required. If it is omitted (or misspelled) then the variable
won't be visible in the framework.

The argument *desc*, contains a description, or a string of text that describes this
variable. This argument, while not required, is encouraged.

The variable is given a name by which it will be known internally and externally.

For the Paraboloid component, we've created two inputs and one output. Later
in this example, an optimizer will set these inputs. In later examples, we
will see how they can be set by connecting them to an output of another
component.

Finally, we need a function to execute this component:

.. testcode:: simple_component_Paraboloid_pieces

	def execute(self):
	    """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
	    Optimal solution (minimum): x = 6.6667; y = -7.3333
	    """
        
	    x = self.x
	    y = self.y
        
	    self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0
	    
The execute function is where you define what a component does when it runs.
For our Paraboloid component, the equation is evaluated here. The input and
output public variables are members of the Paraboloid class, which means that
they must be accessed using *self*. For example, *self.x* gives you the value
stored in *x*. This *self.* can be cumbersome in a big equation, so a pair of
internal variables *x* and *y* are used in the calculation.

Often, you will already have the code for evaluating your component outputs,
but it will be in some other language, such as Fortran or C/C++. The :ref:`Plugin-Developer's-Guide` 
gives some examples of how to incorporate these kinds of components into OpenMDAO.

The Paraboloid component is now built and ready for inclusion in a model.


Building a Model - Unconstrained Optimization using CONMIN
-----------------------------------------------------------

Our next task is to build a model that finds the minimum value for the
Paraboloid component described above. This model contains the Paraboloid as
well as a public domain gradient optimizer called :term:`CONMIN`, for which a
Python-wrapped driver has been included in OpenMDAO. As the name implies,
CONMIN finds the minimum of a function. The model can be found in the Python
file ``optimization_unconstrained.py``:

.. testcode:: simple_model_Unconstrained

	from openmdao.main.api import Assembly
	from openmdao.lib.api import CONMINdriver
	from openmdao.examples.simple.paraboloid import Paraboloid

	class OptimizationUnconstrained(Assembly):
	    """Unconstrained optimization of the Paraboloid with CONMIN."""
    
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
 		
In OpenMDAO terminology, we describe this as the *top level Assembly.* An
:term:`Assembly` is a container that can hold any number of components,
drivers, and other assemblies. An Assembly also manages the connections
between the components and assemblies that it owns, and it executes all
components and drivers in the correct order. For our problem, this assembly
will include a Paraboloid component and a CONMIN driver. It will tell the
CONMIN driver when to run and what to run.

This is an Assembly, so we derive the class from Assembly instead
of Component.

.. testsetup:: simple_model_Unconstrained_pieces

	from openmdao.main.api import Assembly
	from openmdao.lib.api import CONMINdriver
	from openmdao.examples.simple.paraboloid import Paraboloid
	from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
	
	self = OptimizationUnconstrained()
	
.. testcode:: simple_model_Unconstrained_pieces

	class OptimizationUnconstrained(Assembly):
	    """Unconstrained optimization of the Paraboloid with CONMIN."""
    
For the Paraboloid component, we create an execute function to tell it what to
do when the component is run. The *OptimizationUnconstrained* assembly does
not need an execute function because the Assembly class already has one that
is sufficient for most cases. However, this assembly does need an initialize
function to set parameters for the optimization. This is done using the
*__init__* function.

.. testcode:: simple_model_Unconstrained_pieces

    	    def __init__(self):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
	        super(OptimizationUnconstrained, self).__init__()

.. index:: StringRef
		
The __init__ function is called by the class constructor on a new
uninitialized instance of the class, so it's a good spot to set up any
parameters that CONMIN needs. The *super* command calls the
__init__ function of the parent (Assembly). This is required, and forgetting it
can lead to unexpected behavior.

Next, the Paraboloid and the CONMIN driver have to be instantiated and added
to OptimizationUnconstrained. The function *add_container* is used to add them
to the assembly.

.. testcode:: simple_model_Unconstrained_pieces

	        # Create Paraboloid component instances
	        self.add_container('paraboloid', Paraboloid())

	        # Create CONMIN Optimizer instance
	        self.add_container('driver', CONMINdriver())
		
Here we make an instance of the *Paraboloid* component we created above and
give it the name paraboloid. Similarly we create an instance of the CONMIN
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
name combines the public variable name with its parents' names. You can think
of it as something similar to the path name in a file system, but using a "."
as a separator. This allows for two components to have the same variable name
while still assuring that you can refer to each of them uniquely. Here, the
*f_xy* output of the Paraboloid component is selected as the objective for
minimization.

StringRefs are also used to define the design variables (decision variables)
for the optimization problem. While CONMIN operates only on a single objective,
it allows multiple design variables. These are assigned in a Python list:
        
.. testcode:: simple_model_Unconstrained_pieces

	        # CONMIN Design Variables 
	        self.driver.design_vars = ['paraboloid.x', 
	                                 'paraboloid.y' ]
					 
Here, both *x* and *y* are chosen as the design variables. We can also add a range
of validity for these variables, which allows an unconstrained optimization to be
performed on what is essentially a bounded region. For this problem, we have
created a lower and an upper bound, constraining *x* and *y* to lie on [-50, 50].
        
.. testcode:: simple_model_Unconstrained_pieces

	        self.driver.lower_bounds = [-50, -50]
        	self.driver.upper_bounds = [50, 50]

The problem is now essentially ready to execute. CONMIN contains quite a few
additional control parameters, though the default values for many of them are
adequate. These parameters are detailed in :ref:`CONMIN-driver`.
		
.. testcode:: simple_model_Unconstrained_pieces

	        # CONMIN Flags
	        self.driver.iprint = 1
	        self.driver.itmax = 30
	        self.driver.fdch = .000001
	        self.driver.fdchm = .000001

The parameters specified here include the debug verbosity (*iprint*) and the number of
iterations (*itmax*). The relative and absolute step sizes for the
numerical gradient calculation are adjusted to reduce the step size for this
problem (*fdch* and *fdchm*). If the default values are used, only two places of
accuracy can be obtained in the calculated minimum because CONMIN's default step
size is too large for this problem.

This model is now finished, and ready to be run. The next section will show how this is done.
		
Executing the Simple Optimization Problem
------------------------------------------

To run our model, we need to create an instance of OptimizationUnconstrained and tell it to run. A
convenient way to do this is to add some code to the end of the file that contains OptimizationUnconstrained,
so that it can be executed in Python, either at the command line or in the Python shell. Using the conditional:

::

	``if __name__ == "__main__":``
	
we can include some Python code at the bottom of
``optimization_unconstrained.py``. It will execute only when we call it at the
command line or the shell, and not when another module imports it. So, the
final lines in this file are:

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
	    print "CONMIN Iterations: ", opt_problem.driver.iter_count
	    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                         opt_problem.paraboloid.y)
	    print "Elapsed time: ", time.time()-tt, "seconds"

.. testoutput:: simple_model_Unconstrained_run
    :hide:

    ...
    CONMIN Iterations:  5
    Minimum found at (6.666309, -7.333026)
    Elapsed time:  ... seconds
	    
						 
This block of code does four things. In the first statement, we create an
instance of the class *OptimizationUnconstrained* with the name
*opt_problem.* In the second statement, we set *opt_problem* as the top
assembly in the model hierarchy. This will be explained in a later tutorial.
In the fifth statement, we tell *opt_problem* to run. The model will execute
until the optimizer's termination criteria are reached. The rest of the
statements print the results and report the elapsed time.

Make sure that you have activated your Python environment, so that you have
access to OpenMDAO and the example problems. At the command prompt, type:

::

        python optimization_unconstrained.py
	
This should produce the output:

:: 

    [ CONMIN output not shown ]
    CONMIN Iterations:  5
    Minimum found at (6.666309, -7.333026)
    Elapsed time:  0.0558300018311 seconds

Now we are ready to solve a more advanced optimization problem with constraints.    
    
.. index:: constraints, CONMIN

Building a Model - Constrained Optimization using CONMIN
---------------------------------------------------------

Usually, an optimization problem also contains constraints that reduce the
design space. *Constraints* are equations or inequalities that are expressed as functions
of the design variables. 

In OpenMDAO, you can construct one with a StringRef using any available public
variables to build an expression with Python mathematical syntax. For CONMIN,
the *constraints* parameter is a list of inequalities that are defined to be
satisfied when they return a negative value or zero and violated when they
return a positive value.

We want to add the constraint *(y-x+15)<0* to the problem. The unconstrained
minimum violates this constraint, so a new minimum must be found by
the optimizer. We can add a constraint to our existing OptimizationUnconstrained
model by adding one line to the init function:

.. testcode:: simple_model_Unconstrained_pieces

        # CONMIN Constraints
        self.driver.constraints = ['paraboloid.y-paraboloid.x+15.0']

This new script should be saved as optimization_constrained.py. Execute it by typing:

::

        python optimization_constrained.py
	
When this is executed, it should produce the output:

:: 

    [ CONMIN output not shown ]
    CONMIN Iterations:  6
    Minimum found at (7.175775, -7.824225)
    Elapsed time:  0.0295481681824 seconds
    
Notice that the minimum of the constrained problem is different from the minimum of
the unconstrained problem.

This concludes an introduction to a simple problem of component creation and execution in
OpenMDAO. The next tutorial section introduces a problem with more complexity and
presents some more of the features of the framework.
