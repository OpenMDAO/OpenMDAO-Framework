.. index:: simple example

.. _Getting-Started-with-OpenMDAO:

Getting Started with OpenMDAO: A Simple Example Problem
=======================================================

The purpose of this section is to teach the novice user how to set up and
execute a simple optimization problem using the OpenMDAO script interface. This
tutorial assumes that you are familiar (though not necessarily proficient)
with the Python language and have a reasonable understanding of the concepts
presented in :ref:`Overview-of-the-OpenMDAO-Framework`, particularly the
concepts of the :term:`Component`, :term:`Assembly`, and :term:`Driver`.

The problem we present here is a paraboloid that is a function of two design 
variables. The optimization goal is to find the minimum values of this function
over the design space. For the simplest form of this problem, there are no
constraints, and we assume that the analytical derivative of the objective function is
unavailable. The problem can be expressed graphically as follows:


.. _`OpenMDAO_overview`:

.. figure:: ../../examples/openmdao.examples.simple/openmdao/examples/simple/Simple1.png
   :align: center

   A Simple Optimization Problem
   
This graphical view of the problem also helps in understanding how to map the
problem onto the OpenMDAO framework. The CONMIN optimizer is the :term:`Driver`,
and its job is to manipulate the two design variables (*x* and *y*) to 
minimize the output of the paraboloid function (*f*). The objective equation fits
into the OpenMDAO process as a :term:`Component`. More specifically, this 
Paraboloid component contains an execute function which takes the inputs 
(*x* and *y*) and returns the value of the function (*f*) evaluated at that design 
point. Finally, both the driver and the component are contained in an 
:term:`Assembly` which maintains the connections between the driver and the
component, and knows how to run the system.

We assume that you are familiar with Python and the basic concepts of object-oriented
programming, and have either installed an official distribution bundle or have access to the
OpenMDAO source tree. The following instructions will help you locate the directory containing
the pieces needed for the model relative to the install directory.

If you have a branch from the source repository:

	``examples/openmdao.examples.simple/openmdao/examples/simple``
	
If you have a distribution bundle:

	``buildout/eggs/openmdao.examples.simple-x.x.x-xxxxxx.egg/openmdao/examples/simple``
	
.. index:: Component

Building a Simple Component - Paraboloid
-----------------------------------------

At the highest level, a component is simply something that takes a set of
inputs and operates on them, producing a set of outputs. In the OpenMDAO
architecture, a class called :term:`Component` provides this behavior. Any
component has inputs and outputs and contains a function that executes the
component. This function operates on the inputs to produce the outputs. Our
purpose in creating a component for the Paraboloid equation is to provide an
object that can evaluate the objective function during the optimization process.
The Python code for the Paraboloid component is as follows:

.. testcode:: simple_component_Paraboloid

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
    class Paraboloid(Component):
        """ Evaluates the equation (x-3)^2 + xy + (y+4)^2 = 3 """
    
	# set up interface to the framework  
	x = Float(0.0, iotype='in', desc='The variable y')
        y = Float(0.0, iotype='in', desc='The variable x')

        f_xy = Float(0.0, iotype='out', desc='F(x,y)')        

        
	def execute(self):
	    """ Solve (x-3)^2 + xy + (y+4)^2 = 3
	        Optimal solution (minimum): x = 6.6667; y = -7.3333
	        """
        
	    x = self.x
	    y = self.y
        
	    self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

We will now explain the details of this. One thing to note is that a component
is implemented in the OpenMDAO framework by writing Python code and placing
it in a file. Typically, a file will contain one component, although it is 
possible to include more than one component in a single file. The file 
``paraboloid.py`` contains the code shown above. Later in this tutorial we will discuss how to
execute a model containing this component.

Python is a very extensible language and comes with a convenient way to manage
and load add-ons and extensions. The OpenMDAO source was also structured to
allow its functions and classes to follow a namespace convention (i.e., dotted
paths that compartmentalize the functions). Additionally, we added two special namespaces
called *openmdao.main.api* and *openmdao.lib.api*; the former contains
some of the more commonly used infrastructure functions, while the latter includes
commonly used plugins from the OpenMDAO Standard Library.

.. testcode:: simple_component_Paraboloid_pieces

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
These first two lines in the Paraboloid component contain the two inputs that
are used here: *Float* and *Component.* One guideline that should always be followed
is to import only what you need.

The very next line creates a class called Paraboloid:

.. testcode:: simple_component_Paraboloid_pieces

    class Paraboloid(Component):
        """ Evaluates the equation (x-3)^2 + xy + (y+4)^2 = 3 """
	
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
Another side benefit is that you don't have to worry about any of the
framework back-end; typically there are just two functions that you would
provide -- one for initialization (any calculation that needs to be done
before the optimization loop), and one to execute the component (provide the 
objective evaluation.)

Note that if we stop here, we have a Paraboloid component with no inputs, no 
outputs, and an execute function that does nothing. The first thing we need
to do is to define the inputs and outputs. We do this in the class definition
by adding these lines:

.. testcode:: simple_component_Paraboloid_pieces

	# set up interface to the framework  
	x = Float(0.0, iotype='in', desc='The variable y')
        y = Float(0.0, iotype='in', desc='The variable x')

        f_xy = Float(0.0, iotype='out', desc='F(x,y)')  


.. index:: Traits

There are two kinds of variables in OpenMDAO: *internal variables* and *Public Variables*.
Internal variables are variables that are used internally to a component but that cannot
be seen outside of that component's scope. Public Variables are variables that are 
publicly visible (and manipulatable if they are inputs) in the framework.

Here we are using a Public Variable called *Float,* which was imported above, that creates
a floating point variable available to the framework. The constructor contains
a default value (set to 0 for these), an *iotype* (which declares this 
variable as an input or an output), and a *desc*, or description (just a string of text
that describes this variable). (This will be more useful in the GUI.) For the
Paraboloid component, we've created two inputs and one output. Note that the two
inputs can be set by something else in the model, whether that be triggered
by an optimization or by the change of some other component output to which
this input is connected. The variable names are limited to those names that are
valid as Python variables.

Finally, we need a function to execute this component:

.. testcode:: simple_component_Paraboloid_pieces

	def execute(self):
	    """ Solve (x-3)^2 + xy + (y+4)^2 = 3
	        Optimal solution (minimum): x = 6.6667; y = -7.3333
	        """
        
	    x = self.x
	    y = self.y
        
	    self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0
	    
The execute function is where you define what a component will do when it is told to run. For
the Paraboloid component, the  equation for the paraboloid goes here. Note that the framework
variables are  accessed as members of the Paraboloid class, meaning that *self.x* returns the
value of the framework input *x.* To make the equation clearer, *self.x* was assigned to *x*
and *self.y* was assigned to *y* before the equation. Note also that the output value is
assigned here similarly via *self.f_xy.* This changes the value of the framework variable and
completes the component execution.

It will often be the case that you will already have the code for evaluating the objective function,
but it will be in some other language, such as Fortran or C/C++. The :ref:`Plugin-Developer-Guide` 
gives some examples of how to incorporate these kinds of components into OpenMDAO.

The Paraboloid component is now built and ready for inclusion in a model.


Building a Simple Model - Unconstrained Optimization using CONMIN
------------------------------------------------------------------

The next task is to build a model that finds the minimum objective value for the
Paraboloid component described above. This model will contain the Paraboloid as well as
a public domain gradient optimizer called :term:`CONMIN`, for which a Python-wrapped
driver has been included in the OpenMDAO standard library. The model can be found in
the Python file ``optimization_unconstrained.py``:

.. testcode:: simple_model_Unconstrained

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


.. index:: top level assembly
 		
In OpenMDAO parlance, we usually describe this as the *top level Assembly.* An 
:term:`Assembly` is a container that can hold some number of components, drivers, and 
other assemblies. An Assembly also manages the interconnections between the
components and assemblies that it owns, and it has its own workflow, which it
uses to execute the components and drivers in the correct order. For our
problem, this top level Assembly will include a Paraboloid component and a 
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
    	    """ Top level assembly for optimizing a vehicle. """
    
For the Paraboloid component, we created an execute function to tell it what to
do when the component is run. This is not needed for the 
*OptimizationUnconstrained* assembly because the Assembly class already has an
execution function that should be usable for most cases. However, this assembly
does need an initialize function to set parameters for the optimization. This
is done using the *__init__* function.

.. testcode:: simple_model_Unconstrained_pieces

    	    def __init__(self, directory=''):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
	        super(OptimizationUnconstrained, self).__init__(directory)

.. index:: StringRef, constructor
		
This initialize function is actually a special function called a *constructor,*
which is the function that instantiates an object. The double leading and
trailing underscores are a required part of the syntax. This function executes
once when the top Level Assembly is created, so it's a good spot to set up
any parameters that are needed for CONMIN. The *super* command calls the
constructor of the parent (Assembly); this is also required.

Next, the Paraboloid and the CONMIN driver have to be created (instantiated.)
This is done using the function *add_container,* which is one of the framework
management functions that are part of the Assembly class.

.. testcode:: simple_model_Unconstrained_pieces

	        # Create Paraboloid component instances
	        self.add_container('paraboloid', Paraboloid())

	        # Create CONMIN Optimizer instance
	        self.add_container('driver', CONMINdriver())
		
Here, a Paraboloid component is created and given the name *paraboloid.* Similarly
a CONMIN driver is created and given the name *driver.* As with other class
members, these now become accessible via *self.paraboloid* and *self.driver.*
		
The objective function is defined using the concept of a StringRef variable:		
        
.. testcode:: simple_model_Unconstrained_pieces

	        # CONMIN Objective 
	        self.driver.objective = 'paraboloid.f_xy'
		
A *StringRef* is a special kind of Public Variable that contains a string that points to
some location in the OpenMDAO variable tree. This string is analogous to the
path name in a file system, using the "." as a separator. This allows for two
components to have the same variable name while still assuring they'll be
uniquely referable. Here, the *f_xy* output of the Paraboloid component is
selected as the objective for minimization.

StringRefs are also used to define the design variables (decision variables)
for the optimization problem. While CONMIN operates only on a single objective,
it allows multiple design variables. These are assigned in a Python list:
        
.. testcode:: simple_model_Unconstrained_pieces

	        # CONMIN Design Variables 
	        self.driver.design_vars = ['paraboloid.x', 
	                                 'paraboloid.y' ]
					 
Here, both *x* and *y* are chosen as the design variables. We can also add a range
of validity for these variables. CONMIN provides a specialized constraint
called a *Side Constraint,* which allows an unconstrained optimization to be
performed on what is essentially a bounded problem. For this problem, we have
created a lower and an upper bound, constraining *x* and *y* to lie on [-50, 50].
        
.. testcode:: simple_model_Unconstrained_pieces

	        self.driver.lower_bounds = [-50, -50]
        	self.driver.upper_bounds = [50, 50]

The problem is now essentially ready to execute. CONMIN contains quite a few
additional control parameters; these are detailed in :ref:`CONMIN-driver`.
		
.. testcode:: simple_model_Unconstrained_pieces

	        # CONMIN Flags
	        self.driver.iprint = 0
	        self.driver.itmax = 30
	        self.driver.fdch = .000001
	        self.driver.fdchm = .000001

The ones used here include the debug verbosity (*iprint*) and the number of
iterations (*itmax*). Additionally, the relative and absolute step sizes for the
numerical gradient calculation are adjusted to reduce the step size for this
problem (*fdch* and *fdchm*). If the default values are used, only two places of
accuracy can be obtained in the calculated minimum because the default step
size is too large for this problem.
		
Executing the Simple Optimization Problem
-----------------------------------------

In the absence of an OpenMDAO GUI, we must take one other step to set up and execute
this optimization problem. We have to create the top Level Assembly and tell it to run. One
convenient way to do this is to include some code in the top level Assembly file that
allows execution in Python, either at the command line or in the shell. Using the check
``if __name__ == "__main__":`` we can include some Python code at the bottom of
``optimization_unconstrained.py``; it will execute only when we call it at the command line or
the shell. So, the final lines in this file are:

.. testsetup:: simple_model_Unconstrained_run

	from openmdao.main.api import set_as_top
	from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
	__name__ = "__main__"

.. testcode:: simple_model_Unconstrained_run

	if __name__ == "__main__": 

	    opt_problem = OptimizationUnconstrained("Top")
	    set_as_top(opt_problem)
	    opt_problem.run()

	    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.get("x"), \
                                                 opt_problem.paraboloid.get("y"))
						 
This fragment of code really does just four things. In the first statement, an
instance of the class *OptimizationUnconstrained* is created with the name 
*opt_problem.* In the second statement, *opt_problem* is set as the top assembly in
the model hierarchy. In the third statement, *opt_problem* is told to run, which executes
the model until the optimizer's termination criteria are reached. Finally, the 
fourth statement prints the results.

This script can be executed in the shell by going to the
``...../openmdao/examples/simple`` directory, and typing:

::

        [Path to your OpenMDAO install]/buildout/bin/python optimization_unconstrained.py
	
This should produce the output:

.. testoutput:: simple_model_Unconstrained_run

    Minimum found at (6.666309, -7.333026)

An *OptimizationUnconstrained* top level Assembly is instantiated and given the
name *opt_problem.* This created the problem and instantiates a Paraboloid and
a CONMIN driver. The run function is used to run the model, which solves the
optimization problem as set up above. And last, the final design variables are
accessed using the get function on the Paraboloid component, which is
accessible even from outside the top level Assembly.

.. index:: contraints, CONMIN

Building a Simple Model - Constrained Optimization using CONMIN
---------------------------------------------------------------

Usually, an optimization problem also contains a number of constraints on the
design space. 

*Constraints* are equations (generally inequalities) that are expressed as functions
of the design variables. In OpenMDAO, they are constructed much like the objective functions
using the available framework variables to build an expression with Python
mathematical syntax. For CONMIN, the constraints parameter is a list of inequalities that
are defined to be satisfied when they return a negative value or zero and violated
when they return a positive value. We can add a constraint to our existing 
model by adding another line to the init function:

.. testcode:: simple_model_Unconstrained_pieces

        # CONMIN Constraints
        self.driver.constraints = ['paraboloid.y-paraboloid.x+15.0']

Here, the constraint *(y-x+15)<0* is added to the problem. The unconstrained
minimum actually violates this constraint, so a new minimum must be found.
The optimizer will now return the minimum solution that does not violate
this constraint. 

When this is executed, it should produce the output:

.. testoutput:: simple_model_Constrained_run

    Minimum found at (7.175775, -7.824225)



Afterword
---------

This concludes an introduction to a simple problem of component creation and execution in
OpenMDAO. The next tutorial section introduces a problem with more complexity and
presents some more of the features of the framework.
