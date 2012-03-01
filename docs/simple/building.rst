.. index:: Component

Building a Component - Paraboloid
==================================

A component takes a set of inputs and operates on them to produce a set of
outputs. In the OpenMDAO architecture, a class called *Component*
provides this behavior. Any Component has inputs and outputs and
contains a function called *execute* that calculates the outputs based on the
values of the inputs. Let's take a look at how you would implement the
paraboloid as an OpenMDAO component:

.. testcode:: simple_component_Paraboloid

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Float
    
    
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

Your component should look pretty close to this when it is complete. 
To implement a component in the OpenMDAO framework, you write some Python
code and place it in a file. This file is called a *module* in Python.
Typically, a module will contain one component, although you can include more
than one component in a single file. 

In Python, a class or function must be imported before it can be used. Most of what you need in OpenMDAO
can be imported from: ``openmdao.main.api`` and the ``openmdao.lib`` api modules: 
``openmdao.lib.caseiterators.api``, ``openmdao.lib.caserecorders.api``,
``openmdao.lib.components.api``,  ``openmdao.lib.datatypes.api``, ``openmdao.lib.doegenerators.api``,
``openmdao.lib.drivers.api``, and ``openmdao.lib.surrogatemodels.api``.

The first two lines in the ``paraboloid.py`` module import the definitions
of the Component class and the Float class. You will use these in the definition
of your Paraboloid class. Open an editor and create a file called ``paraboloid.py``.
Type these two lines into that file:

.. testcode:: simple_component_Paraboloid_pieces

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Float
    
There are many other objects you could impoprt from ``openmdao.main.api`` and ``openmdao.lib.datatypes.api``, but you
only import the classes that you need for your particular component to keep things neater. In other words:

.. testcode:: package

    # BAD
    from openmdao.main.api import *
    
    # INCONVENIENT
    import openmdao.main.api
    
    # GOOD
    from openmdao.main.api import Component

The next line defines a class called *Paraboloid:*

.. testcode:: simple_component_Paraboloid_pieces

    
    class Paraboloid(Component):
        """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
.. index:: classes, functions

You define the Paraboloid class by deriving it from the Component class. All of your analyses 
will derive from the Component class and typically there are just two functions that you
provide -- one for initialization (anything that needs to be set up once) and one to execute the
component (calculate the outputs from the inputs.)

Right now, your paraboloid class is defined but has no inputs, no 
outputs, and an ``execute`` function that does nothing. So the next thing you need
to do is define the inputs and outputs in the class definition
by adding these lines:

.. testcode:: simple_component_Paraboloid_pieces

        # set up interface to the framework  
        x = Float(0.0, iotype='in', desc='The variable x')
        y = Float(0.0, iotype='in', desc='The variable y')

        f_xy = Float(iotype='out', desc='F(x,y)')

.. index:: Traits

In Python, all objects have *attributes*, but making all of those attributes
visible to the framework would be overwhelming, so OpenMDAO requires you to
declare what we call *Variables* to indicate the specific inputs and outputs
that you want your component to make available to other components. Variables
are usually declared in the class definition of a component.

In this example, all of your inputs and outputs are floating point numbers, so
you will use a type of variable called *Float*. The Float constructor contains
a default value and some arguments. The default value has been set to zero for
the `x` and `y`.

The argument *iotype* declares this variable as an input or an output. This
argument is required. If it is omitted (or misspelled), then the variable
won't be visible in the framework.

The argument *desc* contains a description, or a string of text that describes this
variable. This argument, while not required, is encouraged.

Finally, you need a function to execute this component:

.. testcode:: simple_component_Paraboloid_pieces

    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """
        
        x = self.x
        y = self.y
        
        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0
        
The ``execute`` function is where you define what a component does when it runs.
For your Paraboloid component, the equation is evaluated here. The input and
output variables are attributes of the Paraboloid class, which means that
they must be accessed using *self*. For example, ``self.x`` gives you the value
stored in x. This ``self.`` can be cumbersome in a big equation, so a pair of
internal variables, *x* and *y*, are used in the calculation.

Often, you will already have the code for evaluating your component outputs,
but it will be in some other language, such as Fortran or C/C++. The :ref:`Plugin-Developer-Guide` 
gives some examples of how to incorporate these kinds of components into OpenMDAO.

To make sure this component works, try running it. The first thing you must do before 
running OpenMDAO is to activate the environment. You need to do this anytime you want 
to run code in OpenMDAO. If you can't remember how to activate your environment, then refer to the 
instructions in the section on :ref:`installation <Installation>`.

Once you have activated your environment, you can run the Python interperater by typing
::

    python

at the command prompt. Now you will create an instance of your Paraboloid component,
set a new value for each of the inputs, run the component, and look at the output.

::

    >>> from paraboloid import Paraboloid
    >>> my_comp = Paraboloid()
    >>> my_comp.x = 3
    >>> my_comp.y = -5
    >>> my_comp.run()
    >>> my_comp.f_xy
    -17.0

If you have done everything correctly, you should also get ``-17.0`` as the solution.    

The Paraboloid component is now built and ready for inclusion in a larger model. If you want to download
our version of this file, you can find it 
:download:`here <../../examples/openmdao.examples.simple/openmdao/examples/simple/paraboloid.py>`



    
