
.. index:: Component

Creating New Components
=======================

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

