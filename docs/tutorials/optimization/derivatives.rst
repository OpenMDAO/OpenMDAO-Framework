.. index:: derivatives

.. _`Adding-Derivatives-to-Your-Components`:

Adding Derivatives to Your Components
======================================

Many optimization algorithms make use of gradients. In our simple example
problem, the SLSQP driver estimates the gradient at various times during the
solution procedure by performing a local finite-difference step. Calculating
the gradient typically involves one or more executions of the objective
function depending on the finite difference method that is used. This, of
course, means that your model is executed additional times each iteration.

Sometimes the solution process can be sped up by having a component supply
its own derivatives. These derivatives may be analytical (as you will see in
this example), or they might be estimated by some other means. Additionally,
these derivatives can be more accurate than those estimated by finite
differencing the component, and they are not dependent on the right choice of
a step-size parameter.

.. index:: Finite Difference

In OpenMDAO, derivatives can be specified in the component API by following
these two steps:

:: 
 
   1. Declare a ``linearize`` method that calculates and saves the Jacobian that 
      contains the derivatives between its numerical outputs and inputs.
   2. Declare a ``provideJ`` method that returns the Jacobian along with a 
      list of inputs and outputs.


Let's look at an example, using the Paraboloid component, to see how this would work in 
practice. Starting with the original code, but calling our new
class ``ParaboloidDerivative``, we have:

.. testcode:: Paraboloid_derivative

    from numpy import array

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Float
    
    class ParaboloidDerivative(Component):
        """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """
    
        # set up interface to the framework  
        x = Float(0.0, iotype='in', desc='The variable x')
        y = Float(0.0, iotype='in', desc='The variable y')

        f_xy = Float(iotype='out', desc='F(x,y)')


The first function we need to add is ``linearize``. This function calculates
and saves a matrix (called the Jacobian) of the derivatives of the
component's outputs with respect to its inputs, evaluated at the current
state of the model. The paraboloid model has one input and two outputs, so
the Jacobian is a 1 by 2 numpy array. This also requires the numpy import
seen in the code above. The ``linearize`` function doesn't return the
Jacobian but instead stores it in our component. 


.. testcode:: Paraboloid_derivative

    def linearize(self):
        """Calculate the Jacobian"""
        
        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x
    
        self.J = array([[df_dx, df_dy]])

So ``self.J`` is the stored Jacobian that we will use later. The ``linearize``
function is called once when an optimizer asks for a gradient of its workflow. When
this component is part of a much larger model, it only contributes a small portion of
the full Jacobian. OpenMDAO uses a numerical method developed by `Martins and Hwang
<http://mdolab.engin.umich.edu/content/review-and-unification-discrete-methods-computing-derivatives-single-and-multi-disciplinary>`_ [1]
to solve for the gradient of the full problem. For large problems the Jacobian could
grow to be very large, and it would become impractical to construct the entire thing. 
Instead, the scheme used here represents the Jacobian as a linear operator or a
function that returns a product of the Jacobian with an input vector.  In this way,
the full Jacobian never needs to be stored. However, since the solution is  iterative,
a component's Jacobian needs to be queried multiple times after it is  calculated. So,
we need a method to provide the Jacobian and its ordering:

.. testcode:: Paraboloid_derivative

    def provideJ(self):
        """Provide full Jacobian."""
        
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        
        return input_keys, output_keys, self.J


Here ``input_keys`` and ``output_keys`` provide an index into the Jacobian
so OpenMDAO knows which columns correspond to each input and which rows to 
each output. For our paraboloid example, if you linearized around the point (0,0)
then the Jacobian would look like: 

+--------+--------+-------+
|        |  **x** | **y** |
+========+========+=======+
|**f_xy**| -6.0   |  8.0  |
+--------+--------+-------+

Note that you don't have to include all of the inputs and
outputs in the Jacobian. There is certainly no reason to provide the
derivative of inputs that are never hooked up to other
outputs or irrelevant to the gradient for some other reason. 

The ParaboloidDerivative component can be placed into a model, and the
derivatives will be used with no changes required to the
OptimizationConstrained or OptimizationUnconstrained assembly at this point.
If the driver uses gradients (or Hessians) and can take advantage of the
analytical ones you provide, then it will do so. Below is our model, using
the new component with derivatives. We put this model in a file called
:download:`optimization_constrained_derivative.py
<../../../examples/openmdao.examples.simple/openmdao/examples/simple/optimization_constrained_derivative.py>`.

.. literalinclude:: ../../../examples/openmdao.examples.simple/openmdao/examples/simple/optimization_constrained_derivative.py

.. [#] Martins, J. R. R. A., and Hwang, J. T., "Review and Unification of Methods for
       Computing Derivatives of Multidisciplinary Computational Models," `AIAA Journal,` 2013.

*Benchmarking*
~~~~~~~~~~~~~~

Sometimes it is useful to know how many times your component executes and
how many times it calculates its derivatives. OpenMDAO provides this
information for every component through a pair of counters: ``exec_count``
is incremented whenever a component executes, and ``derivative_exec_count``
is incremented whenever the derivatives are calculated. The following
example shows how they can be accessed and used.

.. doctest::
    :options: +SKIP

        >>> # Paraboloid Model
        >>>
        >>> from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
        >>> model = OptimizationConstrained()
        >>> model.run()
        >>> print model.paraboloid.exec_count
        10
        >>> print model.paraboloid.derivative_exec_count
        0
        >>> # Paraboloid Model with analytical derivatives
        >>>
        >>> from openmdao.examples.simple.optimization_constrained_derivative import OptimizationConstrained
        >>> model = OptimizationConstrained()
        >>> model.run()
        >>> print model.paraboloid.exec_count
        4
        >>> print model.paraboloid.derivative_exec_count
        3

Here we've printed out the number of function and derivative executions for
the paraboloid examples, both without and with analytical derivatives.
Because this model is a simple equation, the advantage of using the
analytical derivative isn't evident in a comparison of the clock time, but
the number of functional executions is much lower when you have them, at a
cost of a small number of derivative evaluations.
        
This concludes an introduction to OpenMDAO using a simple problem of
component creation and execution. The next tutorial introduces a problem with
more complexity and presents additional features of the framework.

*Finite Difference*
~~~~~~~~~~~~~~~~~~~

If you don't specify a ``provideJ`` function for you component, then OpenMDAO
will finite difference it during the calculation of the full model gradient. OpenMDAO
can identify groups of nondifferentiable components to finite difference as a block.
Also, OpenMDAO can detect a non-differentiable connection between two differentiable
components (e.g, components passing a file or string) and will include both components
in with the nondifferentiables.

At present, the user doesn't have much direct control over the finite difference,
but a stepsize can be assigned to any Float or Array input as a variable attribute:

.. testcode:: FD1

        x = Float(0.0, iotype='in', desc='The variable x', fd_step=0.01)
        
The default stepsize is 1.0e-6, which will not be adequate for your problem if your
variable is very large or small, so it is essential to choose this value carefully.