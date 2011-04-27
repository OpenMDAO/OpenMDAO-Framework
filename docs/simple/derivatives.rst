.. index:: derivatives

.. _`Adding-Derivatives-to-Your-Components`:

Adding Derivatives to Your Components
======================================

Optimizers such as CONMIN are gradient optimizers because they move toward the
optimum value by traveling in the direction of the steepest gradient of the
objective function. In our simple example problem, the CONMIN driver estimates
the gradient at various times during the solution procedure by performing a
local finite-difference step. Calculating the gradient typically involves one or
more executions of the objective function depending on the finite difference
method that is used. This, of course, means that your model is executed 
additional times each iteration.

Sometimes, the solution process can be sped up by having a component supply its own
derivatives. These derivatives may be analytical (as you will see in this example),
or they might be estimated by some other means. Additionally, these derivatives can
be more accurate than those estimated by finite differencing the component, and
they are not dependent on the right choice of a step-size parameter.

.. index:: Fake Finite Difference

In OpenMDAO, derivatives can be specified in the component API. A component's a set of specified
derivatives is used to replace that component's output with the first-order Taylor series expansion
whenever the optimizer initiates a finite difference estimation of the gradient. This is called
*Fake Finite Difference*. It provides an efficient way of calculating gradients for mixed models --
models with components that can provide derivatives and those that cannot. Via Fake Finite
Difference you can specify gradients (first derivatives) and Hessians (second derivatives) in mixed
models. The CONMIN driver uses only gradients, but the NEWSUMT optimizer can use both gradients and
Hessians. For details, see the :ref:`Derivatives` section in the *Script Interface.*


Two steps are involved in specifying derivatives for a component:

:: 
 
  1. Declare derivatives in the "__init__" method
  2. Calculate the derivatives in the "calculate_derivatives" method

You must declare the derivatives that you want to define so that the component can
be checked for missing derivatives. In declaration, you aren't defining a value
but just declaring that this derivative is needed and provided by the component. In
the general case, you need to have derivatives for all possible permutations
between the inputs and outputs of your component. However, during any specific
optimization, you only need the derivatives for inputs that are connected to
upstream components and outputs that pass info to downstream components. This set
can be reduced further when you consider that you only need the inputs and outputs
that are active in the loop between the optimizer's parameters and its objective and
constraints. Presently, derivatives are valid only for the `Float` variable type.

.. index:: sparse matrix

Derivative declaration is guided by the *sparse matrix* policy: if you don't
declare a derivative, it is assumed to be zero. You don't have to actively
set it to zero, and there is no superfluous multiplication by zero in any part
of the calculation. This philosophy leads to a clean interface and efficient
calculation, but the burden is on the component developer to make sure not
to miss a declaration of a derivative for an important output pair.

You can add analytical derivatives to the Paraboloid component by adding the
two functions mentioned above.

The ``__init__`` method is a function that every class calls when it is instantiated.
We need to add an ``__init__`` method that defines derivatives between the inputs
(`x, y`) and the output ``f_xy``. Let's add both first and second derivatives.

.. testcode:: Paraboloid_derivative
    :hide:
    
    from openmdao.examples.simple.paraboloid import Paraboloid
    self = Paraboloid()

.. testcode:: Paraboloid_derivative

    def __init__(self):
        """ declare what derivatives that we can provide"""
        
        super(Paraboloid_Derivative, self).__init__()

        self.derivatives.declare_first_derivative(self, 'f_xy', 'x')
        self.derivatives.declare_first_derivative(self, 'f_xy', 'y')
        self.derivatives.declare_second_derivative(self, 'f_xy', 'x', 'x')
        self.derivatives.declare_second_derivative(self, 'f_xy', 'x', 'y')
        self.derivatives.declare_second_derivative(self, 'f_xy', 'y', 'y')

The ``super`` command executes the parent's ``__init__`` function. **This is
required for the component to behave properly in OpenMDAO, so don't forget to
include it.**

Also, don't forget the cross-variable terms when declaring second derivatives
(in this case, the second derivative of ``f_xy`` with respect to `x` **and** `y`.)

Next, we define the ``calculate_derivatives`` method.

.. testcode:: Paraboloid_derivative

    def calculate_derivatives(self, first, second):
        """Analytical derivatives"""
        
        if first:
        
            df_dx = 2.0*self.x - 6.0 + self.y
            df_dy = 2.0*self.y + 8.0 + self.x
        
            self.derivatives.set_first_derivative('f_xy', 'x', df_dx)
            self.derivatives.set_first_derivative('f_xy', 'y', df_dy)
        
        if second:
        
            df_dxdx = 2.0
            df_dxdy = 1.0
            df_dydy = 2.0
            
            self.derivatives.set_second_derivative('f_xy', 'x', 'x', df_dxdx)
            self.derivatives.set_second_derivative('f_xy', 'x', 'y', df_dxdy)
            self.derivatives.set_second_derivative('f_xy', 'y', 'y', df_dydy)
            
This ``calculate_derivatives`` method calculated both the first and second
derivatives, but we can take advantage of two Booleans, *first* and *second*,
so that we perform only the calculation that is requested by the optimizer.
Note that the Hessian matrix is symmetric, so ``df/dxdy`` is the same as
``df/dydx``, and only one has to be set.

Note that no changes are required to the OptimizationConstrained or
OptimizationUnconstrained assembly at this point. If the driver uses
gradients (or Hessians) and can take advantage of the analytical ones
you provide, then it will do so.

This concludes an introduction to OpenMDAO using a simple problem of component creation and
execution. The next tutorial introduces a problem with more complexity and presents additional
features of the framework.

