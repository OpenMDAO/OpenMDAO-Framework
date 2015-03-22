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

   #  Define a ``list_deriv_vars`` function that tell openmdao which inputs and outputs you have derivatives w.r.t and of
   #. Define a ``provideJ`` method that calculates and returns the Jacobian.


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


The two function we need to add are ``list_deriv_vars`` and ``provideJ``.
The first function indicates which derivatives you're proving.
The order that you provide the variables in is important. The order of the variables
is important. The first set, the inputs, is given in the same order as the columns
of the Jacobian. The second set, the outputs, is given in the same order as the rows of the
Jacobian. The second function calculates and returns a matrix (the Jacobian)
of the derivatives, evaluated at the current state of the model.
The paraboloid model has two inputs and one output, so
the Jacobian is a 1 by 2 numpy array. If you linearized around the point (0,0)
then the Jacobian would look like:

+--------+--------+-------+
|        |  **x** | **y** |
+========+========+=======+
|**f_xy**| -6.0   |  8.0  |
+--------+--------+-------+

Here's what the code to implement these derivatives looks like.

.. testcode:: Paraboloid_derivative

    def list_deriv_vars(self):
        """specified the inputs and outputs where derivatives are defined"""
        return ('x', 'y'), ('f_xy',)

    def provideJ(self):
        """Calculate the Jacobian"""

        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x

        J = array([[df_dx, df_dy]])
        return J

So ``J`` is the Jacobian that OpenMDAO will use when assembling the system level derivatives. If
this component was part of a much larger model with other components, it only contributes
a small portion of the full Jacobian. OpenMDAO uses a numerical method developed by
`Martins and Hwang <http://mdolab.engin.umich.edu/content/review-and-unification-discrete-methods-computing-derivatives-single-and-multi-disciplinary>`_ [1]
to solve for the gradient of the full problem.

.. note::
    You don't have to include all of the inputs and
    outputs in the Jacobian. There is certainly no reason to provide the
    derivative of inputs that are never hooked up to other
    outputs or irrelevant to the gradient for some other reason. If you omit derivatives,
    which end up being needed as part of the optimization OpenMDAO will throw an error
    to alert you of problem.

The ParaboloidDerivative component can be placed into a model, and the
derivatives will be used with no changes required to the
OptimizationConstrained or OptimizationUnconstrained assembly at this point.
If the driver uses gradients and can take advantage of the
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

If you don't specify any derivatives (you don't define ``list_deriv_vars`` or ``provideJ`` functions) for your component, then OpenMDAO
will finite difference it during the calculation of the full model gradient. OpenMDAO
can identify groups of non-differentiable components to finite difference as a block.
Also, OpenMDAO can detect a non-differentiable connection between two differentiable
components (e.g, components passing a file or string) and will include both components
with the non-differentiables.

There are a number of ways to control how OpenMDAO finite differences your
components and your full model. Every driver contains a variable tree called
``gradient_options``. This tree contains the settings that control how that
driver performs a finite difference. Note that since each driver has one, it
is possible to use different settings for different drivers. Consider the same
example from above, but let's see how you can change some settings.

.. testcode:: Paraboloid_derivative

        from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
        model = OptimizationConstrained()
        model.driver.gradient_options.fd_form = 'central'
        model.driver.gradient_options.fd_step = 1.0e-3
        model.driver.gradient_options.fd_step_type = 'relative'

The default form for finite difference is a forward difference, but sometimes
you may want the second order accuracy of a central difference (and you are
fine with the extra execution per call.) The default stepsize is 1.0e-6,
which will not be adequate for your problem if your variable is very large or
small, so it is essential to choose this value carefully. Fiinally, the
default step type is ``'absolute'``, but you may want to set it to ``'relative'`` for
variables that have a wider range of possible magnitudes. Relative
differencing calculates a step size by taking the current variable value and
multipying it by the ``fd_step`` value.

You can also tell a driver to ignore all analytic derivatives and just use finite
difference.

.. testcode:: Paraboloid_derivative

        from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
        model = OptimizationConstrained()
        model.driver.gradient_options.force_fd = True

When you use this setting, OpenMDAO will finite difference your problem from the inputs to the
outputs as one large block.

Finally, there are a couple of settings for the analytic solution of the system equations
that yields the derivatives. OpenMDAO uses Scipy's GMRES solver, and it exposes both its
tolerance and its maximum iteration count to be controlled by the user.

.. testcode:: Paraboloid_derivative

        from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
        model = OptimizationConstrained()
        model.driver.gradient_options.atol = 1.0e-9
        model.driver.gradient_options.maxiter = 100


For fine control of the finite difference stepsize, some of the global
settings can also be overriden by specifying them as metadata in the
`Variable` definition. Consider the following variables:

.. testcode:: Paraboloid_derivative

        x1 = Float(0.0, iotype='in', fd_step=0.01)
        x2 = Float(0.01, iotype='in', fd_step_type='relative')
        x3 = Float(1000.0, iotype='in', fd_step = 0.1, fd_form='central')

Here, we have locally set the finite difference stepsize on x1 to 0.01. For x2,
we chose to use a relative stepsize instead of an absolute one, which means that the
global stepsize of 1.0e-6 that is set in the driver is applied to this variable as
a relative stepsize. Finally, x3 assigns a stepsize of 0.1, but a central difference
will be performed instead of a forward difference.

You may also want to force OpenMDAO to finite difference a component even though you
have defined derivatives for it. You can do this by setting its ``force_fd`` attribute
to True.

.. testcode:: Paraboloid_derivative
    :hide:

    from openmdao.main.api import Assembly, set_as_top

.. testcode:: Paraboloid_derivative

    top = set_as_top(Assembly())
    top.add('my_comp', ParaboloidDerivative())
    top.my_comp.force_fd = True


*Complex Step*
~~~~~~~~~~~~~~~~~~~

You can also choose to calculate the derivatives with the Complex Step method instead of finite
difference. Its advantage over finite difference is that its accuracty is not sensitive to the choice of
stepsize. However, to use this method, your model needs to be able to operate on complex inputs and
produce complex outputs. This will already be true of most python modules, but your external codes
may need special modification to use complex step.

To use complex step to calculate the gradient during the paraboloid optimization:

.. testcode:: Paraboloid_derivative

        from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
        model = OptimizationConstrained()
        model.driver.gradient_options.fd_form = 'complex_step'
        model.driver.gradient_options.fd_step = 1.0e-3
        model.driver.gradient_options.fd_step_type = 'relative'

you can also specify it on a specific model inputs:

.. testcode:: Paraboloid_derivative

        x = Float(0.0, iotype='in', fd_step=0.01, fd_form = 'complex_step')
        y = Float(0.0, iotype='in', fd_step=0.01)
