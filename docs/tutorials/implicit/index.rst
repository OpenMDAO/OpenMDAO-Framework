
.. _Tutorial:-Implicit:

Implicit Components
============================

In the basic tutorial, we learned how to define an OpenMDAO component that represents an
explicit function of outputs with respect to its inputs.

          ``y = F(x)``

OpenMDAO also allows us to define a component that represents an implicit function of the
same variables:

          ``R(x, y) = 0``

Here, the variable `x` is a known input that is passed to the implicit
component, but the variable `y`, called the `state`, is an unknown that needs
to be solved to satisfy the equation. The left-hand side of the
implicit equation is called the `residual`. Since an implicit function may
not have a closed-form solution, the `state` is typically determined by numerically
solving the residual equation, or in other words, iterating on the state until the
residual is driven to zero.

An implicit equation can be solved using the base `Component` class in
OpenMDAO as long as the component solves the residual equations in its
`execute` method. However, there are some advantages to letting OpenMDAO be
aware of states and residuals. First, this allows us to define analytic
derivatives so that the implicit component can form part of the coupled
solution of the full model gradient without having to resort to finite
difference. In addition, solving the residual equations can be performed by
OpenMDAO, both locally as part of the execution of the implicit component, as well
as globally if we want to converge our implicit component simultaneously with
other solver loops in the model.

For the sake of convenience, we've broadened the definition of an implicit component
to include additional inputs and outputs that aren't part of the implicit
equations.


              ``R(x, y) = 0``

              ``z = F(x, y, u)``

In this set of equations, `u` is an input that does not affect the residuals, and `z` is
an output that is not a state but satisfies an additional explicit equation. This is
analogous to the output equation of a state space model in control theory.

Let's set up a simple component that can solve this set of equations, three
of which are implicit while one is explicit:


              ``r0(x, y, z) = c*(3x + 2y - z) - 1 = 0``

              ``r1(x, y, z) = 2x - 2y + 4z + 2 = 0``

              ``r2(x, y, z) = -x + y/2. - z =0``

              ``y_out = c + x + y + z``

In these equations, the states are `x`, `y`, and `z`, and the residuals are
`r0`, `r1`, and `r2`. The variable `c` is a normal input, and `y_out` is an
explicit output. Note that the number of states must equal the number of
residuals for the system to have a unique and valid solution. We
can start defining our implicit component by inheriting from
`ImplicitComponent` instead of `Component`. This class allows the definition
of two additional iotypes: `state` and `residual`.

When an ImplicitComponent executes, it must solve its residual equations to
find its states. We can provide this method, or the user can implement a
``solve`` method on the component to converge the residuals. First, we will
show an example of an implicit component that solves itself, in this case, using
fsolve from scipy.optimize.

Let's write our first implicit component.

.. testcode:: implicit

    import numpy as np
    from scipy.optimize import fsolve

    from openmdao.main.api import ImplicitComponent
    from openmdao.main.datatypes.api import Float, Array

    class MyComp_No_Deriv(ImplicitComponent):
        ''' Single implicit component with 3 states and residuals.

        For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
        '''

        # External inputs
        c = Float(2.0, iotype="in", fd_step = .00001,
                  desc="non-state input")

        # States
        x = Float(0.0, iotype="state")
        y = Float(0.0, iotype="state")
        z = Float(0.0, iotype="state")

        # Residuals
        res = Array(np.zeros((3)), iotype="residual")

        # Outputs
        y_out = Float(iotype='out')

        def evaluate(self):
            """run a single step to calculate the residual
            values for the given state var values"""

            c, x, y, z = self.c, self.x, self.y, self.z

            self.res[0] = self.c*(3*x + 2*y - z) - 1
            self.res[1] = 2*x - 2*y + 4*z + 2
            self.res[2] = -x + y/2. - z

            self.y_out = c + x + y + z

        def solve(self):
            """Calculates the states that satisfy residuals using scipy.fsolve.
            You can override this function to provide your own internal solve."""

            x0 = self.get_state()
            fsolve(self._solve_callback, x0, xtol=1e-12)

        def _solve_callback(self, X):
            """This function is passed to the internal solver to set a new state,
            evaluate the residuals, and return them."""

            self.set_state(X)
            self.evaluate()
            return self.get_residuals()

We have taken our three residuals and placed them in a single variable array
called `res`, but we could also create a separate floating point variable
for each of them. Also, the initial values of our states serve as the
initial conditions for their iterative solution. Now, let's put this in an
assembly:

.. testcode:: implicit

    from openmdao.main.api import Assembly, set_as_top

    class Model(Assembly):

        def configure(self):
            self.add('comp', MyComp_No_Deriv())
            self.driver.workflow.add('comp')
            self.comp.eval_only = False

and run the model. We will let the implicit component solve its own residuals.

.. doctest:: implicit

        >>> top = set_as_top(Model())
        >>> top.run()
        >>> # The residuals will vary depending on your system, but should be near zero.
        >>> print top.comp.res
        [...]
        >>> print top.comp.x, top.comp.y, top.comp.z
        1.0 -2.3333... -2.1666...

The implicit component completes its iteration until the state values satisfy
the residual equations.

We can also configure an OpenMDAO solver to solve for the states. Here, we
set up a new assembly with the Broyden solver as the top driver. Then we
assign the states as the solver's parameters and constrain the residuals to
be equal to zero. Also, we don't want the implicit component's internal
solver to solve this in competition with the BroydenSolver solver, so we set
``eval_only`` to True. This means that running the implicit component just
runs the `eval` statement we defined in the class definition.

.. testcode:: implicit

    from openmdao.main.api import Assembly, set_as_top
    from openmdao.lib.drivers.api import BroydenSolver

    class Model2(Assembly):

        def configure(self):
            self.add('comp', MyComp_No_Deriv())
            self.comp.eval_only = True
            self.add('driver', BroydenSolver())
            self.driver.workflow.add('comp')
            self.driver.add_parameter('comp.x')
            self.driver.add_parameter('comp.y')
            self.driver.add_parameter('comp.z')
            self.driver.add_constraint('comp.res[0] = 0')
            self.driver.add_constraint('comp.res[1] = 0')
            self.driver.add_constraint('comp.res[2] = 0')

Now, when we run the model, we get the same solution for the state.

.. doctest:: implicit

        >>> top = set_as_top(Model2())
        >>> top.run()
        >>> # The residuals will vary depending on your system, but should be near zero.
        >>> print top.comp.res
        [...]
        >>> print top.comp.x, top.comp.y, top.comp.z
        1.0 -2.3333... -2.1666...

Finally, since one of the advantages to this implementation of implicit components is
in the derivative calculation, let's specify the analytic derivatives for this simple
set of equations using the ``apply_deriv`` and ``apply_derivT`` methods. To do this, we need
to provide all permutations of the derivatives: namely, the derivatives of the residuals
with respect to both the states and the explicit inputs, and the derivatives of the
explicit output with respect to both the states and the explicit inputs. Here, we specify
these as separate Jacobians in the ``provideJ`` method, but this was purely to make the
matrix-vector multiplication in ``apply_deriv`` and ``apply_derivT`` clean and simple.

.. testcode:: implicit

    class MyComp_Deriv(MyComp_No_Deriv):
        ''' This time with derivatives.
        '''

        def provideJ(self):
            #partial w.r.t c
            c, x, y, z = self.c, self.x, self.y, self.z

            dc = [3*x + 2*y - z, 0, 0]
            dx = [3*c, 2, -1]
            dy = [2*c, -2, .5]
            dz = [-c, 4, -1]

            self.J_res_state = np.array([dx, dy, dz]).T
            self.J_res_input = np.array([dc]).T

            self.J_output_input = np.array([[1.0]])
            self.J_output_state = np.array([[1.0, 1.0, 1.0]])

        def apply_deriv(self, arg, result):

            # Residual Equation derivatives
            res = self.list_residuals()[0]
            if res in result:

                # wrt States
                for k, state in enumerate(self.list_states()):
                    if state in arg:
                        result[res] += self.J_res_state[:, k]*arg[state]

                # wrt External inputs
                for k, inp in enumerate(['c']):
                    if inp in arg:
                        result[res] += self.J_res_input[:, k]*arg[inp]

            # Output Equation derivatives
            for j, outp in enumerate(['y_out']):
                if outp in result:

                    # wrt States
                    for k, state in enumerate(self.list_states()):
                        if state in arg:
                            result[outp] += self.J_output_state[j, k]*arg[state]

                    # wrt External inputs
                    for k, inp in enumerate(['c']):
                        if inp in arg:
                            result[outp] += self.J_output_input[j, k]*arg[inp]

        def apply_derivT(self, arg, result):

            # wrt States
            for k, state in enumerate(self.list_states()):
                if state in result:

                    # Residual Equation derivatives
                    res = self.list_residuals()[0]
                    if res in arg:
                        result[state] += self.J_res_state.T[k, :].dot(arg[res])

                    # Output Equation derivatives
                    for j, outp in enumerate(['y_out']):
                        if outp in arg:
                            result[state] += self.J_output_state.T[k, j]*arg[outp]

            # wrt External inputs
            for k, inp in enumerate(['c']):
                if inp in result:

                    # Residual Equation derivatives
                    res = self.list_residuals()[0]
                    if res in arg:
                        result[inp] += self.J_res_input.T[k, :].dot(arg[res])

                    # Output Equation derivatives
                    for j, outp in enumerate(['y_out']):
                        if outp in arg:
                            result[inp] += self.J_output_input.T[k, j]*arg[outp]

Specifying these derivative functions removes the need for finite differencing this
component in any workflow.
