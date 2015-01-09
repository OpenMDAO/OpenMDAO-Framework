
Linear Solvers for Derivative Calculation
------------------------------------------

The wrapper code for all linear solvers can be found in
``openmdao.main/src/openmdao/main/linearsolver.py``. Their base class is LinearSolver.

The OpenMDAO System Hierarchy uses the LinearSolver class to solve a linear
system of equations that is assembled from the components, drivers, and
assemblies in an OpenMDAO model via the system tree. Such a linear system is
solved for the derivatives of a set of given outputs with respect to inputs.
Whenever an optimizer requests a gradient, a LinearSolver calculates it. This
solver also calculates the direction to take the next step during iteration of
a Newton solver.

A LinearSolver has two methods that are called by the systems:

**calc_gradient** -- return a Jacobian of outputs with respect to inputs. The
return format can be an array or a dict as specified by ``return_format``.

**solve** -- solve the current linear system with a custom right-hand-side
vector. This is used by Newton solvers.

OpenMDAO currently has three linear solvers, selectable in any Driver by
changing the ``lin_solver`` enum in ``gradient_options``. All linear solvers
can be run in either forward or adjoint mode by setting
``derivative_direction``. If you don't set this, OpenMDAO will pick the best
direction based on the width of the parameter and response spaces.

Scipy GMRES
++++++++++++

Scipy GMRES is the default linear solver used by OpenMDAO; it was also the method
introduced in OpenMDAO 0.9.0 and continued as the only linear solver through
version 0.10.3.2. GMRES is an iterative method that relies on successive
multiplication of the right-hand-side vector by the Jacobian. If you have no
cycles in your model, then it will converge after a number of iterations
equal to the deepest number of components that it has to traverse from input
to response.

You can specify the tolerance for GMRES by setting `atol` in the
``gradient_options``. The value of atol tells GMRES what values of the matrix
vector product return to ignore when determining the next direction to go. If
you have very small derivatives at any point in your model, you may need to
decrease this number. If you have a large number of components, you may also
need to increase `maxiter`.

Scipy GMRES is not supported in MPI. If you select it for a driver that is
running under MPI, the PetSC KSP solver will be used instead.

PetSC KSP
++++++++++

This linear solver uses PetSC's KSP solver. KSP supports a number of different
Krylov Space methods, but we are just using GMRES, so the solution method is
the same as the Scipy GMRES solver. However, the PetSC package provides
support for solving a linear problem when the incoming and solution vectors
are distributed across multiple processes in MPI.

You can specify the tolerance for KSP by setting `atol` or `rtol` in the
``gradient_options`` (i.e., absolute and relative tolerances). They likely 
function the same way as in GMRES. If you have a large number of components, you
may also need to increase `maxiter`.

The KSP solvers also add formal support for preconditioning; this will be
supported in OpenMDAO soon.

PetSC KSP is the default linear solver for any driver executing under MPI.
However, you can also use it for serial execution provided that you have
installed PetSC and PetSC4py, openMPI and MPI4py.

Linear Gauss-Seidel
++++++++++++++++++++

The Linear Gauss-Seidel linear solver is essentially a chain rule solver. It
iterates over a workflow in dataflow order and calls the matrix vector
product. For a model without cycles, one iteration is enough for an accurate
derivative, and you should set maxiter to 1 in this case.

This currently doesn't work in MPI, and it might require enough modifications
that we just implement a new linear solver to use under MPI.
