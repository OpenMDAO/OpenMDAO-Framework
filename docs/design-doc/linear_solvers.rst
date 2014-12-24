
Linear Solvers for Derivative Calculation
------------------------------------------

The wrapper code for all linear solvers can be found in
openmdao.main/src/openmdao/main/linearsolver.py. Their base class is ``LinearSolver``.

The OpenMDAO System Hierarchy uses the LinearSolver class to solve a linear
system of equations that is assembled from the components, drivers, and
assemblies in an OpenMDAO model via the system tree. This is used to solve
for the derivatives of a set of given outputs with respect to inputs.
Whenever an optimizer requests a gradient, it is calculated by a
LinearSolver. It is also used to calculate the direction to take the next
step during iteration of a Newton solver.

A LinearSolver has 2 methods that are called by the systems:

**calc_gradient** -- return a Jacobian of outputs with respect to inputs. The
return format can be an array or a dict as specified by return_format.

solve -- solve the current linear system with a custom right hand side
vector. This is used by Newton solvers

There are currently 3 linear solvers in OpenMDAO, selectable in any
``Driver`` by changing the ``lin_solver`` enum in ``gradient_options``.

Scipy GMRES
++++++++++++

This is the default linear solver used by OpenMDAO, and was also the method
introduced in OpenMDAO 0.9.0 and continued as the only linear solver through
0.10.3.2. GMRES is an iterative method that relies on successive
multiplication of the right-hand-side vector by the Jacobian. If you have no
cycles in your model, then it will converge after a number of iterations
equal to the deepest number of components that it has to traverse from input
to response.

You can specify the tolerance for GMRES by setting ``atol`` in the
gradient_options. The value of atol tells GMRES what values of the matrix
vector product return to ignore when determining the next direction to go. If
you have very small derivatives at any point in your model, you may need to
decrease this number.

Scipy GMRES is not supported in MPI. If you select it for a driver that is
running under MPI, the PetSC KSP solver will be used instead.

PetSC KSP
++++++++++

Linear Gauss-Seidel
++++++++++++++++++++
