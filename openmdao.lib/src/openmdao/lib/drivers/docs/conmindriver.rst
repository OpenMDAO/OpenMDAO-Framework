
.. index:: CONMIN

.. _CONMINDriver:

*CONMINDriver*
~~~~~~~~~~~~~~

:term:`CONMIN` is a Fortran program written as a subroutine to solve linear
or nonlinear constrained optimization problems. The basic optimization
algorithm is the Method of Feasible Directions. If analytic gradients of the
objective or constraint functions are not available, or you do not wish to
use OpenMDAO's gradient calculation, then you can use CONMIN's internal
finite difference code to calculate the gradient. While the program is
intended primarily for efficient solution of constrained problems,
unconstrained function minimization problems may also be solved. The
conjugate direction method of Fletcher and Reeves is used for this purpose.

More information on CONMIN can be found in the `CONMIN User's Manual
<http://www.eng.buffalo.edu/Research/MODEL/mdo.test.orig/CONMIN/manual.html>`_.

CONMIN has been included in the OpenMDAO standard library to provide users
with a basic gradient-based optimization algorithm.

**Basic Interface**

The CONMIN code contains a number of different parameters and switches that
are useful for controlling the optimization process. These can be subdivided
into those parameters that will be used in a typical optimization problem and
those that are more likely to be used by an expert user.

For the simplest possible unconstrained optimization problem, CONMIN just needs
an objective function and one or more decision variables (parameters.) The
basic interface conforms to OpenMDAO's driver API.

The OpenMDAO CONMIN driver can be imported from ``openmdao.lib.drivers.api``.

.. testcode:: CONMIN_load

    from openmdao.lib.drivers.api import CONMINdriver

Typically, CONMIN will be used as a driver in the top level assembly, though it also
can be used in a subassembly as part of a nested driver scheme. Using the
OpenMDAO script interface, a simple optimization problem can be set up as
follows:

.. testcode:: CONMIN_load

    from openmdao.main.api import Assembly
    from openmdao.examples.simple.paraboloid import Paraboloid
    from openmdao.lib.drivers.api import CONMINdriver
        
    class Top(Assembly):
        """Constrained optimization of the Paraboloid with whatever optimizer
        we want."""
            
        def configure(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
                
            # Create Paraboloid component instances
            self.add('comp', Paraboloid())
        
            # Create Optimizer instance
            self.add('driver', CONMINdriver())
                
            # Driver process definition
            self.driver.workflow.add('comp')
        
            # Objective 
            self.driver.add_objective('comp.f_xy')
                
            # Design Variables 
            self.driver.add_parameter('comp.x', low=-50., high=50.)
            self.driver.add_parameter('comp.y', low=-50., high=50.)

            # CONMIN Flags
            self.driver.iprint = 0
            self.driver.itmax = 30


This first section of code defines an assembly called Top.
This assembly contains a Paraboloid component and a CONMINdriver, both of
which are created and added inside the ``__init__`` function with ``add``. The
Paraboloid component is also added to the driver's workflow. The objective
function, design variables, constraints, and any CONMIN parameters are also
assigned in the ``__init__`` function.

**Controlling the Optimization**

It is often necessary to control the convergence criteria for an optimization.
The CONMIN driver allows control over both the number of iterations
before termination as well as the convergence tolerance (both absolute and
relative).

The maximum number of iterations is specified by setting the `itmax` parameter.
The default value is 10.

.. testsetup:: CONMIN_show

    from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    from openmdao.main.api import set_as_top
    self = set_as_top(OptimizationUnconstrained())

.. testcode:: CONMIN_show

        self.driver.itmax = 30

The convergence tolerance is controlled with `dabfun` and `delfun`. `Dabfun` is the
absolute change in the objective function to indicate convergence (i.e., if the
objective function changes by less than `dabfun`, then the problem is converged).
Similarly, `delfun` is the relative change of the objective function with respect
to the value at the previous step. Note that `delfun` has a hard-wired minimum of
1e-10 in the Fortran code, and `dabfun` has a minimum of 0.0001.

.. testcode:: CONMIN_show

        self.driver.dabfun = .001
        self.driver.delfun = .1

All of these convergence checks are always active during optimization. The
tests are performed in the following sequence:

1. Check number of iterations
2. Check absolute change in objective
3. Check relative change in objective
4. Reduce constraint thickness for slow convergence

The number of successive iterations that the convergence tolerance should be checked before
terminating the loop can also be specified with the `itrm` parameter, whose
default value is 3.

.. testcode:: CONMIN_show

        self.driver.itrm = 3

By default, OpenMDAO calculates the gradient and provides it to CONMIN. However,
you may want to use CONMIN's internal finite-difference to calculate the gradient.
This can be done by setting the `conmin_diff` flag to True.

.. testcode:: CONMIN_show

        self.conmin_diff = True

Two parameters control the step size used for numerically estimating the
local gradient: `fdch` and `fdchm`. The `fdchm` parameter is the minimum
absolute step size that the finite difference will use, and `fdch` is the
step size relative to the design variable.

.. testcode:: CONMIN_show

        self.driver.fdch = .0001
        self.driver.fdchm = .0001

.. note::
   The default values of `fdch` and `fdchm` are set to 0.01. This may be too
   large for some problems and will manifest itself by converging to a value that
   is not the minimum. It is important to evaluate the scale of the objective
   function around the optimum so that these can be chosen well.

For certain problems, it is desirable to scale the inputs.
Several scaling options are available, as summarized here:

==============  ========================================================
Value           Result
==============  ========================================================
``nscal < 0``   User-defined scaling with the vector in scal
--------------  --------------------------------------------------------
``nscal = 0``   No scaling of the design variables
--------------  --------------------------------------------------------
``nscal > 0``   Scale the design variables every NSCAL iteration. Please
                see the `CONMIN User's Manual <http://www.eng.buffalo.edu/Research/MODEL/mdo.test.orig/CONMIN/manual.html>`_
                for additional notes about using this option.
==============  ========================================================

If your problem uses linear constraints, you can improve the efficiency of the
optimization process by designating those that are linear functions of the design
variables as follows:

.. testcode:: CONMIN_show

    self.driver.add_constraint('paraboloid.x - paraboloid.y >= 15.0')
    self.driver.add_constraint('paraboloid.x*paraboloid.y < 77.0', linear=True)

Here, the first constraint is linear, and the second constraint is nonlinear. If
the ``linear`` attribute is not specified, then the constraints is assumed to be
nonlinear. Note that the original CONMIN parameter for this is `ISC`. If
your constraint includes some framework output in the equation, then it is
probably not a linear function of the design variables.

Finally, the `iprint` parameter can be used to display diagnostic
messages inside of CONMIN. These messages are currently sent to the standard
output.

.. testcode:: CONMIN_show

        self.driver.iprint = 0

Higher positive values of `iprint` turn on the display of more levels of output, as summarized
below. To make it easier to swap drivers, an `iprint` of -1 also suppresses all
output.

================  ========================================================
Value             Result
================  ========================================================
``iprint = 0``    All output is suppressed
----------------  --------------------------------------------------------
``iprint = 1``    Print initial and final function information
----------------  --------------------------------------------------------
``iprint = 2``    Debug level 1: All of the above plus control parameters
----------------  --------------------------------------------------------
``iprint = 3``    Debug level 2: All of the above plus all constraint
                  values, number of active/violated constraints, direction
                  vectors, move parameters, and miscellaneous information
----------------  --------------------------------------------------------
``iprint = 4``    Complete debug: All of the above plus objective function
                  gradients, active and violated constraint gradients, and
                  miscellaneous information
----------------  --------------------------------------------------------
``iprint = 5``    All of above plus each proposed design vector, objective
                  and constraints during the one-dimensional search
----------------  --------------------------------------------------------
``iprint = 101``  All of above plus a dump of the arguments passed to
                  subroutine CONMIN
================  ========================================================


**Advanced Options**

The following options exercise some of the more advanced
capabilities of CONMIN. The details given here briefly summarize the effects of these
parameters; more information is available in the `CONMIN User's Manual
<http://www.eng.buffalo.edu/Research/MODEL/mdo.test.orig/CONMIN/manual.html>`_.

**icndir**
  Conjugate direction restart parameter. For an unconstrained problem
  (no side constraints either), Fletcher-Reeves conjugate direction method will
  be restarted with the steepest descent direction every `ICNDIR` iterations.  If
  ``ICNDIR = 1``, only the steepest descent will be used. Default value is the number of
  design variables + 1.

**Constraint Thickness**
  CONMIN gives four parameters for controlling the
  thickness of constraints -- `ct, ctmin, ctl,` and `ctlmin`. Using these parameters
  essentially puts a tolerance around a constraint surface. Note that `ct` is used
  for general constraints, and `ctl` is used only for linear constraints. A wide
  initial value of the constraint thickness is desirable for highly nonlinear
  problems so that when a constraint becomes active, it tends to remain active,
  thus reducing the zigzagging problem. The values of `ct` and `ctl` adapt as the
  problem converges, so the minima can be set with `ctl` and `ctlmin`.

**theta**
  Mean value of the push-off factor in the method of feasible
  directions. A larger value of theta is desirable if the constraints are known
  to be highly nonlinear, and a smaller value may be used if all constraints are
  known to be nearly linear. The actual value of the push-off factor used in the
  program is a quadratic function of each constraint (G(J)), varying from ``0.0
  for G(J) = ct to 4.0*theta for G(J) = ABS(ct)``. A value of ``theta = 0.0`` is used
  in the program for constraints which are identified by the user to be strictly
  linear. Theta is called a *push-off* factor because it pushes the design away
  from the active constraints into the feasible region. The default value is
  usually adequate. This is used only for constrained problems.

**phi**
  Participation coefficient, used if a design is infeasible (i.e.,
  one or more violated constraints). `Phi` is a measure of how hard the design
  will be "pushed" towards the feasible region and is, in effect, a penalty
  parameter. If in a given problem, a feasible solution cannot be obtained with
  the default value, `phi` should be increased, and the problem run again. If a
  feasible solution cannot be obtained with ``phi = 100``, it is probable that no
  feasible solution exists. The default value of 5.0 is usually adequate. `Phi` is
  used only for constrained problems.

**linobj**
  Set this to 1 if the objective function is known to be linear.

*Source Documentation for conmindriver.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
