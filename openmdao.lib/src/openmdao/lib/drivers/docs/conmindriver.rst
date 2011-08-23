  
.. index:: CONMIN

.. _CONMINDriver:

*CONMINDriver*
~~~~~~~~~~~~~~

:term:`CONMIN` is a Fortran program written as a subroutine to solve linear or
nonlinear constrained optimization problems. The basic optimization algorithm
is the Method of Feasible Directions. If analytic gradients of the objective
or constraint functions are not available (i.e., if a Differentiator is not
plugged into the differentiator socket), then the gradients are calculated
by CONMIN's internal finite difference code. While the program is intended
primarily for efficient solution of constrained problems, unconstrained
function minimization problems may also be solved. The conjugate direction
method of Fletcher and Reeves is used for this purpose.

More information on CONMIN can be found in the `CONMIN User's Manual
<http://www.eng.buffalo.edu/Research/MODEL/mdo.test.orig/CONMIN/manual.html>`_. (In the :ref:`simple
tutorial <A-Simple-Tutorial-Problem>`, CONMIN is used for an
unconstrained and a constrained optimization.)

CONMIN has been included in the OpenMDAO standard library to provide users
with a basic gradient-based optimization algorithm.

**Basic Interface**

The CONMIN code contains a number of different parameters and switches that
are useful for controlling the optimization process. These can be subdivided
into those parameters that will be used in a typical optimization problem and
those that are more likely to be used by an expert user.

For the simplest possible unconstrained optimization problem, CONMIN just needs
an objective function and one or more decision variables (parameters.) The
basic interface conforms to OpenMDAO's driver API, which is discussed in 
:ref:`Driver-API`, and covers how to assign design variables, constraints, and
objectives.

The OpenMDAO CONMIN driver can be imported from ``openmdao.lib.drivers.api``.

.. testcode:: CONMIN_load

    from openmdao.lib.drivers.api import CONMINdriver

Typically, CONMIN will be used as a driver in the top level assembly, though it also
can be used in a subassembly as part of a nested driver scheme. Using the
OpenMDAO script interface, a simple optimization problem can be set up as
follows:

.. testcode:: CONMIN_load

    from openmdao.main.api import Assembly
    from openmdao.examples.enginedesign.vehicle import Vehicle
    from openmdao.lib.drivers.api import CONMINdriver

    class EngineOptimization(Assembly):
        """ Top level assembly for optimizing a vehicle. """
    
        def __init__(self):
            """ Creates a new Assembly for vehicle performance optimization."""
            
            super(EngineOptimization, self).__init__()

            # Create CONMIN Optimizer instance
            self.add('driver', CONMINdriver())
        
            # Create Vehicle instance
            self.add('vehicle', Vehicle())
        
            # add Vehicle to optimizer workflow
            self.driver.workflow.add('vehicle')
    
            # CONMIN Flags
            self.driver.iprint = 0
            self.driver.itmax = 30
            
            # CONMIN Objective 
            self.driver.add_objective('vehicle.fuel_burn')
        
            # CONMIN Design Variables 
            self.driver.add_parameter('vehicle.spark_angle', low=-50. , high=10.)
            self.driver.add_parameter('vehicle.bore', low=65. , high=100.)

This first section of code defines an assembly called EngineOptimization.
This assembly contains a DrivingSim component and a CONMINdriver, both of
which are created and added inside the ``__init__`` function with ``add``. The
DrivingSim component is also added to the driver's workflow. The objective
function, design variables, constraints, and any CONMIN parameters are also
assigned in the ``__init__`` function. The specific syntax for all of these is
discussed in :ref:`Driver-API`.


**Controlling the Optimization**

It is often necessary to control the convergence criteria for an optimization.
The CONMIN driver allows control over both the number of iterations
before termination as well as the convergence tolerance (both absolute and
relative).

The maximum number of iterations is specified by setting the ``itmax`` parameter.
The default value is 10.

.. testsetup:: CONMIN_show
    
    from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
    self = EngineOptimization()

.. testcode:: CONMIN_show

        self.driver.itmax = 30

The convergence tolerance is controlled with ``dabfun`` and ``delfun``. ``Dabfun`` is the
absolute change in the objective function to indicate convergence (i.e., if the
objective function changes by less than ``dabfun``, then the problem is converged).
Similarly, ``delfun`` is the relative change of the objective function with respect
to the value at the previous step. Note that ``delfun`` has a hard-wired minimum of 
``1e-10`` in the Fortran code, and ``dabfun`` has a minimum of 0.0001.

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
terminating the loop can also be specified with the ``itrm`` parameter, whose
default value is 3.

.. testcode:: CONMIN_show

        self.driver.itrm = 3

CONMIN can calculate the gradient of both the objective functions and of the
constraints using a finite difference approximation. This is the default
behavior if no Differentiator is plugged into the differentiator socket. Two
parameters control the step size used for numerically estimating the local
gradient: ``fdch`` and ``fdchm``. The ``fdchm`` parameter is the minimum
absolute step size that the finite difference will use, and ``fdch`` is the
step size relative to the design variable.

.. testcode:: CONMIN_show

        self.driver.fdch = .0001
        self.driver.fdchm = .0001

.. note::
   The default values of ``fdch`` and ``fdchm`` are set to 0.01. This may be too
   large for some problems and will manifest itself by converging to a value that
   is not the minimum. It is important to evaluate the scale of the objective
   function around the optimum so that these can be chosen well.

You can also replace CONMIN's finite difference with OpenMDAO's built-in
capability by inserting a differentiator into the Differentiator slot in the
driver, as shown in :ref:`Calculating-Derivatives-with-Finite-Difference`.

For certain problems, it is desirable to scale the inputs.
Several scaling options are available, as summarized here:

==============  ========================================================
Value           Result
==============  ========================================================
``nscal < 0``   User-defined scaling with the vector in ``scal``
--------------  --------------------------------------------------------
``nscal = 0``   No scaling of the design variables
--------------  --------------------------------------------------------
``nscal > 0``   Scale the design variables every ``NSCAL`` iteration. Please
                see the `CONMIN User's Manual <http://www.eng.buffalo.edu/Research/MODEL/mdo.test.orig/CONMIN/manual.html>`_ 
                for additional notes about using this option.
==============  ========================================================

If your problem uses linear constraints, you can improve the efficiency of the
optimization process by designating those that are linear functions of the design
variables as follows:

.. testcode:: CONMIN_show

    map(self.driver.add_constraint, ['vehicle.stroke < vehicle.bore',
                               'vehicle.stroke * vehicle.bore > 1.0'])
    self.driver.cons_is_linear = [1, 0]
    
Here, the first constraint is linear, and the second constraint is nonlinear. If 
``cons_is_linear`` is not specified, then all the constraints are assumed to be
nonlinear. Note that the original CONMIN parameter for this is ``ISC``. If
your constraint includes some framework output in the equation, then it is 
probably not a linear function of the design variables.

Finally, the ``iprint`` parameter can be used to display diagnostic
messages inside of CONMIN. These messages are currently sent to the standard
output.

.. testcode:: CONMIN_show

        self.driver.iprint = 0

Higher positive values of ``iprint`` turn on the display of more levels of output, as summarized
below.

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
  be restarted with the steepest descent direction every ``ICNDIR`` iterations.  If 
  ``ICNDIR = 1``, only the steepest descent will be used. Default value is the number of
  design variables + 1.

**Constraint Thickness** 
  CONMIN gives four parameters for controlling the 
  thickness of constraints -- ``ct, ctmin, ctl,`` and ``ctlmin``. Using these parameters
  essentially puts a tolerance around a constraint surface. Note that ``ct`` is used
  for general constraints, and ``ctl`` is used only for linear constraints. A wide
  initial value of the constraint thickness is desirable for highly nonlinear 
  problems so that when a constraint becomes active, it tends to remain active,
  thus reducing the zigzagging problem. The values of ``ct`` and ``ctl`` adapt as the
  problem converges, so the minima can be set with ``ctl`` and ``ctlmin``.

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
  one or more violated constraints). ``Phi`` is a measure of how hard the design
  will be "pushed" towards the feasible region and is, in effect, a penalty
  parameter. If in a given problem, a feasible solution cannot be obtained with
  the default value, ``phi`` should be increased, and the problem run again. If a
  feasible solution cannot be obtained with ``phi = 100``, it is probable that no
  feasible solution exists. The default value of 5.0 is usually adequate. ``Phi`` is
  used only for constrained problems.

**linobj**
  Set this to 1 if the objective function is known to be linear.

*Source Documentation for conmindriver.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
