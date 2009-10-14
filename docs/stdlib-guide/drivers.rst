.. index:: standard library drivers

Drivers
=======

The CONMIN Driver
-----------------

CONMIN is a FORTRAN program written in subroutine form, for the solution of
linear or nonlinear constrained optimization problems. The basic optimization
algorithm is the Method of Feasible Directions. If analytic gradients of the
objective or constraint functions are not available, this information is
calculated by finite difference. While the program is intended primarily for
efficient solution of constrained problems, unconstrained function
minimization problems may also be solved, and the conjugate direction method
of Fletcher and Reeves is used for this purpose.

More information on CONMIN can be found in the CONMIN User's Manual (file://../../contrib/conmin/CONMIN_user_manual.html).

CONMIN has been included in the OpenMDAO standard library to provide users
with a basic gradient-based optimization algorithm.

Basic Interface
~~~~~~~~~~~~~~~

The CONMIN code contains a number of different parameters and switches that
are useful for controlling the optimization process. These can be subdivided
into those parameters that will be used in a typical optimization problem, and
those that are more likely to be used by an expert user.

For the simplest possible unconstrained optimization problem, CONMIN just needs
an objective function and one or more decision variables (design variables.)

The OpenMDAO CONMIN driver can be loaded by importing the CONMINdriver component
from the stadnard library drivers namespace.

.. testcode:: CONMIN_load

	from openmdao.lib.drivers.conmindriver import CONMINdriver

.. testsetup:: CONMIN_show

	from openmdao.examples.engine_design.engine_optimization import EngineOptimization
	
	TopLevelAssembly = EngineOptimization()
	
Both the objective function and the design variables are assigned via a
StringRef variable. A StringRef is a string that points to some other OpenMDAO
variable in the variable tree. There is only 1 objective function, but there
can be multiple design variables which are assigned as a Python list.

.. testcode:: CONMIN_show
        
	# CONMIN Objective 
	TopLevelAssembly.driver.objective = 'driving_sim.accel_time'
        
	# CONMIN Design Variables 
	TopLevelAssembly.driver.design_vars = ['driving_sim.spark_angle', 
                                               'driving_sim.bore' ]

These StringRef variables must point to something that can be seen in the scope
of the CONMIN driver. In other words, if an assembly contains a CONMIN driver,
the objective function and design variables cannot be located outside of that
assembly.

The objective function can actually be a function:

.. testcode:: CONMIN_show

	# CONMIN Objective = Maximize weighted sum of EPA city and highway fuel economy 
	TopLevelAssembly.driver.objective = '-(.93*driving_sim.EPA_city + 1.07*driving_sim.EPA_highway)'

In this example, the objective is to maximize the weighted sum of two variables.
The equation must be constructed using valid Python operators. All variables in
the function are expressed in the scope of the local assembly that contains the
CONMIN driver.

More realistically, optimization problems usually have constraints. There are
two types of constrains in CONMIN -- ordinary constraints which are expressed
as functions of the design variables, and side constraints which are used to
bound the design space (i.e., specify a range for each design variable.)

Side constraints are defined using the lower_bounds and upper_bounds parameters:

.. testcode:: CONMIN_show

	TopLevelAssembly.driver.lower_bounds = [-50, 65]
	TopLevelAssembly.driver.upper_bounds = [10, 100]

These size of these lists must be equal to the number of design variables or 
OpenMDAO will raise an exception. Similarly, the upper bound must be greater
than the lower bound for each design variable.

Constraints are equations (or inequalities) much like the objective function, so
they are also constructed from the available OpenMDAO variables using Python
mathematical syntax. The constraints parameter is a list of inequalities that
are defined to be satisfied when they return a negative value or zero, and violated
when they return positive value.

.. testcode:: CONMIN_show

	TopLevelAssembly.driver.constraints = ['driving_sim.stroke - driving_sim.bore']
	    
Note that any equation can also be expressed as an inequality.


Controlling the Optimization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is often necessary to control the convergence criteria for an optimization.
The CONMIN driver allows the user to control both the number of iterations
before termination as well as the convergence tolerance (both absolute and
relative.)

The maximum number of iterations is specified by setting the itmax parameter.
The default value is 10.

.. testcode:: CONMIN_show

        TopLevelAssembly.driver.itmax = 30

The convergence tolerance is controlled with delfun and dabfun. Delfun is the
absolute change in the objective function to indicate convergence (i.e., if the
objective function changes by less than delfun, then the problem is converged.)
Similarly, dabfun is the relative change of the objective function with respect
to the value at the previous step. Note that dabfun has a hard-wired minimum of 
1e-10 in the Fortran code, and delfun has a minimum of 0.0001.

.. testcode:: CONMIN_show

        TopLevelAssembly.driver.dabfun = .001
        TopLevelAssembly.driver.dabfun = .1

All of these convergence checks are always active during optimization. The 
tests are performed in the following sequence:

	1. Check number of iterations
	2. Check absolute change in objective
	3. Check relative change in objective
	4. Reduce constraint thickness for slow convergence
	
CONMIN can calculate the gradient of both the objective functions and of the
constraints using a finite difference approximation. This is the current
default behavior of the OpenMDAO driver. The CONMIN code can also accept
user-calculated gradients, but these are not yet supported in OpenMDAO. There
are two parameters that control the stepsize used for numerically estimating
the local graident.

.. testcode:: CONMIN_show

        TopLevelAssembly.driver.fdch = .0001
        TopLevelAssembly.driver.fdchm = .0001

Finally, the iprint parameter can be used to turn on the display of diagonstic
messages inside of CONMIN. These messages are currently sent to the standard
output.

.. testcode:: CONMIN_show

       	TopLevelAssembly.driver.iprint = 0
	
Higher positive values of iprint turn on the display of more levels of output, as summarized below.

============  ========================================================
*Value*	      *Result*	
------------  --------------------------------------------------------
IPRINT = 0    All output is suppressed
------------  --------------------------------------------------------
IPRINT = 1    Print initial and final function information
------------  --------------------------------------------------------
IPRINT = 2    Debug level 1: All of the above plus control parameters
------------  --------------------------------------------------------
IPRINT = 3    Debug level 2: All of the above plus all constraint
	      values, number of active/violated constraints, direction
	      vectors, move parameters, and miscellaneous info
------------  --------------------------------------------------------
IPRINT = 4    Complete debug: All of the above plus objective function
              gradients, active and violated constraint gradients, and
	      misc info
------------  --------------------------------------------------------
IPRINT = 5    All of above plus each proposed design vector, objective
              and constraints during the one-dimensional search
------------  --------------------------------------------------------
IPRINT = 101  All of above plus a dump of the arguments passed to
              subroutine CONMIN
============  ========================================================

	
Advanced Options
~~~~~~~~~~~~~~~~