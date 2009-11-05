.. index:: standard library drivers

Drivers
=======

.. _CONMIN-driver:

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

More information on CONMIN can be found in the `CONMIN User's Manual
<file:../../../../contrib/conmin/CONMIN_user_manual.html>`_.

CONMIN has been included in the OpenMDAO standard library to provide users
with a basic gradient-based optimization algorithm.

Basic Interface
~~~~~~~~~~~~~~~

The CONMIN code contains a number of different parameters and switches that
are useful for controlling the optimization process. These can be subdivided
into those parameters that will be used in a typical optimization problem and
those that are more likely to be used by an expert user.

For the simplest possible unconstrained optimization problem, CONMIN needs just
an objective function and one or more decision variables (design variables.)

The OpenMDAO CONMIN driver can be loaded by importing the CONMINdriver component
from the standard library drivers namespace.

.. testcode:: CONMIN_load

	from openmdao.lib.drivers.conmindriver import CONMINdriver

Typically, CONMIN will be used as a driver in the top level assemblly, though it
can be also used in a subassembly as part of a nested driver scheme. Using the
OpenMDAO script interface, a simple optimization problem can be set up as
follows:
	
.. testcode:: CONMIN_load

	from openmdao.main.api import Assembly
	from openmdao.lib.drivers.conmindriver import CONMINdriver

	class EngineOptimization(Assembly):
	    """ Top level assembly for optimizing a vehicle. """
    
	    def __init__(self, directory=''):
	        """ Creates a new Assembly containing a DrivingSim and an optimizer"""
        
	        super(EngineOptimization, self).__init__(directory)

	        # Create DrivingSim component instances
	        self.add_container('driving_sim', DrivingSim())

	        # Create CONMIN Optimizer instance
	        self.add_container('driver', CONMINdriver())

This first section of code defines an assembly called EngineOptimization. This
assembly contains a DrivingSim component and a CONMIN driver, both of which are
created and added inside the __init__ function with add_container(). The 
objective function, design variables, constraints, and any CONMIN parameters
are also assigned in the __init__ function. The specific syntax for all of 
these is given below.
	
.. testsetup:: CONMIN_show

	from openmdao.examples.engine_design.engine_optimization import EngineOptimization
	
	# Note: This block of code does not display in the documentation.
	# This is a trick to get around a limitation in Sphinx's doctest, where
	# there is no way to preserve the indentation level between code
	# blocks, and the concept of "self" is not defined when we fall
	# out of the class scope.
	
	self = EngineOptimization()
	
Both the objective function and the design variables are assigned via a
StringRef variable. A StringRef is a string that points to some other OpenMDAO
variable in the variable tree. There is only one objective function, but there
can be multiple design variables which are assigned as a Python list.

.. testcode:: CONMIN_show
        
	# CONMIN Objective 
	self.driver.objective = 'driving_sim.accel_time'
        
	# CONMIN Design Variables 
	self.driver.design_vars = ['driving_sim.spark_angle', 
                                               'driving_sim.bore' ]
					       
Note that all input parameters for the CONMIN driver are assigned via 
"self.driver".

These StringRef variables must point to something that can be seen in the scope
of the CONMIN driver. In other words, if an assembly contains a CONMIN driver,
the objective function and design variables cannot be located outside of that
assembly. Also, each design variable must point to a component input. During
the optimization process, the desgin variables are modified, and the relevant
portion of the model is executed to evaluate the new objective. Note that it
is generally not possible to connect more than 1 driver to an available input.

Additionally, the objective function must always be either an output from a
component, or a function of available component outputs:

.. testcode:: CONMIN_show

	# CONMIN Objective = Maximize weighted sum of EPA city and highway fuel economy 
	self.driver.objective = '-(.93*driving_sim.EPA_city + 1.07*driving_sim.EPA_highway)'

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

	self.driver.lower_bounds = [-50, 65]
	self.driver.upper_bounds = [10, 100]

These size of these lists must be equal to the number of design variables or 
OpenMDAO will raise an exception. Similarly, the upper bound must be greater
than the lower bound for each design variable.

..index:: constraints

Constraints are equations (or inequalities) much like the objective function, so
they are also constructed from the available OpenMDAO variables using Python
mathematical syntax. The constraints parameter is a list of inequalities that
are defined to be satisfied when they return a negative value or zero, and violated
when they return positive value.

.. testcode:: CONMIN_show

	self.driver.constraints = ['driving_sim.stroke - driving_sim.bore']
	    
Note that any equation can also be expressed as an inequality.


Controlling the Optimization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is often necessary to control the convergence criteria for an optimization.
The CONMIN driver allows the user to control both the number of iterations
before termination as well as the convergence tolerance (both absolute and
relative).

The maximum number of iterations is specified by setting the itmax parameter.
The default value is 10.

.. testcode:: CONMIN_show

        self.driver.itmax = 30

The convergence tolerance is controlled with delfun and dabfun. Delfun is the
absolute change in the objective function to indicate convergence (i.e., if the
objective function changes by less than delfun, then the problem is converged.)
Similarly, dabfun is the relative change of the objective function with respect
to the value at the previous step. Note that dabfun has a hard-wired minimum of 
1e-10 in the Fortran code, and delfun has a minimum of 0.0001.

.. testcode:: CONMIN_show

        self.driver.dabfun = .001
        self.driver.dabfun = .1

All of these convergence checks are always active during optimization. The 
tests are performed in the following sequence:

	1. Check number of iterations
	2. Check absolute change in objective
	3. Check relative change in objective
	4. Reduce constraint thickness for slow convergence

There is also a parameter to control how many iterations the convergence
tolerance should be checked before terminating the loop. This is done with the 
itrm parameter, whose default value is 3.
	
.. testcode:: CONMIN_show

        self.driver.itrm = 3

CONMIN can calculate the gradient of both the objective functions and of the
constraints using a finite difference approximation. This is the current
default behavior of the OpenMDAO driver. The CONMIN code can also accept
user-calculated gradients, but these are not yet supported in OpenMDAO. There
are two parameters that control the step size used for numerically estimating
the local gradient.

.. testcode:: CONMIN_show

        self.driver.fdch = .0001
        self.driver.fdchm = .0001
	
The fdchm parameter is the minimum absolute step size that the finite
difference will use, and fdch is the step size relative to the design variable.
**Note: the default values of fdch and fdchm are set to 0.01. This may be too
low for some problems, and will manifest itself by converging to a value that
is not the minimum.** It is important to evaluate the scale of the objective
function around the optimum so that these can be chosen well.

For certain problems, it is desirable to scale the inputs. There are 
several scaling options available, as summarized here:

============  ========================================================
*Value*	      *Result*	
------------  --------------------------------------------------------
nscal < 0     User-defined scaling with the vector in scal
------------  --------------------------------------------------------
nscal = 0     No scaling of the design variables
------------  --------------------------------------------------------
nscal > 0     Scale the design variables every NSCAL iterations.
              Please see the CONMIN user's manual for additional notes
	      about using this option
============  ========================================================

The default setting is nscal=0 for no scaling of the design variables. The 
nscal parameter can be set to a negative number to turn on user-defined
scaling. When this is enabled, the array of values in the vector "scal" is
used to scale the design variables.

.. testcode:: CONMIN_show

        self.driver.scal = [10.0, 10.0, 10.0, 10.0]
        self.driver.nscal = -1
	
Note that there need to be as many scale values as there are design variables.
	
Finally, the iprint parameter can be used to turn on the display of diagnostic
messages inside of CONMIN. These messages are currently sent to the standard
output.

.. testcode:: CONMIN_show

       	self.driver.iprint = 0
	
Higher positive values of iprint turn on the display of more levels of output, as summarized below.

============  ========================================================
*Value*	      *Result*	
------------  --------------------------------------------------------
iprint = 0    All output is suppressed
------------  --------------------------------------------------------
iprint = 1    Print initial and final function information
------------  --------------------------------------------------------
iprint = 2    Debug level 1: All of the above plus control parameters
------------  --------------------------------------------------------
iprint = 3    Debug level 2: All of the above plus all constraint
	      values, number of active/violated constraints, direction
	      vectors, move parameters, and miscellaneous info
------------  --------------------------------------------------------
iprint = 4    Complete debug: All of the above plus objective function
              gradients, active and violated constraint gradients, and
	      misc info
------------  --------------------------------------------------------
iprint = 5    All of above plus each proposed design vector, objective
              and constraints during the one-dimensional search
------------  --------------------------------------------------------
iprint = 101  All of above plus a dump of the arguments passed to
              subroutine CONMIN
============  ========================================================

	
Advanced Options
~~~~~~~~~~~~~~~~
The following options exercise some of the more advanced capabilities of CONMIN.
The details given here briefly summarize the effects of these parameters; more
info is available in the CONMIN User's Manual <file:../../../../contrib/conmin/CONMIN_user_manual.html>`_.


**icndir** -- Conjugate direction restart parameter. For a unconstrained problem
(no side constraints either), Fletcher-Reeves conjugate direction method will
be restarted with a steepest descent direction every ICNDIR iterations.  If 
ICNDIR = 1 only steepest descent will be used. Default value is the number of
design variables + 1.

**Constraint Thickness** -- CONMIN gives 4 parameters for controlling the 
thickness of constraints -- ct, ctmin, ctl, and ctlmin. Using these parameters
essentially puts a tolerance around a constraint surface. Note that ct is used
for general constraints, and ctl is just used for linear constraints. A wide
initial value of the constraint thickness is desirable for highly nonlinear 
problems so that when a constraint becomes active it tends to remain active,
thus reducing the zigzagging problem. The values of ct and ctl adapt as the
problem converges, so the minima can be set with ctl and ctlmin.

**theta** -- Mean value of the push-off factor in the method of feasible
directions. A larger value of theta is desirable if the constraints are known
to be highly nonlinear, and a smaller value may be used if all constraints are
known to be nearly linear. The actual value of the push-off factor used in the
program is a quadratic function of each constraint (G(J)), varying from 0.0
for G(J) = ct to 4.0*theta for G(J) = ABS(ct). A value of theta = 0.0 is used
in the program for constraints which are identified by the user to be strictly
linear. Theta is called a "push-off" factor because it pushes the design away
from the active constraints into the feasible region. The default value is
usually adequate. This is only used for constrained problems.

**phi** -- Participation coefficient, used if a design is infeasible (i.e.,
one or more violated constraints). Phi is a measure of how hard the design
will be "pushed" towards the feasible region and is, in effect, a penalty
parameter. If in a given problem, a feasible solution cannot be obtained with
the default value, PHI should be increased, and the problem run again. If a
feasible solution cannot be obtained with phi = 100, it is probable that no
feasible solution exists. The default value of 5.0 is usually adequate. This
is only used for constrained problems.

**linobj** -- Set this to 1 if the objective function is known to be linear.

