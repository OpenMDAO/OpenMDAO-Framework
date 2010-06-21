.. index:: standard library guide overview
.. index:: Components
.. index:: Drivers
.. index:: Factories
.. index:: Traits


.. _OpenMDAO-Standard-Library:

OpenMDAO Standard Library
=========================

The OpenMDAO standard library contains an assortment of useful plugins to the
framework sorted into four categories: components, drivers, factories, and traits.

.. index:: standard library components

Components
----------
.. index:: CONMIN 


Drivers
--------

.. _CONMIN-driver:

*The CONMIN Driver*
+++++++++++++++++++

:term:`CONMIN` is a Fortran program written as a subroutine to solve
linear or nonlinear constrained optimization problems. The basic optimization
algorithm is the Method of Feasible Directions. If analytic gradients of the
objective or constraint functions are not available, this information is
calculated by finite difference. While the program is intended primarily for
efficient solution of constrained problems, unconstrained function
minimization problems may also be solved. The conjugate direction method
of Fletcher and Reeves is used for this purpose.

More information on CONMIN can be found in the `CONMIN User's Manual
<file:../../../plugin-guide/CONMIN_user_manual.html>`_. (In the :ref:`simple
tutorial <Getting-Started-with-OpenMDAO>` in the *User Guide*, CONMIN is used for an
unconstrained and a constrained optimization.)

CONMIN has been included in the OpenMDAO standard library to provide users
with a basic gradient-based optimization algorithm.

Basic Interface
~~~~~~~~~~~~~~~

The CONMIN code contains a number of different parameters and switches that
are useful for controlling the optimization process. These can be subdivided
into those parameters that will be used in a typical optimization problem and
those that are more likely to be used by an expert user.

For the simplest possible unconstrained optimization problem, CONMIN just needs
an objective function and one or more decision variables (design variables)

The OpenMDAO CONMIN driver can be imported from ``openmdao.lib.api``.

.. testcode:: CONMIN_load

    from openmdao.lib.api import CONMINdriver

Typically, CONMIN will be used as a driver in the top level assembly, though it
can be also used in a subassembly as part of a nested driver scheme. Using the
OpenMDAO script interface, a simple optimization problem can be set up as
follows:

.. testcode:: CONMIN_load

    from openmdao.main.api import Assembly
    from openmdao.lib.api import CONMINdriver

    class EngineOptimization(Assembly):
        """ Top level assembly for optimizing a vehicle. """
    
        def __init__(self):
            """ Creates a new Assembly containing a DrivingSim and an optimizer"""
        
            super(EngineOptimization, self).__init__()

            # Create DrivingSim component instances
            self.add_container('driving_sim', DrivingSim())

            # Create CONMIN Optimizer instance
            self.add_container('driver', CONMINdriver())

This first section of code defines an assembly called *EngineOptimization.* This
assembly contains a DrivingSim component and a CONMIN driver, both of which are
created and added inside the ``__init__`` function with ``add_container()``. The 
objective function, design variables, constraints, and any CONMIN parameters
are also assigned in the ``__init__`` function. The specific syntax for all of 
these is given below.

.. testsetup:: CONMIN_show

    from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
    
    # Note: This block of code does not display in the documentation.
    # This is a trick to get around a limitation in Sphinx's doctest, where
    # there is no way to preserve the indentation level between code
    # blocks, and the concept of "self" is not defined when we fall
    # out of the class scope.
    
    self = EngineOptimization()

Both the objective function and the design variables are assigned via an
:term:`Expression` variable. An Expression is a string that points to some other OpenMDAO
variable in the variable tree. There is only one objective function, but there
can be multiple design variables which are assigned as a Python list.

.. testcode:: CONMIN_show
        
    # CONMIN Objective 
    self.driver.objective = 'driving_sim.accel_time'
        
    # CONMIN Design Variables 
    self.driver.design_vars = ['driving_sim.spark_angle', 
                                               'driving_sim.bore' ]

These Expression variables must point to something that can be seen in the
scope of the asssembly that contains the CONMIN driver. In other words,
if an assembly contains a CONMIN driver, the objective function and design
variables cannot be located outside of that assembly. Also, each design
variable must point to a component input. During the optimization process, the
design variables are modified, and the relevant portion of the model is
executed to evaluate the new objective. It is generally not possible
to connect more than one driver to an available input.

Additionally, the objective function must always be either an output from a
component or a function of available component outputs:

.. testcode:: CONMIN_show

    # CONMIN Objective = Maximize weighted sum of EPA city and highway fuel economy 
    self.driver.objective = '-(.93*driving_sim.EPA_city + 1.07*driving_sim.EPA_highway)'

In this example, the objective is to maximize the weighted sum of two variables.
The equation must be constructed using valid Python operators. All variables in
the function are expressed in the scope of the local assembly that contains the
CONMIN driver.

.. index:: pair: constraints; CONMIN

There are two types of constraints in CONMIN -- *ordinary* constraints, which
are expressed as functions of the design variables, and *side* constraints,
which are used to bound the design space (i.e., specify a range for each
design variable).

Side constraints are defined using the ``lower_bounds`` and ``upper_bounds`` parameters:

.. testcode:: CONMIN_show

    self.driver.lower_bounds = [-50, 65]
    self.driver.upper_bounds = [10, 100]

The size of these lists must be equal to the number of design variables or 
OpenMDAO will raise an exception. Similarly, the upper bound must be greater
than the lower bound for each design variable.

Constraints are equations or inequalities that are constructed from the available OpenMDAO variables using Python
mathematical syntax. The constraints parameter is a list of inequalities that
are defined to be **satisfied when they return a negative value or zero**, and **violated
when they return a positive value**.

.. testcode:: CONMIN_show

    self.driver.constraints = ['driving_sim.stroke - driving_sim.bore']

Any equation can also be expressed as an inequality.


Controlling the Optimization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is often necessary to control the convergence criteria for an optimization.
The CONMIN driver allows control over both the number of iterations
before termination as well as the convergence tolerance (both absolute and
relative).

The maximum number of iterations is specified by setting the *itmax* parameter.
The default value is 10.

.. testcode:: CONMIN_show

        self.driver.itmax = 30

The convergence tolerance is controlled with *dabfun* and *delfun*. *Dabfun* is the
absolute change in the objective function to indicate convergence (i.e., if the
objective function changes by less than dabfun, then the problem is converged).
Similarly, *delfun* is the relative change of the objective function with respect
to the value at the previous step. Note that delfun has a hard-wired minimum of 
1e-10 in the Fortran code, and dabfun has a minimum of 0.0001.

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
terminating the loop can also be specified with the *itrm* parameter, whose
default value is 3.

.. testcode:: CONMIN_show

        self.driver.itrm = 3

CONMIN can calculate the gradient of both the objective functions and of the
constraints using a finite difference approximation. This is the current
default behavior of the OpenMDAO driver. The CONMIN code can also accept
user-calculated gradients, but these are not yet supported in OpenMDAO. Two
parameters control the step size used for numerically estimating the local
gradient: *fdch* and *fdchm.* The *fdchm* parameter is the minimum absolute step size that the finite
difference will use, and *fdch* is the step size relative to the design variable.

.. testcode:: CONMIN_show

        self.driver.fdch = .0001
        self.driver.fdchm = .0001

.. note::
   The default values of *fdch* and *fdchm* are set to 0.01. This may be too
   large for some problems and will manifest itself by converging to a value that
   is not the minimum. It is important to evaluate the scale of the objective
   function around the optimum so that these can be chosen well.

For certain problems, it is desirable to scale the inputs.
Several scaling options are available, as summarized here:

============  ========================================================
Value	      Result	
============  ========================================================
nscal < 0     User-defined scaling with the vector in scal
------------  --------------------------------------------------------
nscal = 0     No scaling of the design variables
------------  --------------------------------------------------------
nscal > 0     Scale the design variables every NSCAL iterations.
              Please see the CONMIN user's manual for additional notes
	      about using this option
============  ========================================================

The default setting is nscal=0 for no scaling of the design variables. The 
*nscal* parameter can be set to a negative number to turn on user-defined
scaling. When this is enabled, the array of values in the vector *scal* is
used to scale the design variables.

.. testcode:: CONMIN_show

        self.driver.scal = [10.0, 10.0, 10.0, 10.0]
        self.driver.nscal = -1

There need to be as many scale values as there are design variables.

If your problem uses linear  constraints, you can improve the efficiency of the
optimization process by designating those that are linear functions of the design
variables as follows:

.. testcode:: CONMIN_show

    self.driver.constraints = ['driving_sim.stroke - driving_sim.bore',
                               '1.0 - driving_sim.stroke * driving_sim.bore']
    self.cons_is_linear = [1, 0]

If ``cons_is_linear`` is not specified, then all the constraints are assumed to be
nonlinear. Note that the original CONMIN parameter for this is *ISC.*	

Finally, the *iprint* parameter can be used to display diagnostic
messages inside of CONMIN. These messages are currently sent to the standard
output.

.. testcode:: CONMIN_show

        self.driver.iprint = 0

Higher positive values of *iprint* turn on the display of more levels of output, as summarized below.

============  ========================================================
Value         Result
============  ========================================================
iprint = 0    All output is suppressed
------------  --------------------------------------------------------
iprint = 1    Print initial and final function information
------------  --------------------------------------------------------
iprint = 2    Debug level 1: All of the above plus control parameters
------------  --------------------------------------------------------
iprint = 3    Debug level 2: All of the above plus all constraint
              values, number of active/violated constraints, direction
              vectors, move parameters, and miscellaneous information
------------  --------------------------------------------------------
iprint = 4    Complete debug: All of the above plus objective function
              gradients, active and violated constraint gradients, and
              miscellaneous information
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
information is available in the `CONMIN User's Manual <file:../../../plugin-guide/CONMIN_user_manual.html>`_.

**icndir** -- Conjugate direction restart parameter. For an unconstrained problem
(no side constraints either), Fletcher-Reeves conjugate direction method will
be restarted with the steepest descent direction every ICNDIR iterations.  If 
ICNDIR = 1, only the steepest descent will be used. Default value is the number of
design variables + 1.

**Constraint Thickness** -- CONMIN gives four parameters for controlling the 
thickness of constraints -- *ct, ctmin, ctl,* and *ctlmin.* Using these parameters
essentially puts a tolerance around a constraint surface. Note that *ct* is used
for general constraints, and *ctl* is used only for linear constraints. A wide
initial value of the constraint thickness is desirable for highly nonlinear 
problems so that when a constraint becomes active, it tends to remain active,
thus reducing the zigzagging problem. The values of *ct* and *ctl* adapt as the
problem converges, so the minima can be set with *ctl* and *ctlmin.*

**theta** -- Mean value of the push-off factor in the method of feasible
directions. A larger value of theta is desirable if the constraints are known
to be highly nonlinear, and a smaller value may be used if all constraints are
known to be nearly linear. The actual value of the push-off factor used in the
program is a quadratic function of each constraint (G(J)), varying from ``0.0
for G(J) = ct to 4.0*theta for G(J) = ABS(ct)``. A value of theta = 0.0 is used
in the program for constraints which are identified by the user to be strictly
linear. Theta is called a *push-off* factor because it pushes the design away
from the active constraints into the feasible region. The default value is
usually adequate. This is used only for constrained problems.

**phi** -- Participation coefficient, used if a design is infeasible (i.e.,
one or more violated constraints). *Phi* is a measure of how hard the design
will be "pushed" towards the feasible region and is, in effect, a penalty
parameter. If in a given problem, a feasible solution cannot be obtained with
the default value, phi should be increased, and the problem run again. If a
feasible solution cannot be obtained with phi = 100, it is probable that no
feasible solution exists. The default value of 5.0 is usually adequate. Phi is
used only for constrained problems.

**linobj** -- Set this to 1 if the objective function is known to be linear.


.. _`Genetic`:

*Genetic*
++++++++++

:term:`Genetic` is a driver which performs optimization using a genetic algorithm based
on `Pyevolve <http://pyevolve.sourceforge.net/>`_. Genetic is a global optimizer and
is ideal for optimizing problems with integer or discrete design variables because it
is a non-derivative based optimization method. 

Genetic can be used in any simulation by importing it from ``openmdao.lib.api``:

.. testcode:: Genetic_load

    from openmdao.lib.api import Genetic

.. index:: pair: design; variables
.. index:: Float, Int, Enum

Design Variables
~~~~~~~~~~~~~~~~

Public variables are added to Genetic and become design variables. Genetic will vary the set of
design variables to search for an optimum. Genetic supports three public variable types:
:term:`Float`, :term:`Int`, and :Term:`Enum`. These public variable types can be used as design
variables in any optimization. 

You add design variables to Genetic using the ``add_des_var`` method.

.. testcode:: Genetic

    from openmdao.main.api import Assembly,Component, set_as_top
    from openmdao.lib.api import Genetic
    from openmdao.lib.api import Float,Int,Enum
    
    class SomeComp(Component):
        """Arbitrary component with a few public variables, but which does not really do 
	any calculations"""

	w = Float(0.0,low=-10,high=10,iotype="in")
	
	x = Float(0.0,low=0.0,high=100.0,iotype="in")
	y = Int(10,low=10,high=100,iotype="in")
	z = Enum([-10,-5,0,7],iotype="in")
	
    class Simulation(Assembly):
	"""Top Level Assembly used for simulation"""
	
	def __init__(self):
	    """Adds the Genetic driver to the assembly"""
	    
	    super(Simulation,self).__init__()
	    
	    self.add_container('optimizer',Genetic())
	    self.add_container('comp',SomeComp())
	    
	    self.optimizer.add_des_var('comp.x')
	    self.optimizer.add_des_var('comp.y')
	    self.optimizer.add_des_var('comp.z')
	
    top = Simulation()	    
    set_as_top(top)
	    
In the above example, three design variables were added to the optimizer. The optimizer 
figures out for itself what type of variable it is and behaves appropriately. In all three
cases, since no *low* or *high* arguments were provided, the optimizer will use the values
from the metadata provided in the variable deceleration. 

For ``comp.x`` the optimizer will try floats between 0.0 and 100.0. For ``comp.y`` the optimizer
will try integers between 10 and 100. For ``comp.z`` the optimizer will pick from
the list of allowed values: ``[-10,-5,0,7]``. 

You can override the low and high values from the metadata if you want
the optimizer to use a different range instead of the default. 

.. testcode:: Genetic
    
    top.optimizer.add_des_var('comp.w',low=5.0,high=7.0)

Now, for ``comp.x`` the optimizer will only try values between 5.0 and 7.0. Note that `low` and `high`
are only applicable to Float and Int public variables. For Enum public variables, `low` and `high`
are not applicable.


Configuration
~~~~~~~~~~~~~

When setting the `objective` attribute you can specify a single 
public variable or a more complex function, such as 

.. testcode:: Genetic

    top.optimizer.objective = "comp.x"
    
or 

.. testcode:: Genetic

    top.optimizer.objective = "2*comp.x+comp.y+3*comp.z"

In the second example above, a more complex objective was created where the overall objective was 
a weighted combination of ``comp.x, comp.y,`` and ``comp.z``. 

To set the optimizer to either minimize or maximize your objective, you set the
``opt_type`` attribute of the driver to "minimize" or "maximize."

.. testcode:: Genetic

    top.optimizer.opt_type = "minimize"
    
You can control the size of the population in each generation and the maximum number of generations in 
your optimization with the ``population_size`` and ``generations`` attributes. 
    
.. testcode:: Genetic

    top.optimizer.population_size = 80
    top.optimizer.generations = 100
    
As you increase the population size, you are effectively adding diversity in to the gene pool of your
optimization. A large population means that a larger number of individuals from a given generation will
be chosen to provide genetic material for the next generation. So there is a better chance that weaker individuals
will pass on their genes. This diversity helps to ensure that your optimization will 
find a true global optimum within the allowed design space. However, it also serves to slow down the 
optimization because of the increased number of function evaluations necessary for each generation. 

Picking an appropriate value for the maximum number of generations will depend highly on the specifics of 
your problem. Setting this number too low will likely prevent the optimization from converging on a true 
optimum. Setting it too high will help you find the true optimum, but you may end up wasting the computation
time on later generations where the optimum has been found. 

You can further control the behavior of the genetic algorithm by setting the ``crossover_rate``,
``mutation_rate``, ``selection_method``, and ``elitism`` attributes. These settings will allow you to
fine-tune the convergence of your optimization to achieve the desired result; however, for many
optimizations the default values will work well and won't need to be changed. 

The ``crossover_rate`` controls the rate at which the crossover operator gets applied to the genome of a set of
individuals who are reproducing. The allowed values are between 0.0 and 1.0. A higher rate will mean  that more of
the genes are swapped between parents. The result will be a more uniform population and better searching of the
design space. If the rate is set too high, then it is likely that stronger individuals could be lost to churn. 

.. testcode:: Genetic

    top.optimizer.crossover_rate = 0.9

The ``mutation_rate`` controls how likely any particular gene is to experience a mutation. A low, but non-zero,
mutation rate will help prevent stagnation in the gene pool by randomly moving the values of genes. If this 
rate is set too high, the algorithm basically degrades into a random search through the design space. The
allowed values are between 0.0 and 1.0. 

.. testcode:: Genetic

    top.optimizer.mutation_rate = .02

In a pure genetic algorithm, it is possible that your best performing individual will not survive from one
generation to the next due to competition, mutation, and crossover. If you want to ensure that the best 
individual survives in tact from one generation to the next, then turn on the `elitism` flag for your
optimization. This will ensure that the best individual is always copied to the next generation no matter
what. 

.. testcode:: Genetic

    top.optimizer.elitism = True

A number of different commonly used selection algorithms are available. The default algorithm is the Roulette
Wheel Algorithm, but Tournament Selection, Rank Selection, and Uniform Selection are also available. The
``selection_method`` attribute allows you to select the algorithm; allowed values are: "roulette_wheel," 
"tournament," "rank," and "uniform."

.. testcode:: Genetic
    
    top.optimizer.selection_method="rank"


*The Case Iterator*
+++++++++++++++++++

.. todo:: Case Iterator documentation

Factories
---------

Traits
------

