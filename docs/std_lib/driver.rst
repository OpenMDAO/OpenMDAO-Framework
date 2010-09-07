
.. index:: Drivers

.. _Drivers:

Drivers
=======

.. index:: BroydenSolver

.. _BroydenSolver:

*BroydenSolver*
~~~~~~~~~~~~~~~

The BroydenSolver can be used to solve for the set of inputs
(independents) that are needed to to make a model satisfy an equation (the
dependent equation) that is a function of model outputs. BroydenSolver is based
on the quasi-Newton-Raphson algorithms found in SciPy's nonlinear solver library.
As the name implies, a Broyden update is used to approximate the Jacobian matrix.
In fact, no Jacobian is evaluated with these algorithms, so they are quicker than
a full Newton solver, but they may not be suitable for all problems.

To see how to use the BroydenSolver, consider a problem where we'd like to solve
for the intersection of a line and a parabola. We can implement this as a single
component. (It is also possible to implement it as two components if you'd
prefer. See :ref:`Tutorial-MDAO-Architectures` to learn how to broadcast variables.)

.. testcode:: Broyden

    from openmdao.lib.api import Float
    from openmdao.main.api import Component
    
    class MIMOSystem(Component):
        """ Two equations, two unknowns """
        
        x = Float(10.0, iotype="in", doc="Input 1")
        y = Float(10.0, iotype="in", doc="Input 2")
        
        f_xy = Float(0.0, iotype="out", doc="Output 1")
        g_xy = Float(0.0, iotype="out", doc="Output 2")
        
        def execute(self):
            """ Evaluate:
            f_xy = 2.0*x**2 - y + 2.0 
            g_xy = 2.0*x - y - 4.0 
            """
          
            x = self.x
            y = self.y
            
            self.f_xy = 2.0*x**2 - y + 2.0 
            self.g_xy = 2.0*x - y + 4.0 

Notice that this is a two-input problem -- the variables are *x* and *y*. There are
also two equations that need to be satisfied: the equation for the line and
the equation for the parabola. There are actually two solutions to this set
of equations. The solver will return the first one that it finds. You can
usually find other solutions by starting the solution from different initial
points. We start at ``(10, 10)``, as designated by the default values for the variables
*x* and *y*.

Next, we build a model that uses the Broyden solver to find a root for the 
equations defined in MIMOSystem.

.. testcode:: Broyden

    from openmdao.lib.api import BroydenSolver
    from openmdao.main.api import Assembly
    
    class SolutionAssembly(Assembly):
        """ Solves for the root of MIMOSystem. """
    
        def __init__(self):
            """ Creates a new Assembly with this problem
            root at (0,1)
            """
            
            super(SolutionAssembly, self).__init__()    
            
            self.add('driver', BroydenSolver())
            self.add('problem', MIMOSystem())
        
            self.driver.workflow.add(self.problem)
        
            self.driver.add_parameter('problem.x', low=-1.0e99, high=1.0e99)
            self.driver.add_parameter('problem.y', low=-1.0e99, high=1.0e99)
        
            self.driver.add_constraint('problem.f_xy = 0.0')
            self.driver.add_constraint('problem.g_xy = 0.0')
            self.driver.itmax = 20
            self.driver.alpha = .4
            self.driver.tol = .000000001
            
The parameters are the independent variables that the solver is allowed to vary. The
method ``add_parameter`` is used to define these. Broyden does not utilize
the low and high arguments, so they are set to some large arbitrary negative and positive values.

The equations that we want to satisfy are added as equality constraints using the
``add_constraint`` method. We want to find *x* and *y* that satisfy ``f_xy=0`` and ``g_xy =0``,
so these two equations are added to the solver.

Both the ``add_parameter`` and ``add_constraint`` methods are presented in more detail in
:ref:`Tutorial:-MDAO-Architectures`.

The resulting solution should yield:

.. doctest:: Broyden

    >>> top = SolutionAssembly()
    >>> top.run()
    >>> print top.problem.x, top.problem.y
    1.61... 7.23...

.. index:: algorithm, Enum, SciPy

There are five parameters that control the solution process in the Broyden solver.

**algorithm** -- SciPy's nonlinear package contained several algorithms for solving
a set of nonlinear equations. Three of these methods were considered by their
developers to be of good quality, so those three were implemented as part of 
the BroydenSolver. The variable *algorithm* is an Enum where the following values
represent the algorithms that follow.

- **broyden2**: Broyden's second method -- the same as broyden1 but
  updates the inverse Jacobian directly
- **broyden3**: Broyden's third method -- the same as broyden2, but instead of
  directly computing the inverse Jacobian, it remembers how to construct it using
  vectors. When computing ``inv(J)*F``, it uses those vectors to compute this
  product, thus avoiding the expensive NxN matrix multiplication. 
- **excitingmixing**: The excitingmixing algorithm. ``J=-1/alpha``

The default value for *algorithm* is ``"broyden2"``.

.. testsetup:: Broyden3

    from openmdao.lib.api import BroydenSolver
    from openmdao.main.api import Assembly
    
    self = Assembly()
    self.add('driver', BroydenSolver())

.. testcode:: Broyden3

    self.driver.algorithm = "broyden2"
    
**itmax** -- This parameter specifies the maximum number of iterations before
BroydenSolver terminates. The default value is 10.
    
.. testcode:: Broyden3

    self.driver.itmax = 10
    
**alpha** -- This parameter specifies the mixing coefficient for the algorithm. The
mixing coefficient is a linear scale factor applied to the update of the parameters, so
increasing it can lead to quicker convergence but can also lead to instability. The 
default value is 0.4. If you use the *excitingmixing* algorithm, you should try a lower
value, such as 0.1.
    
.. testcode:: Broyden3

    self.driver.alpha = 0.1
    
**tol** -- Convergence tolerance for the solution. Iteration ends when the constraint
equation is satisfied within this tolerance. The default value is 0.00001.
    
.. testcode:: Broyden3

    self.driver.tol = 0.00001
    
**alphamax** -- This parameter is only used for the *excitingmixing* algorithm
where the mixing coefficient is adaptively adjusted. It specifies the maximum
allowable mixing coefficient for adaptation. The default value is 1.0.

.. testcode:: Broyden3

    self.driver.alphamax = 1.0
    
(See the source documentation for more information on
 :ref:`BroydenSolver<openmdao.lib.drivers.broydensolver.py>`.)


.. index:: Case Iterator Driver

.. _Case-iterator-driver:

*CaseIteratorDriver*
~~~~~~~~~~~~~~~~~~~~~~

(See the source documentation for more information on 
:ref:`CaseIterDriver<openmdao.lib.drivers.caseiterdriver.py>`.)

.. todo::

    Discuss the CaseIteratorDriver in more detail.

    
.. index:: CONMIN

.. _CONMINDriver:

*CONMINDriver*
~~~~~~~~~~~~~~~~~~~

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
+++++++++++++++

The CONMIN code contains a number of different parameters and switches that
are useful for controlling the optimization process. These can be subdivided
into those parameters that will be used in a typical optimization problem and
those that are more likely to be used by an expert user.

For the simplest possible unconstrained optimization problem, CONMIN just needs
an objective function and one or more decision variables (parameters.) The
basic interface conforms to OpenMDAO's driver API, which is discussed in 
:ref:`Driver-API`, and covers how to assign design variables, constraints, and
objectives.

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
            self.add('driving_sim', DrivingSim())

            # Create CONMIN Optimizer instance
            self.add('driver', CONMINdriver())
        
            # add DrivingSim to workflow
            driver.workflow.add(self.driving_sim)
        
            # CONMIN Objective 
            self.driver.objective = 'driving_sim.accel_time'
                
            # CONMIN Design Variables 
            self.driver.add_parameter('driving_sim.spark_angle', low=-50. , high=10.)
            self.driver.add_parameter('driving_sim.bore', low=65. , high=100.)

            # CONMIN Objective = Maximize weighted sum of EPA city and highway fuel economy 
            self.driver.objective = '-(.93*driving_sim.EPA_city + 1.07*driving_sim.EPA_highway)'

This first section of code defines an assembly called *EngineOptimization.*
This assembly contains a DrivingSim component and a CONMIN driver, both of
which are created and added inside the ``__init__`` function with *add*. The
DrivingSim component is also added to the driver's workflow. The objective
function, design variables, constraints, and any CONMIN parameters are also
assigned in the ``__init__`` function. The specific syntax for all of these is
discussed in :ref:`Driver-API`.


Controlling the Optimization
++++++++++++++++++++++++++++

It is often necessary to control the convergence criteria for an optimization.
The CONMIN driver allows control over both the number of iterations
before termination as well as the convergence tolerance (both absolute and
relative).

The maximum number of iterations is specified by setting the *itmax* parameter.
The default value is 10.

.. testsetup:: CONMIN_show
    
    from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
    self = EngineOptimization()

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
Value         Result
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

If your problem uses linear constraints, you can improve the efficiency of the
optimization process by designating those that are linear functions of the design
variables as follows:

.. testcode:: CONMIN_show

    map(self.driver.add_constraint, ['driving_sim.stroke - driving_sim.bore',
                               '1.0 - driving_sim.stroke * driving_sim.bore'])
    self.cons_is_linear = [1, 0]

Here, the first constraint is linear, and the second constraint is nonlinear. If 
*cons_is_linear* is not specified, then all the constraints are assumed to be
nonlinear. Note that the original CONMIN parameter for this is *ISC.* If
your constraint includes some framework output in the equation, then it is 
probably not a linear function of the design variables.

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
++++++++++++++++
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

(See the source documentation for more information on 
:ref:`CONMINdriver<openmdao.lib.drivers.conmindriver.py>`.)

.. index:: DOEdriver, design of experiments

.. _DOEdriver:

*DOEdriver*
~~~~~~~~~~~

The DOEdriver provides the capability to execute a DOE on a workflow.
This Driver supports the IHasParameters interface. At execution time, 
the driver will use the list of parameters added to it by the user to 
create a specific DOE and then iteratively execute the DOE cases on the
workflow. 

The user can pick from any of the DOEgenerators provided in the standard
library, or provide their own custom instance of a DOEgenerator. A DOEgenerator
must be plugged into the DOEgenerator socket on the DOEdriver in order to
operate. 
    
    .. testcode:: DOEdriver
    
        from openmdao.main.api import Assembly
        from openmdao.lib.api import DOEdriver
        from openmdao.lib.doegenerators.full_factorial import FullFactorial

        from openmdao.examples.singleEI.branin_component import BraninComponent
        
        class Analysis(Assembly): 
            def __init__(self,doc=None): 
                super(Analysis,self).__init__()
                
                self.add('branin', BraninComponent())
                self.add('driver', DOEdriver())
                self.driver.workflow.add(self.branin)

                self.driver.add_parameter('branin.x')
                self.driver.add_parameter('branin.y')
                
                #use a full factorial DOE with 2 variables, and 3 levels
                #   for each variable
                self.driver.DOEgenerator = FullFactorial(3,2)
   
The *min* and *max* metadata of the parameters are used to denote the range for
each variable over which the DOE will span.
                
(See the source documentation for more information on 
:ref:`DOEdriver<openmdao.lib.drivers.doedriver.py>`.)

.. index:: Fixed Point Iterator

.. _FixedPointIterator:

*FixedPointIterator*
~~~~~~~~~~~~~~~~~~~~

The FixedPointIterator is a simple solver that can solve a single-input
single-output problem using fixed point iteration. It provides a way
to iterate on a single input to match an output. In other words, fixed
point iteration can be used to solve the equation ``x = f(x)``. By extension,
FixedPointIterator can be used to close a loop in the data flow. The
algorithm is probably useful for some problems, so it is included here.
However, it may require more functional evaluations than the BroydenSolver.

As an example, let's implement a component that can be run iteratively to
produce the square root of a number.

.. testcode:: FPI

    from openmdao.lib.api import Float
    from openmdao.main.api import Component
    
    class Babylonian(Component):
        """ The Babylonians had a neat way of calculating square
        roots using Fixed Point Iteration"""
        
        x = Float(1.0, iotype="in", doc="Input is x")
        y = Float(iotype="out", doc="Output is y")
        
        def execute(self):
            """ Iterate to find the square root of 2, the Babylonian way:
            """
          
            x = self.x
            self.y = 0.5*(2.0/x + x)
            
An assembly with this component and the FixedPointIterator would look
like this.

.. testcode:: FPI

    from openmdao.lib.api import FixedPointIterator
    from openmdao.main.api import Assembly
    
    class SolutionAssembly(Assembly):
        """ Solves for the root of MIMOSystem. """
    
        def __init__(self):
            """ Creates a new Assembly with this problem
            the answer should be 1.4142.....
            """
            
            super(SolutionAssembly, self).__init__()    
            
            self.add('driver', FixedPointIterator())
            self.add('problem', Babylonian())
        
            self.driver.workflow.add(self.problem)
            
            # Set our independent and dependent
            self.driver.x_in = 'problem.x'    
            self.driver.x_out = 'problem.y'

The *x* input and the *F(x)* output are specified as Expressions and assigned to
``x_in`` and ``x_out`` in the solver.
            
.. doctest:: FPI

    >>> top = SolutionAssembly()
    >>> top.run()
    >>> print top.problem.x
    1.4142...

Two additional parameters control the FixedPointIterator. The
parameter ``tolerance`` sets the convergence tolerance for the comparison
between value of ``x_out`` at the current iteration and the previous iteration.
The default value for tolerance is 0.00001. The parameter ``max_iteration``
specifies the number of iterations to run. The default value for
``max_iterations`` is 25.

A more useful example in which the FixedPointIterator is used to converge two
coupled components is shown in :ref:`Tutorial-MDAO-Architectures` .

(See the source documentation for more information on 
:ref:`FixedPointIterator<openmdao.lib.drivers.iterate.py>`.)

.. index:: Genetic


.. _`Genetic`:

*Genetic*
~~~~~~~~~

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
++++++++++++++++

IOtraits are added to Genetic and become optimization parameters. Genetic will vary the set of
parameters to search for an optimum. Genetic supports three variable types:
:term:`Float`, :term:`Int`, and :Term:`Enum`. These types can be used as parameters in any 
optimization. 

You add design variables to Genetic using the ``add_parameter`` method.

.. testcode:: Genetic

    from openmdao.main.api import Assembly,Component, set_as_top
    from openmdao.lib.api import Genetic
    from openmdao.lib.api import Float,Int,Enum
    
    class SomeComp(Component):
        """Arbitrary component with a few variables, but which does not really do 
           any calculations
        """

        w = Float(0.0, low=-10, high=10, iotype="in")
    
        x = Float(0.0, low=0.0, high=100.0, iotype="in")
        y = Int(10, low=10, high=100, iotype="in")
        z = Enum([-10, -5, 0, 7], iotype="in")
    
    class Simulation(Assembly):
        """Top Level Assembly used for simulation"""
    
        def __init__(self):
            """Adds the Genetic driver to the assembly"""
        
            super(Simulation,self).__init__()
        
            self.add('driver', Genetic())
            self.add('comp', SomeComp())
        
            # Driver process definition
            self.driver.workflow.add(self.comp)

            self.driver.add_parameter('comp.x')
            self.driver.add_parameter('comp.y')
            self.driver.add_parameter('comp.z')
    
    top = Simulation()        
    set_as_top(top)
        
In the above example, three parameters were added to the optimizer. The optimizer 
figures out for itself what type of variable it is and behaves appropriately. In all three
cases, since no *low* or *high* arguments were provided, the optimizer will use the values
from the metadata provided in the variable deceleration. 

For ``comp.x`` the optimizer will try floats between 0.0 and 100.0. For ``comp.y`` the optimizer
will try integers between 10 and 100. For ``comp.z`` the optimizer will pick from
the list of allowed values: ``[-10,-5,0,7]``. 

You can override the low and high values from the metadata if you want
the optimizer to use a different range instead of the default. 

.. testcode:: Genetic
    
    top.driver.add_parameter('comp.w', low=5.0, high=7.0)

Now, for ``comp.x`` the optimizer will only try values between 5.0 and 7.0. Note that `low` and `high`
are only applicable to Float and Int variables. For Enum variables, `low` and `high`
are not applicable.

Configuration
+++++++++++++

When setting the `objective` variable you can specify a single 
variable name or a more complex function, such as 

.. testcode:: Genetic

    top.driver.objective = "comp.x"
    
or 

.. testcode:: Genetic

    top.driver.objective = "2*comp.x + comp.y + 3*comp.z"

In the second example above, a more complex objective function was created where the overall objective was 
a weighted combination of ``comp.x, comp.y,`` and ``comp.z``. 

To set the optimizer to either minimize or maximize your objective, you set the
``opt_type`` variable of Genetic to "minimize" or "maximize."

.. testcode:: Genetic

    top.driver.opt_type = "minimize"
    
You can control the size of the population in each generation and the maximum number of generations in 
your optimization with the ``population_size`` and ``generations`` variables. 
    
.. testcode:: Genetic

    top.driver.population_size = 80
    top.driver.generations = 100
    
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
``mutation_rate``, ``selection_method``, and ``elitism`` variables. These settings will allow you to
fine-tune the convergence of your optimization to achieve the desired result; however, for many
optimizations the default values will work well and won't need to be changed. 

The ``crossover_rate`` controls the rate at which the crossover operator gets applied to the genome of a set of
individuals who are reproducing. The allowed values are between 0.0 and 1.0. A higher rate will mean  that more of
the genes are swapped between parents. The result will be a more uniform population and better searching of the
design space. If the rate is set too high, then it is likely that stronger individuals could be lost to churn. 

.. testcode:: Genetic

    top.driver.crossover_rate = 0.9

The ``mutation_rate`` controls how likely any particular gene is to experience a mutation. A low, but non-zero,
mutation rate will help prevent stagnation in the gene pool by randomly moving the values of genes. If this 
rate is set too high, the algorithm basically degrades into a random search through the design space. The
allowed values are between 0.0 and 1.0. 

.. testcode:: Genetic

    top.driver.mutation_rate = .02

In a pure genetic algorithm, it is possible that your best performing individual will not survive from one
generation to the next due to competition, mutation, and crossover. If you want to ensure that the best 
individual survives intact from one generation to the next, then turn on the `elitism` flag for your
optimization. This will ensure that the best individual is always copied to the next generation no matter
what. 

.. testcode:: Genetic

    top.driver.elitism = True

A number of different commonly used selection algorithms are available. The default algorithm is the Roulette
Wheel Algorithm, but Tournament Selection, Rank Selection, and Uniform Selection are also available. The
``selection_method`` variable allows you to select the algorithm; allowed values are: ``"roulette_wheel," 
"tournament," "rank,"`` and ``"uniform"``.

(See the source documentation for more information on :ref:`Genetic<openmdao.lib.drivers.genetic.py>`.)

.. testcode:: Genetic
    
    top.driver.selection_method="rank"
