.. index:: NEWSUMTDriver

.. _NEWSUMTDriver:

*NEWSUMTDriver*
~~~~~~~~~~~~~~~~

:term:`NEWSUMT` is a Fortran subroutine for solving linear and nonlinear constrained or unconstrained
function minimization problems. It has been included in the OpenMDAO standard library to provide users
with a basic gradient-based optimization algorithm.


The minimization algorithm used in NEWSUMT is a sequence of unconstrained minimizations technique (SUMT)
where the modified Newton's method is used for unconstrained function minimizations.

If analytic gradients of the objective or constraint functions are not
available, this information is calculated by finite difference.

NEWSUMT treats inequality constraints in a way that is especially well suited to engineering design
applications. 

More information on NEWSUMT can be found in the `NEWSUMT Users Guide
<http://openmdao.org/releases/misc/newsumt-manual.pdf>`_.


**Basic Interface**

The NEWSUMT code contains a number of different parameters and switches that
are useful for controlling the optimization process. These can be subdivided
into those parameters that will be used in a typical optimization problem and
those that are more likely to be used by an expert user.

For the simplest possible unconstrained optimization problem, NEWSUMT just needs
an objective function and one or more decision variables (parameters.) The
basic interface conforms to OpenMDAO's driver API, which is discussed in 
:ref:`Driver-API`. This document covers how to assign design variables, constraints, and
objectives.

The OpenMDAO NEWSUMT driver can be imported from ``openmdao.lib.drivers.api``.

.. testcode:: NEWSUMT_load

    from openmdao.lib.drivers.api import NEWSUMTdriver

Typically, NEWSUMT will be used as a driver in the top level assembly, though it
can be also used in a subassembly as part of a nested driver scheme. Using the
OpenMDAO script interface, a simple optimization problem can be set up as
follows:

.. testcode:: NEWSUMT_load

    from openmdao.main.api import Assembly
    from openmdao.examples.enginedesign.vehicle import Vehicle
    from openmdao.lib.drivers.api import CONMINdriver

    class EngineOptimization(Assembly):
        """ Top level assembly for optimizing a vehicle. """
    
        def __init__(self):
            """ Creates a new Assembly for vehicle performance optimization."""
            
            super(EngineOptimization, self).__init__()

            # Create CONMIN Optimizer instance
            self.add('driver', NEWSUMTdriver())
        
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
This assembly contains a DrivingSim component and a NEWSUMTdriver, both of
which are created and added inside the ``__init__`` function with ``add``. The
DrivingSim component is also added to the driver's workflow. The objective
function, design variables, constraints, and any NEWSUMT parameters are also
assigned in the ``__init__`` function. The specific syntax for all of these is
discussed in :ref:`Driver-API`.

.. index:: gradients, Hessians

**Basic Parameters**

This section contains the basic parameters for NEWSUMT. 

The default behavior for NEWSUMT is to calculate its own gradients and Hessians
of the objective and constraints using a first-order forward finite difference.
The second derivatives are approximated from the first order differences. You
can replace NEWSUMT's finite difference with OpenMDAO's built-in capability by
inserting a differentiator into the Differentiator slot in the driver, as shown
in :ref:`Calculating-Derivatives-with-Finite-Difference`.

If you want to use NEWSUMT for the finite difference calculation and want the
same finite difference step size in all your variables, you can set the ``default_fd_stepsize``
parameter.

.. testcode:: NEWSUMT_fd
    :hide:
    
    from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
    self = EngineOptimization()
    
.. testcode:: NEWSUMT_fd

    self.driver.default_fd_stepsize = .0025

The default step size will be used for all parameters for which you have not
set the ``fd_step`` attribute.

When using NEWSUMT, if you have any linear constraints, it may be
advantageous to specify them as such so that NEWSUMT can treat them
differently. Use the integer array ``ilin`` to designate whether a constraint
is linear. A value of 0 indicates that that constraint is non-linear, while a
value of 1 indicates that that the constraint is linear. This parameter is
optional, and when it is omitted, all constraints are assumed to be nonlinear.

.. testcode:: NEWSUMT_show

    map(self.driver.add_constraint, ['vehicle.stroke < vehicle.bore',
                               'vehicle.stroke * vehicle.bore > 1.0'])
    self.driver.ilin_linear = [1, 0]


Similarly, NEWSUMT has a flag parameter to indicate whether the objective
function is linear or nonlinear. Setting ``lobj`` to 1 indicates a linear
objective function. Setting it to 0, which is the default value, indicates a
nonlinear objective function.

.. testcode:: NEWSUMT_show

        self.driver.lobj = 0

The ``jprint`` parameter can be used to display diagnostic
messages. These messages are currently sent to the standard
output.

.. testcode:: NEWSUMT_show

        self.driver.jprint = 0

Higher positive values of ``jprint`` turn on the display of more levels of output, as summarized below.

===============  ========================================================
Value            Result
===============  ========================================================
``jprint = -1``  All output is suppressed, including warnings
---------------  --------------------------------------------------------
``jprint = 0``   Print initial and final designs only
---------------  --------------------------------------------------------
``jprint = 1``   Print brief results of analysis for initial and final designs 
                 together with minimal intermediate information
---------------  --------------------------------------------------------
``jprint = 2``   Detailed printing
---------------  --------------------------------------------------------
``jprint = 3``   Debugging printing
===============  ========================================================


**Controlling the Optimization**

NEWSUMT provides a variety of parameters to control the convergence criteria for an optimization.

The maximum number of iterations is specified by setting the ``itmax`` parameter.
The default value is 10.

.. testsetup:: NEWSUMT_show
    
    from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
    self = EngineOptimization()

.. testcode:: NEWSUMT_show

        self.driver.itmax = 30

The convergence tolerance is controlled with six parameters. The following
table summarizes these parameters.

==========  ===================================================  =======
Parameter   Description                                          Default
==========  ===================================================  =======
``epsgsn``  Convergence criteria of the golden section           0.001
            algorithm used for the one-dimensional minimization
----------  ---------------------------------------------------  -------
``epsodm``  Convergence criteria of the unconstrained            0.001
            minimization
----------  ---------------------------------------------------  -------
``epsrsf``  Convergence criteria for the overall process         0.001
----------  ---------------------------------------------------  -------
``maxgsn``  Maximum allowable number of golden section           20
            iterations used for 1D minimization
----------  ---------------------------------------------------  -------
``maxodm``  Maximum allowable number of one-dimensional          6
            minimizations
----------  ---------------------------------------------------  -------
``maxrsf``  Maximum allowable number of unconstrained            15
            minimizations
==========  ===================================================  =======

.. testcode:: NEWSUMT_show

        self.driver.epsgsn = .000001
        self.driver.maxgsn = 40


**Advanced Options** 

There are additional options for advanced users.  More information on these parameters can be
found in the `NEWSUMT Users Guide <http://openmdao.org/releases/misc/newsumt-manual.pdf>`_. (This doc is
slow to load.)


=========  ===========================================  ===========
Parameter  Description                                  Default
=========  ===========================================  ===========
``mflag``  Flag for penalty multiplier.                 0
           If 0, initial value computed by NEWSUMT.
           If 1, initial value set by `ra`
---------  -------------------------------------------  -----------
``ra``     Penalty multiplier. Required if ``mflag=1``  1.0
---------  -------------------------------------------  -----------
``racut``  Penalty multiplier decrease ratio.           0.1
           Required if ``mflag=1``
---------  -------------------------------------------  -----------
``ramin``  Lower bound of penalty multiplier.           ``1.0e-13``
           Required if ``mflag=1``
---------  -------------------------------------------  -----------
``g0``     Initial value of the transition parameter    0.1
=========  ===========================================  ===========

*Source Documentation for newsumtdriver.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
