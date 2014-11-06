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
basic interface conforms to OpenMDAO's driver API.

The OpenMDAO NEWSUMT driver can be imported from ``openmdao.lib.drivers.api``.

.. testcode:: NEWSUMT_load

    from openmdao.lib.drivers.api import NEWSUMTdriver

Typically, NEWSUMT will be used as a driver in the top level assembly, although it
can also be used in a subassembly as part of a nested driver scheme. Using the
OpenMDAO script interface, a simple optimization problem can be set up as
follows:

.. testcode:: NEWSUMT_load

    from openmdao.main.api import Assembly
    from openmdao.examples.simple.paraboloid import Paraboloid
    from openmdao.lib.drivers.api import NEWSUMTdriver
        
    class Top(Assembly):
        """Constrained optimization of the Paraboloid with whatever optimizer
        we want."""
            
        def configure(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
                
            # Create Paraboloid component instances
            self.add('comp', Paraboloid())
        
            # Create Optimizer instance
            self.add('driver', NEWSUMTdriver())
                
            # Driver process definition
            self.driver.workflow.add('comp')
        
            # Objective 
            self.driver.add_objective('comp.f_xy')
                
            # Design Variables 
            self.driver.add_parameter('comp.x', low=-50., high=50.)
            self.driver.add_parameter('comp.y', low=-50., high=50.)

            # NEWSUMT Flags
            self.driver.iprint = 0
            self.driver.itmax = 30
            

This first section of code defines an assembly called Top.
This assembly contains a Paraboloid component and a NEWSUMTdriver, both of
which are created and added inside the ``__init__`` function with ``add``. The
Paraboloid component is also added to the driver's workflow. The objective
function, design variables, constraints, and any NEWSUMT parameters are also
assigned in the ``__init__`` function.

.. index:: gradients, Hessians

**Basic Parameters**

This section contains the basic parameters for NEWSUMT. 

The default behavior for NEWSUMT is to calculate its own gradients and Hessians
of the objective and constraints using a first-order forward finite difference.
The second derivatives are approximated from the first order differences. Presently,
OpenMDAO's built-in differentiation capability does not support second derivatives, so
NEWSUMT's gradient and Hessian calculation is the only available option.

If you want to use NEWSUMT for the finite difference calculation and want the
same finite difference step size in all your variables, you can set the ``default_fd_stepsize``
parameter.

.. testcode:: NEWSUMT_fd
    :hide:
    
    from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    from openmdao.main.api import set_as_top
    self = set_as_top(OptimizationUnconstrained())
    
.. testcode:: NEWSUMT_fd

    self.driver.default_fd_stepsize = .0025

The default step size will be used for all parameters for which you have not
set the ``fd_step`` attribute.

If your problem uses linear constraints, you can improve the efficiency of the
optimization process by designating those that are linear functions of the design
variables as follows:

.. testcode:: NEWSUMT_fd

    self.driver.add_constraint('paraboloid.x - paraboloid.y >= 15.0')
    self.driver.add_constraint('paraboloid.x*paraboloid.y < 77.0', linear=True)

Note that this method of specification replaces the use of the ``iln_linear`` flag.

Similarly, NEWSUMT has a flag parameter to indicate whether the objective
function is linear or nonlinear. Setting ``lobj`` to 1 indicates a linear
objective function. Setting it to 0, which is the default value, indicates a
nonlinear objective function.

.. testcode:: NEWSUMT_fd

        self.driver.lobj = 0

The `iprint` parameter can be used to display diagnostic
messages. These messages are currently sent to the standard
output.

.. testcode:: NEWSUMT_fd

        self.driver.iprint = 0

Higher positive values of `iprint` turn on the display of more levels of output, as summarized below.

===============  ========================================================
Value            Result
===============  ========================================================
``iprint = 0``   All output is suppressed, including warnings
---------------  --------------------------------------------------------
``iprint = 1``   Print initial and final designs only
---------------  --------------------------------------------------------
``iprint = 2``   Print brief results of analysis for initial and final designs 
                 together with minimal intermediate information
---------------  --------------------------------------------------------
``iprint = 3``   Detailed printing
---------------  --------------------------------------------------------
``iprint = 4``   Debugging printing
===============  ========================================================


**Controlling the Optimization**

NEWSUMT provides a variety of parameters to control the convergence criteria for an optimization.

The maximum number of iterations is specified by setting the `itmax` parameter.
The default value is 10.

.. testsetup:: NEWSUMT_show
    
    from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    from openmdao.main.api import set_as_top
    self = set_as_top(OptimizationUnconstrained())

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
