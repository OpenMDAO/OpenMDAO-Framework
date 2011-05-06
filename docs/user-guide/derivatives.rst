.. index:: derivatives, Finite Difference, Hessians, constraints

.. _Derivatives:

Derivatives
===========

OpenMDAO provides the capability for a driver to determine the derivative of
its outputs (typically the objective and constraints) with respect to its
inputs (the parameters). It supports both first derivative (gradients) and second
derivatives (Hessians). This capability is particularly useful
for gradient-descent optimizers and Newton solvers, where the solution moves
toward the optimum value by traveling in the direction of the steepest
gradient of the objective function.

Some drivers, such as CONMINdriver and NEWSUMTdriver, include their own methods for
calculating derivatives. These are usually based on a finite difference
approximation of specific derivatives, and thus requires one or more additional
evaluations of the driver's workflow. OpenMDAO includes its own differentiator that
uses the Finite Difference method to calculate both gradients and Hessians. (See the
section :ref:`FiniteDifference` in the *Standard Library Reference.*)

Sometimes, the solution process can be sped up by having a component supply
its own derivatives. These derivatives may be analytical, or they might be
estimated by some other means. The derivatives provided by a component may be
more accurate than those estimated by finite differencing the component and
are also independent of the choice of step-size parameter.

.. index:: Fake Finite Difference

OpenMDAO can take advantage of the derivatives that a component supplies to
potentially speed up the computation through the process of *Fake Finite
Difference (FFD).* During a finite difference step, a component can be told to
use its derivatives and the first (or second) Taylor series term to produce
its output. This output is not an accurate output at the requested input, but
it is *the output that yields the exact derivative when finite differenced*.

A simple tutorial that covers how to specify derivatives can be found in
:ref:`Adding-Derivatives-to-Your-Components`.

.. _Calculating-Derivatives-with-Finite-Difference:

Calculating Derivatives with Finite Difference
------------------------------------------------

In OpenMDAO finite differencing is accomplished by using the ``FiniteDifference`` object, which
is a part of a special class of ``Differentiator`` objects that a driver uses to provide 
derivatives between the parameters and the constraints and objectives. If a driver supports
derivative calculation (like the CONMIN and NEWSUMT optimizers do), then it contains a socket
called *differentiator*, into which a FiniteDifference instance can be placed:


.. testcode:: NEWSUMT_fd

    from openmdao.examples.enginedesign.vehicle import Vehicle
    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import NEWSUMTdriver
    from openmdao.lib.differentiators.finite_difference import FiniteDifference

    class EngineOptimization(Assembly):
        """ Top level assembly for optimizing a vehicle. """
    
        def __init__(self):
            """ Creates a new Assembly containing a DrivingSim and an optimizer"""
        
            super(EngineOptimization, self).__init__()

            # Create NEWSUMT Optimizer instance
            self.add('driver', NEWSUMTdriver())
        
            # Create Vehicle component instances
            self.add('vehicle', Vehicle())

            # add Vehicle to workflow
            self.driver.workflow.add('vehicle')
        
            # CONMIN Objective 
            self.driver.add_objective('vehicle.fuel_burn')
        
            # CONMIN Design Variables 
            self.driver.add_parameter('vehicle.spark_angle', low=-50. , high=10.)
            self.driver.add_parameter('vehicle.bore', low=65. , high=100.)

            # Use OpenMDAO to calculate gradients
            self.driver.differentiator = FiniteDifference(self.driver)
            self.driver.differentiator.form = 'central'
            self.driver.differentiator.default_stepsize = 1.0e-6

If the driver has its own internal gradient calculation, it is disabled when
you fill the differentiator socket, and the FiniteDifference component is used
for the calculation.

The FiniteDifference gradient calculation supports forward, central, and
backward differencing via the attribute ``form``. The default value is
``'central'`` for central differencing. You can also define the step size that is
applied for all of the parameter inputs when they are differenced. The default
value is ``1.0e-6``. Note also that the parameter interface allows you to specify a
separate step-size value for each parameter using the keyword argument ``fd_step`` in
the ``add_parameter`` call. The code fragment above shows an example of all of these.

