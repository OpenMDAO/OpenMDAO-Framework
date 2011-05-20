
.. index:: Differentiators

.. _Differentiators:

Differentiators
===============

A `differentiator` is a special object that can be used by a driver to calculate
the first or second derivatives of a workflow. The derivatives are calculated
from the parameter inputs to the objective and constraint outputs. Any driver
that has been decorated with the ``add_delegate`` decorator containing the
UsesGradients or UsesHessians delegates contains a Socket 
called `Differentiator`. This socket can take a Differentiator object.
 
.. index:: FiniteDifference

.. _FiniteDifference:

FiniteDifference
~~~~~~~~~~~~~~~~

The ``FiniteDifference`` differentiator provides the gradient vector and Hessian
matrix of the workflow using the finite difference method. For first derivatives,
you have a choice of forward, backward, or central differencing. Second
derivatives are calculated using the standard three-point difference for both
on-diagonal and off-diagonal terms.

The ``FiniteDifference`` differentiator can be used with the CONMIN or NEWSUMT
optimizer by plugging it into the differentiator socket.

.. testcode:: FD

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import NEWSUMTdriver
    from openmdao.lib.differentiators.finite_difference import FiniteDifference
    from openmdao.examples.simple.paraboloid import Paraboloid
    
    class OptimizationConstrained(Assembly):
        """Constrained optimization of the Paraboloid."""
            
        def __init__(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
                
            super(OptimizationConstrained, self).__init__()
        
            # Create Paraboloid component instances
            self.add('paraboloid', Paraboloid_Derivative())
        
            # Create Optimizer instance
            self.add('driver', NEWSUMTdriver())
                
            # Driver process definition
            self.driver.workflow.add('paraboloid')
                
            # Differentiator
            self.driver.differentiator = FiniteDifference(self.driver)
            self.driver.differentiator.form = 'central'
            self.driver.differentiator.default_stepsize = .0001
                
            # Objective 
            self.driver.add_objective('paraboloid.f_xy')
                
            # Design Variables 
            self.driver.add_parameter('paraboloid.x', low=-50., high=50., fd_step=.01)
            self.driver.add_parameter('paraboloid.y', low=-50., high=50.)
                
            # Constraints
            self.driver.add_constraint('paraboloid.x-paraboloid.y >= 15.0')
            
The only argument that ``FiniteDifference`` takes is the driver you are plugging into.

``FiniteDifference`` has two additional control variables. The ``form`` parameter is used to declare
which difference the first derivative will use. (The default is ``'central'``.) The ``default_stepsize`` parameter is used to set a
default finite difference step size. Note that you can declare a separate finite difference step size
for each parameter in the call to ``add_parameter``. Here, the finite difference step size for the input
``'x'`` to paraboloid is set to .01. If you don't specify ``fd_step`` for a parameter, then the default
step size is used.

Fake Finite Difference is fully supported by the finite difference generator.

