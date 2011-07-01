.. index:: constraints, CONMIN
.. _`constrained-optimization`:

Building a Model - Constrained Optimization using CONMIN
=========================================================

Usually, an optimization problem also contains constraints that reduce the design space.
*Constraints* are equations or inequalities that are expressed as functions of the design
variables. You will add a constraint to your model in ``optimization_unconstrained.py``.
First, copy the file and give the new file the name ``optimization_constrained.py``. Inside
of this file, change the name of the assembly from ``OptimizationUnconstrained`` to
``OptimizationConstrained``. Don't forget to also change it in the bottom section where it is
instantiated and run.

In OpenMDAO, you can construct a constraint with an expression string, which is
an equation or inequality built using available variables with Python
mathematical syntax and functions. CONMIN supports inequality
constraints but not equality constraints.

You want to add the constraint ``x-y >= 15`` to this problem. The unconstrained
minimum violates this constraint, so a new minimum must be found by
the optimizer. You can add a constraint to your existing ``OptimizationUnconstrained``
model by adding one line to the ``initialize`` function:

.. testsetup:: simple_model_Unconstrained_pieces

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import CONMINdriver
    from openmdao.examples.simple.paraboloid import Paraboloid
    from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    
    self = OptimizationUnconstrained()
    
.. testcode:: simple_model_Unconstrained_pieces

        # CONMIN Constraints
        self.driver.add_constraint('paraboloid.x-paraboloid.y >= 15.0')

The ``add_constraint`` method is used to add a constraint to the driver.

Please add this line to the ``__init__`` function in
``optimization_constrained.py`` and save it. Execute it by typing:

::

        python optimization_constrained.py
    
When it is executed, it should produce this output:

:: 

    [ CONMIN output not shown ]
    CONMIN Iterations:  6
    Minimum found at (7.175775, -7.824225)
    Elapsed time:  0.0295481681824 seconds
    
Notice that the minimum of the constrained problem is different from the minimum of
the unconstrained problem.

Now you are ready to add derivatives to your comnponent, which you will do in the next section.

