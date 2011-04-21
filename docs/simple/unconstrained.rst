.. index:: CONMIN

.. _`using-CONMIN`:


Building a Model - Unconstrained Optimization using CONMIN
===========================================================

Your next task is to build a model that finds the minimum value for the Paraboloid component
described above. This model contains the Paraboloid as well as a public domain gradient optimizer
called :term:`CONMIN`, for which a Python-wrapped driver has been included in OpenMDAO. As the name
implies, CONMIN finds the minimum of a function. The model can be found in
the Python file ``optimization_unconstrained.py``:

.. testcode:: simple_model_Unconstrained

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import CONMINdriver
    from openmdao.examples.simple.paraboloid import Paraboloid

    class OptimizationUnconstrained(Assembly):
        """Unconstrained optimization of the Paraboloid with CONMIN."""
    
        def __init__(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
            super(OptimizationUnconstrained, self).__init__()

            # Create CONMIN Optimizer instance
            self.add('driver', CONMINdriver())
        
            # Create Paraboloid component instances
            self.add('paraboloid', Paraboloid())

            # Driver process definition
            self.driver.workflow.add('paraboloid')

            # CONMIN Flags
            self.driver.iprint = 0
            self.driver.itmax = 30
            self.driver.fdch = .000001
            self.driver.fdchm = .000001
        
            # CONMIN Objective 
            self.driver.add_objective('paraboloid.f_xy')
        
            # CONMIN Design Variables 
            self.driver.add_parameter('paraboloid.x', low=-50., high=50.)
            self.driver.add_parameter('paraboloid.y', low=-50., high=50.)


Please create a file called ``optimization_unconstrained.py`` and copy this
block of code into it. We will discuss this code next.

.. index:: top level Assembly

An :term:`Assembly` is a container that can hold any number of components, drivers, and other
assemblies. An Assembly also manages the connections between the components that it
contains. In OpenMDAO the top assembly
in a model is called the *top level assembly.* In this problem, the top level assembly includes a
Paraboloid component and a CONMINdriver called *driver*. The name *driver* is special. When an 
assembly is executed, it looks for a Driver named *driver* and executes it. That Driver is the root
of what is called an :term:`iteration hierarchy`.

The OptimizationUnconstrained class is derived from Assembly instead of Component.

.. testsetup:: simple_model_Unconstrained_pieces

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import CONMINdriver
    from openmdao.examples.simple.paraboloid import Paraboloid
    from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    
    self = OptimizationUnconstrained()
    
.. testcode:: simple_model_Unconstrained_pieces

    class OptimizationUnconstrained(Assembly):
        """Unconstrained optimization of the Paraboloid with CONMIN."""
    
In the Paraboloid component, you created an ``execute`` function to tell it what to do when the
component is run. The ``OptimizationUnconstrained`` assembly does not need an ``execute`` function because
the Assembly class already has one that is sufficient for most cases. However, this assembly does
need an ``initialize`` function to set parameters for the optimization. This is what the ``__init__``
function does:

.. testcode:: simple_model_Unconstrained_pieces

        def __init__(self):
            """ Creates a new Assembly containing a Paraboloid and an optimizer"""
        
            super(OptimizationUnconstrained, self).__init__()

The ``__init__`` function is called by the class constructor on a new
uninitialized instance of the class, so it's a good spot to set up any
parameters that CONMIN needs. The *super* command calls the
``__init__`` function of the parent (Assembly). This is required, and forgetting it
can lead to unexpected behavior.

Next, the Paraboloid and the CONMIN driver have to be instantiated and added
to ``OptimizationUnconstrained``. The function ``add`` is used to add them
to the assembly:

.. testcode:: simple_model_Unconstrained_pieces

            # Create CONMIN Optimizer instance
            self.add('driver', CONMINdriver())

            # Create Paraboloid component instances
            self.add('paraboloid', Paraboloid())

Here you will make an instance of the *Paraboloid* component that you created above and
give it the name *paraboloid.* Similarly you will create an instance of CONMINdriver and 
give it the name *driver.* It will be the root of the iteration hierarchy for our class. 
As with other class members,
these are now accessible in the ``OptimizationUnconstrained`` assembly via ``self.paraboloid``
and ``self.driver``.

Next, the CONMINdriver needs to be told what to run. Every driver has a :term:`Workflow`
that contains a list of the components that the driver tells to run. We can add the
*Paraboloid* component to the driver's workflow by using its ``add`` function.

.. testcode:: simple_model_Unconstrained_pieces

            # Iteration Hierarchy
            self.driver.workflow.add('paraboloid')

For this problem, you want to minimize ``f_xy``. In optimization, this is called
the *objective function*. In OpenMDAO, you define the ``objective`` function by
calling the driver's ``add_objective`` function.
        
.. testcode:: simple_model_Unconstrained_pieces

            # CONMIN Objective 
            self.driver.add_objective('paraboloid.f_xy')

Every variable has a unique name in the OpenMDAO data hierarchy. This
name combines the variable name with its parents' names. You can think
of it as something similar to the path name in a file system, but it uses a "."
as a separator. This allows two components to have the same variable name
while assuring that you can still refer to each of them uniquely. Here, the
``f_xy`` output of the Paraboloid component is selected as the objective for
minimization.

While CONMIN operates only on a single objective,
it allows multiple design variables. The design variables can be declared
individually using the ``add_parameter`` method:
        
.. testcode:: simple_model_Unconstrained_pieces

            # CONMIN Design Variables 
            self.driver.add_parameter('paraboloid.x', -50, 50)
            self.driver.add_parameter('paraboloid.y', -50, 50)

Here, both `x` and `y` from the *Paraboloid* component are chosen as the design
variables. The ``add_parameter`` method also allows you to add a range of
validity for these variables, so that the unconstrained optimization can be
performed on a bounded region. For this problem, you are constraining `x` and `y`
to lie on ``[-50, 50]``.
        
The problem is now essentially ready to execute. CONMIN contains quite a few
additional control parameters, though the default values for many of them are
adequate. These parameters are detailed in the section on :ref:`CONMINDriver`.
        
.. testcode:: simple_model_Unconstrained_pieces

            # CONMIN Flags
            self.driver.iprint = 1
            self.driver.itmax = 30
            self.driver.fdch = .000001
            self.driver.fdchm = .000001

The parameters specified here include the debug verbosity (*iprint*) and the number of
iterations (*itmax*). The relative and absolute step sizes for the
numerical gradient calculation are adjusted to reduce the step size for this
problem (*fdch* and *fdchm*). If the default values are used, only two places of
accuracy can be obtained in the calculated minimum because CONMIN's default step
size is too large for this problem.

This model is now finished and ready to run. The next section will show how this is done.
