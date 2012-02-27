.. index:: CONMIN

.. _`using-CONMIN`:


Building a Model - Unconstrained Optimization using CONMIN
===========================================================

Your next task is to build a model that finds the minimum value for the Paraboloid component
described above. This model contains the Paraboloid as well as a gradient optimizer
called :term:`CONMIN`, from the OpenMDAO standard library. 
Create a file called ``optimization_unconstrained.py`` and copy this
block of code into it.

.. testcode:: simple_model_Unconstrained

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import CONMINdriver
    from openmdao.examples.simple.paraboloid import Paraboloid

    class OptimizationUnconstrained(Assembly):
        """Unconstrained optimization of the Paraboloid with CONMIN."""
    
        def configure(self):
        
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


.. index:: top level Assembly

An :term:`Assembly` is a container that can hold any number of components, drivers, and other
assemblies. An Assembly also manages the connections between the components that it
contains. In this problem the assembly includes a single
Paraboloid component and a CONMINdriver named *driver*. The name *driver* is special. When an 
assembly is executed, it looks for a Driver named *driver* and executes it. The 
OptimizationUnconstrained class is derived from Assembly. 

.. testsetup:: simple_model_Unconstrained_pieces

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import CONMINdriver
    from openmdao.examples.simple.paraboloid import Paraboloid
    from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    
    self = OptimizationUnconstrained()
    
.. testcode:: simple_model_Unconstrained_pieces

    class OptimizationUnconstrained(Assembly):
        """Unconstrained optimization of the Paraboloid with CONMIN."""
        
        def configure(self): 
            """function that sets up the architecture"""
        
In the Paraboloid component, you created an ``execute`` function to tell it what to do when the
component is run. Assemblies don't need an execute function, since they don't really do any 
caluclations of their own. Instead the assembly uses a ``configure`` function defined 
which manages the creation of all the components, drivers, and data connections. 

When configuring you use the ``add`` function put things into the assembly:

.. testcode:: simple_model_Unconstrained_pieces

            # Create CONMIN Optimizer instance
            self.add('driver', CONMINdriver())

            # Create Paraboloid component instances
            self.add('paraboloid', Paraboloid())
            

Here you will make an instance of the *Paraboloid* component that you created above and
give it the name *paraboloid.* Similarly you will create an instance of CONMINdriver and 
give it the name *driver*. These are now accessible in the ``OptimizationUnconstrained`` assembly 
via ``self.paraboloid`` and ``self.driver``. Remember, assemblies always look for the Driver called ``driver`` to run the
model. 

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
minimization by specifying its full name, ``paraboloid.f_xy``.

To find the minimum value of the objective function, we want to optimizer to 
vary the ``x`` and ``y`` variables. The design variables are declared
individually using the ``add_parameter`` method:
        
.. testcode:: simple_model_Unconstrained_pieces

            # CONMIN Design Variables 
            self.driver.add_parameter('paraboloid.x', -50, 50)
            self.driver.add_parameter('paraboloid.y', -50, 50)

Once again, you specify the parameters with the full name of each variable: ``parabolid.x``
and ``paraboloid.y``. The ``add_parameter`` method also allows you to add a range of
validity for these variables, so that the unconstrained optimization can be
performed on a bounded region. For this problem, you are constraining `x` and `y`
to lie between ``[-50, 50]``.
        
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

Congradulations! You just built your first model in OpenMDAO. Now let's run it. 


Executing the Simple Optimization Problem
-----------------------------------------

To run your model, you need to create an instance of ``OptimizationUnconstrained`` and tell it to run.

To do this, we're going to add some code to the end of the
``optimization_unconstrained.py`` so that it can be executed in Python. 
Using the conditional ``if __name__ == "__main__":`` you can include some Python code at the bottom 
of ``optimization_unconstrained.py``. This makes it so you can run the file, but so that you could 
also import your assembly into another model without running it. So the final lines in this file are:

.. testsetup:: simple_model_Unconstrained_run

    from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    __name__ = "__main__"

.. testcode:: simple_model_Unconstrained_run

    if __name__ == "__main__": 

        opt_problem = OptimizationUnconstrained()

        import time
        tt = time.time()
        
        opt_problem.run()

        print "\n"
        print "CONMIN Iterations: ", opt_problem.driver.iter_count
        print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                         opt_problem.paraboloid.y)
        print "Elapsed time: ", time.time()-tt, "seconds"

.. testoutput:: simple_model_Unconstrained_run
    :hide:

    ...
    CONMIN Iterations:  5
    Minimum found at (6.666309, -7.333026)
    Elapsed time:  ... seconds
        
 
In this block of code you are doing four things: 

   1. In the first statement, you create an instance of the class ``OptimizationUnconstrained`` with
      the name ``opt_problem``. 
   2. In the second statement, you set ``opt_problem`` as the top Assembly in the model hierarchy. (This will be explained in a later tutorial.)    
   3. In the fifth statement, you tell ``opt_problem`` to run. (The model will execute until the optimizer's
      termination criteria are reached.) 
   4. In the remaining statements, you define the results to print, including the elapsed time.

Add the above code into your ``optimization_unconstrained.py`` file and save it. 
Then type the following at the command prompt:

::

        python optimization_unconstrained.py

This should produce the output:

:: 

    [ CONMIN output not shown ]
    CONMIN Iterations:  5
    Minimum found at (6.666309, -7.333026)
    Elapsed time:  0.0558300018311 seconds

Now you are ready to solve a more advanced optimization problem with constraints.

