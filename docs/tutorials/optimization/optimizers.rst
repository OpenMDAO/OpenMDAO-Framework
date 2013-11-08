.. index:: Optimizers

.. _`Optimizers`:


Choosing an Optimizer
======================

When the initial versions of OpenMDAO were released, the standard library contained only
two optimizers: the gradient optimizer CONMIN, and the genetic optimizer Genetic.
OpenMDAO now includes several optimizers, provides access to a few more optimizers as plugins,
and will continue to benefit from community contributions. Let's walk through how you can 
use these different optimizers on the paraboloid example problem. 

OpenMDAO Optimizers
~~~~~~~~~~~~~~~~~~~

The following tables summarizes the optimizers that are currently included in OpenMDAO.

+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
| Optimizer        | Gradients             |Inequality |Equality   | Algorithm                                                                                                           |
|                  |                       |Constraints|Constraints|                                                                                                                     |
+==================+=======================+===========+===========+=====================================================================================================================+
|``COBYLAdriver``  |  None                 |   Yes     |   No      | Constrained Optimization BY Linear Approximation of the objective and constraint functions via linear interpolation |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
|``CONMINdriver``  |  Computed by OpenMDAO |   Yes     |   No      | CONstrained function MINimization. Implements the Method of Feasible Directions to solve the NLP problem            |
|                  |  or CONMIN            |           |           |                                                                                                                     |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
|``Genetic``       |  None                 |   No      |   No      | General genetic algorithm framework based on PyEvolve                                                               |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
|``NEWSUMTdriver`` |  Computed by OpenMDAO |   Yes     |   No      | NEWton's method Sequence of Unconstrained Minimizations                                                             |
|                  |  or NEWSUMT           |           |           |                                                                                                                     |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
|``SLSQPdriver``   |  Computed by OpenMDAO |   Yes     |   Yes     | Sequential Least SQuares Programming                                                                                |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+

Any of these optimizers can be added to your model by importing it from ``openmdao.lib.drivers.api``. Which
optimizer should you use? The answer is highly problem dependent. For example, if your problem has equality
constraints, then you can use only the SLSQPdriver unless you can rewrite your equality constraint as a
pair of inequality constraints. Similarly, COBYLAdriver is a gradient-free optimization, which might be
suitable if you want to avoid finite difference calculations. 

In the example below, you will have the opportunity to try out all of the OpenMDAO optimizers on the
Paraboloid problem. This will give you some basic insight into their performance and should familiarize
you with their most useful settings.


Swapping Optimizers
~~~~~~~~~~~~~~~~~~~

In previous examples, we found the minimum value of a paraboloid both with and without constraints. We also
learned how to add derivative functions to our components and use them in an optimizer's calculation of the
gradient. We did all of this using the CONMIN optimizer. Now, let's investigate the rest of OpenMDAO's 
optimizers and see how they compare to CONMIN.

Let's start by creating the following model and calling it ``demo_opt.py``.

.. testcode:: CONMIN_I_choose_you

        ''' Demonstration of swapping optimizers on a problem '''
        
        from openmdao.examples.simple.paraboloid_derivative import ParaboloidDerivative
        from openmdao.lib.drivers.api import COBYLAdriver, CONMINdriver, \
                NEWSUMTdriver, SLSQPdriver, Genetic
        from openmdao.main.api import Assembly
        
        class DemoOpt(Assembly):
            """Constrained optimization of the Paraboloid with whatever optimizer
            we want."""
            
            def configure(self):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
                
                # Create Paraboloid component instances
                self.add('comp', ParaboloidDerivative())
        
                # Create Optimizer instance
                self.add('driver', CONMINdriver())
                
                # Driver process definition
                self.driver.workflow.add('comp')
        
                # Objective 
                self.driver.add_objective('comp.f_xy')
                
                # Design Variables 
                self.driver.add_parameter('comp.x', low=-50., high=50.)
                self.driver.add_parameter('comp.y', low=-50., high=50.)
                
                # Inequality Constraints
                self.driver.add_constraint('comp.x-comp.y >= 15.0')
                
                # Equality Constraints
                #self.driver.add_constraint('comp.x-comp.y=15.0')
                
                # General flag - suppress output
                self.driver.iprint = 0
                
                # CONMIN-specific Settings
                self.driver.itmax = 30
                self.driver.fdch = 0.00001
                self.driver.fdchm = 0.000001
                self.driver.ctlmin = 0.01
                self.driver.delfun = 0.001
                self.driver.conmin_diff = True
                
                # NEWSUMT-specific Settings
                #self.driver.itmax = 10
                
                # COBYLA-specific Settings
                #self.driver.rhobeg = 1.0
                #self.driver.rhoend = 1.0e-4
                #self.driver.maxfun = 1000
                
                # SLSQP-specific Settings
                #self.driver.accuracy = 1.0e-6
                #self.driver.maxiter = 50
                
                # Genetic-specific Settings
                #self.driver.population_size = 90
                #self.driver.crossover_rate = 0.9
                #self.driver.mutation_rate = 0.02
                #self.selection_method = 'rank'
                
        if __name__ == "__main__": # pragma: no cover         
        
            import time
            
            opt_problem = DemoOpt()
            
            t1 = time.time()
            opt_problem.run()
            t2 = time.time()
        
            print "\n"
            print "Optimizer: %s" % type(opt_problem.driver)
            print "Function executions: ", opt_problem.comp.exec_count
            print "Gradient executions: ", opt_problem.comp.derivative_exec_count
            print "Minimum: %f" % opt_problem.driver.eval_objective()
            print "Minimum found at (%f, %f)" % (opt_problem.comp.x, \
                                                 opt_problem.comp.y)
            print "Elapsed time: ", t2-t1, "seconds"

We've gone ahead and imported every optimizer to make swapping them fairly
easy. Several blocks of lines are commented out in this code. Most of these
contain settings for the optimizers. Settings are usually very specific to an
optimizer, so we'll want to take care that only the lines for the optimizer
we are using are active. The parameters, objective(s), and constraints(s) can
all stay the same when you swap in a new optimizer, provided they are
supported (e.g., equality constraints are only supported by SLSQPdriver.)

Some optimizers, like CONMINdriver, have their own finite difference
capability for calculating the gradient. Others, like ``SLSQ_driver``, do not
have any gradient calculation ability, and need to use the one from OpenMDAO.
We can tell the CONMIN driver to calculate its own gradient by setting the
``conmin_diff`` flag to True, and it will perform a backward difference with
automatic step-size reduction. For this example, we are using the
ParaboloidDerivative component, which contains the analytical derivatives
between all of its inputs and outputs. Even though CONMIN is controlling the
finite-difference, OpenMDAO will use the analytic derivatives that are
provided to speed up the calculation.

So first, let's run :download:`demo_opt.py
<../../../examples/openmdao.examples.simple/openmdao/examples/simple/demo_opt.py>`.  This first case is the
constrained optimization of the paraboloid using CONMIN's internal finite difference calculation.

Note that the sample results presented here are representative of what you should see, but they
may differ depending on your system architecture.

:: 

    Optimizer: <class 'openmdao.lib.drivers.conmindriver.CONMINdriver'>
    Function executions:  16
    Gradient executions:  6
    Minimum: -27.083084
    Minimum found at (7.175777, -7.824223)
    Elapsed time:  0.0239610671997 seconds

We obtained this value after adjusting some of CONMIN's settings from their defaults. 
CONMIN is notoriously sensitive to the values of these settings, in particular the
relative and minimum absolute stepsize changes in the finite difference calculation (`fdch` and
`fdchm`). The answer that CONMIN gives here didn't quite reach the minimum, which we've found with
other optimizers to lie at ``(7.166667, -7.833333)``. Exploring CONMIN's settings could
possibly yield a better answer, but that's not a reasonable thing to do for a real problem. 

Next we'll let OpenMDAO perform the finite difference instead of CONMIN. To
do this, set this ``conmin_diff`` flag to False, which is its default value.

::

                self.driver.conmin_diff = False

Then run ``demo_opt.py``.

::

    Function executions:  18
    Gradient executions:  5
    Minimum: -27.075841
    Minimum found at (7.200896, -7.808874)
    Elapsed time:  0.0260651111603 seconds
    
The answer here is actually a little worse than before. There are a couple of possible reasons for
this. OpenMDAO's finite difference is fairly simple, with a single non-adapting stepsize. This
stepsize could be specified for each parameter, though the scaling for `x` and `y` here is
roughly the same, so it wouldn't be needed. On the other hand, CONMIN uses an adaptive stepsize
which presumably takes smaller steps as it approaches the optimum, so this should do a better
job. Moreover, some time was spent picking a reasonable stepsize for CONMIN, but for
OpenMDAO, we just kept the default value.

Now, let's try the NEWSUMT driver. First, replace ``CONMINdriver`` with ``NEWSUMTdriver``
where it is added to the assembly.

::

                # Create Optimizer instance
                self.add('driver', NEWSUMTdriver())
                
We need to use the NEWSUMT settings and deactivate the CONMIN settings.
Let's also unsocket OpenMDAO's finite difference. 
                
::

                # CONMIN-specific Settings
                #self.driver.itmax = 30
                #self.driver.fdch = 0.00001
                #self.driver.fdchm = 0.000001
                #self.driver.ctlmin = 0.01
                #self.driver.delfun = 0.001
                self.driver.conmin_diff = False
                
                # NEWSUMT-specific Settings
                self.driver.itmax = 10

Then run ``demo_opt.py``.

::

    Optimizer: <class 'openmdao.lib.drivers.newsumtdriver.NEWSUMTdriver'>
    Function executions:  126
    Gradient executions:  10
    Minimum: -25.785512
    Minimum found at (7.910433, -8.577796)
    Elapsed time:  0.0497758388519 seconds
    
We didn't do as well here with NEWSUMT. However, the default number of iterations for NEWSUMT is 10. We can
tell that we're hitting this number because we've performed 10 gradient executions. We could also tell this
from the number of driver iterations, which in NEWSUMT is stored in ``iter_count``. Note that not every
driver reports an iteration count, so we didn't print it here. Let's boost our maximum number of iterations:

::

                # NEWSUMT-specific Settings
                self.driver.itmax = 50

Then run ``demo_opt.py``.

::

    Optimizer: <class 'openmdao.lib.drivers.newsumtdriver.NEWSUMTdriver'>
    Function executions:  253
    Gradient executions:  26
    Minimum: -27.079630
    Minimum found at (7.170354, -7.837026)
    Elapsed time:  0.107419013977 seconds
    
Our answer has improved and is slightly better than what CONMIN reported. Notice that the
number of functional executions is an order of magnitude more than CONMIN. For a problem
with a long runtime, this optimizer may be significantly slower.

NEWSUMT need the Hessian or second-derivatives of the objective and
constraints with respect to the design variables. Presently, OpenMDAO cannot
provide these, so NEWSUMT uses its own internal gradient and Hessian
calculation.

Now let's try the COBYLAdriver.

::

                # Create Optimizer instance
                self.add('driver', COBYLAdriver())
                

We don't have to unsocket the finite difference driver, as COBYLA is a gradient-free method and
will not use it. But you can comment it out if you want to; the answer won't change. 

::
                
                # COBYLA-specific Settings
                self.driver.rhobeg = 1.0
                self.driver.rhoend = 1.0e-4
                self.driver.maxfun = 1000

COBYLA has very few settings. The ``rhoend`` parameter is equivalent to a convergence tolerance, and
``maxfun`` is the maximum number of iterations. Now try running ``demo_opt.py``.

::

    Optimizer: <class 'openmdao.lib.drivers.cobyladriver.COBYLAdriver'>
    Function executions:  47
    Gradient executions:  0
    Minimum: -27.083333
    Minimum found at (7.166766, -7.833234)
    Elapsed time:  0.0164699554443 seconds
    

The answer is considerably better than CONMIN. Let's experiment with the convergence criterion
by decreasing ``rhoend`` to ``1.0e-5``.

::

                # COBYLA-specific Settings
                self.driver.rhobeg = 1.0
                self.driver.rhoend = 1.0e-5
                self.driver.maxfun = 1000

Run ``demo_opt.py``:

::

    Optimizer: <class 'openmdao.lib.drivers.cobyladriver.COBYLAdriver'>
    Function executions:  54
    Gradient executions:  0
    Minimum: -27.083333
    Minimum found at (7.166661, -7.833339)
    Elapsed time:  0.0184278488159 seconds
    
This results in seven more function executions and a better minimum (although the value of the minimum is cut
off in our printout because of the print display resolution -- you can make it more explicit with a
specified-width format, like ``%.15f``). COBYLA needed three times the number of function evaluations as
CONMIN, but it got to a much better value, and it does not exhibit any hyper-sensitivity with respect to its
settings. Note also that COBYLA's elapsed time is still lower. The optimizer seems to have less overhead,
which affects the total wall time for trivial functions like our paraboloid. But that overhead won't matter
for real analyses that have any appreciable computational cost. 

Next up is SLSQP. This optimizer requires a gradient but has no internal finite difference calculations,
so by default SLSQPdriver always uses the OpenMDAO finite difference engine. Add an SLSQPdriver
instance to your model:

::

                # Create Optimizer instance
                self.add('driver', SLSQPdriver())
                

SLSQP only has a couple of settings, none of which will be moved off the default.

::

                
                # SLSQP-specific Settings
                self.driver.accuracy = 1.0e-6
                self.driver.maxiter = 50
                
Now, let's run ``demo_opt.py``:

::

    Optimizer: <class 'openmdao.lib.drivers.slsqpdriver.SLSQPdriver'>
    Function executions:  4
    Gradient executions:  3
    Minimum: -27.083333
    Minimum found at (7.166667, -7.833334)
    Elapsed time:  0.00905513763428 seconds

The SLSQP driver performs incredibly well on this problem! It gets the closest to the minimum with the least
number of function executions and in the quickest wall time. It's also our only optimizer that can directly
handle equality constraints, so let's try one. We already know that the solution to our constrained problem
lies along the constraint. We could express this as an equality constraint and expect that the same solution
would be reached. The equality constraint was included in ``demo_opt.py``, so comment and uncomment as such:

::

        # Inequality Constraints
        #self.driver.add_constraint('comp.x-comp.y >= 15.0')
        
        # Equality Constraints
        self.driver.add_constraint('comp.x-comp.y=15.0')
        
Equality constraints are constructed as expression strings just like inequality constraints. Now
let's run ``demo_opt.py``:

::

    Optimizer: <class 'openmdao.lib.drivers.slsqpdriver.SLSQPdriver'>
    Function executions:  4
    Gradient executions:  3
    Minimum: -27.083333
    Minimum found at (7.166667, -7.833334)
    Elapsed time:  0.00990891456604 seconds

We arrive at the same answer with the equality constraint.

And now for something completely different, let's try the Genetic optimizer.

::

                # Create Optimizer instance
                self.add('driver', Genetic())
                
Genetic is currently our only evolutionary algorithm optimizer. As such, it has some
settings that are quite different:
                
::

                # SLSQP-specific Settings
                #self.driver.accuracy = 1.0e-6
                #self.driver.maxiter = 50
                
                # Genetic-specific Settings
                self.driver.population_size = 90
                self.driver.crossover_rate = 0.9
                self.driver.mutation_rate = 0.02
                self.selection_method = 'rank'
                
These are mostly the default values, although ``selection_method`` was changed to ``'rank'`` because
it seemed to give better answers for this problem. Genetic doesn't use any gradient 
information, so we don't need to worry about finite difference calculations here. Also, Genetic doesn't handle any kind of
constraints, so we'll only be able to play around with the unconstrained problem.

::

        # Inequality Constraints
        #self.driver.add_constraint('comp.x-comp.y >= 15.0')
        
        # Equality Constraints
        #self.driver.add_constraint('comp.x-comp.y=15.0')
        


Now we are ready to run ``demo_opt.py``:

::

    Optimizer: <class 'openmdao.lib.drivers.genetic.Genetic'>
    Function executions:  8072
    Gradient executions:  0
    Minimum: -23.461808
    Minimum found at (8.805645, -9.066226)
    Elapsed time:  2.13916110992 seconds

There should be no surprises here. This is not the kind of problem you would normally throw at
a genetic algorithm. Note that the answers are not deterministic, so re-running this will always give
different results.

Optimizers from Plugins
~~~~~~~~~~~~~~~~~~~~~~~

If you would like to choose from even more optimizers, look at the official plugins repository. This
repository generally contains OpenMDAO plugins that are wrappers of other existing external applications which
could not be included in OpenMDAO. Some of these may be commercial products (like Nastran), but others may be
open source packages. Most of  the time, the plugin contains just the OpenMDAO wrapper file, and you will need
to procure and install the application on its own. Presently, the official plugins repository contains two
optimizers.  The ``ipopt_wrapper`` optimizer is a wrapper for the `IPOPT
<https://projects.coin-or.org/Ipopt>`_  interior point optimizer, while ``pyopt_driver`` is a wrapper for the
`pyOpt <http://www.pyopt.org/>`_ optimization framework. You should definitely check out pyOpt because it
contains more than 15 optimization algorithms, most of which aren't in OpenMDAO. Roughly half of them are
included in the pyOpt installation, while the other half are commercial and require a separate installation of
the optimization code. Some of the pyOpt's optimizers include ALPSO (Augmented Lagrangian Particle Swarm
Optimizer), SNOPT (Sparse NOnlinear OPTimizer), and the famous NSGA2 (Non Sorting Genetic Algorithm II). To
install the ``pyopt_driver``, type the following in an activated OpenMDAO environment at your operating system
prompt:

::

                plugin install --github pyopt_driver

Note that you will also need to install pyOpt separately, either into your system environment or
directly into OpenMDAO's Python.

This concludes the tutorial on optimizers.
