.. index:: Optimizers

.. _`Optimizers`:


Choosing an Optimizer
======================

When the initial versions of OpenMDAO were released, the standard library only contained
two optimizers -- the gradient optimizer ``CONMIN``, and the genetic optimizer ``genetic``.
OpenMDAO now includes several optimizers, provides access to a few more optimizers as plugins,
and will continue to benefit from community contributions. This tutorial will discuss the
currently available optimizers as well as mention some of the official plugins. Finally,
the Paraboloid example will be used to demonstrate optimizer swapping.

OpenMDAO Optimizers
~~~~~~~~~~~~~~~~~~~

The following tables summarizes the optimizers that are currently included in OpenMDAO.

+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
| Optimizer        | Gradients             |Inequality |Equality   | Algorithm                                                                                                           |
|                  |                       |Constraints|Constraints|                                                                                                                     |
+==================+=======================+===========+===========+=====================================================================================================================+
|``COBYLAdriver``  |  None                 |   Yes     |   No      | Constrained Optimization BY Linear Approximation of the objective and constraint functions via linear interpolation.|
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
|``CONMINdriver``  |  Computed by OpenMDAO |   Yes     |   No      | CONstrained function MINimization. Implements the Method of Feasible Directions to solve the NLP problem.           |
|                  |  or CONMIN            |           |           |                                                                                                                     |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
|``Genetic``       |  None                 |   No      |   No      | General genetic algorithm framework based on PyEvolve.                                                              |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
|``NEWSUMTdriver`` |  Computed by OpenMDAO |   Yes     |   No      | NEWton’s method Sequence of Unconstrained Minimizations                                                             |
|                  |  or NEWSUMT           |           |           |                                                                                                                     |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+
|``SLSQPdriver``   |  Computed by OpenMDAO |   Yes     |   Yes     | Sequential Least SQuares Programming                                                                                |
+------------------+-----------------------+-----------+-----------+---------------------------------------------------------------------------------------------------------------------+

Any of these optimizers can be added to your model by importing them from ``openmdao.lib.drivers.api``.
You will generally choose an optimizer based on prior knowlege of how well a particular algorithm
is suited to your type of problem. Specific limitations of the optimizers may also come into play. For
example, if your problem has equality constraints, then you can only use the ``SLSQPdriver`` unlessyou
can rewrite your equality constraint as a pair of inequality constraints. Likewise, if you are optimizing
a component that provides an analytical gradient, that gradient can only be used by an optimizer that
supports them. 

In the example below, you will have the opportunity to try out all of the OpenMDAO optimizers on the
Paraboloid problem. This will give you some basic insight into their performance and should familarize
you with their most useful settings.


Swapping Optimizers
~~~~~~~~~~~~~~~~~~~

In previous examples, we found the minimum value of a paraboid both with and without constraints. We also
learned how to add derivative functions to our components and use them in an optimizer's calculation of the
gradient. We did all of this using the CONMIN optimizer. Now, let's investigate the rest of OpenMDAO's 
optimizers and see how they compare to CONMIN.

Let's start by creating the following model and calling it ``demo_opt.py``.

.. testcode:: CONMIN_I_choose_you

        ''' Demonstration of swapping optimizers on a problem '''
        
        from openmdao.examples.simple.paraboloid_derivative import ParaboloidDerivative
        from openmdao.lib.differentiators.api import FiniteDifference
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
                
                # Differentiator
                #self.driver.differentiator = FiniteDifference()
                
                # General flag - suppress output
                self.driver.iprint = 0
                
                # CONMIN-specific Settings
                self.driver.itmax = 30
                self.driver.fdch = 0.00001
                self.driver.fdchm = 0.000001
                self.driver.ctlmin = 0.01
                self.driver.delfun = 0.001
                
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

We've gone ahead and imported every optimizer to make swapping them fairly easy. There are several
blocks of lines that are commmented out in this code. Most of these contain settings for the optimizers.
Settings are usually very specific to an optimizer, so we will want to take care that only the lines for
the optimizer we are using are active. The parameters, objective(s), and constraints(s) can all stay the
same when you swap in a new optimizer, provided they are supported (e.g., equality constraints are only
supported by ``SLSQPdriver``.) Also, we will sometimes slot a FiniteDifferent differentiator, though that
line of code is currently commented out. We are using the ``ParaboloidDerivative`` component, which
contains the analytical derivatives. In all of the finite difference calculations, whether initiated by 
OpenMDAO, CONMIN, or NEWSUMT, the FDAD (Finite Difference with Analytical Derivatives) approach is
used, so the analytical derivatives are used to replace model execution under finite difference.

So first, let's run ``demo_opt.py``. This first case is the constrainted optimization of the
paraboloid using CONMIN's internal finite difference calculation.

Note that the sample results presented here are representative of what you should see, but they
may differ depending on your system architecture.

:: 

    Optimizer: <class 'openmdao.lib.drivers.conmindriver.CONMINdriver'>
    Function executions:  16
    Gradient executions:  6
    Minimum: -27.083084
    Minimum found at (7.175777, -7.824223)
    Elapsed time:  0.0239610671997 seconds

We obtained this value after adjusting some of CONMIN's settings from their defaults. In our
experience, CONMIN is notoriously sensitive to the values of these settings, in particular the
relative and minimum absolute stepsize changes in the finite difference calculation (fdch and
fdchm). The answer that CONMIN gives here didn't quite reach the minimum, which we've found with
other optimizers to lie at(7.166667, -7.833333). More exploration of CONMIN's settings could
possibly yield a better answer, but this would not be a reasonable avenue of investigation when the
optimum isn't known.

Let's let OpenMDAO perform the finite difference instead of CONMIN. To do this, uncomment the
line that sockets the differentiator.

::

                # Differentiator
                self.driver.differentiator = FiniteDifference()

Then run ``demo_opt.py``.

::

    Function executions:  18
    Gradient executions:  5
    Minimum: -27.075841
    Minimum found at (7.200896, -7.808874)
    Elapsed time:  0.0260651111603 seconds
    
The answer here is actually a little worse than before. There are a couple of possible reasons for
this. OpenMDAO's finite difference is fairly simple, with a single non-adapting stepsize. This
stepsize could be specified for each parameter, though the scaling for ``x`` and ``y`` here is
roughly the same, so it wouldn't be needed. On the other hand, CONMIN uses an adaptive stepsize
which presumably takes smaller steps as it approaches the optimum, so this should do a better
job. Moreover, some time was spent in picking a reasonable stepsize for CONMIN, but for the
OpenMDAO differentiator, we just kept the default value.

Now, let's try the NEWSUMT driver. First, replace ``CONMINdriver`` with ``NEWSUMTdriver``
where it is added to the assembly.

::

                # Create Optimizer instance
                self.add('driver', NEWSUMTdriver())
                
Now, we need to change over to use the NEWSUMT settings and deactivate the CONMIN settings.
Let's also unsocket OpenMDAO's finite difference. Th
                
::

                # Differentiator
                #self.driver.differentiator = FiniteDifference()
                
::

                # CONMIN-specific Settings
                #self.driver.itmax = 30
                #self.driver.fdch = 0.00001
                #self.driver.fdchm = 0.000001
                #self.driver.ctlmin = 0.01
                #self.driver.delfun = 0.001
                
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
    
We didn't do as well here with NEWSUMT. However, the default number of iterations for NEWSUMT
is 10. We can tell that we are hitting that because we've perfomed 10 gradient executions. Note
that we would also be able to tell that from the number of driver iterations, which in NEWSUMT
is stored in iter_count. Note that not every driver reports an iteration count, so we didn't
print it here. Let's boost our maximum number of iterations:

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
    
Our answer has improved, and is slightly better than what CONMIN reported. Note that the
number of functional executions is an order of magnitude more than CONMIN. For a problem
with a long runtime, this optimizer may be significantly slower.

Let's slot OpenMDAO's finite difference differentiator.

::

                # Differentiator
                self.driver.differentiator = FiniteDifference()

Then run ``demo_opt.py``.

::

    Optimizer: <class 'openmdao.lib.drivers.newsumtdriver.NEWSUMTdriver'>
    Function executions:  255
    Gradient executions:  50
    Minimum: -27.079630
    Minimum found at (7.170357, -7.837023)
    Elapsed time:  0.133186101913 seconds

The answer is about the same. One notable difference is a doubling of the number of gradient executions.
This is because NEWSUMT is the only optimizer which asks for an explicit Hessian (i.e., 2nd derivative)
of the objective and constraints. Hessian calculation is expensive, and scales n-squared with the number
of parameters. When NEWSUMT calculates its own Hessian, it seems to be using some approximations to speed
the calculation. Thus, it might be advisable to use NEWSUMT's gradient calculation.

Next, let's try the ``COBYLAdriver``.

::

                # Create Optimizer instance
                self.add('driver', COBYLAdriver())
                

We don't have to unsocket the finite difference driver, as COBYLA is a gradient-free method, and
will not use it.

::

                # NEWSUMT-specific Settings
                #self.driver.itmax = 50
                
                # COBYLA-specific Settings
                self.driver.rhobeg = 1.0
                self.driver.rhoend = 1.0e-4
                self.driver.maxfun = 1000

COBYLA has very few settings. The ``rhoend`` parameter is equivalent to a convergence tolerance, and
of course ``maxfun`` is the maximum number of iterations. Now try running ``demo_opt.py``.

::

    Optimizer: <class 'openmdao.lib.drivers.cobyladriver.COBYLAdriver'>
    Function executions:  47
    Gradient executions:  0
    Minimum: -27.083333
    Minimum found at (7.166766, -7.833234)
    Elapsed time:  0.0164699554443 seconds
    

The answer is considerably better than CONMIN. Let's experiment with the convergence criterion
by decreasing ``rhoend`` to 1.0e-5.

::

                # COBYLA-specific Settings
                self.driver.rhobeg = 1.0
                self.driver.rhoend = 1.0e-5
                self.driver.maxfun = 1000

Running ``demo_opt.py``:

::

    Optimizer: <class 'openmdao.lib.drivers.cobyladriver.COBYLAdriver'>
    Function executions:  54
    Gradient executions:  0
    Minimum: -27.083333
    Minimum found at (7.166661, -7.833339)
    Elapsed time:  0.0184278488159 seconds
    
This results in 7 more function executions and a better minimum (although the value of the minumum
is cut off in our printout because of the print display resolution -- you can make it more explicit
with a specified-width format like %.15f.) COBYLA needed 3 times the number of function evaluation as CONMIN, but
it got to a much better value, and it does not exhibit any hyper-sensitivity with respect to its
settings. Note also that COBYLA's elapsed time is stil lower. The optimizer seems to have less
overhead, which could be important for evaluating large numbers of fast objectives, but will be
lost in the wash when dealing with slow functions.

Next up is SLSQP. This optimizer requires a gradient, but it cannot provide its own. Since a
finite difference driver would always be required, one is always socketed in the SLSQPdriver by default,
so adding it is unnecessary. No harm is done if you socket one after instantiating. Add an SLSQPdriver
instance to your model:

::

                # Create Optimizer instance
                self.add('driver', SLSQPdriver())
                

SLSQP only has a couple of settings, none of which will be moved off the default.

::

                # COBYLA-specific Settings
                #self.driver.rhobeg = 1.0
                #self.driver.rhoend = 1.0e-5
                #self.driver.maxfun = 1000
                
                # SLSQP-specific Settings
                self.driver.accuracy = 1.0e-6
                self.driver.maxiter = 50
                
Now, lets run ``demo_opt.py``:

::

    Optimizer: <class 'openmdao.lib.drivers.slsqpdriver.SLSQPdriver'>
    Function executions:  4
    Gradient executions:  3
    Minimum: -27.083333
    Minimum found at (7.166667, -7.833334)
    Elapsed time:  0.00905513763428 seconds

The SLSQP driver performs incredibly well on this problem! It gets the closest to the minimum with
the least number of function executions and in the quickest clock time. This optimizer is quickly
becoming our favorite. It is also our only optimizer that can handle equality constraints, so let's
try one. We already know that the solution to our constrained problem lies along the constraint. We
could express this as an equality constraint, and expect that the same solution would be reached. The
equality constraint was included in ``demo_opt.py``, so comment and uncomment as such:

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

And now for something completely different, lets try the Genetic optimizer.

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
                
These are mostly the default values, although ``selection_method`` was changed to 'rank' because
it seemed to give better answers for this problem. ``Genetic`` does not use the differentiator
socket, so we don't need to worry about gradients. Run ``demo_opt.py``:

::

    Optimizer: <class 'openmdao.lib.drivers.genetic.Genetic'>
    Function executions:  8072
    Gradient executions:  0
    Minimum: -23.461808
    Minimum found at (8.805645, -9.066226)
    Elapsed time:  2.13916110992 seconds

There should be no suprises here. This is not the kind of problem you would normally throw at
a genetic algorithm. Note that the answers are not deterministic, so re-running this will always give
different results.

Optimizers from Plugins
~~~~~~~~~~~~~~~~~~~~~~~

