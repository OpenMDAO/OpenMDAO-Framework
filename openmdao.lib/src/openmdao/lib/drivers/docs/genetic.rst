
.. index:: Genetic

.. _`Genetic`:

*Genetic*
~~~~~~~~~

:term:`Genetic` is a driver which performs optimization using a genetic algorithm based
on `Pyevolve <http://pyevolve.sourceforge.net/>`_. Genetic is a global optimizer and
is ideal for optimizing problems with integer or discrete design variables because it
is a non-derivative based optimization method. 

Genetic can be used in any simulation by importing it from ``openmdao.lib.drivers.api``:

.. testcode:: Genetic_load

    from openmdao.lib.drivers.api import Genetic

.. index:: pair: design; variables
.. index:: Float, Int, Enum

**Design Variables**

IOtraits are added to Genetic and become optimization parameters. Genetic will vary the set of
parameters to search for an optimum. Genetic supports three variable types:
:term:`Float`, :term:`Int`, and :Term:`Enum`. These types can be used as parameters in any 
optimization. 

You add design variables to Genetic using the ``add_parameter`` method.

.. testcode:: Genetic

    from openmdao.main.api import Assembly,Component, set_as_top
    from openmdao.lib.drivers.api import Genetic
    from openmdao.lib.datatypes.api import Float,Int,Enum
    
    
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
            self.driver.workflow.add('comp')

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
are applicable only to Float and Int variables. For Enum variables, `low` and `high`
are not applicable.

**Configuration**

When setting the objective you can specify a single 
variable name or a more complex function, such as 

.. testcode:: Genetic

    top.driver.add_objective("comp.x")
    
or 


.. testcode:: Genetic

    top.driver.clear_objectives()
    top.driver.add_objective("2*comp.x + comp.y + 3*comp.z")

In the second example above, a more complex objective function was created where the overall objective was 
a weighted combination of ``comp.x, comp.y,`` and ``comp.z``. 

To set the optimizer to either minimize or maximize your objective, you set the
``opt_type`` variable of Genetic to ``"minimize"`` or ``"maximize``.

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
Wheel Algorithm, but Rank Selection, and Uniform Selection are also available. The
``selection_method`` variable allows you to select the algorithm; allowed values are: ``"roulette_wheel," 
"rank,"`` and ``"uniform"``.

.. testcode:: Genetic
    
    top.driver.selection_method="rank"


*Source Documentation for genetic.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
