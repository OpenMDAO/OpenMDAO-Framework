.. index:: drivers

Drivers
========

Drivers are generally iterative solvers, such as optimizers, that operate on
their respective workflow until certain conditions are met. OpenMDAO includes
several drivers that are distributable (i.e., either open source or
public domain.) This section describes the driver interface that is common
to most drivers. A more complete discussion on how to use each of the :ref:`drivers
<drivers>` can be found in the source documentation.

.. _Driver-API: 

The Driver API
--------------

Drivers in OpenMDAO share a functional interface for setting up certain common
parts of the problem. There are functions to handle parameters, which are inputs
to a system and are also known as *design variables* for optimizers or *independents*
for solvers. Likewise, there are also functions to handle constraints.

.. index:: parameter, design variable

To illustrate the parameter interface, consider a model in which our goal
is to optimize the design of a vehicle with several design variables using
the CONMINdriver optimizer.

.. testcode:: Parameter_API

    from openmdao.examples.enginedesign.vehicle import Vehicle
    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import CONMINdriver

    class EngineOpt(Assembly):
        """ Top level assembly for optimizing a vehicle. """
    
        def __init__(self):
            """ Creates a new Assembly containing a Vehicle and an optimizer"""
        
            super(EngineOptimization, self).__init__()

            # Create Vehicle component instances
            self.add('vehicle', Vehicle())

            # Create CONMIN Optimizer instance
            self.add('driver', CONMINdriver())
        
            # add Vehicle to workflow
            driver.workflow.add('vehicle')

We add design variables to the driver ``self.driver`` using the ``add_parameter``
function. 

.. testsetup:: Parameter_API
    
    from openmdao.examples.enginedesign.engine_optimization import EngineOptimization
    self = EngineOptimization()
    self.driver.clear_parameters()

.. testcode:: Parameter_API

    # CONMIN Design Variables 
    self.driver.add_parameter('vehicle.spark_angle', low=-50. , high=10.)
    self.driver.add_parameter('vehicle.bore', low=65. , high=100.)

Parameters are assigned via a string that contains the pathname of an OpenMDAO
variable. This variable must exist in the scope of the assembly that contains
the driver. In other words, if an assembly contains a driver, the parameters
added to that driver cannot be located outside of that assembly. Also, each
parameter must point to a component input, not a component output. During
driver execution, the parameter values are set, and the relevant portion of
the model is executed to evaluate the new objective.
    
The *low* and *high* arguments can be used to specify an allowable range for a parameter. Using these
parameters is useful for optimization problems where the design variables are constrained. Generally, the
optimizer treats these as a special kind of constraint, so they should be defined using the low and high
parameters rather than the ``add_constraint method``. If low and high values are not given, then they are
pulled from the corresponding low and high parameters that are defined in the variable. If low and high aren't
defined in either place, then an exception is raised. Some drivers (in particular solvers) do not support a
low or high value; in such a case, you can just set each of them to a large number, e.g., ``low=-1e99`` and
``high=1e99``.


The ``IHasParameters`` interface also includes some other functions that are more useful when
used interactively or when writing more advanced components. The functions ``list_param_targets``,
``remove_parameters``, and ``clear_parameters`` can be used to respectively list all parameter targets, delete a
single parameter, and clear all parameters.

.. doctest:: more_parameter_interface

    >>> from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
    >>> top = OptimizationConstrained()
    >>> top.driver.list_param_targets()
    ['paraboloid.x', 'paraboloid.y']
    >>> top.driver.remove_parameter('paraboloid.x')
    >>> top.driver.list_param_targets()
    ['paraboloid.y']
    >>> top.driver.clear_parameters()
    >>> top.driver.list_param_targets()
    []

There are also ``get_parameters`` and ``set_parameters`` methods, but these
methods are typically used by drivers to manage the parameters in their
workflow and are not called directly by users. These will be described in the
section :ref:`Adding-new-Drivers`.

.. index:: constraint

A similar interface is present for interacting with constraints. *Constraints*
are defined using strings containing equations or inequalities that reference
available OpenMDAO variables. Both equality and
inequality constraints are supported via the interface; however, when you use a
driver, you should verify that it supports the desired type of constraint. For
example, the CONMIN driver supports inequality constraints but not equality
constraints.

Constraints are added to a driver using the ``add_constraint`` method.
Constraints are defined using boolean expressions, so they are considered to
be satisfied when the expressions evaluate to *True* and violated when they
evaluate to *False*. The following constraint declarations are all equivalent:

.. testcode:: Parameter_API

    self.driver.add_constraint('vehicle.stroke - vehicle.bore < 0')
    self.driver.add_constraint('vehicle.stroke < vehicle.bore')
    self.driver.add_constraint('vehicle.bore > vehicle.stroke')
    
Using the ``eval_eq_constraints`` and ``eval_ineq_constraints`` methods,
an optimizer or solver can query for the status and values of its constraints. Both
methods return a list of tuples of the form ``(lhs, rhs, relation, result)``, where
*lhs* is the value of the left hand side of the expression, *rhs* is the value of
the right hand side of the expression, *result* is the boolean result of evaluating
the expression, and *relation* is a string indicating the type of
relation used in the expression, e.g., ``>, <, >=, <=, or =``. The
values of the left- and right-hand sides are needed by gradient optimizers that 
apply the constraint via a penalty function.

The *IHasConstraints* interface also supports equality constraints. At
present, none of the optimizers in OpenMDAO support equality constraints, but
they are used by the BroydenSolver to assign the dependent equation. The
syntax includes an equal sign in the expression.

.. testsetup:: Parameter_API2

    from openmdao.lib.drivers.api import BroydenSolver
    from openmdao.main.api import Assembly
    from openmdao.lib.optproblems import sellar
    
    self = Assembly()
    self.add('dis1', sellar.Discipline1())
    self.add('driver', BroydenSolver())

.. testcode:: Parameter_API2

    self.driver.add_constraint('dis1.y1 = 0.0')

.. note::

    OpenMDAO only detects duplicate constraints if the have the exact same
    form aside from white space, so be careful when adding them.
    
Sometimes you want to change the scaling on constraints, particularly for
cases where the constrained variables are of disparate orders of magnitude. You can do this 
conveniently with the optional ``scale`` argument in the call to ``add_constraint``.

.. testcode:: Parameter_API

    self.driver.add_constraint('vehicle.stroke - vehicle.bore < .00001', scaler=10000.0)
    
Here, the constraint has been scaled up so that when its value is passed to the optimizer, it is in
a similar range (and hence, of similar weight) as the other constraints in the model. Although an 
optional ``adder`` argument was also added to shift both the left- and the right-hand sides of a
constraint, the current OpenMDAO gradient optimizer (CONMINdriver) internally shifts all
constraints to the origin, so this parameter is not needed.


Constraints can be removed using ``remove_constraint``.  The same string used
to add the constraint should be used to remove it. Whitespace within the expression
is ignored.

.. testcode:: Parameter_API2

    self.driver.remove_constraint('dis1.y1 = 0.0')

A list of constraint expression strings can be obtained using ``list_constraints``.

.. testcode:: Parameter_API2

    lst = self.driver.list_constraints()
    
Calling ``clear_constraints`` will remove all constraints from a driver.

.. testcode:: Parameter_API2

    self.driver.clear_constraints()
    

.. index:: objective

Finally, OpenMDAO uses a similar interface for specifying objectives. A single
objective (some future optimizers will handle multiple objectives) can be
added to a driver using the ``add_objective`` method with an argument that is
a string expression built up from available OpenMDAO outputs.

.. testsetup:: Parameter_API

    self.driver.clear_objectives()

    
.. testcode:: Parameter_API

    # CONMIN Objective = Maximize weighted sum of two variables
    self.driver.add_objective('-(-.93*vehicle.fuel_burn + 1.07*vehicle.torque)')

In this example, the objective is to maximize the weighted sum of two variables.
The equation must be constructed using valid Python operators. All variables in
the function are expressed in the scope of the local assembly that contains the
driver.

The *IHasObjectives* interface also includes functions to retrieve the objective 
dict and to query for the objective values.

.. doctest:: more_objective_interface

    >>> from openmdao.examples.simple.optimization_unconstrained import OptimizationUnconstrained
    >>> model = OptimizationUnconstrained()
    >>> model.driver.get_objectives().keys()
    ['paraboloid.f_xy']
    >>> model.driver.eval_objectives()
    [0.0]

.. _Adding-new-Drivers:

Adding new Drivers
---------------------

.. todo::

    Show how to add new drivers.

.. index:: derivatives, Finite Difference, Hessians, constraints
