
.. index:: Multidisciplinary Design Feasible (MDF)
            
.. _Multidisciplinary-Design-Feasible-(MDF):

Multidisciplinary Design Feasible (MDF)
=======================================

In a Multidisciplinary Design Feasible (MDF) problem, the disciplines are directly coupled
via some kind of solver, and the design variables are optimized in a single loop. The
following diagram illustrates the data flow for MDF applied to the Sellar problem.

.. figure:: Arch-MDF.png
   :align: center
   :alt: Diagram consists of boxes and arrows to show data flow for MDF applied to the Sellar problem.
   
   Data Flow for MDF Applied to the Sellar Problem

This diagram introduces a component called a *Broadcaster.* A Broadcaster is a component that
enables a design variable to be set to the same value at multiple locations. If you recall, a
driver such as the CONMIN optimizer contains a list of *parameters,* where each parameter is
a location in OpenMDAO's data hierarchy. Each parameter is a single design variable, and there
is no way to indicate that one design variable might be needed at multiple component inputs
in the model. We can overcome this by creating a component that passes an input to its output.
Thus, CONMIN can set the design variable in this Broadcaster, and when the Broadcaster executes,
the new value gets passed to all of the components that need it.

OpenMDAO doesn't have a built-in Broadcaster, so we need to make our own. It's a simple
component with some inputs, some outputs, and an ``execute`` function that passes the inputs
to the outputs.

.. testcode:: Broadcaster

    from openmdao.main.api import Component
    from openmdao.lib.datatypes.api import Float
    
    
    class Broadcaster(Component):
        """Component that holds some design variables.
        This is only needed because we can't hook an optimizer up to multiple
        locations of the same design variable"""
        
        # pylint: disable-msg=E1101
        z1_in = Float(0.0, iotype='in', desc='Global Design Variable')
        z2_in = Float(0.0, iotype='in', desc='Global Design Variable')
        x1_in = Float(0.0, iotype='in', desc='Local Design Variable for CO')
        y1_in = Float(0.0, iotype='in', desc='Coupling Variable')
        y2_in = Float(0.0, iotype='in', desc='Coupling Variable')
        z1 = Float(0.0, iotype='out', desc='Global Design Variable')
        z2 = Float(0.0, iotype='out', desc='Global Design Variable')
        x1 = Float(0.0, iotype='out', desc='Local Design Variable for CO')
        y1 = Float(0.0, iotype='out', desc='Coupling Variable')
        y2 = Float(0.0, iotype='out', desc='Coupling Variable')
        
        def execute(self):
            """ Pass everything through"""
            self.z1 = self.z1_in
            self.z2 = self.z2_in
            self.x1 = self.x1_in
            self.y1 = self.y1_in
            self.y2 = self.y2_in

We've added the coupling variables in our Broadcaster as well, foreseeing the need
for them in some of the other MDAO architectures.

.. index:: WorkFlow, BroydenSolver, FixedPointIterator

The diagram also shows a solver that takes the output of the component dataflow
and feeds it back into the input. OpenMDAO presently has two solvers: FixedPointIterator
and BroydenSolver. The FixedPointIterator is a solver that performs fixed point iteration,
which means that it keeps driving ``x_new = f(x_old)`` until convergence is achieved. In
other words, *y2* is passed from the output of ``SellarDiscipline2`` to the input of ``SellarDiscipline1``,
and the loop keeps executing until the change in the value of *y2* between iterations is
smaller than a tolerance. The BroydenSolver is a solver based on a quasi-Newton-Raphson
algorithm that uses a Broyden update to approximate the Jacobian. This solver reads
the output and calculates a new input each iteration. Convergence is achieved when the
residual between the output and input is driven to zero.

The major difference between the MDF problem and some of the previous examples is the
presence of nested drivers. Drivers can be nested in OpenMDAO using WorkFlows
in the iteration hierarchy. A :term:`WorkFlow` is an object that determines execution
order for a group of Components. Each driver contains a single WorkFlow. For
each iteration, a Driver will execute one pass through the WorkFlow, executing
the components contained therein in the order the WorkFlow prescribes.
Although in many cases a WorkFlow contains just Components, it can also
contain Drivers. This allows nested iterative processes to be created. The
following diagram shows an iteration hierarchy for the MDF problem.
   
.. figure:: Arch-MDF-OpenMDAO.png
   :align: center
   :alt: Diagram showing the Optimizer, workflow for the Optimizer, and workflow for the Solver
   
   Iteration Hierarchy for the MDF Problem
   
In the top left of this diagram, the gray box labeled *Optimizer* is the
top level (or outermost) driver. This driver has a workflow that contains
two objects -- the Broadcaster and a Solver -- so each time the optimizer runs
an iteration, both of these components run. The Solver also has a workflow
which contains the two discipline components. With the nesting of the drivers
we get the behavior we want, namely, that for each optimizer iteration, the 
solver runs the discipline components until they converge. We now have a nested
driver loop.

The execution order is determined by the components' dataflow. Here, the
broadcaster feeds the design variables to the discipline components, which
are contained in the solver's workflow, so the broadcaster must run first. Also,
the data connection between the two discipline components means that ``SellarDiscipline1``
runs before ``SellarDiscipline2``. Sometimes a workflow may contain components that are
not directly connected and can be run concurrently. Future tutorials will
demonstrate this.

Now, let's create the assembly for the MDF problem. First, we'll create
the top level optimization loop.

.. testcode:: MDF_parts

        from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                                       SellarDiscipline2
        from openmdao.examples.mdao.broadcaster import Broadcaster
        
        from openmdao.main.api import Assembly, set_as_top
        from openmdao.lib.drivers.api import CONMINdriver, FixedPointIterator
        
        class SellarMDF(Assembly):
            """ Optimization of the Sellar problem using MDF
            Disciplines coupled with FixedPointIterator.
            """
            
            def __init__(self):
                """ Creates a new Assembly with this problem
                
                Optimal Design at (1.9776, 0, 0)
                
                Optimal Objective = 3.18339"""
                
                # pylint: disable-msg=E1101
                super(SellarMDF, self).__init__()
        
                # create Optimizer instance
                self.add('driver', CONMINdriver())
                
                # Outer Loop - Global Optimization
                self.add('bcastr', Broadcaster())
                self.add('fixed_point_iterator', FixedPointIterator())
                self.driver.workflow.add(['bcastr', 'fixed_point_iterator'])
                
So far nothing is really new in terms of syntax. Note that the top level driver is
always named *'driver'*. However, all other drivers can be given any valid name. For this
model, we've chosen to use the ``FixedPointIterator``.

Next, we need to create the workflow for the solver. We create instances of ``SellarDiscipline1``
and ``SellarDiscipline2`` and add them to the assembly. Then, instead of adding them to the
workflow of ``'driver'``, we add them to the workflow of ``'fixed_point_iterator'``.

.. testcode:: MDF_parts
    :hide:
    
    self = SellarMDF()

.. testcode:: MDF_parts

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        self.add('dis1', SellarDiscipline1())
        self.add('dis2', SellarDiscipline2())
        self.fixed_point_iterator.workflow.add(['dis1', 'dis2'])
        
Now the iteration hierarchy is finished. We still need to hook up the data connections
and set up the CONMIN optimization and the fixed point iteration.

We need one connection between ``'dis1'`` and ``'dis2'``. We also need to hook up ``'bcastr'``
so that the design variables carry through to the discipline components.

.. testcode:: MDF_parts

        self.connect('bcastr.z1','dis1.z1')
        self.connect('bcastr.z1','dis2.z1')
        self.connect('bcastr.z2','dis1.z2')
        self.connect('bcastr.z2','dis2.z2')
        self.connect('dis1.y1','dis2.y1')


Next, the parameters for the fixed point iterator must be set.
``FixedPointIterator`` is a specialized solver that is applicable only to
single-input/single-output problems. The interface for this driver requires a
single parameter and a single objective. The input is selected using
``add_parameter``. For this parameter, we've given a *low* and a *high*
attribute, but we've set them to very large negative and positive values as
the Broyden solver doesn't use either of these. The output is specified by
adding an equality constraint. A fixed point iterator essentially tries to
drive to zero the difference between two quantities, where one has been
expressed as an output and the other as an input, by iteratively feeding the
output into the input. This can be viewed as solving the equation ``x = f(x)``.
In this case, we want to drive the residual error in the
coupled variable *y2* to zero. An equality constraint is defined with an
expression string which is parsed for the equals sign, so the following
constraints are equivalent:

.. testcode:: MDF_parts

        # Iteration loop
        self.fixed_point_iterator.add_parameter('dis1.y2', low=-9.e99, high=9.e99)
        self.fixed_point_iterator.add_constraint('dis2.y2 = dis1.y2')
        self.fixed_point_iterator.max_iteration = 1000
        self.fixed_point_iterator.tolerance = .0001       

Finally, the CONIM optimization is set up.

.. testcode:: MDF_parts

        # Optimization parameters
        self.driver.add_objective('(dis1.x1)**2 + bcastr.z2 + dis1.y1 + math.exp(-dis2.y2)')
                
        self.driver.add_parameter('bcastr.z1_in', low = -10.0, high = 10.0)
        self.driver.add_parameter('bcastr.z2_in', low = 0.0,   high = 10.0)
        self.driver.add_parameter('dis1.x1',      low = 0.0,   high = 10.0)
        
        self.driver.add_constraint('3.16 < dis1.y1')
        self.driver.add_constraint('dis2.y2 < 24.0')
        
        self.driver.cons_is_linear = [1, 1]
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .001
        self.driver.fdchm = .001
        self.driver.delfun = .0001
        self.driver.dabfun = .000001
        self.driver.ctlmin = 0.0001
        
The process of getting the optimizer to reach a value close enough to the
correct optimum required the use of more of CONMIN's settings. The *fdchm*
parameter is the minimum absolute step size that the finite difference uses,
and *fdch* is the step size relative to the design variable. *Dabfun* is the
absolute change in the objective function to indicate convergence (i.e., if
the objective function changes by less than *dabfun,* then the problem is
converged). Similarly, *delfun* is the relative change of the objective
function with respect to the value at the previous step. Finally, *ctlmin* is
the minimum constraint thickness for the linear constraints. We also use
``cons_is_linear`` to let CONMIN know that both constraints are linear. This
can speed up the algorithm, though it hardly matters here.

As before, the ``add_constraint`` method is used to add our constraints. This
time however, we used a more general expression for the first constraint. Expression strings
in OpenMDAO can also be parsed as inequalities, so all of the following are
equivalent ways of defining this constraint:

.. testcode:: MDF_parts

        self.driver.add_constraint('3.16 - dis1.y1 < 0')
        self.driver.add_constraint('3.16 < dis1.y1')
        self.driver.add_constraint('-3.16 > -dis1.y1')

Finally, putting it all together gives:

.. testcode:: MDF_full

        from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                                       SellarDiscipline2
        from openmdao.examples.mdao.broadcaster import Broadcaster
        
        from openmdao.main.api import Assembly, set_as_top
        from openmdao.lib.drivers.api import CONMINdriver, FixedPointIterator
        
        class SellarMDF(Assembly):
            """ Optimization of the Sellar problem using MDF
            Disciplines coupled with FixedPointIterator.
            """
            
            def __init__(self):
                """ Creates a new Assembly with this problem
                
                Optimal Design at (1.9776, 0, 0)
                
                Optimal Objective = 3.18339"""
                
                # pylint: disable-msg=E1101
                super(SellarMDF, self).__init__()
        
                # create Optimizer instance
                self.add('driver', CONMINdriver())
                
                # Outer Loop - Global Optimization
                self.add('bcastr', Broadcaster())
                self.add('fixed_point_iterator', FixedPointIterator())
                self.driver.workflow.add(['bcastr', 'fixed_point_iterator'])
        
                # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
                self.add('dis1', SellarDiscipline1())
                self.add('dis2', SellarDiscipline2())
                self.fixed_point_iterator.workflow.add(['dis1', 'dis2'])
                
                # Make all connections
                self.connect('bcastr.z1','dis1.z1')
                self.connect('bcastr.z1','dis2.z1')
                self.connect('bcastr.z2','dis1.z2')
                self.connect('bcastr.z2','dis2.z2')
                self.connect('dis1.y1','dis2.y1')
        
                # Iteration loop
                self.fixed_point_iterator.add_parameter('dis1.y2', low=-9.e99, high=9.e99)
                self.fixed_point_iterator.add_constraint('dis2.y2 = dis1.y2')
                self.fixed_point_iterator.max_iteration = 1000
                self.fixed_point_iterator.tolerance = .0001
        
                # Optimization parameters
                self.driver.add_objective('(dis1.x1)**2 + bcastr.z2 + dis1.y1 + math.exp(-dis2.y2)')
                
                self.driver.add_parameter('bcastr.z1_in', low = -10.0, high = 10.0)
                self.driver.add_parameter('bcastr.z2_in', low = 0.0,   high = 10.0)
                self.driver.add_parameter('dis1.x1',      low = 0.0,   high = 10.0)
        
                self.driver.add_constraint('3.16 < dis1.y1')
                self.driver.add_constraint('dis2.y2 < 24.0')
                    
                self.driver.cons_is_linear = [1, 1]
                self.driver.iprint = 0
                self.driver.itmax = 30
                self.driver.fdch = .001
                self.driver.fdchm = .001
                self.driver.delfun = .0001
                self.driver.dabfun = .000001
                self.driver.ctlmin = 0.0001

This problem is contained in ``sellar_MDF.py``. Executing it at the command line should produce
output that resembles this:

::

        $ python sellar_MDF.py
        CONMIN Iterations:  12
        Minimum found at (1.977657, 0.000000, 0.000000)
        Couping vars: 3.160068, 3.755315
        Minimum objective:  3.18346116811
        Elapsed time:  0.121051073074 seconds

We can also replace the fixed point iterator with a better solver. Fixed point
iteration works for some problems, including this one, but may not converge to
a solution for other problems. OpenMDAO also contains a Broyden solver called
*BroydenSolver*. This solver is based on a quasi-Newton-Raphson algorithm found in 
``scipy.nonlinear``. It uses a Broyden update to approximate the Jacobian. If we
replace ``FixedPointIterator`` with ``BroydenSolver``, the optimizer's workflow
looks like this:

.. testcode:: MDF_parts

        # Don't forget to put the import in your header
        from openmdao.lib.drivers.api import BroydenSolver

        # Outer Loop - Global Optimization
        self.add('bcastr', Broadcaster())
        self.add('solver', BroydenSolver())
        self.driver.workflow.add(['bcastr', 'solver'])

Next, we set up our parameters for the inner loop. The Broyden solver can be
connected using the standard driver interface. It can take multiple inputs and outputs
though we only have one input and one output in this example.
        
.. testcode:: MDF_parts

        # Iteration loop
        self.solver.add_parameter('dis1.y2', low=-9.e99, high=9.e99)
        self.solver.add_constraint('dis2.y2 = dis1.y2')
        self.solver.itmax = 10
        self.solver.alpha = .4
        self.solver.tol = .0000001
        self.solver.algorithm = "broyden2"
        
The input is selected using ``add_parameter``. Note that the interface is the same
as in the FixedPointIterator. As before, we've given a *low* and a
*high* attribute, but we've set them to very large negative and positive values
as the Broyden solver doesn't use either of these. The output is specified by adding an equality constraint.
A solver essentially tries to drive something to zero. In this case, we want to
drive the residual error in the coupled variable *y2* to zero. An equality constraint
is defined with an expression string which is parsed for the equals sign, so the
following constraints are equivalent:

.. testcode:: MDF_parts

        # Iteration loop
        self.solver.add_constraint('dis2.y2 = dis1.y2')
        self.solver.add_constraint('dis2.y2 - dis1.y2 = 0')
        
Equality constraints may also be available for some optimizers, but you should 
verify that they are supported. CONMIN does not support equality constraints.

