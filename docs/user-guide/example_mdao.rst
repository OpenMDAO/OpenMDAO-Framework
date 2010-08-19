
.. index:: MDAO tutorial problem

.. _Tutorial-MDAO-Architectures:

Tutorial: MDAO Architectures
============================

This tutorial shows how to create a model to solve a simple problem consisting of
two coupled disciplines using several MDAO strategies including:

#. Multidisciplinary Design Feasible (MDF)
#. Independent Design Feasible (IDF)
#. Collaborative Optimization (CO)

The tutorial will introduce you to some new topics that include using the itereation
hieararchy to set up models with nested optimization loops, using a solver to "close
the loop" in a coupled multidisciplinary simulation, and using a broadcaster to set
the values of design variables in multiple places at one time.

This tutorial covers some of the more advanced capabilities of OpenMDAO. You should read and understand
the :ref:`simple tutorial problem <Getting-Started-with-OpenMDAO>` before starting this one. An
understanding of the material presented in ref:`A-More-Complex-Tutorial-Problem` is also
recommended.

All of these tutorials use the Sellar problem which consists of 2 disciplines as follows:

.. figure:: ../images/user-guide/Sellar.png
   :align: center

Variables z1, z2, and x1 are the design variables over which we'd like to minimize
the objective. Both disciplines are functions of z1 and z2, so they are called the 
global design variables, while only the first discipline is a function of x1, so it
is called the local design variable. The two disciplines are coupled by the
coupling variables y1 and y2. Discipline 1 takes y2 as an input, and computes y1 as
an output, while discipline 2 takes y1 as in input and computes y2 as an output. As
such, the two disciplines depend on each other's output so iteration is required to
find a set of coupling variables that satisfies both equations.

Disciplines 1 and 2 were implemented in OpenMDAO as components.

.. testcode:: Disciplines

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
    class SellarDiscipline1(Component):
        """Component containing Discipline 1"""
        
        # pylint: disable-msg=E1101
        z1 = Float(0.0, iotype='in', desc='Global Design Variable')
        z2 = Float(0.0, iotype='in', desc='Global Design Variable')
        x1 = Float(0.0, iotype='in', desc='Local Design Variable')
        y2 = Float(0.0, iotype='in', desc='Disciplinary Coupling')
    
        y1 = Float(iotype='out', desc='Output of this Discipline')        
    
            
        def execute(self):
            """Evaluates the equation  
            y1 = z1**2 + z2 + x1 - 0.2*y2"""
            
            z1 = self.z1
            z2 = self.z2
            x1 = self.x1
            y2 = self.y2
            
            self.y1 = z1**2 + z2 + x1 - 0.2*y2
    
    
    class SellarDiscipline2(Component):
        """Component containing Discipline 2"""
        
        # pylint: disable-msg=E1101
        z1 = Float(0.0, iotype='in', desc='Global Design Variable')
        z2 = Float(0.0, iotype='in', desc='Global Design Variable')
        y1 = Float(0.0, iotype='in', desc='Disciplinary Coupling')
    
        y2 = Float(iotype='out', desc='Output of this Discipline')        
    
            
        def execute(self):
            """Evaluates the equation  
            y1 = y1**(.5) + z1 + z2"""
            
            z1 = self.z1
            z2 = self.z2
            
            # Note: this may cause some issues. However, y1 is constrained to be
            # above 3.16, so lets just let it converge, and the optimizer will 
            # throw it out
            y1 = abs(self.y1)
            
            self.y2 = y1**(.5) + z1 + z2
            
SellarDiscipline 2 contains a square root of variable y1 in its calculation. For negative values
of y1, the result would be imaginary, so the absolute value is taken before the square root
is applied. This component is clearly not valid for y1 < 0, and our first was to add
a *low* attribute to the variable definition for y1. However, the solver that was used to
converge the two disciplines occasionally forced y1 to go slightly negative. The inclusion
of the absolute value solved the problem without impacting the eventual convergence of the
solver.

These two components are contained in the file ``disciplines.py``.

**References:**

_`1`. Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based,
Concurrent Subspace Optimization for Multidisciplinary System Design,"
Proceedings References 79 of the 34th AIAA Aerospace Sciences Meeting and
Exhibit, Reno, NV, January 1996.
            
Multidisciplinary Design Feasible (MDF)
---------------------------------------

In a Multidisciplinary Design Feasible (MDF) problem, the disciplines are directly coupled
via some kind of solver, and the design variables are optimized in a single loop. The
following diagram illustrates the data flow for MDF applied to the Sellar problem.

.. figure:: ../images/user-guide/Arch-MDF.png
   :align: center

This diagram introduces a component called a Broadcaster. A Broadcaster is a component that
enables a design variable to be set to the same value at multiple locations. Recall that a
driver such as the CONMIN optimizer contains a list of *parameters*, where each parameter is
a location in OpenMDAO's data hierarchy. Each parameter is a single design variable, and there
is no way to indicate that one design variable might be needed at multiple component inputs
in the model. We can overcome this by creating a component that passes an input to its output.
Thus, CONMIN can set the design variable in this Broadcaster, and when the Broadcaster executes,
the new value gets passed to all of the components that need it.

OpenMDAO doesn't have a built-in Broadcaster, so we need to make our own. It's a simple
component with some inputs, some outputs, and an execute function that passes the inputs
to the outputs.

.. testcode:: Broadcaster

    from openmdao.main.api import Component
    from openmdao.lib.api import Float
    
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

We've added the coupling variables in our Broadcaster as well, foreshadowing the need
for them in some of the other MDAO architectures.

The diagram also shows a solver that takes the output of the component dataflow
and feeds it back into the input. OpenMDAO presently has two solvers: *FixedPointIterator*
and *BroydenSolver*. The FixedPointIterator is a solver that performs fixed point iteration,
which means that it keeps driving x_new = f(x_old) until convergence is acheived. In
other words, y2 is passed from the output of SellarDiscipline2 to the input of SellarDiscipline1,
and the loop keeps executing until the change in the value of y2 between iterations is
smaller than a tolerance. The BroydenSolver is a solver based on a quasi-Newton-Raphson
algorithm that uses a Broyden update to approximate the Jacobian. This solver reads
the output and calculates a new input each iteration. Convergence is achieved when the
residual between the output and input is driven to zero.

The major difference between the MDF problem and previous examples is the
presence of nested drivers. Drivers can be nested in OpenMDAO using WorkFlows
in the iteration hierarchy. A WorkFlow is an object that determines execution
order for a group of Components. Each driver contains a single WorkFlow. For
each iteration, a Driver will execute one pass through the WorkFlow, executing
the components contained therein in the order the WorkFlow prescribes.
Although in many cases a WorkFlow contains just Components, it can also
contain Drivers. This allows nested iterative processes to be created. The
following diagram shows an iteration hierarchy for the MDF problem.
   
.. figure:: ../images/user-guide/Arch-MDF-OpenMDAO.png
   :align: center
   
In the top left of this diagram, the gray bubble labeled "Optimizer" is the
top-level (or outermost) driver. This driver has a workflow that contains
two objects -- the Broadcaster, and a Solver, so each time the optimizer runs
an iteration, both of these components run. The Solver also has a workflow
which contains the two discipline components. With the nesting of the drivers
we get the behavior we want, namely that for each optimizer iteration, the 
solver runs the discipline components until they converge. We have a nested
driver loop.

The execution order is determined by the components' dataflow. Here, the
broadcaster feeds the design variables to the discipline components, which
are contained in the solver's workflow, so the broadcaster must run first. Also,
the data connection between the two discipline components means that SellarDiscipline1
runs before SellarDiscipline2. Sometimes a workflow may contain components that are
not directly connected, and could be run concurrently. Future tutorials will
demonstrate this.

Now, let's create the assembly for the MDF problem. First, we'll create
the top level optimization loop.

.. testcode:: MDF_parts

        from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                                       SellarDiscipline2
        from openmdao.examples.mdao.broadcaster import Broadcaster
        
        from openmdao.main.api import Assembly, set_as_top
        from openmdao.lib.api import CONMINdriver, FixedPointIterator
        
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
                self.driver.workflow.add([self.bcastr, self.fixed_point_iterator])
                
There is nothing really new so far in terms of syntax. Note that the top level driver is
always named 'driver'. However, all other drivers can be named any valid name. For this
model, we've chosen to use the FixedPointIterator.

Next, we need to create the workflow for the solver. We create instances of SellarDiscipline1
and SellarDiscipline2 and add them to the assembly. Then, instead of adding them to the
workflow of 'driver' we add them to the workflow of 'fixed_point_iterator'.

.. testcode:: MDF_parts
    :hide:
    
    self = SellarMDF()

.. testcode:: MDF_parts

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        self.add('dis1', SellarDiscipline1())
        self.add('dis2', SellarDiscipline2())
        self.fixed_point_iterator.workflow.add([self.dis1, self.dis2])
        
Now the iteration hierarchy is finished. We still need to hook up the data connections,
and set up the CONMIN optimization as well as the fixed point iteration.

We need one connection between 'dis1' and 'dis2'. We also need to hook up 'bcastr'
so that the design variables carry through to the discipline components.

.. testcode:: MDF_parts

        self.connect('bcastr.z1','dis1.z1')
        self.connect('bcastr.z1','dis2.z1')
        self.connect('bcastr.z2','dis1.z2')
        self.connect('bcastr.z2','dis2.z2')
        self.connect('dis1.y1','dis2.y1')

Next, the parameters for the fixed point iterator must be set. FixedPointIterator
is a specialized solver that is only applicable to single-input single-output problems.
As such, it does not conform to the standard driver interface. The output from SellarDiscipline2
is 'dis2.y2'. During iteration, this is the variable that is going to be sent to the input
of SellarDiscipline1, which is 'dis1y2'. The parameter x_out takes the output variable
while the parameter x_in takes the input variable. These are Expressions, but fixed point
iteration doesn't make sense using anything other than single variables. We also set the
maximum number of iterations, and a convergence tolerance.
        
.. testcode:: MDF_parts

        # Iteration loop
        self.fixed_point_iterator.x_out = 'dis2.y2'
        self.fixed_point_iterator.x_in = 'dis1.y2'
        self.fixed_point_iterator.max_iteration = 1000
        self.fixed_point_iterator.tolerance = .0001       

Finally, the CONIM optimization is set up.

.. testcode:: MDF_parts

        # Optimization parameters
        self.driver.objective = '(dis1.x1)**2 + bcastr.z2 + dis1.y1 + math.exp(-dis2.y2)'
                
        self.driver.add_parameter('bcastr.z1_in', low = -10.0, high = 10.0)
        self.driver.add_parameter('bcastr.z2_in', low = 0.0,   high = 10.0)
        self.driver.add_parameter('dis1.x1',      low = 0.0,   high = 10.0)
        
        self.driver.add_constraint('3.16 < dis1.y1')
        self.driver.add_constraint('dis2.y2 - 24.0')
        
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
the objective function changes by less than dabfun, then the problem is
converged). Similarly, *delfun* is the relative change of the objective
function with respect to the value at the previous step. Finally, *ctlmin* is
the minimum constraint thickness for the linear constraints. We also use
*cons_is_linear* to let CONMIN know that both constraints are linear. This
can speed up the algorithm, though it hardly matters here.

As before, the add_constraint function is used to add our constraints. This
time however, we used a more general expression for the first constraint. Expressions
in OpenMDAO can also be parsed as inequalities, so all of the following are
equivalent ways of defining this constraint:

.. testcode:: MDF_parts

        self.driver.add_constraint('3.16 - dis1.y1')
        self.driver.add_constraint('3.16 - dis1.y1 < 0')
        self.driver.add_constraint('3.16 < dis1.y1')
        self.driver.add_constraint('-3.16 > -dis1.y1')

Finally, putting it all together gives:

.. testcode:: MDF_full

        from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                                       SellarDiscipline2
        from openmdao.examples.mdao.broadcaster import Broadcaster
        
        from openmdao.main.api import Assembly, set_as_top
        from openmdao.lib.api import CONMINdriver, FixedPointIterator
        
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
                self.driver.workflow.add([self.bcastr, self.fixed_point_iterator])
        
                # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
                self.add('dis1', SellarDiscipline1())
                self.add('dis2', SellarDiscipline2())
                self.fixed_point_iterator.workflow.add([self.dis1, self.dis2])
                
                # Make all connections
                self.connect('bcastr.z1','dis1.z1')
                self.connect('bcastr.z1','dis2.z1')
                self.connect('bcastr.z2','dis1.z2')
                self.connect('bcastr.z2','dis2.z2')
                self.connect('dis1.y1','dis2.y1')
        
                # Iteration loop
                self.fixed_point_iterator.x_out = 'dis2.y2'
                self.fixed_point_iterator.x_in = 'dis1.y2'
                self.fixed_point_iterator.max_iteration = 1000
                self.fixed_point_iterator.tolerance = .0001
        
                # Optimization parameters
                self.driver.objective = '(dis1.x1)**2 + bcastr.z2 + dis1.y1 + math.exp(-dis2.y2)'
                
                self.driver.add_parameter('bcastr.z1_in', low = -10.0, high = 10.0)
                self.driver.add_parameter('bcastr.z2_in', low = 0.0,   high = 10.0)
                self.driver.add_parameter('dis1.x1',      low = 0.0,   high = 10.0)
        
                self.driver.add_constraint('3.16 < dis1.y1')
                self.driver.add_constraint('dis2.y2 - 24.0')
                    
                self.driver.cons_is_linear = [1, 1]
                self.driver.iprint = 0
                self.driver.itmax = 30
                self.driver.fdch = .001
                self.driver.fdchm = .001
                self.driver.delfun = .0001
                self.driver.dabfun = .000001
                self.driver.ct = -.01
                self.driver.ctlmin = 0.0001

This problem is contained in sellar_MDF.py. Executing it at the command line should produce
output that resembles this:

::

        $ python sellar_MDF.py
        CONMIN Iterations:  12
        Minimum found at (1.977657, 0.000000, 0.000000)
        Couping vars: 3.160068, 3.755315
        Minimum objective:  3.18346116811
        Elapsed time:  0.121051073074 seconds

        
Individual Design Feasible (IDF)
--------------------------------

.. figure:: ../images/user-guide/Arch-IDF.png
   :align: center

.. figure:: ../images/user-guide/Arch-IDF-OpenMDAO.png
   :align: center

