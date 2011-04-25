.. index:: Individual Design Feasible (IDF)

.. _Individual-Design-Feasible-(IDF):
        
Individual Design Feasible (IDF)
=================================

Next, we will look at how to set up the Individual Design Feasible (IDF)
architecture using the Sellar problem. In IDF, the coupling between the
disciplines is removed, and the input coupling variables are added to
the optimizer's design variables. The algorithm calls for two new equality
constraints that constrain to zero the residual error between the coupling variable
output by the optimizer and the coupling variable output by the components.
This assures that the solution is a feasible coupling, though it is achieved
through the optimizer's additional effort instead of a solver. The data
flow for IDF is illustrated in the following diagram:

.. figure:: ../images/user-guide/Arch-IDF.png
   :align: center
   :alt: diagram of boxes and arrows showing the data flow for the Individual Design Feasible 
   
   Data Flow for IDF
   
IDF needs only one driver, so there is just one workflow. The broadcaster and
the two disciplines are executed sequentially.
   
.. figure:: ../images/user-guide/Arch-IDF-OpenMDAO.png
   :align: center
   :alt: The Broadcaster and two disciplines are represented by rounded boxes inside a square box, which is the workflow.
    
   Iteration Hierarchy for IDF
   
Next, we will create the ``SellarIDF`` assembly. First, all of our components
are instantiated and the workflow is defined.
   
.. testcode:: IDF_parts

        from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                                       SellarDiscipline2
        from openmdao.examples.mdao.broadcaster import Broadcaster
        
        from openmdao.main.api import Assembly, set_as_top
        from openmdao.lib.drivers.api import CONMINdriver
        
        
        class SellarIDF(Assembly):
            """ Optimization of the Sellar problem using IDF"""
            
            def __init__(self):
                """ Creates a new Assembly with this problem
                
                Optimal Design at (1.9776, 0, 0)
                
                Optimal Objective = 3.18339"""
                
                # pylint: disable-msg=E1101
                
                super(SellarIDF, self).__init__()
        
                # create Optimizer instance
                self.add('driver', CONMINdriver())
        
                # Disciplines
                self.add('bcastr', Broadcaster())
                self.add('dis1', SellarDiscipline1())
                self.add('dis2', SellarDiscipline2())
                
                # Driver process definition
                self.driver.workflow.add(['bcastr', 'dis1', 'dis2'])
                
                # Make all connections
                self.connect('bcastr.z1','dis1.z1')
                self.connect('bcastr.z1','dis2.z1')
                self.connect('bcastr.z2','dis1.z2')
                self.connect('bcastr.z2','dis2.z2')

We've also hooked up our data connections. Only the design variables that are shared
by both components need to be connected to the broadcaster.

All that is left to do is set up the CONMIN optimizer.

.. testcode:: IDF_parts
    :hide:
    
    self = SellarIDF()

.. testcode:: IDF_parts

        # Optimization parameters
        self.driver.add_objective('(dis1.x1)**2 + bcastr.z2 + dis1.y1 + math.exp(-dis2.y2)')
        
        self.driver.add_parameter('bcastr.z1_in', low = -10.0, high=10.0)
        self.driver.add_parameter('bcastr.z2_in', low = 0.0,   high=10.0)
        self.driver.add_parameter('dis1.x1',      low = 0.0,   high=10.0)
        self.driver.add_parameter('dis2.y1',      low = 3.16,  high=10.0)
        self.driver.add_parameter('dis1.y2',      low = -10.0, high=24.0)
            
        self.driver.add_constraint('(dis2.y1-dis1.y1)**3 < 0')
        self.driver.add_constraint('(dis1.y1-dis2.y1)**3 < 0')
        self.driver.add_constraint('(dis2.y2-dis1.y2)**3 < 0')
        self.driver.add_constraint('(dis1.y2-dis2.y2)**3 < 0')
        self.driver.iprint = 0
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .0001
        self.driver.dabfun = .00001
        self.driver.ct = -.01
        self.driver.ctlmin = 0.001
        
Notice that the coupling variables are included as optimizer parameters. We
also introduce the CONMIN parameter *ct*, which is the constraint thickness for
nonlinear constraints. Our constraints are nonlinear, but note that any
constraint that involves a component output is most likely a nonlinear
constraint because outputs are usually nonlinear functions of the design variables.

Since CONMIN doesn't support equality constraints, we have to fall back on a
trick where we replace it with an equivalent pair of inequality constraints.
For example, if we want to constrain ``x=2``, we could constraint ``x<=2`` and ``x>=2`` and
let the optimizer converge to a solution where both constraints are active.
Stability may be questionable for such a method, so it is always advisable to use an
optimizer that has equality constraints rather than trying to squeeze a solution
out of an optimizer this way. In particular, be careful about trying a fancier
solution such as constraining ``abs(dis2.y1-dis1.y1)<=0``. This nonlinear
constraint has a discontinuous slope, and CONMIN won't handle that constraint very well.
Here, we take ``(dis2.y1-dis1.y1)`` and turn it into a cubic expression, which seemed
to make the problem a little less sensitive to changes in the computational
environment (32 bit vs 64 bit, etc.)

This problem is contained in ``sellar_IDF.py``. Executing it at the command line should produce
output that resembles this:

::

        $ python sellar_IDF.py
        CONMIN Iterations:  10
        Minimum found at (1.976427, 0.000287, 0.000000)
        Couping vars: 3.156521, 3.754359
        Minimum objective:  3.18022323743
        Elapsed time:  0.200541973114 seconds


