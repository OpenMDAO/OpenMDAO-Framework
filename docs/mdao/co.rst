.. index:: Collaborative Optimization (CO)

.. _Collaborative-Optimization-(CO):

Collaborative Optimization (CO)
===============================

Next, we will set up a model that solves the Sellar problem by Collaborative
Optimization (CO). CO is a two-level architecture with three optimizer loops,
one at each discipline, and one acting globally. The global optimizer drives
the design and coupling variables towards an optimal solution that minimizes
the objective while constraining to zero the sum of the squares of the
residuals between the values commanded by the global optimizer and those set
by the local optimizers. Each local optimizer operates on its own
discipline, driving its design variables while minimizing the residual between
the actual value of the design variables and the values commanded by the global
optimizer.

Unlike IDF and MDF, for CO the global optimizer does not work directly with any of the inputs in any of the
disciplines. Instead, the global optimizer works on a set of copies of the global design variables. These
copies are sometimes called *targets*, so we'll put an ``_t`` at the end of the variable name. Each discipline
has its own  local optimization which works with the local input associated with the global target. This is all
represented in the figure below, and it's a bit of a mess. You can see that it's important to separate data
flow and workflow to help keep things manageable. 


.. figure:: Arch-CO.png
   :align: center
   :alt: Arrows and boxes showing data flow for collaborative optimization
   
   Data Flow for CO

The CO model has three optimizers, so there are three workflows. The top level
workflow includes just the two lower level optimizers, and each of those optimizers has a 
workflow with just the discipline component. This can be seen in the next figure.
   
.. figure:: Arch-CO-OpenMDAO.png
   :align: center
   :alt: Rounded and straight-sided rectangles showing the iteration hierarchy of the three workflows for CO
      
   Iteration Hierarchy for CO

First, we define the global target variables, create the component instances, 
and set up this iteration hierarchy. Pay special attention to how we define the
variables. Notice that the ``iotype`` metadata is not set at all. Normally 
you would set ``iotype="in"`` or ``iotype="out"`` when you create a variable. In this 
case, the variables are neither inputs nor outputs to the assembly. Instead, they are
meant only to be internal variables, varied by the top level driver and used 
in the lower level optimization objectives. Hence, no `iotype` is set. 
        
.. testcode:: CO_parts

        from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2
        from openmdao.lib.datatypes.api import Float
        from openmdao.main.api import Assembly, set_as_top
        from openmdao.lib.drivers.api import CONMINdriver
        
        
        class SellarCO(Assembly):
            """Solution of the sellar analytical problem using CO.
            """
            
            z1_t = Float()
            z2_t = Float()
            x1_t = Float()
            y1_t = Float()
            y2_t = Float()
        
            def __init__(self):
                """ Creates a new Assembly with this problem
                
                Optimal Design at (1.9776, 0, 0)
                
                Optimal Objective = 3.18339"""
                
                # pylint: disable-msg=E1101
                super(SellarCO, self).__init__()
                
                # Global Optimization
                self.add('driver', CONMINdriver())
                self.add('localopt1', CONMINdriver())
                self.add('localopt2', CONMINdriver())
                self.driver.workflow.add(['localopt1', 
                                          'localopt2'])
                
                # Local Optimization 1
                self.add('dis1', SellarDiscipline1())
                self.localopt1.workflow.add('dis1')
                
                # Local Optimization 2
                self.add('dis2', SellarDiscipline2())
                self.localopt2.workflow.add('dis2')

Since there are no data connections, we never need to call ``self.connect``.

Now we need to set up the parameters for the outer optimization loop. 

.. testcode:: CO_parts
    :hide:
    
    self = SellarCO()

.. testcode:: CO_parts

        #Parameters - Global Optimization
        self.driver.add_objective('(x1_t)**2 + z2_t + y1_t' + \
                                                '+ math.exp(-y2_t)')
        self.driver.add_parameter('z1_t', low = -10.0, high = 10.0)
        self.driver.add_parameter('z2_t', low = 0.0,   high = 10.0)
        self.driver.add_parameter('x1_t', low = 0.0,   high = 10.0)
        self.driver.add_parameter('y1_t', low = 3.16,  high = 10.0)
        self.driver.add_parameter('y2_t', low = -10.0, high = 24.0)

        con1 = '(z1_t-dis1.z1)**2 + (z2_t-dis1.z2)**2 + ' + \
               '(x1_t-dis1.x1)**2 + ' + \
               '(y1_t-dis1.y1)**2 + (y2_t-dis1.y2)**2 <= 0'
        
        con2 = '(z1_t-dis2.z1)**2 + (z2_t-dis2.z2)**2 + ' + \
               '(y1_t-dis2.y1)**2 + (y2_t-dis2.y2)**2 <= 0'
        self.driver.add_constraint(con1)
        self.driver.add_constraint(con2)
        
        self.driver.printvars = ['dis1.y1', 'dis2.y2']
        self.driver.iprint = 0
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .0001
        self.driver.dabfun = .00001
        self.driver.ct = -.0008
        self.driver.ctlmin = 0.0008

Here we are able to build a complicated expression for the sum of the squares
of all of the residuals and use it as our constraint. This is another
example of a constraint that could be better served as an equality constraint, 
but there is some research which indicates the performance of CO can be 
improved by switching to an inequality constraint with a small, but non zero
tolerance. We created two constraints, one for each discipline.

Finally, we set up our local optimization loops.

.. testcode:: CO_parts

        #Parameters - Local Optimization 1
        self.localopt1.add_objective('(z1_t-dis1.z1)**2 + ' + \
                                   '(z2_t-dis1.z2)**2 + ' + \
                                   '(x1_t-dis1.x1)**2 + ' + \
                                   '(y1_t-dis1.y1)**2 + ' + \
                                   '(y2_t-dis1.y2)**2')
        self.localopt1.add_parameter('dis1.z1', low = -10.0, high = 10.0)
        self.localopt1.add_parameter('dis1.z2', low = 0.0,   high = 10.0)
        self.localopt1.add_parameter('dis1.x1', low = 0.0,   high = 10.0)
        self.localopt1.add_parameter('dis1.y2', low = -10.0, high = 24.0)
        self.localopt1.iprint = 0
        self.localopt1.itmax = 100
        self.localopt1.fdch = .003
        self.localopt1.fdchm = .003
        self.localopt1.delfun = .0001
        self.localopt1.dabfun = .000001
        
        #Parameters - Local Optimization 2
        self.localopt2.add_objective('(z1_t-dis2.z1)**2 + ' + \
                                   '(z2_t-dis2.z2)**2 + ' + \
                                   '(y1_t-dis2.y1)**2 + ' + \
                                   '(y2_t-dis2.y2)**2')
        self.localopt2.add_parameter('dis2.z1', low = -10.0, high = 10.0)
        self.localopt2.add_parameter('dis2.z2', low = 0.0,   high = 10.0)
        self.localopt2.add_parameter('dis2.y1', low = 3.16,  high = 10.0)
        self.localopt2.iprint = 0
        self.localopt2.itmax = 100
        self.localopt2.fdch = .003
        self.localopt2.fdchm = .003
        self.localopt2.delfun = .001
        self.localopt2.dabfun = .00001

This problem is contained in 
:download:`sellar_CO.py </../examples/openmdao.examples.mdao/openmdao/examples/mdao/sellar_CO.py>`. 
The full file follows, again with a little bit of code which runs the model and prints out some 
data about it. 

.. testcode:: CO_full

        from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                                       SellarDiscipline2
        from openmdao.lib.datatypes.api import Float
        from openmdao.main.api import Assembly, set_as_top
        from openmdao.lib.drivers.api import CONMINdriver
        
        
        class SellarCO(Assembly):
            """Solution of the sellar analytical problem using CO.
            """
            
            z1_t = Float()
            z2_t = Float()
            x1_t = Float()
            y1_t = Float()
            y2_t = Float()
        
            def __init__(self):
                """ Creates a new Assembly with this problem
                
                Optimal Design at (1.9776, 0, 0)
                
                Optimal Objective = 3.18339"""
                
                super(SellarCO, self).__init__()
                
                # Global Optimization
                self.add('driver', CONMINdriver())
                self.add('localopt1', CONMINdriver())
                self.add('localopt2', CONMINdriver())
                self.driver.workflow.add(['localopt1', 
                                          'localopt2'])
                
                # Local Optimization 1
                self.add('dis1', SellarDiscipline1())
                self.localopt1.workflow.add('dis1')
                
                # Local Optimization 2
                self.add('dis2', SellarDiscipline2())
                self.localopt2.workflow.add('dis2')
                
                #Parameters - Global Optimization
                self.driver.add_objective('(x1_t)**2 + z2_t + y1_t' + \
                                                        '+ math.exp(-y2_t)')
                self.driver.add_parameter('z1_t', low = -10.0, high = 10.0)
                self.driver.add_parameter('z2_t', low = 0.0,   high = 10.0)
                self.driver.add_parameter('x1_t', low = 0.0,   high = 10.0)
                self.driver.add_parameter('y1_t', low = 3.16,  high = 10.0)
                self.driver.add_parameter('y2_t', low = -10.0, high = 24.0)
        
                con1 = '(z1_t-dis1.z1)**2 + (z2_t-dis1.z2)**2 + ' + \
                       '(x1_t-dis1.x1)**2 + ' + \
                       '(y1_t-dis1.y1)**2 + (y2_t-dis1.y2)**2 <= 0'
                
                con2 = '(z1_t-dis2.z1)**2 + (z2_t-dis2.z2)**2 + ' + \
                       '(y1_t-dis2.y1)**2 + (y2_t-dis2.y2)**2 <= 0'
                self.driver.add_constraint(con1)
                self.driver.add_constraint(con2)
                
                self.driver.printvars = ['dis1.y1', 'dis2.y2']
                self.driver.iprint = 0
                self.driver.itmax = 100
                self.driver.fdch = .003
                self.driver.fdchm = .003
                self.driver.delfun = .0001
                self.driver.dabfun = .00001
                self.driver.ct = -.0008
                self.driver.ctlmin = 0.0008
        
                #Parameters - Local Optimization 1
                self.localopt1.add_objective('(z1_t-dis1.z1)**2 + ' + \
                                           '(z2_t-dis1.z2)**2 + ' + \
                                           '(x1_t-dis1.x1)**2 + ' + \
                                           '(y1_t-dis1.y1)**2 + ' + \
                                           '(y2_t-dis1.y2)**2')
                self.localopt1.add_parameter('dis1.z1', low = -10.0, high = 10.0)
                self.localopt1.add_parameter('dis1.z2', low = 0.0,   high = 10.0)
                self.localopt1.add_parameter('dis1.x1', low = 0.0,   high = 10.0)
                self.localopt1.add_parameter('dis1.y2', low = -10.0, high = 24.0)
                self.localopt1.iprint = 0
                self.localopt1.itmax = 100
                self.localopt1.fdch = .003
                self.localopt1.fdchm = .003
                self.localopt1.delfun = .0001
                self.localopt1.dabfun = .000001
                
                #Parameters - Local Optimization 2
                self.localopt2.add_objective('(z1_t-dis2.z1)**2 + ' + \
                                           '(z2_t-dis2.z2)**2 + ' + \
                                           '(y1_t-dis2.y1)**2 + ' + \
                                           '(y2_t-dis2.y2)**2')
                self.localopt2.add_parameter('dis2.z1', low = -10.0, high = 10.0)
                self.localopt2.add_parameter('dis2.z2', low = 0.0,   high = 10.0)
                self.localopt2.add_parameter('dis2.y1', low = 3.16,  high = 10.0)
                self.localopt2.iprint = 0
                self.localopt2.itmax = 100
                self.localopt2.fdch = .003
                self.localopt2.fdchm = .003
                self.localopt2.delfun = .001
                self.localopt2.dabfun = .00001
        
        
        if __name__ == "__main__":
        
            import time
            
            prob = SellarCO()
            set_as_top(prob)
                    
            prob.z1_t = 5.0
            prob.dis1.z1 = 5.0
            prob.dis2.z1 = 5.0
        
            prob.z2_t = 2.0
            prob.dis1.z2 = 2.0
            prob.dis2.z2 = 2.0
        
            prob.x1_t = 1.0
            prob.dis1.x1 = 1.0
            
            prob.y1_t = 3.16
            prob.y2_t = 0.0
            prob.dis1.y2 = 0.0
            prob.dis2.y1 = 3.16
            
            tt = time.time()
            prob.run()
        
            print "\n"
            print "CONMIN Iterations: ", prob.driver.iter_count
            print "Minimum found at (%f, %f, %f)" % (prob.z1_t, \
                                                     prob.z2_t, \
                                                     prob.dis1.x1)
            print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
            print "Minimum objective: ", prob.driver.eval_objective()
            print "Elapsed time: ", time.time()-tt, "seconds"

Executing it at the command line should produce
output that resembles this:

::

        $ python sellar_CO.py
        CONMIN Iterations:  44
        Minimum found at (1.979061, 0.000000, 0.007239)
        Couping vars: 3.157715, 3.788089
        Minimum objective:  3.18226721772
        Elapsed time:  16.130461216 seconds


After 44 iterations of the top level optimizer, CO gives an answer that's slightly off the optimum, but the calculated
objective is still very close.
