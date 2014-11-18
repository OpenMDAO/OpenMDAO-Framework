"""
    Solution of the sellar analytical problem using MDF.
    Disciplines coupled using Fixed Point Iteration
"""
from openmdao.main.api import Assembly, Component
from openmdao.lib.drivers.api import SLSQPdriver, FixedPointIterator
from openmdao.lib.optproblems import sellar


class SellarMDF(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with FixedPointIterator.
    """

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # create Optimizer instance
        self.add('driver', SLSQPdriver())

        # Outer Loop - Global Optimization
        self.add('solver', FixedPointIterator())
        self.driver.workflow.add(['solver'])

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        self.add('dis1', sellar.Discipline1_WithDerivatives())
        self.add('dis2', sellar.Discipline2_WithDerivatives())
        self.solver.workflow.add(['dis1', 'dis2'])

        # Add Parameters to optimizer
        self.driver.add_parameter(('dis1.z1','dis2.z1'), low=-10.0, high=10.0)
        self.driver.add_parameter(('dis1.z2','dis2.z2'), low= 0.0,  high=10.0)
        self.driver.add_parameter('dis1.x1', low=0.0, high=10.0)

        # Make all connections
        self.connect('dis1.y1', 'dis2.y1')

        # Iteration loop
        self.solver.add_parameter('dis1.y2')
        self.solver.add_constraint('dis2.y2 = dis1.y2')

        # Solver settings
        self.solver.max_iteration = 100
        self.solver.tolerance = .00001
        self.solver.print_convergence = False

        # Optimization parameters
        self.driver.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)')

        self.driver.add_constraint('3.16 < dis1.y1')
        self.driver.add_constraint('dis2.y2 < 24.0')

        self.driver.iprint = 0

if __name__ == "__main__": # pragma: no cover

    import time

    prob = SellarMDF()
    prob.name = "top"

    prob.dis1.z1 = prob.dis2.z1 = 5.0
    prob.dis1.z2 = prob.dis2.z2 = 2.0
    prob.dis1.x1 = 1.0
    prob.dis2.y1 = 3.16

    tt = time.time()
    prob.run()
    ttot = time.time()-tt

    print "\n"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", ttot, "seconds"


# End sellar_MDF.py
