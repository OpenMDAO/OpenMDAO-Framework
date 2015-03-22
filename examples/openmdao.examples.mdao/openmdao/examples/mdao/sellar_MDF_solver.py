"""
    Solution of the sellar analytical problem using MDF.
    Disciplines coupled using Newton solver
"""
from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import SLSQPdriver, BroydenSolver

from openmdao.lib.optproblems import sellar

class SellarMDF(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with BroydenSolver.
    """

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # pylint: disable-msg=E1101
        # create Optimizer instance
        self.add('driver', SLSQPdriver())

        #Outer Loop - Global Optimization
        self.add('solver', BroydenSolver())
        self.driver.workflow.add(['solver'])

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        self.add('dis1', sellar.Discipline1())
        self.add('dis2', sellar.Discipline2())
        self.solver.workflow.add(['dis1', 'dis2'])

        # Add Parameters to optimizer
        self.driver.add_parameter(('dis1.z1','dis2.z1'), low=-10.0, high=10.0)
        self.driver.add_parameter(('dis1.z2','dis2.z2'), low=0.0,   high=10.0)
        self.driver.add_parameter('dis1.x1', low=0.0, high=10.0)

        # Make all connections
        self.connect('dis1.y1', 'dis2.y1')

        # Solver Iteration loop
        self.solver.add_parameter('dis1.y2')
        self.solver.add_constraint('dis2.y2 = dis1.y2')
        # equivilent form
        # self.solver.add_constraint('dis2.y2 - dis1.y2 = 0')

        #Driver Settings
        self.solver.itmax = 25
        #self.solver.alpha = .4
        self.solver.tol = 1e-9
        self.solver.algorithm = "broyden2"

        # Optimization parameters
        self.driver.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)')

        self.driver.add_constraint('3.16 < dis1.y1')
        self.driver.add_constraint('dis2.y2 < 24.0')

        self.driver.iprint = 0

if __name__ == "__main__": # pragma: no cover

    import time

    prob = SellarMDF()

    # pylint: disable-msg=E1101

    prob.dis1.z1 = prob.dis2.z1 = 5.0
    prob.dis1.z2 = prob.dis2.z2 = 2.0
    prob.dis1.x1 = 1.0

    tt = time.time()
    prob.run()

    #prob.driver.check_gradient()

    print "\n"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis2.z2,
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"


# End sellar_MDF_solver.py
