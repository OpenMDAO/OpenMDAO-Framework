"""
    Solution of the sellar analytical problem using MDF.
    Disciplines coupled using Quasi-Newton-Raphson solver
"""

# pylint: disable-msg=E0611,F0401
from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2
from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver

class SellarMDF(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with BroydenSolver.
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
        self.add('solver', BroydenSolver())
        self.driver.workflow.add('solver')

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        self.add('dis1', SellarDiscipline1())
        self.add('dis2', SellarDiscipline2())
        self.solver.workflow.add(['dis1', 'dis2'])
        
        # Make all connections
        self.connect('dis1.y1','dis2.y1')

        # Iteration loop
        self.solver.add_parameter('dis1.y2', low=-9.e99, high=9.e99)
        self.solver.add_constraint('dis2.y2 = dis1.y2')
        # equivilent form
        # self.solver.add_constraint('dis2.y2 - dis1.y2 = 0')
        
        #Driver Settings
        self.solver.itmax = 10
        self.solver.alpha = .4
        self.solver.tol = .0000001
        self.solver.algorithm = "broyden2"

        # Optimization parameters
        self.driver.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)')
        for param, low, high in zip([('dis1.z1','dis2.z1'), ('dis1.z2','dis2.z2'),
                                     'dis1.x1'],
                                    [-10.0, 0.0, 0.0],
                                    [10.0, 10.0, 10.0]):
            self.driver.add_parameter(param, low=low, high=high)
        map(self.driver.add_constraint, ['3.16 < dis1.y1',
                                              'dis2.y2 < 24.0' ])
        self.driver.cons_is_linear = [1, 1]
        self.driver.iprint = 0
        self.driver.itmax = 30
        self.driver.fdch = .001
        self.driver.fdchm = .001
        self.driver.delfun = .0001
        self.driver.dabfun = .000001
        self.driver.ctlmin = 0.0001
        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    prob = SellarMDF()
    set_as_top(prob)
    
    # pylint: disable-msg=E1101
        
    prob.dis1.z1 = prob.dis2.z1 = 5.0
    prob.dis1.z2 = prob.dis2.z2 = 2.0
    prob.dis1.x1 = 1.0
    prob.dis2.z1_in = 5.0
    prob.dis2.z2_in = 2.0
    
    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1, \
                                             prob.dis2.z2, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"

    
# End sellar_MDF_solver.py