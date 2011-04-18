"""
    Solution of the sellar analytical problem using MDF.
    Disciplines coupled using Fixed Point Iteration
"""

# pylint: disable-msg=E0611,F0401
from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2
from openmdao.lib.components.api import Broadcaster
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
        self.add('bcastr', Broadcaster(['z1','z2','x1','y1','y2']))
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
        self.fixed_point_iterator.x_out = 'dis2.y2'
        self.fixed_point_iterator.x_in = 'dis1.y2'
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
        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    prob = SellarMDF()
    set_as_top(prob)
    
    # pylint: disable-msg=E1101
        
    prob.bcastr.z1_in = 5.0
    prob.bcastr.z2_in = 2.0
    prob.dis1.x1 = 1.0
    prob.dis2.z1_in = 5.0
    prob.dis2.z2_in = 2.0
    
    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.bcastr.z1_in, \
                                             prob.bcastr.z2_in, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"

    
# End sellar_MDF.py