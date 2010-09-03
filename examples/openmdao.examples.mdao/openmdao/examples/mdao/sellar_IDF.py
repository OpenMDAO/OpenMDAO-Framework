"""
    Solution of the sellar analytical problem using IDF.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2
from openmdao.examples.mdao.broadcaster import Broadcaster

from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.api import CONMINdriver


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
        self.driver.workflow.add([self.bcastr, self.dis1, self.dis2])
        
        # Make all connections
        self.connect('bcastr.z1','dis1.z1')
        self.connect('bcastr.z1','dis2.z1')
        self.connect('bcastr.z2','dis1.z2')
        self.connect('bcastr.z2','dis2.z2')

        # Optimization parameters
        self.driver.objective = \
            '(dis1.x1)**2 + bcastr.z2 + dis1.y1 + math.exp(-dis2.y2)'
        
        self.driver.add_parameter('bcastr.z1_in', low = -10.0, high=10.0)
        self.driver.add_parameter('bcastr.z2_in', low = 0.0,   high=10.0)
        self.driver.add_parameter('dis1.x1',      low = 0.0,   high=10.0)
        self.driver.add_parameter('dis2.y1',      low = 3.16,  high=10.0)
        self.driver.add_parameter('dis1.y2',      low = -10.0, high=24.0)
            
        self.driver.add_constraint('(dis2.y1-dis1.y1)**3 <= 0')
        self.driver.add_constraint('(dis1.y1-dis2.y1)**3 <= 0')
        self.driver.add_constraint('(dis2.y2-dis1.y2)**3 <= 0')
        self.driver.add_constraint('(dis1.y2-dis2.y2)**3 <= 0')
  
        self.driver.iprint = 0
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .0001
        self.driver.dabfun = .00001
        self.driver.ct = -.01
        self.driver.ctlmin = 0.001


if __name__ == "__main__": # pragma: no cover         

    import time
    
    prob = SellarIDF()
    set_as_top(prob)
    
    # pylint: disable-msg=E1101
        
    prob.bcastr.z1_in = 5.0
    prob.bcastr.z2_in = 2.0
    prob.dis1.x1 = 1.0
    prob.dis2.y1 = 3.16
    
    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.bcastr.z1_in, \
                                             prob.bcastr.z2_in, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.objective.evaluate()
    print "Elapsed time: ", time.time()-tt, "seconds"

    
# End sellar_IDF.py