"""
    Solution of the sellar analytical problem using IDF.
"""
from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import CONMINdriver
from openmdao.lib.optproblems import sellar


class SellarIDF(Assembly):
    """ Optimization of the Sellar problem using IDF"""
    
    def configure(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
                
        # create Optimizer instance
        self.add('driver', CONMINdriver())

        # Disciplines
        self.add('dis1', sellar.Discipline1())
        self.add('dis2', sellar.Discipline2())
        
        # Driver process definition
        self.driver.workflow.add(['dis1', 'dis2'])
        

        # Optimization parameters
        self.driver.add_objective('(dis1.x1)**2 + dis1.z2 + dis1.y1 + math.exp(-dis2.y2)')
        
        #Global Design Variables
        self.driver.add_parameter(('dis1.z1','dis2.z1'), low = -10.0, high=10.0)
        self.driver.add_parameter(('dis1.z2','dis2.z2'), low = 0.0,   high=10.0)
        
        #Local Design Variables and Coupling Variables
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
    
    # pylint: disable-msg=E1101
        
    prob.dis1.z1 = prob.dis2.z1 = 5.0
    prob.dis1.z2 = prob.dis2.z2 = 2.0
    prob.dis1.x1 = 1.0
    prob.dis2.y1 = 3.16
    
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

    
# End sellar_IDF.py