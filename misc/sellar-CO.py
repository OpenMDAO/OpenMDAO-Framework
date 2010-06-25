"""
Collaborative Optimization (CO) Architecture
"""

from disciplines import Discipline1, Discipline2, Coupler

# pylint: disable-msg=E0611,F0401
from openmdao.lib.api import CONMINdriver
from openmdao.main.api import Assembly, Dataflow, set_as_top


class SellarCO(Assembly):
    """Solution of the sellar analytical problem using CO."""

    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
        
        # pylint: disable-msg=E1101
        
        super(SellarCO, self).__init__()

        # Global Optimization
        self.add('driver', CONMINdriver(), False)
        self.add('coupler', Coupler(), False)
        self.add('localopt1', CONMINdriver(), False)
        self.add('localopt2', CONMINdriver(), False)
        self.driver.add_to_workflow([self.coupler, self.localopt1, self.localopt2])
        
        # Local Optimization 1
        self.add('dis1', Discipline1(), False)
        self.localopt1.add_to_workflow(self.dis1)
        
        # Local Optimization 2
        self.add('dis2', Discipline2(), False)
        self.localopt2.add_to_workflow(self.dis2)
        
        #Parameters - Global Optimization
        self.driver.objective = '(coupler.x1)**2 + coupler.z2 + coupler.y1 + math.exp(-coupler.y2)'
        self.driver.design_vars = ['coupler.z1_in',
                                   'coupler.z2_in',
                                   'coupler.x1_in',
                                   'coupler.y1_in',
                                   'coupler.y2_in']
        self.driver.constraints = ['(coupler.z1-dis1.z1)**2 + ' + \
                                  '(coupler.z2-dis1.z2)**2 + ' + \
                                  '(coupler.x1-dis1.x1)**2 + ' + \
                                  '(coupler.y1-dis1.y1)**2 + ' + \
                                  '(coupler.y2-dis1.y2)**2',
                                  '(coupler.z1-dis2.z1)**2 + ' + \
                                  '(coupler.z2-dis2.z2)**2 + ' + \
                                  '(coupler.y1-dis2.y1)**2 + ' + \
                                  '(coupler.y2-dis2.y2)**2' ]
        self.driver.lower_bounds = [-10.0, 0.0, 0.0, 3.16, -10.0]
        self.driver.upper_bounds = [10.0, 10.0, 10.0, 10, 24.0]
        self.driver.iprint = 1
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .001
        self.driver.dabfun = .0001
        self.driver.ct = -.001
        self.driver.ctlmin = 0.001

        #Parameters - Local Optimization 1
        self.localopt1.objective = '(coupler.z1-dis1.z1)**2 + ' + \
                                   '(coupler.z2-dis1.z2)**2 + ' + \
                                   '(coupler.x1-dis1.x1)**2 + ' + \
                                   '(coupler.y1-dis1.y1)**2 + ' + \
                                   '(coupler.y2-dis1.y2)**2'
        self.localopt1.design_vars = ['dis1.z1',
                                      'dis1.z2',
                                      'dis1.x1',
                                      'dis1.y2']
        self.localopt1.lower_bounds = [-10.0, 0.0, 0.0, -10.0]
        self.localopt1.upper_bounds = [10.0, 10.0, 10.0, 24.0]
        self.localopt1.iprint = 0
        self.localopt1.itmax = 100
        self.localopt1.fdch = .003
        self.localopt1.fdchm = .003
        self.localopt1.delfun = .001
        self.localopt1.dabfun = .00001
        
        #Parameters - Local Optimization 2
        self.localopt2.objective = '(coupler.z1-dis2.z1)**2 + ' + \
                                   '(coupler.z2-dis2.z2)**2 + ' + \
                                   '(coupler.y1-dis2.y1)**2 + ' + \
                                   '(coupler.y2-dis2.y2)**2'
        self.localopt2.design_vars = ['dis2.z1',
                                      'dis2.z2',
                                      'dis2.y1']
        self.localopt2.lower_bounds = [-10.0, 0.0, 3.16]
        self.localopt2.upper_bounds = [10.0, 10.0, 10]
        self.localopt2.iprint = 0
        self.localopt2.itmax = 100
        self.localopt2.fdch = .003
        self.localopt2.fdchm = .003
        self.localopt2.delfun = .001
        self.localopt2.dabfun = .00001


        
if __name__ == "__main__": # pragma: no cover         

    import time
    
    prob = SellarCO()
    set_as_top(prob)
    
    prob.coupler.z1_in = 5.0
    prob.dis1.z1 = 5.0
    prob.dis2.z1 = 5.0
    
    prob.coupler.z2_in = 2.0
    prob.dis1.z2 = 2.0
    prob.dis2.z2 = 2.0
    
    prob.coupler.x1_in = 1.0
    prob.dis1.x1 = 1.0
    
    prob.dis1.y2 = 0
    prob.dis2.y1 = 0
    
    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.coupler.z1_in, \
                                             prob.coupler.z2_in, \
                                             prob.coupler.x1_in)
    print "Couping vars: %f, %f" % (prob.coupler.y1_in, prob.coupler.y2_in)
    print "Minimum objective: ", prob.driver.objective.evaluate()
    print "Elapsed time: ", time.time()-tt, "seconds"

    
# End sellar-CO.py