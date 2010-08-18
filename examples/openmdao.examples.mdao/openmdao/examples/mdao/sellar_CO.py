"""
    Solution of the sellar analytical problem using Collaborative Optimization.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2
from openmdao.examples.mdao.broadcaster import Broadcaster

from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.api import CONMINdriver


class SellarCO(Assembly):
    """Solution of the sellar analytical problem using CO.
    """

    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
        
        # pylint: disable-msg=E1101
        super(SellarCO, self).__init__()
        
        # Global Optimization
        self.add('driver', CONMINdriver())
        self.add('coupler', Broadcaster())
        self.add('localopt1', CONMINdriver())
        self.add('localopt2', CONMINdriver())
        self.driver.workflow.add([self.coupler, self.localopt1, 
                                  self.localopt2])
        
        # Local Optimization 1
        self.add('dis1', SellarDiscipline1())
        self.localopt1.workflow.add(self.dis1)
        
        # Local Optimization 2
        self.add('dis2', SellarDiscipline2())
        self.localopt2.workflow.add(self.dis2)
        
        #Parameters - Global Optimization
        self.driver.objective = '(coupler.x1)**2 + coupler.z2 + coupler.y1' + \
                                                '+ math.exp(-coupler.y2)'
        for param,low,high in zip(['coupler.z1_in', 'coupler.z2_in', 'coupler.x1_in',
                                   'coupler.y1_in', 'coupler.y2_in'],
                                  [-10.0, 0.0, 0.0, 3.16, -10.0],
                                  [10.0, 10.0, 10.0, 10, 24.0]):
            self.driver.add_parameter(param, low=low, high=high)

        map(self.driver.add_constraint, [
            '(coupler.z1-dis1.z1)**2 + (coupler.z2-dis1.z2)**2 + (coupler.x1-dis1.x1)**2 + '
            '(coupler.y1-dis1.y1)**2 + (coupler.y2-dis1.y2)**2',
            '(coupler.z1-dis2.z1)**2 + (coupler.z2-dis2.z2)**2 + (coupler.y1-dis2.y1)**2 + '
            '(coupler.y2-dis2.y2)**2' ])
        
        self.driver.printvars = ['dis1.y1','dis2.y2']
        self.driver.iprint = 0
        self.driver.itmax = 100
        self.driver.fdch = .003
        self.driver.fdchm = .003
        self.driver.delfun = .0001
        self.driver.dabfun = .00001
        self.driver.ct = -.001
        self.driver.ctlmin = 0.001

        #Parameters - Local Optimization 1
        self.localopt1.objective = '(coupler.z1-dis1.z1)**2 + ' + \
                                   '(coupler.z2-dis1.z2)**2 + ' + \
                                   '(coupler.x1-dis1.x1)**2 + ' + \
                                   '(coupler.y1-dis1.y1)**2 + ' + \
                                   '(coupler.y2-dis1.y2)**2'
        for param, low, high in zip(['dis1.z1', 'dis1.z2', 'dis1.x1', 'dis1.y2'],
                                    [-10.0, 0.0, 0.0, -10.0],
                                    [10.0, 10.0, 10.0, 24.0]):
            self.localopt1.add_parameter(param, low=low, high=high)
        #self.localopt1.lower_bounds = [-10.0, 0.0, 0.0, -10.0]
        #self.localopt1.upper_bounds = [10.0, 10.0, 10.0, 24.0]
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
        for param, low, high in zip(['dis2.z1', 'dis2.z2', 'dis2.y1'],
                                    [-10.0, 0.0, 3.16],
                                    [10.0, 10.0, 10]):
            self.localopt2.add_parameter(param, low=low, high=high)
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
    
    # pylint: disable-msg=E1101
        
    prob.coupler.z1_in = 5.0
    prob.dis1.z1 = 5.0
    prob.dis2.z1 = 5.0

    prob.coupler.z2_in = 2.0
    prob.dis1.z2 = 2.0
    prob.dis2.z2 = 2.0

    prob.coupler.x1_in = 1.0
    prob.dis1.x1 = 1.0
    
    prob.coupler.y1_in = 3.16
    prob.coupler.y2_in = 0.0
    prob.dis1.y2 = 0.0
    prob.dis2.y1 = 3.16
    
    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.coupler.z1_in, \
                                             prob.coupler.z2_in, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.objective.evaluate()
    print "Elapsed time: ", time.time()-tt, "seconds"

    
# End sellar_CO.py