"""
    Solution of the sellar analytical problem using Collaborative Optimization.
"""

# pylint: disable-msg=E0611,F0401
from openmdao.examples.mdao.disciplines import SellarDiscipline1, \
                                               SellarDiscipline2
from openmdao.lib.datatypes.api import Float
from openmdao.main.api import Assembly, set_as_top
from openmdao.lib.drivers.api import CONMINdriver


class SellarCO(Assembly):
    """Solution of the sellar analytical problem using CO.
    """
    
    z1 = Float(iotype="in")
    z2 = Float(iotype="in")
    x1 = Float(iotype="in")
    y1 = Float(iotype="in")
    y2 = Float(iotype="in")

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
        
        #Parameters - Global Optimization
        self.driver.add_objective('(x1)**2 + z2 + y1' + \
                                                '+ math.exp(-y2)')
        self.driver.add_parameter('z1', low = -10.0, high = 10.0)
        self.driver.add_parameter('z2', low = 0.0,   high = 10.0)
        self.driver.add_parameter('x1', low = 0.0,   high = 10.0)
        self.driver.add_parameter('y1', low = 3.16,  high = 10.0)
        self.driver.add_parameter('y2', low = -10.0, high = 24.0)

        con1 = '(z1-dis1.z1)**2 + (z2-dis1.z2)**2 + ' + \
               '(x1-dis1.x1)**2 + ' + \
               '(y1-dis1.y1)**2 + (y2-dis1.y2)**2 <= 0'
        
        con2 = '(z1-dis2.z1)**2 + (z2-dis2.z2)**2 + ' + \
               '(y1-dis2.y1)**2 + (y2-dis2.y2)**2 <= 0'
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
        self.localopt1.add_objective('(z1-dis1.z1)**2 + ' + \
                                   '(z2-dis1.z2)**2 + ' + \
                                   '(x1-dis1.x1)**2 + ' + \
                                   '(y1-dis1.y1)**2 + ' + \
                                   '(y2-dis1.y2)**2')
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
        self.localopt2.add_objective('(z1-dis2.z1)**2 + ' + \
                                   '(z2-dis2.z2)**2 + ' + \
                                   '(y1-dis2.y1)**2 + ' + \
                                   '(y2-dis2.y2)**2')
        self.localopt2.add_parameter('dis2.z1', low = -10.0, high = 10.0)
        self.localopt2.add_parameter('dis2.z2', low = 0.0,   high = 10.0)
        self.localopt2.add_parameter('dis2.y1', low = 3.16,  high = 10.0)
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
        
    prob.z1 = 5.0
    prob.dis1.z1 = 5.0
    prob.dis2.z1 = 5.0

    prob.z2 = 2.0
    prob.dis1.z2 = 2.0
    prob.dis2.z2 = 2.0

    prob.x1 = 1.0
    prob.dis1.x1 = 1.0
    
    prob.y1 = 3.16
    prob.y2 = 0.0
    prob.dis1.y2 = 0.0
    prob.dis2.y1 = 3.16
    
    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.z1, \
                                             prob.z2, \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"

    
# End sellar_CO.py
