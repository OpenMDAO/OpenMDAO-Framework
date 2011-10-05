"""
    Solution of the sellar analytical problem using Collaborative Optimization.
"""


from openmdao.lib.datatypes.api import Float, Array
from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import CONMINdriver
from openmdao.lib.optproblems import sellar


class SellarCO(Assembly):
    """Solution of the sellar analytical problem using CO.
    """
    
    global_des_var_targets = Array([5.0,2.0])
    local_des_var_targets = Array([1.0,])
    coupling_var_targets = Array([3.16,0])

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
        self.add('dis1', sellar.Discipline1())

        # Local Optimization 2
        self.add('dis2', sellar.Discipline2())
        
        #Parameters - Global Optimization
        self.driver.add_objective('(local_des_var_targets[0])**2 + global_des_var_targets[1] + coupling_var_targets[0] + math.exp(-coupling_var_targets[1])')
        self.driver.add_parameter('global_des_var_targets[0]', low = -10.0, high = 10.0)
        self.driver.add_parameter('global_des_var_targets[1]', low = 0.0,   high = 10.0)
        
        self.driver.add_parameter('coupling_var_targets[0]', low = -1e99,  high = 1e99)
        self.driver.add_parameter('coupling_var_targets[1]', low = -1e99, high = 1e99)
        self.driver.add_parameter('local_des_var_targets[0]', low = 0.0,   high = 10.0)

        con1 = '(local_des_var_targets[0]-dis1.x1)**2+'+\
               '(global_des_var_targets[0]-dis1.z1)**2+'+\
               '(global_des_var_targets[1]-dis1.z2)**2+'+\
               '(coupling_var_targets[1]-dis1.y2)**2+'+\
               '(coupling_var_targets[0]-dis1.y1)**2<=.001'
        
        con2 = '(global_des_var_targets[0]-dis2.z1)**2 +'+\
               '(global_des_var_targets[1]-dis2.z2)**2 +'+\
               '(coupling_var_targets[0]-dis2.y1)**2 +'+\
               '(coupling_var_targets[1]-dis2.y2)**2 <= .001'
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
        """self.localopt1.add_objective('(global_des_var_targets[0]-dis1.z1)**2 + ' + \
                                   '(global_des_var_targets[1]-dis1.z2)**2 + ' + \
                                   '(local_des_var_targets[0]-dis1.x1)**2 + ' + \
                                   '(coupling_var_targets[0]-dis1.y1)**2 + ' + \
                                   '(coupling_var_targets[1]-dis1.y2)**2')"""
        
        self.localopt1.add_objective('(local_des_var_targets[0]-dis1.x1)**2+'
                                     '(global_des_var_targets[0]-dis1.z1)**2+'
                                     '(global_des_var_targets[1]-dis1.z2)**2+'
                                     '(coupling_var_targets[1]-dis1.y2)**2+'
                                     '(coupling_var_targets[0]-dis1.y1)**2')
        self.localopt1.add_parameter('dis1.x1', low = 0.0,   high = 10.0)        
        self.localopt1.add_parameter('dis1.z1', low = -10.0, high = 10.0)
        self.localopt1.add_parameter('dis1.z2', low = 0.0,   high = 10.0)
        self.localopt1.add_parameter('dis1.y2', low = -1e99, high = 1e99)
        self.localopt1.add_constraint('3.16 < dis1.y1')
        self.localopt1.iprint = 0
        self.localopt1.itmax = 100
        self.localopt1.fdch = .001
        self.localopt1.fdchm = .001
        self.localopt1.delfun = .0001
        self.localopt1.dabfun = .000001
        self.localopt1.force_execute = True
        
        #Parameters - Local Optimization 2
        self.localopt2.add_objective('(global_des_var_targets[0]-dis2.z1)**2 + ' + \
                                   '(global_des_var_targets[1]-dis2.z2)**2 + ' + \
                                   '(coupling_var_targets[0]-dis2.y1)**2 + ' + \
                                   '(coupling_var_targets[1]-dis2.y2)**2')
        self.localopt2.add_parameter('dis2.z1', low = -10.0, high = 10.0)
        self.localopt2.add_parameter('dis2.z2', low = 0.0,   high = 10.0)
        self.localopt2.add_parameter('dis2.y1', low = -1e99,  high = 1e99)
        self.localopt2.add_constraint('dis2.y2 < 24.0')
        self.localopt2.iprint = 0
        self.localopt2.itmax = 100
        self.localopt2.fdch = .003
        self.localopt2.fdchm = .003
        self.localopt2.delfun = .001
        self.localopt2.dabfun = .00001
        self.localopt2.force_execute = True


if __name__ == "__main__":        

    import time
    from openmdao.main.api import set_as_top
    
    prob = SellarCO()
    set_as_top(prob)
            
    prob.dis1.z1 = 5.0
    prob.dis2.z1 = 5.0

    prob.dis1.z2 = 2.0
    prob.dis2.z2 = 2.0

    prob.dis1.x1 = 1.0
    
    prob.dis1.y2 = 0.0
    prob.dis2.y1 = 3.16
    
    tt = time.time()
    prob.run()

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.global_des_var_targets[0], \
                                             prob.global_des_var_targets[1], \
                                             prob.dis1.x1)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"

    
# End sellar_CO.py
