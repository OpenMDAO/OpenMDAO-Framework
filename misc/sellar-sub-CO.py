"""
Collaborative Optimization (CO) Architecture
"""

from disciplines import Discipline1, Discipline2a, Discipline2b, \
                        Discipline2c, Coupler

# pylint: disable-msg=E0611,F0401
from openmdao.lib.api import CONMINdriver
from openmdao.main.api import Assembly, set_as_top


class SellarCO(Assembly):
    """Solution of the sellar analytical problem using CO.
    
    Sellar, R. S., Batill, S. M., and Renaud, J. E., Response Surface Based, Concur-
    rent Subspace Optimization for Multidisciplinary System Design," Proceedings
    References 79 of the 34th AIAA Aerospace Sciences Meeting and Exhibit, Reno, NV,
    January 1996.
    """

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
        self.driver.add_to_workflow([self.coupler, self.localopt1, \
                                                   self.localopt2])
        
        # Local Optimization 1
        self.add('dis1', Discipline1(), False)
        self.localopt1.add_to_workflow(self.dis1)
        
        # Local Optimization 2
        self.add('dis2a', Discipline2a(), False)
        self.add('dis2b', Discipline2b(), False)
        self.add('dis2c', Discipline2c(), False)
        self.connect('dis2a.temp1','dis2b.temp1')
        self.connect('dis2b.temp2','dis2c.temp2')
        self.localopt2.add_to_workflow([self.dis2a, self.dis2b, self.dis2c])
        
        #Parameters - Global Optimization
        self.driver.objective = '(coupler.x1)**2 + coupler.z2 + coupler.y1' + \
                                                '+ math.exp(-coupler.y2)'
        self.driver.design_vars = ['coupler.z1_in',
                                   'coupler.z2_in',
                                   'coupler.x1_in',
                                   'coupler.y1_in',
                                   'coupler.y2_in']
        self.driver.constraints = ['(coupler.z1-dis1.z1)**2 + (coupler.z2-dis1.z2)**2 + (coupler.x1-dis1.x1)**2 + '
                                   '(coupler.y1-dis1.y1)**2 + (coupler.y2-dis1.y2)**2',
                                   
                                   '(coupler.z1-dis2b.z1)**2 + (coupler.z2-dis2c.z2)**2 + (coupler.y1-dis2a.y1)**2 + '
                                   '(coupler.y2-dis2c.y2)**2' ]
        
        self.driver.lower_bounds = [-10.0, 0.0, 0.0, 3.16, -10.0]
        self.driver.upper_bounds = [10.0, 10.0, 10.0, 10, 24.0]
        self.driver.printvars = ['dis1.y1','dis2c.y2']
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
        self.localopt2.objective = '(coupler.z1-dis2b.z1)**2 + ' + \
                                   '(coupler.z2-dis2c.z2)**2 + ' + \
                                   '(coupler.y1-dis2a.y1)**2 + ' + \
                                   '(coupler.y2-dis2c.y2)**2'
        self.localopt2.design_vars = ['dis2b.z1',
                                      'dis2c.z2',
                                      'dis2a.y1']
        self.localopt2.lower_bounds = [-10.0, 0.0, 3.16]
        self.localopt2.upper_bounds = [10.0, 10.0, 10]
        self.localopt2.iprint = 0
        self.localopt2.itmax = 100
        self.localopt2.fdch = .003
        self.localopt2.fdchm = .003
        self.localopt2.delfun = .001
        self.localopt2.dabfun = .00001


        
if __name__ == "__main__": # pragma: no cover         

    import sys
    import time
    #import matplotlib
    
    #if sys.platform == 'darwin':
        #matplotlib.use('MacOSX')
    #else:
        #matplotlib.use('TkAgg')
    
    #import matplotlib.pyplot as plt
    
    from openmdao.lib.api import DBCaseRecorder, DBCaseIterator
    
    prob = SellarCO()
    set_as_top(prob)
    
    # Set up initial conditions
    
    prob.coupler.z1_in = 5.0
    prob.dis1.z1 = 5.0
    prob.dis2b.z1 = 5.0
    
    prob.coupler.z2_in = 2.0
    prob.dis1.z2 = 2.0
    prob.dis2c.z2 = 2.0
    
    prob.coupler.x1_in = 1.0
    prob.dis1.x1 = 1.0
    
    prob.dis1.y2 = 0.0
    prob.dis2a.y1 = 0.0
    
    # Plug a database recorder into the global optimization driver.
    prob.driver.recorder = DBCaseRecorder('data.db')
    
    # Run the model
    tt = time.time()
    prob.run()

    # Extract and plot some data from the optimization
    
    db_case_iter = DBCaseIterator('data.db')
    
    db_case_iter.var_selector = "name = 'objective'"
    x_iter = []
    y_obj = []
    for i, case in enumerate(db_case_iter):
        x_iter.append(i)
        y_obj.append(case.outputs[0][2])

    db_case_iter.var_selector = "name = 'Constraint1'"
    y_constraint1 = []
    for i, case in enumerate(db_case_iter):
        y_constraint1.append(case.outputs[0][2])
        
    db_case_iter.var_selector = "name = 'coupler.y1_in'"
    y_y1 = []
    for i, case in enumerate(db_case_iter):
        y_y1.append(case.inputs[0][2])
        
    db_case_iter.var_selector = "name = 'coupler.y2_in'"
    y_y2 = []
    for i, case in enumerate(db_case_iter):
        y_y2.append(case.inputs[0][2])
        
    db_case_iter.var_selector = "name = 'coupler.z1_in'"
    y_z1 = []
    for i, case in enumerate(db_case_iter):
        y_z1.append(case.inputs[0][2])
        
    db_case_iter.var_selector = "name = 'coupler.z2_in'"
    y_z2 = []
    for i, case in enumerate(db_case_iter):
        y_z2.append(case.inputs[0][2])
        
    db_case_iter.var_selector = "name = 'coupler.x1_in'"
    y_x1 = []
    for i, case in enumerate(db_case_iter):
        y_x1.append(case.inputs[0][2])
        
    db_case_iter.var_selector = "name = 'dis1.y1'"
    y_couple1 = []
    for i, case in enumerate(db_case_iter):
        y_couple1.append(case.inputs[0][2])

    print "\n"
    print "CONMIN Iterations: ", prob.driver.iter_count
    print "Minimum found at (%f, %f, %f)" % (prob.coupler.z1_in, \
                                             prob.coupler.z2_in, \
                                             prob.coupler.x1_in)
    print "Couping vars: %f, %f" % (prob.coupler.y1_in, prob.coupler.y2_in)
    print "Minimum objective: ", prob.driver.objective.evaluate()
    print "Elapsed time: ", time.time()-tt, "seconds"
    
    #fig = plt.figure()
    #ax = fig.add_subplot(111)
    #plt.plot(x_iter,y_obj)
    #plt.grid(True)
    #ax.set_xlabel('Iteration number')
    #ax.set_ylabel('Objective Value')

    #fig = plt.figure()
    #ax = fig.add_subplot(111)
    #line1 = plt.plot(x_iter,y_z1)
    #line2 = plt.plot(x_iter,y_z2)
    #line3 = plt.plot(x_iter,y_x1)
    #line4 = plt.plot(x_iter,y_y1)
    #line5 = plt.plot(x_iter,y_y2)
    #plt.grid(True)
    #ax.legend((line1, line2, line3, line4, line5), ('z1','z2','x1','y1','y2'))
    #ax.set_xlabel('Iteration number')
    #ax.set_ylabel('Top Level Design Variables')
    
    #fig = plt.figure()
    #ax = fig.add_subplot(111)
    #plt.plot(x_iter,y_y1)
    #plt.plot(x_iter,y_couple1)
    #plt.grid(True)
    #ax.set_xlabel('Iteration number')
    #ax.set_ylabel('Coupling Variables')
    
    #fig = plt.figure()
    #ax = fig.add_subplot(111)
    #plt.semilogy(x_iter,y_constraint1)
    #plt.grid(True)
    #ax.set_xlabel('Iteration number')
    #ax.set_ylabel('Constraint 1')
    #plt.show()    
    
# End sellar-CO.py