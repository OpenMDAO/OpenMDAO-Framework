"""
    Solution of the sellar analytical problem using MDF.
    Problem forumulation is specified, and MDF is automatically
    set up for you. 
"""

from openmdao.lib.architectures.api import MDF, BLISS, CO
from openmdao.lib.casehandlers.api import DBCaseRecorder
    
from openmdao.lib.optproblems.api import SellarProblem    
        
        
if __name__ == "__main__": # pragma: no cover

    import time
    from openmdao.main.api import set_as_top
    
    import matplotlib
    font = {'family' : 'normal',
        'weight' : 'bold',
        'size'   : 15}

    matplotlib.rc('font', **font)
    import pylab as p
    
    solution = (1.9776, 0, 0)
    
    prob = SellarProblem()
    set_as_top(prob)
    prob.architecture = MDF()
    prob.configure()
    
    prob.driver.recorder = DBCaseRecorder()
    
        
    tt = time.time()
    prob.run()
    
    p.figure()
    mdf_objective = [(i,case['objective']) for i,case in enumerate(prob.driver.recorder.get_iterator())]
    mdf_objective = zip(*mdf_objective)    
       
       

    print "\nUsing MDF Architecture"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)
    print "Minimum differs from expected by (%f, %f, %f)" % (prob.dis1.z1-solution[0],
                                                             prob.dis1.z2-solution[1],
                                                             prob.dis1.x1-solution[2])
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", prob.driver.eval_objective()
    print "Elapsed time: ", time.time()-tt, "seconds"
    print "\n"
    
    
    prob = SellarProblem()
    set_as_top(prob)
    prob.architecture = BLISS() 
    prob.configure()
    
    prob.driver.recorder = DBCaseRecorder()
    prob.driver.printvars = ['ssa.F[0]+ssa.dF[0][0]*(global_des_vars[0]-dis1.z1)+ssa.dF[0][1]*(global_des_vars[1]-dis1.z2)']
    
    tt = time.time()
    prob.run()

    bliss_objective = [(i,case['ssa.F[0]+ssa.dF[0][0]*(global_des_vars[0]-dis1.z1)+ssa.dF[0][1]*(global_des_vars[1]-dis1.z2)']) for i,case in enumerate(prob.driver.recorder.get_iterator())]
    bliss_objective = zip(*bliss_objective)    


    print "\nUsing BLISS Architecture"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)
    print "Minimum differs from expected by (%f, %f, %f)" % (prob.dis1.z1-solution[0],
                                                             prob.dis1.z2-solution[1],
                                                             prob.dis1.x1-solution[2])
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Elapsed time: ", time.time()-tt, "seconds"
    print "\n"
    
    
    p.plot(mdf_objective[0],mdf_objective[1],label="MDF",c='r') 
    p.plot(bliss_objective[0],bliss_objective[1],label="BLISS",c='b') 
    p.xlabel("top level iteration #")
    p.ylabel("Objective Value")
    p.title("Optimization Convergence")
    p.axis([0,10,3,4])
    p.legend()
    
    p.figure()
    p.plot(mdf_objective[0],mdf_objective[1],label="MDF",c='r') 
    p.plot(bliss_objective[0],bliss_objective[1],label="BLISS",c='b') 
    p.xlabel("top level iteration #")
    p.ylabel("Objective Value")
    p.title("Optimization Convergence")
    p.legend()
    
    
       
    
    prob = SellarProblem()
    set_as_top(prob)
    prob.architecture = CO() 
    
    tt = time.time()
    prob.run()

    print "\nUsing CO Architecture"
    print "CONMIN Iterations: ", prob.driver.iter_count 
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)
    print "Minimum differs from expected by (%f, %f, %f)" % (prob.dis1.z1-solution[0],
                                                             prob.dis1.z2-solution[1],
                                                             prob.dis1.x1-solution[2])
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Elapsed time: ", time.time()-tt, "seconds"
    
    
    