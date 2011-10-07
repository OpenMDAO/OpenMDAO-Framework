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
    
    prob = SellarProblem()
    prob.architecture = MDF()
    prob.configure()
    
    prob.driver.recorder = DBCaseRecorder()
    
    tt = time.time()
    prob.run() 
    
    error = prob.check_solution()
    
    print "\nUsing MDF Architecture"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)
    
    print "Minimum differs from expected by (%f, %f, %f)" % (error["z1"],
                                                             error["z2"],
                                                             error['dis1.x1'])
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", error['obj1']
    print "Elapsed time: ", time.time()-tt, "seconds"
    print "\n"
        
    prob = SellarProblem()
    prob.architecture = BLISS() 
    prob.configure()
    
    prob.driver.recorder = DBCaseRecorder()
    prob.driver.printvars = ['ssa.F[0]+ssa.dF[0][0]*(global_des_vars[0]-dis1.z1)+ssa.dF[0][1]*(global_des_vars[1]-dis1.z2)']
    
    tt = time.time()
    prob.run()
    
    error = prob.check_solution()

    print "\nUsing BLISS Architecture"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)

    print "Minimum differs from expected by (%f, %f, %f)" % (error["z1"],
                                                             error["z2"],
                                                             error['dis1.x1'])
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", error['obj1']

    print "Elapsed time: ", time.time()-tt, "seconds"
    print "\n"
        
    prob = SellarProblem()
    prob.architecture = CO() 
    
    tt = time.time()
    prob.run()
    error = prob.check_solution()
    
    print "\nUsing CO Architecture"
    print "CONMIN Iterations: ", prob.driver.iter_count 
    print "Minimum found at (%f, %f, %f)" % (prob.dis1.z1,
                                             prob.dis1.z2,
                                             prob.dis1.x1)
    print "Minimum differs from expected by (%f, %f, %f)" % (error["z1"],
                                                             error["z2"],
                                                             error['dis1.x1'])
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "Minimum objective: ", error['obj1']

    print "Elapsed time: ", time.time()-tt, "seconds"
    
    
    