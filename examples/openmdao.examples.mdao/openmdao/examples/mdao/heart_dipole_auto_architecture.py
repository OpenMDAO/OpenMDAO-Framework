from openmdao.lib.architectures.api import MDF, BLISS, CO
from openmdao.lib.casehandlers.api import DBCaseRecorder
    
from openmdao.lib.optproblems.api import HeartDipoleProblem    
        
        
if __name__ == "__main__": # pragma: no cover

    import time
    from openmdao.main.api import set_as_top
    
    prob = HeartDipoleProblem()
    prob.architecture = MDF()
    prob.configure()
    
    #prob.driver.iprint = 1
    
    prob.driver.recorder = DBCaseRecorder()
    
    tt = time.time()
    prob.run() 
    
    error = prob.check_solution()
    
    print "\nUsing MDF Architecture"
    print "iterations: "
    
    print prob.d1.x1,prob.d1.x2,prob.d2.x3,prob.d1.x4, \
          prob.d2.x5,prob.d1.x6,prob.d1.x7,prob.d1.x8
    
    exit()
    print "Minimum found at (%f, %f, %f, %f)" % (prob.d1.x1,
                                             prob.d1.x4,
                                             prob.d1.x6,
                                             prob.d1.x7)
    
    #print "Minimum differs from expected by (%f, %f, %f)" % (error["z1"],
    #                                                         error["z2"],
    #                                                         error['dis1.x1'])
    print "Couping vars: %f, %f, %f, %f" % (prob.d1.x2, 
                                            prob.d1.x8, 
                                            prob.d2.x3, 
                                            prob.d2.x5)
    #print "Minimum objective: ", prob.solution['obj1']
    #print "Elapsed time: ", time.time()-tt, "seconds"
    print "\n"
        
    
    
    