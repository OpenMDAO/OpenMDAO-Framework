from openmdao.lib.architectures.api import MDF, BLISS, CO, BLISS2000
from openmdao.lib.casehandlers.api import DBCaseRecorder
    
from openmdao.lib.optproblems.api import UnitScalableProblem
#from openmdao.lib.optproblems.api import SellarProblem as UnitScalableProblem

        
        
if __name__ == "__main__": # pragma: no cover

    import time
    from openmdao.main.api import set_as_top
    
    prob = UnitScalableProblem()
    prob.architecture = BLISS2000()
    prob.configure()
    
    #prob.driver.iprint = 1
    
    tt = time.time()
    prob.run() 
    
    error = prob.check_solution()
    
    print error