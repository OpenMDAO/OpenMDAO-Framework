from openmdao.lib.architectures.api import MDF, BLISS, CO, BLISS2000
from openmdao.lib.casehandlers.api import DBCaseRecorder
    
from openmdao.lib.optproblems.api import UnitScalableProblem
#from openmdao.lib.optproblems.api import SellarProblem as UnitScalableProblem

        
from openmdao.main.api import SequentialWorkflow
        
if __name__ == "__main__": # pragma: no cover

    import time
    from openmdao.main.api import set_as_top
    
    prob = UnitScalableProblem()
    prob.architecture = BLISS2000()
    prob.configure()
    
    #prob.driver.iprint = 1
    
    """print [x.name for x in prob.driver.workflow]
    prob.driver.workflow = SequentialWorkflow()
    prob.driver.workflow.add(['local_opt_d2', 'local_opt_d0', 
                              'local_opt_d1', 'reset_train', 
                              'DOE_Trainer_d2', 'DOE_Trainer_d0', 
                              'DOE_Trainer_d1',])"""
    #exit()
    tt = time.time()
    prob.run() 
    
    
    
    error = prob.check_solution()
    
    print error