"""
    Solution of the Sellar analytical problem using BLISS2000.
    
    -MDA solved with a Broyden solver.
    
    -The metamodels for the two disciplines are constructed using response surfaces.
    
    -Both the discipline and system level optimizations are performed using CONMIN
"""


from openmdao.lib.optproblems.sellar import Discipline1 as SellarDiscipline1
from openmdao.lib.optproblems.sellar import Discipline2 as SellarDiscipline2

from openmdao.main.api import Assembly,Component, Driver
from openmdao.lib.datatypes.api import Float, Array, Slot
from openmdao.lib.drivers.api import SLSQPdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator, IterateUntil
from openmdao.lib.drivers.api import NeighborhoodDOEdriver                                     
from openmdao.lib.doegenerators.api import FullFactorial, Uniform, CentralComposite
from openmdao.lib.components.api import MetaModel
from openmdao.lib.casehandlers.api import DBCaseRecorder
from openmdao.lib.surrogatemodels.api import ResponseSurface


class LocalOpt(Assembly): 

    def configure(self): 

        self.add('driver', SLSQPdriver())

        self.add('')


class SellarBLISS2000(Assembly):
    """ Optimization of the Sellar problem using the BLISS2000 algorithm
    Disciplines coupled with FixedPointIterator.
    """
    
    #for a given iteration, x1_store holds the value of dis1.x1 obtained in the previous iteration. used to track convergence 
    x1_store = Float(0.0, iotype="in")
    z1_store = Float(0.0, iotype="in")
    z2_store = Float(0.0, iotype="in")
                     
    
    def configure(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
                        
        #objective = '(dis1.x1)**2 + dis1.z2 + dis1.y1 + exp(-dis2.y2)'
        #constraint1 = 'dis1.y1 > 3.16'
        #constraint2 = 'dis2.y2 < 24.0'
        
        # Metamodel for sellar discipline 1


        self.add("meta_model_dis1",MetaModel())
        self.meta_model_dis1.surrogates = {"y1":ResponseSurface()}
        self.meta_model_dis1.model = SellarDiscipline1()
        self.meta_model_dis1.recorder = DBCaseRecorder()
        self.meta_model_dis1.force_execute = True
        
        # Metamodel for sellar discipline 2
        self.add("meta_model_dis2",MetaModel())
        self.meta_model_dis2.surrogates = {"y2":ResponseSurface()}
        self.meta_model_dis2.model = SellarDiscipline2()
        self.meta_model_dis2.recorder = DBCaseRecorder()
        self.meta_model_dis2.force_execute = True
           
        
        #training metalmodel for disc1
        
        # self.add("DOE_Trainer_dis2",NeighborhoodDOEdriver())
        # self.DOE_Trainer_dis2.DOEgenerator = CentralComposite()
        # self.DOE_Trainer_dis2.alpha = .1
        # self.DOE_Trainer_dis2.add_parameter("meta_model_dis2.z1",low=-10,high=10,start=5.0)        
        # self.DOE_Trainer_dis2.add_parameter("meta_model_dis2.z2",low=0,high=10,start=2.0)   
        # self.DOE_Trainer_dis2.add_parameter("meta_model_dis2.y1",low=0,high=20)   
        # self.DOE_Trainer_dis2.add_event("meta_model_dis2.train_next")
        # self.DOE_Trainer_dis2.force_execute = True               
        
        
        #optimization of global objective function

        self.add('sysopt', SLSQPdriver())     
         
        self.sysopt.add_objective('(meta_model_dis1.x1)**2 + meta_model_dis1.z2 + meta_model_dis1.y1 + math.exp(-meta_model_dis2.y2)')
        
        
        self.sysopt.add_parameter(['meta_model_dis1.z1','meta_model_dis2.z1'], low=-10, high=10.0,start=5.0)
        self.sysopt.add_parameter(['meta_model_dis1.z2','meta_model_dis2.z2'], low=0, high=10.0,start=2.0)        
        self.sysopt.add_parameter('meta_model_dis1.y2', low=-1e99, high=1e99)
        
        self.sysopt.add_parameter('meta_model_dis2.y1', low=-1e99, high=1e99)
        
        #feasibility constraints
        self.sysopt.add_constraint('meta_model_dis1.y2 <= meta_model_dis2.y2')
        self.sysopt.add_constraint('meta_model_dis1.y2 >= meta_model_dis2.y2')
        
        self.sysopt.add_constraint('meta_model_dis2.y1 <= meta_model_dis1.y1')
        self.sysopt.add_constraint('meta_model_dis2.y1 >= meta_model_dis1.y1')
        
        
        self.sysopt.add_constraint('3.16 < meta_model_dis1.y1')
        self.sysopt.add_constraint('meta_model_dis2.y2 < 24.0')
        self.sysopt.force_execute=True
        
        
        #optimization of discipline 1 (discipline 2 of the sellar problem has no local variables)
        
        self.add('local_opt_dis1', SLSQPdriver())
        self.local_opt_dis1.add_objective('meta_model_dis1.y1')
        self.local_opt_dis1.add_parameter('meta_model_dis1.x1', low=0, high=10.0) 
        self.local_opt_dis1.add_constraint('3.16 < meta_model_dis1.y1')
        self.local_opt_dis1.add_event('meta_model_dis1.train_next')
        self.local_opt_dis1.force_execute=True
        
        self.local_opt_dis1.workflow.add(['meta_model_dis1'])

        #training metalmodel for disc1
        
        self.add("DOE_Trainer_dis1",NeighborhoodDOEdriver())
        self.DOE_Trainer_dis1.DOEgenerator = CentralComposite()
        self.DOE_Trainer_dis1.alpha = .1
        self.DOE_Trainer_dis1.add_parameter("meta_model_dis1.z1",low=-10,high=10,start=5.0)        
        self.DOE_Trainer_dis1.add_parameter("meta_model_dis1.z2",low=0,high=10,start=2.0)   
        self.DOE_Trainer_dis1.add_parameter("meta_model_dis1.y2",low=-100,high=100)   
        self.DOE_Trainer_dis1.add_event("meta_model_dis1.train_next")
        self.DOE_Trainer_dis1.force_execute = True 
        self.DOE_Trainer_dis1.workflow.add("local_opt_dis1")     
        
        self.add('reset_train',Driver())
        self.reset_train.add_event('meta_model_dis1.reset_training_data')
        self.reset_train.add_event('meta_model_dis2.reset_training_data')
        self.reset_train.workflow.add(['meta_model_dis1','meta_model_dis2'])
        self.reset_train.force_execute = True
        
        #build workflow for bliss2000
        
        self.add('driver', FixedPointIterator())
        #self.add('main_driver', IterateUntil())
        #self.main_driver.max_iterations = 1
        self.driver.tolerance = .0001  
        # self.driver.workflow.add(['local_opt_dis1','reset_train','DOE_Trainer_dis1','DOE_Trainer_dis2','sysopt'])  
        self.driver.workflow.add(['sysopt'])  
        self.driver.add_parameter('x1_store', low=0, high=10.0)
        self.driver.add_constraint('meta_model_dis1.x1 = x1_store')
        self.driver.add_parameter('z1_store', low=0, high=10.0)
        self.driver.add_constraint('meta_model_dis1.z1 = z1_store')
        self.driver.add_parameter('z2_store', low=0, high=10.0)
        self.driver.add_constraint('meta_model_dis1.z2 = z2_store')
        #self.main_driver.add_event('meta_model_dis1.reset_training_data')
        #self.main_driver.add_event('meta_model_dis2.reset_training_data')
        

        
if __name__ == "__main__":  

    import time
    import math
    
    prob = SellarBLISS2000()
    
    prob.check_config()
   
    
    config=[5,2,1]
    
    prob.meta_model_dis1.z1 = prob.meta_model_dis2.z1= config[0]
    prob.meta_model_dis1.z2 = prob.meta_model_dis2.z2= config[1]
    prob.meta_model_dis1.x1 = config[2]
    
    prob.meta_model_dis1.y2 = 0
    prob.meta_model_dis2.y1 = 3.16
    

    # print [n.name for n in prob.driver.workflow]
    # exit()
    prob.run()
    
    
    
    
    print
    print "Minimum found at", prob.meta_model_dis1.z1,prob.z2_store,prob.meta_model_dis1.x1
    print "Coupling Vars: %f, %f"%(prob.meta_model_dis2.y1,prob.meta_model_dis1.y2)
    print "with objective function value:",(prob.meta_model_dis1.x1)**2 + \
          prob.meta_model_dis1.z2 + prob.meta_model_dis1.y1 + math.exp(-prob.meta_model_dis2
                                                                       .y2)        
    