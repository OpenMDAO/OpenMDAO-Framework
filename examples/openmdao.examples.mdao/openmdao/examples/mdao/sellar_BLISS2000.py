"""
    Solution of the Sellar analytical problem using BLISS2000.
    
    -MDA solved with a Broyden solver.
    
    -The metamodels for the two disciplines are constructed using response surfaces.
    
    -Both the discipline and system level optimizations are performed using CONMIN
"""


from openmdao.lib.optproblems.sellar import Discipline1 as SellarDiscipline1
from openmdao.lib.optproblems.sellar import Discipline2 as SellarDiscipline2

from openmdao.main.api import Assembly,Component,SequentialWorkflow, Driver
from openmdao.lib.datatypes.api import Float, Array, Slot
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator, IterateUntil
from openmdao.lib.drivers.api import DOEdriver                                     
from openmdao.lib.doegenerators.api import FullFactorial, Uniform, CentralComposite
from openmdao.lib.components.api import MetaModel
from openmdao.lib.casehandlers.api import DBCaseRecorder
from openmdao.lib.surrogatemodels.api import ResponseSurface


class SellarBLISS2000(Assembly):
    """ Optimization of the Sellar problem using the BLISS2000 algorithm
    Disciplines coupled with FixedPointIterator.
    """
    
    #for a given iteration, x1_store holds the value of dis1.x1 obtained in the previous iteration. used to track convergence 
    x1_store = Float(0.0)
    z1_store = Float(0.0)
    z2_store = Float(0.0)
                     
    
    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
                
        super(SellarBLISS2000, self).__init__()
        
        #self.add('dis1', SellarDiscipline1())
        #self.add('dis2', SellarDiscipline2())
        
        #objective = '(dis1.x1)**2 + dis1.z2 + dis1.y1 + exp(-dis2.y2)'
        #constraint1 = 'dis1.y1 > 3.16'
        #constraint2 = 'dis2.y2 < 24.0'
        
        # Metamodel for sellar discipline 1
        
        self.add("meta_model_dis1",MetaModel())
        self.meta_model_dis1.surrogate = {"y1":ResponseSurface()}
        self.meta_model_dis1.model = SellarDiscipline1()
        self.meta_model_dis1.recorder = DBCaseRecorder()
        
        # Metamodel for sellar discipline 2
        self.add("meta_model_dis2",MetaModel())
        self.meta_model_dis2.surrogate = {"y2":ResponseSurface()}
        self.meta_model_dis2.model = SellarDiscipline2()
        self.meta_model_dis2.recorder = DBCaseRecorder()
        
        # start with an initial mda
        self.add('mda', BroydenSolver())
        self.mda.add_parameter('meta_model_dis1.y2', low=0, high=20,start=1.)
        self.mda.add_constraint('meta_model_dis2.y2 = meta_model_dis1.y2')
        self.mda.add_parameter(['meta_model_dis2.y1'], low=0, high=20,start=1.)
        self.mda.add_constraint('meta_model_dis2.y1 = meta_model_dis1.y1')
        self.mda.add_event('meta_model_dis1.train_next')
        self.mda.add_event('meta_model_dis2.train_next')
        self.mda.force_execute = True  
        
        #training metalmodel for disc1
        
        self.add("DOE_Trainer_dis1",DOEdriver())
        self.DOE_Trainer_dis1.DOEgenerator = CentralComposite()
        self.DOE_Trainer_dis1.alpha = .1
        self.DOE_Trainer_dis1.add_parameter("meta_model_dis1.z1",low=-10,high=10,start=5.0)        
        self.DOE_Trainer_dis1.add_parameter("meta_model_dis1.z2",low=0,high=10,start=2.0)   
        self.DOE_Trainer_dis1.add_parameter("meta_model_dis1.y2",low=0,high=20)   
        self.DOE_Trainer_dis1.add_event("meta_model_dis1.train_next")
        self.DOE_Trainer_dis1.force_execute = True        
        
        #training metalmodel for disc1
        
        self.add("DOE_Trainer_dis2",DOEdriver())
        self.DOE_Trainer_dis2.DOEgenerator = CentralComposite()
        self.DOE_Trainer_dis2.alpha = .1
        self.DOE_Trainer_dis2.add_parameter("meta_model_dis2.z1",low=-10,high=10,start=5.0)        
        self.DOE_Trainer_dis2.add_parameter("meta_model_dis2.z2",low=0,high=10,start=2.0)   
        self.DOE_Trainer_dis2.add_parameter("meta_model_dis2.y1",low=0,high=20)   
        self.DOE_Trainer_dis2.add_event("meta_model_dis2.train_next")
        self.DOE_Trainer_dis2.force_execute = True               
        
        
        #optimization of global objective function

        self.add('sysopt', CONMINdriver())     
         
        self.sysopt.add_objective('(x1_store)**2 + meta_model_dis1.z2 + meta_model_dis1.y1 + math.exp(-meta_model_dis2.y2)')
        
        
        self.sysopt.add_parameter(['meta_model_dis1.z1','meta_model_dis2.z1'], low=-10, high=10.0,start=5.0)
        self.sysopt.add_parameter(['meta_model_dis1.z2','meta_model_dis2.z2'], low=0, high=10.0,start=2.0)        
        self.sysopt.add_parameter('meta_model_dis1.y2', low=0, high=20.0)
        
        self.sysopt.add_parameter('meta_model_dis2.y1', low=0, high=20.0)
        
        #feasibility constraints
        self.sysopt.add_constraint('meta_model_dis1.y2 <= meta_model_dis2.y2')
        self.sysopt.add_constraint('meta_model_dis1.y2 >= meta_model_dis2.y2')
        
        self.sysopt.add_constraint('meta_model_dis2.y1 <= meta_model_dis1.y1')
        self.sysopt.add_constraint('meta_model_dis2.y1 >= meta_model_dis1.y1')
        
        
        self.sysopt.add_constraint('3.16 < meta_model_dis1.y1')
        self.sysopt.add_constraint('meta_model_dis2.y2 < 24.0')
        self.sysopt.force_execute=True
        
        
        #optimization of discipline 1 (discipline 2 of the sellar problem has no local variables)
        
        self.add('local_opt_dis1', CONMINdriver())
        self.local_opt_dis1.add_objective('meta_model_dis1.y1')
        self.local_opt_dis1.add_parameter('meta_model_dis1.x1', low=0, high=10.0) 
        self.local_opt_dis1.add_constraint('3.16 < meta_model_dis1.y1')
        self.local_opt_dis1.add_event('meta_model_dis1.train_next')
        self.local_opt_dis1.force_execute=True
        
        self.local_opt_dis1.workflow=SequentialWorkflow()
        self.local_opt_dis1.workflow.add(['meta_model_dis1'])
        
        """
        self.add('local_opt_dis1', CONMINdriver())
        self.local_opt_dis1.add_objective('dis1.y1')
        self.local_opt_dis1.add_parameter(['dis1.x1','meta_model_dis1.x1'], low=0, high=10.0) 
        self.local_opt_dis1.add_constraint('3.16 < dis1.y1')
        
        self.local_opt_dis1.force_execute=True
        
        self.local_opt_dis1.workflow=SequentialWorkflow()
        self.local_opt_dis1.workflow.add(['dis1'])
        """
        
        self.add('reset_train',Driver())
        self.reset_train.add_event('meta_model_dis1.reset_training_data')
        self.reset_train.add_event('meta_model_dis2.reset_training_data')
        #self.reset_train.workflow = SequentialWorkflow()
        #self.reset_train.workflow.add(['meta_model_dis1','meta_model_dis2'])
        self.reset_train.force_execute = True
        
        
        
        #build workflow for bliss2000
        
        self.add('main_driver', FixedPointIterator())
        #self.add('main_driver', IterateUntil())
        #self.main_driver.max_iterations = 1
        self.main_driver.tolerance = .0001  
        self.main_driver.workflow = SequentialWorkflow()
        self.main_driver.workflow.add(['local_opt_dis1','reset_train','DOE_Trainer_dis1','DOE_Trainer_dis2','sysopt'])  
        self.main_driver.add_parameter('x1_store', low=0, high=10.0)
        self.main_driver.add_constraint('meta_model_dis1.x1 = x1_store')
        self.main_driver.add_parameter('z1_store', low=0, high=10.0)
        self.main_driver.add_constraint('meta_model_dis1.z1 = z1_store')
        self.main_driver.add_parameter('z2_store', low=0, high=10.0)
        self.main_driver.add_constraint('meta_model_dis1.z2 = z2_store')
        #self.main_driver.add_event('meta_model_dis1.reset_training_data')
        #self.main_driver.add_event('meta_model_dis2.reset_training_data')
        
        
        # Top level is sequential work flow. runs a single mda, then begins bliss2000 
        self.driver.workflow=SequentialWorkflow()
        self.driver.workflow.add(['main_driver'])
        #self.driver.workflow.add(['main_driver']) #note: this should just be driver then
        

        
if __name__ == "__main__":  

    import time
    import math
    from openmdao.main.api import set_as_top
    
    prob = SellarBLISS2000()
    prob.name = "top"  
    
    set_as_top(prob)
   
    
    config=[5,2,1]
    
    prob.meta_model_dis1.z1 = prob.meta_model_dis2.z1= config[0]
    prob.meta_model_dis1.z2 = prob.meta_model_dis2.z2= config[1]
    prob.meta_model_dis1.x1 = config[2]
    
    prob.meta_model_dis1.y2 = 0
    prob.meta_model_dis2.y1 = 3.16
    
    
    prob.run()
    print
    print "Minimum found at", prob.meta_model_dis1.z1,prob.meta_model_dis1.z2,prob.meta_model_dis1.x1
    print "with objective function value:",(prob.meta_model_dis1.x1)**2 + \
          prob.meta_model_dis1.z2 + prob.meta_model_dis1.y1 + math.exp(-prob.meta_model_dis2
                                                                       .y2)
    exit()
        
    print "sysopt params"
    for k in prob.sysopt.get_parameters(): 
        print k
    print "sysopt objectives"
    for k in prob.sysopt.get_objectives(): 
        print k
    print "sysopt constraints"
    for k in prob.sysopt.list_constraints(): 
        print k

    print "local_opt params"
    for k in prob.local_opt_dis1.get_parameters():    
        print k
    print "local_opt objectives"
    for k in prob.local_opt_dis1.get_objectives(): 
        print k
    print "local_opt constraints"
    for k in prob.local_opt_dis1.list_constraints(): 
        print k
    print "main_driver constraints"
    for k in prob.main_driver.list_constraints(): 
        print k
    print "main_driver params"
    for k in prob.main_driver.get_parameters():    
        print k  
    print        
    print
    print [x.name for x in prob.driver.workflow]
    print [x.name for x in prob.main_driver.workflow]
    print [x.name for x in prob.sysopt.workflow]
    print [x.name for x in prob.local_opt_dis1.workflow]
    print "----------------------"