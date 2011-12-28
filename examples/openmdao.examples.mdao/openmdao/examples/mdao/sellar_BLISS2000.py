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
        
        self.add("dis1_meta_model",MetaModel())
        self.dis1_meta_model.surrogate = {"y1":ResponseSurface()}
        self.dis1_meta_model.model = SellarDiscipline1()
        self.dis1_meta_model.recorder = DBCaseRecorder()
        
        # Metamodel for sellar discipline 2
        self.add("dis2_meta_model",MetaModel())
        self.dis2_meta_model.surrogate = {"y2":ResponseSurface()}
        self.dis2_meta_model.model = SellarDiscipline2()
        self.dis2_meta_model.recorder = DBCaseRecorder()
                
        # start with an initial mda
        self.add('mda', BroydenSolver())
        self.mda.add_parameter('dis1_meta_model.y2', low=0, high=20,start=1.)
        self.mda.add_constraint('dis2_meta_model.y2 = dis1_meta_model.y2')
        self.mda.add_parameter('dis2_meta_model.y1', low=0, high=20,start=1.)
        self.mda.add_constraint('dis2_meta_model.y1 = dis1_meta_model.y1')
        self.mda.force_execute = True    
        
        self.mda.workflow=SequentialWorkflow()
        self.mda.workflow.add(['dis1_meta_model','dis2_meta_model'])
        self.mda.add_event('dis1_meta_model.train_next')
        self.mda.add_event('dis2_meta_model.train_next')
        
        #training metalmodel for disc1
        
        self.add("DOE_Trainer1",DOEdriver())
        self.DOE_Trainer1.DOEgenerator = CentralComposite()
        self.DOE_Trainer1.alpha = .1
        self.DOE_Trainer1.add_parameter("dis1_meta_model.z1",low=-10,high=10,start=5.0)        
        self.DOE_Trainer1.add_parameter("dis1_meta_model.z2",low=0,high=10,start=2.0)   
        self.DOE_Trainer1.add_parameter("dis1_meta_model.y2",low=0,high=20)   
        self.DOE_Trainer1.add_event("dis1_meta_model.train_next")
        self.DOE_Trainer1.force_execute = True        
        
        
        #training metalmodel for disc1
        
        self.add("DOE_Trainer2",DOEdriver())
        self.DOE_Trainer2.DOEgenerator = CentralComposite()
        self.DOE_Trainer2.alpha = .1
        self.DOE_Trainer2.add_parameter("dis2_meta_model.z1",low=-10,high=10,start=5.0)        
        self.DOE_Trainer2.add_parameter("dis2_meta_model.z2",low=0,high=10,start=2.0)   
        self.DOE_Trainer2.add_parameter("dis2_meta_model.y1",low=0,high=20)   
        self.DOE_Trainer2.add_event("dis2_meta_model.train_next")
        self.DOE_Trainer2.force_execute = True               
        
        
        #optimization of global objective function

        self.add('sysopt', CONMINdriver())     
         
        self.sysopt.add_objective('(x1_store)**2 + dis1_meta_model.z2 + dis1_meta_model.y1 + math.exp(-dis2_meta_model.y2)')
        
        
        self.sysopt.add_parameter(['dis1_meta_model.z1','dis2_meta_model.z1'], low=-10, high=10.0,start=5.0)
        self.sysopt.add_parameter(['dis1_meta_model.z2','dis2_meta_model.z2'], low=0, high=10.0,start=2.0)        
        self.sysopt.add_parameter('dis1_meta_model.y2', low=0, high=20.0)
        self.sysopt.add_parameter('dis2_meta_model.y1', low=0, high=20.0)
        
        #feasibility constraints
        self.sysopt.add_constraint('dis1_meta_model.y2 < dis2_meta_model.y2')
        self.sysopt.add_constraint('dis1_meta_model.y2 > dis2_meta_model.y2')
        
        self.sysopt.add_constraint('dis2_meta_model.y1 < dis1_meta_model.y1')
        self.sysopt.add_constraint('dis2_meta_model.y1 > dis1_meta_model.y1')
        
        
        self.sysopt.add_constraint('3.16 < dis1_meta_model.y1')
        self.sysopt.add_constraint('dis2_meta_model.y2 < 24.0')
        self.sysopt.force_execute=True
        
        
        #optimization of discipline 1 (discipline 2 of the sellar problem has no local variables)
        
        self.add('disc1opt', CONMINdriver())
        self.disc1opt.add_objective('dis1_meta_model.y1')
        self.disc1opt.add_parameter('dis1_meta_model.x1', low=0, high=10.0) 
        self.disc1opt.add_constraint('3.16 < dis2_meta_model.y1')
        self.disc1opt.add_event('dis1_meta_model.train_next')
        
        self.disc1opt.force_execute=True
        
        self.disc1opt.workflow=SequentialWorkflow()
        self.disc1opt.workflow.add(['dis1_meta_model'])
        
        
        #build workflow for bliss2000
        
        self.add('driver2', FixedPointIterator())
        self.driver2.max_iteration = 50
        self.driver2.tolerance = .0001        
        self.driver2.workflow.add(['DOE_Trainer1','DOE_Trainer2'])#,'sysopt','disc1opt'])  
        self.driver2.add_parameter('x1_store', low=0, high=10.0)
        self.driver2.add_constraint('dis1_meta_model.x1 = x1_store')
        self.driver2.add_event('dis2_meta_model.reset_training_data')
        self.driver2.add_event('dis1_meta_model.reset_training_data')
        
        
        # Top level is sequential work flow. runs a single mda, then begins bliss2000 
        self.add('driver', IterateUntil())
        self.driver.max_iterations = 1
        self.driver.workflow=SequentialWorkflow()
        self.driver.workflow.add(['mda','driver2'])  
        
        

        
if __name__ == "__main__":  

    import time
    import math
    from openmdao.main.api import set_as_top
    
    prob = SellarBLISS2000()
    prob.name = "top"  
    
    set_as_top(prob)
    
    config=[5,2,0]
    
    prob.dis1_meta_model.z1 = prob.dis2_meta_model.z1= config[0]
    prob.dis1_meta_model.z2 = prob.dis2_meta_model.z2= config[1]
    prob.dis1_meta_model.x1 = config[2]
    
    
    prob.run()

    print "Minimum found at", prob.dis2_meta_model.z1,prob.dis2_meta_model.z2,prob.x1_store
    print "Coupling Vars: ", prob.dis2_meta_model.y1, prob.dis1_meta_model.y2
    print "with objective function value:",(prob.dis1_meta_model.x1)**2 + prob.dis1_meta_model.z2 + \
          prob.dis1_meta_model.y1 + math.exp(-prob.dis2_meta_model.y2)