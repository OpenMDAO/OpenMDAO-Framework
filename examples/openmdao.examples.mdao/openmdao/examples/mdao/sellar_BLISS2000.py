"""
    Solution of the Sellar analytical problem using classic BLISS.
    (Bi-Level Integrated System Synthesis)
    
    MDA solved with a Broyden solver.
    
    Global sensitivity calculated by finite-differencing the MDA-coupled
    system. The MDA should be replaced with solution of the GSE to fully
    match the original Sobiesky-Agte implementation.
"""

#from disciplines import SellarDiscipline1,SellarDiscipline2

from openmdao.lib.optproblems.sellar import Discipline1 as SellarDiscipline1
from openmdao.lib.optproblems.sellar import Discipline2 as SellarDiscipline2

from openmdao.main.api import Assembly,Component,SequentialWorkflow
from openmdao.lib.datatypes.api import Float, Array, Slot
from openmdao.lib.differentiators.finite_difference import FiniteDifference
from openmdao.lib.drivers.api import CONMINdriver, BroydenSolver, \
                                     SensitivityDriver, FixedPointIterator, IterateUntil
from openmdao.lib.drivers.api import DOEdriver                                     
from openmdao.lib.doegenerators.api import FullFactorial, Uniform, CentralComposite
from openmdao.lib.components.api import MetaModel
from openmdao.lib.casehandlers.api import DBCaseRecorder
from openmdao.lib.surrogatemodels.api import LogisticRegression, KrigingSurrogate
from openmdao.main.uncertain_distributions import NormalDistribution

class NormMean(Component):
    """Component to return mean of normal distribution"""
    
    # pylint: disable-msg=E1101
    #x = Float(0.0, iotype='in', desc='Normal distribution')
    x=Slot(NormalDistribution(),iotype="in")
    mu = Float(0.0, iotype='out', desc='Computed mean')
    
    def __init__(self): 
        super(NormMean,self).__init__()
        
        #just some initialization
        self.mu = NormalDistribution()

        
    def execute(self):
                
        self.mu=self.x.mu


class Debug(Component):
    """ delete me"""
        
    def __init__(self):
        super(Debug,self).__init__()
        self.force_execute = True
        
    def execute(self):
        print "iteration: ", self.parent.sysopt.exec_count
        print "disc1.z1,disc2.z2,disc1.x1: ", [self.parent.dis1.z1,self.parent.dis1.z2,self.parent.dis1.x1]
        print "disc2.z1,disc2.z2,x1_store", [self.parent.dis2.z1,self.parent.dis2.z2,self.parent.x1_store]
        print "dis1_meta_model.z1,dis1_meta_model.z2,dis1_meta_model.x1: ", [self.parent.dis1_meta_model.z1,self.parent.dis1_meta_model.z2,self.parent.dis1_meta_model.x1]
        print "dis2_meta_model.z1,dis2_meta_model.z2: ", [self.parent.dis2_meta_model.z1,self.parent.dis2_meta_model.z2]
        print
        
        


class SellarBLISS2000(Assembly):
    """ Optimization of the Sellar problem using the BLISS2000 algorithm
    Disciplines coupled with FixedPointIterator.
    """
    x1_store = Float(0.0)
    
    def __init__(self):
        """ Creates a new Assembly with this problem
        
        Optimal Design at (1.9776, 0, 0)
        
        Optimal Objective = 3.18339"""
                
        super(SellarBLISS2000, self).__init__()
        
        self.add('dis1', SellarDiscipline1())
        self.add('dis2', SellarDiscipline2())
        
        self.add("m1",NormMean())
        self.add("m2",NormMean())
        
        #objective = '(dis1.x1)**2 + dis1.z2 + dis1.y1 + exp(-dis2.y2)'
        #constraint1 = 'dis1.y1 > 3.16'
        #constraint2 = 'dis2.y2 < 24.0'
        
       
        
        # Metamodel for sellar discipline 1
        
        self.add("dis1_meta_model",MetaModel())
        self.dis1_meta_model.surrogate = {"y1":KrigingSurrogate()}
        self.dis1_meta_model.model = SellarDiscipline1()
        self.dis1_meta_model.recorder = DBCaseRecorder()
        
        #training metalmodel for disc1
        
        self.add("DOE_Trainer1",DOEdriver())
        self.DOE_Trainer1.DOEgenerator = CentralComposite()
        self.DOE_Trainer1.alpha = .1
        self.DOE_Trainer1.add_parameter("dis1_meta_model.z1",low=-10,high=10,start=5.0)        
        self.DOE_Trainer1.add_parameter("dis1_meta_model.z2",low=0,high=10,start=2.0)   
        self.DOE_Trainer1.add_parameter("dis1_meta_model.y2",low=0,high=20)   
        #self.DOE_Trainer1.case_outputs = ["dis1_meta_model.y1"]
        self.DOE_Trainer1.add_event("dis1_meta_model.train_next")
        #self.DOE_Trainer1.recorders = [DBCaseRecorder()]
        self.DOE_Trainer1.force_execute = True        
        # Metamodel for sellar discipline 2
        
        
        
        self.add("dis2_meta_model",MetaModel())
        self.dis2_meta_model.surrogate = {"y2":KrigingSurrogate()}
        self.dis2_meta_model.model = SellarDiscipline2()
        self.dis2_meta_model.recorder = DBCaseRecorder()
        
        #training metalmodel for disc1
        
        self.add("DOE_Trainer2",DOEdriver())
        self.DOE_Trainer2.DOEgenerator = CentralComposite()
        self.DOE_Trainer2.alpha = .5
        self.DOE_Trainer2.add_parameter("dis2_meta_model.z1",low=-10,high=10,start=5.0)        
        self.DOE_Trainer2.add_parameter("dis2_meta_model.z2",low=0,high=10,start=2.0)   
        self.DOE_Trainer2.add_parameter("dis2_meta_model.y1",low=0,high=20)   
        #self.DOE_Trainer2.case_outputs = ["dis2_meta_model.y2"]
        self.DOE_Trainer2.add_event("dis2_meta_model.train_next")
        #self.DOE_Trainer2.recorders = [DBCaseRecorder()]
        self.DOE_Trainer2.force_execute = True               
        
        #optimization of global objective function
        
        self.add('sysopt', CONMINdriver())
        
        
        self.connect("dis1_meta_model.y1","m1.x")
        self.connect("dis2_meta_model.y2","m2.x")

        
        
        self.sysopt.add_objective('(dis1.x1)**2 + dis1_meta_model.z2 + m1.mu + math.exp(-m2.mu)')
        # -------------------------set param values here
        #------------------------- change doe driver
        #------------------------- add compatibility constraints
        self.sysopt.add_parameter(['dis1_meta_model.z1','dis2_meta_model.z1','dis1.z1','dis2.z1'], low=-10, high=10.0,start=5.0)
        self.sysopt.add_parameter(['dis1_meta_model.z2','dis2_meta_model.z2','dis1.z2','dis2.z2'], low=0, high=10.0,start=2.0)        
        self.sysopt.add_parameter(['dis1_meta_model.y2','dis1.y2'], low=0, high=20.0)
        
        self.sysopt.add_parameter(['dis2_meta_model.y1','dis2.y1'], low=0, high=20.0)
        
        self.sysopt.add_constraint('dis1_meta_model.y2 < m2.mu')
        self.sysopt.add_constraint('dis1_meta_model.y2 > m2.mu')
        
        self.sysopt.add_constraint('dis2_meta_model.y1 < m1.mu')
        self.sysopt.add_constraint('dis2_meta_model.y1 > m1.mu')
        
        
        self.sysopt.add_constraint('3.16 < m1.mu')
        self.sysopt.add_constraint('m2.mu < 24.0')
        self.sysopt.force_execute=True
        #self.sysopt.iprint=1
        
        
        #optimization of discipline 1
        
        self.add('disc1opt', CONMINdriver())
        self.disc1opt.add_objective('dis1.y1')
        
        self.disc1opt.add_parameter(['dis1.x1','dis1_meta_model.x1'], low=0, high=10.0) 
        
        
        self.disc1opt.add_constraint('3.16 < dis1.y1')
        
        self.disc1opt.force_execute=True
        
        self.disc1opt.workflow=SequentialWorkflow()
        self.disc1opt.workflow.add(['dis1'])
        
        
        #self.disc1opt.iprint = 1
        
        
        #build workflow for system driver
         # Top level is Fixed-Point Iteration
        self.add('driver', IterateUntil())
        self.driver.max_iterations = 10
        self.add("debug",Debug())
        self.driver.workflow=SequentialWorkflow()
        
        self.driver.add_event('dis1_meta_model.reset_training_data')
        self.driver.add_event('dis2_meta_model.reset_training_data')
        self.driver.workflow.add(['DOE_Trainer1','DOE_Trainer2','sysopt','disc1opt','debug'])  
        
        #self.driver.workflow.add(['DOE_Trainer1','DOE_Trainer2','sysopt','disc1opt','debug'])  
        
        
        #self.driver.add_parameter('x1_store', low=0, high=10.0)
        #self.driver.add_constraint('dis1.x1 = x1_store')
        
        
        
        
        
        
        
if __name__ == "__main__": # pragma: no cover         

    import time
    import math
    from openmdao.main.api import set_as_top
    
    prob = SellarBLISS2000()
    prob.name = "top"  
    
    set_as_top(prob)
            
    prob.dis1.z1 = prob.dis2.z1 = prob.dis1_meta_model.z1 = prob.dis2_meta_model.z1= 5.0
    prob.dis1.z2 = prob.dis2.z2 = prob.dis1_meta_model.z2 = prob.dis2_meta_model.z2= 2.0
    prob.dis1.x1 = prob.dis1_meta_model.x1 = 1.0
    
    
    tt = time.time()
    prob.run()
    print "\n"
    print "Minimum found at (%f, %f, %f)" % (prob.dis1_meta_model.z1, \
                                             prob.dis1_meta_model.z2, \
                                             prob.x1_store)
    print "Couping vars: %f, %f" % (prob.dis1.y1, prob.dis2.y2)
    print "min objective:",(prob.dis1.x1)**2 + prob.dis1_meta_model.z2 + prob.m1.mu + math.exp(-prob.m2.mu)
    
    
    print "Elapsed time: ", time.time()-tt, "seconds"
