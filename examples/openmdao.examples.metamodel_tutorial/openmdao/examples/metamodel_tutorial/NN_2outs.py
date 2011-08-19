from openmdao.main.api import Assembly, Component, SequentialWorkflow
from math import sin, cos

from openmdao.lib.datatypes.api import Float
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial, Uniform
from openmdao.lib.components.api import MetaModel
from openmdao.lib.casehandlers.api import DBCaseRecorder
from openmdao.lib.surrogatemodels.api import NeuralNet, KrigingSurrogate


class Trig(Component): 
    
    x = Float(0,iotype="in",units="rad",low=0,high=20)
    
    f_x_sin = Float(0.0,iotype="out")
    f_x_cos = Float(0.0,iotype="out")
    
    def execute(self): 
        self.f_x_sin = .5*sin(self.x)
        self.f_x_cos = .5*cos(self.x)
                
class Simulation(Assembly):

        
    def __init__(self):
        super(Simulation,self).__init__()
    
        #Components
        self.add("trig_meta_model",MetaModel())
        self.trig_meta_model.surrogate = {"f_x_sin":NeuralNet(),
                                         "f_x_cos":KrigingSurrogate()}  
        self.trig_meta_model.surrogate_args = {"f_x_sin":{'n_hidden_nodes':3}}
        self.trig_meta_model.model = Trig()        
        self.trig_meta_model.recorder = DBCaseRecorder()
        
        #Training the MetaModel
        self.add("DOE_Trainer",DOEdriver())
        self.DOE_Trainer.DOEgenerator = FullFactorial()
        self.DOE_Trainer.DOEgenerator.num_levels = 20
        self.DOE_Trainer.add_parameter("trig_meta_model.x")
        self.DOE_Trainer.case_outputs = ["trig_meta_model.f_x_sin","trig_meta_model.f_x_cos"]
        self.DOE_Trainer.add_event("trig_meta_model.train_next")
        self.DOE_Trainer.recorder = DBCaseRecorder()
        self.DOE_Trainer.force_execute = True
        
        #MetaModel Validation
        self.add("trig_calc",Trig())
        self.add("DOE_Validate",DOEdriver())
        self.DOE_Validate.DOEgenerator = Uniform()
        self.DOE_Validate.DOEgenerator.num_samples = 20
        self.DOE_Validate.add_parameter(("trig_meta_model.x","trig_calc.x"))
        self.DOE_Validate.case_outputs = ["trig_calc.f_x_sin","trig_calc.f_x_cos","trig_meta_model.f_x_sin","trig_meta_model.f_x_cos"]
        self.DOE_Validate.recorder = DBCaseRecorder()
        self.DOE_Validate.force_execute = True
        
        #Iteration Hierarchy
        self.driver.workflow = SequentialWorkflow()
        self.driver.workflow.add(['DOE_Trainer','DOE_Validate'])
        self.DOE_Trainer.workflow.add('trig_meta_model')
        self.DOE_Validate.workflow.add('trig_meta_model')
        self.DOE_Validate.workflow.add('trig_calc')
    

if __name__ == "__main__":
    
    
    sim = Simulation()
    sim.run()
        
    #This is how you can access any of the data
    train_data = sim.DOE_Trainer.recorder.get_iterator()
    validate_data = sim.DOE_Validate.recorder.get_iterator()
    train_inputs = [case['trig_meta_model.x'] for case in train_data]
    train_actual_sin = [case['trig_meta_model.f_x_sin'] for case in train_data]
    train_actual_cos = [case['trig_meta_model.f_x_cos'].mu for case in train_data]
    inputs = [case['trig_calc.x'] for case in validate_data]    
    actual_sin = [case['trig_calc.f_x_sin'] for case in validate_data]
    actual_cos = [case['trig_calc.f_x_cos'] for case in validate_data]
    predicted_sin = [case['trig_meta_model.f_x_sin'] for case in validate_data]
    predicted_cos = [case['trig_meta_model.f_x_cos'].mu for case in validate_data]
    
    
    
    #.mu is mean, .sigma standard dev

    for a,b,c,d in zip(actual_sin,predicted_sin,actual_cos,predicted_cos):
        print "%1.3f, %1.3f, %1.3f, %1.3f"%(a,b,c,d)
