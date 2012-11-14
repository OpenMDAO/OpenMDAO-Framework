import sys

from openmdao.main.api import Assembly, Component, SequentialWorkflow, set_as_top
from math import sin

from openmdao.lib.datatypes.api import Float
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial, Uniform
from openmdao.lib.components.api import MetaModel
from openmdao.lib.casehandlers.api import DBCaseRecorder
from openmdao.lib.surrogatemodels.api import FloatKrigingSurrogate #, KrigingSurrogate as FloatKrigingSurrogate



class Sin(Component): 
    
    x = Float(0,iotype="in",units="rad")
    
    f_x = Float(0.0,iotype="out")
    
    def execute(self): 
        self.f_x = .5*sin(self.x)

class Simulation(Assembly):

        
    def configure(self):
    
        #Components
        self.add("sin_meta_model",MetaModel())      
        self.sin_meta_model.model = Sin()        
        self.sin_meta_model.default_surrogate = FloatKrigingSurrogate()


        #Training the MetaModel
        self.add("DOE_Trainer",DOEdriver())
        self.DOE_Trainer.DOEgenerator = FullFactorial()
        self.DOE_Trainer.DOEgenerator.num_levels = 25
        self.DOE_Trainer.add_parameter("sin_meta_model.x",low=0,high=20)
        self.DOE_Trainer.case_outputs = ["sin_meta_model.f_x"]
        self.DOE_Trainer.add_event("sin_meta_model.train_next")
        self.DOE_Trainer.recorders = [DBCaseRecorder()]
        
        #MetaModel Validation
        self.add("sin_calc",Sin())
        self.add("DOE_Validate",DOEdriver())
        self.DOE_Validate.DOEgenerator = Uniform()
        self.DOE_Validate.DOEgenerator.num_samples = 100
        self.DOE_Validate.add_parameter(("sin_meta_model.x","sin_calc.x"),low=0,high=20)
        self.DOE_Validate.case_outputs = ["sin_calc.f_x","sin_meta_model.f_x"]
        self.DOE_Validate.recorders = [DBCaseRecorder()]
        
        #Iteration Hierarchy
        self.driver.workflow = SequentialWorkflow()
        self.driver.workflow.add(['DOE_Trainer','DOE_Validate'])
        self.DOE_Trainer.workflow.add('sin_meta_model')
        self.DOE_Validate.workflow.add('sin_meta_model')
        self.DOE_Validate.workflow.add('sin_calc')
    

if __name__ == "__main__":
    
    sim = Simulation()
    sim.run()
        
    #This is how you can access any of the data
    train_data = sim.DOE_Trainer.recorders[0].get_iterator()
    validate_data = sim.DOE_Validate.recorders[0].get_iterator()
    
    train_inputs = [case['sin_meta_model.x'] for case in train_data]
    train_actual = [case['sin_meta_model.f_x'] for case in train_data]
    inputs = [case['sin_calc.x'] for case in validate_data]    
    actual = [case['sin_calc.f_x'] for case in validate_data]  
    predicted = [case['sin_meta_model.f_x'] for case in validate_data]

    if '--noplot' not in sys.argv:
        import pylab as p
    
        p.scatter(train_inputs,train_actual,c='g',label="training data")
        p.scatter(inputs,predicted,c='b',label="predicted result")
        p.legend()
        p.show()
        
    for a,p in zip(actual,predicted): 
        print "%1.3f, %1.3f"%(a,p)
