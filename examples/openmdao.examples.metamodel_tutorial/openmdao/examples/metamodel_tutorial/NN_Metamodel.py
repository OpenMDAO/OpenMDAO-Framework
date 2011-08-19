from openmdao.main.api import Assembly

from openmdao.lib.drivers.api import DOEdriver

from openmdao.lib.components.api import MetaModel
from openmdao.lib.casehandlers.api import DBCaseRecorder
from openmdao.lib.surrogatemodels.api import NeuralNetSurrogate
from openmdao.examples.expected_improvement.testfunc_component import TestFuncComponent

class Simulation(Assembly):
    def setUp(self):
        random.seed(10)
        
    def __init__(self):
        super(Simulation,self).__init__(self)
    
    #Components
    self.add("tf_meta_model",MetaModel())
    self.tf_meta_model.surrogate = {"default":NeuralNetSurrogate()}    
    self.tf_meta_model.model = TestFuncComponent()
    self.tf_meta_model.recorder = DBCaseRecorder(':memory:')
    self.tf_meta_model.force_execute = True
    
    #Training the MetaModel
    self.add("DOE_Trainer",DOEdriver())
    self.DOE_Trainer.workflow.add("tf_meta_model")
    self.DOE_Trainer.sequential = True
    self.DOE_Trainer.DOEgenerator = Uniform()
    self.DOE_Trainer.num_samples = 500
    self.DOE_Trainer.add_parameter("tf_meta_model.x")
    self.DOE_Trainer.add_parameter("tf_meta_model.y")
    self.DOE_Trainer.add_event("tf_meta_model.train_next")
    self.DOE_Trainer.case_outputs = ["tf_meta_model.f_xy"]
    self.DOE_Trainer.recorder = DBCaseRecorder(os.path.join(self._tdir,'trainer.db'))
    
    #MetaModel Validation
    self.add("DOE_Predict",DOEdriver())
    self.DOE_Validate.workflow.add("tf_meta_model")
    self.DOE_Validate.sequential = True
    self.DOE_Validate.DOEgenerator = Uniform()
    self.DOE_Validate.num_samples = 500
    self.DOE_Validate.add_parameter("tf_meta_model.x")
    self.DOE_Validate.add_parameter("tf_meta_model.y")
    self.DOE_Validate.case_outputs = ["tf_meta_model.f_xy"]
    self.DOE_Validate.recorder = DBCaseRecorder(os.path.join(self._tdir,'trainer.db'))
    
    #Iteration Hierarchy
    self.driver.workflow.add(['DOE_Trainer', 'DOE_Predict'])
    self.DOE_Trainer.workflow.add('tf_meta_model')
    self.DOE_Validate.workflow.add('tf_meta_model')
    self.DOE_Validate.workflow.add('TestFuncComponent')
    
    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)
    
if __name__ == "__main__":
    