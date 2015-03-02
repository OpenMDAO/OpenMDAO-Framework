from openmdao.main.api import Assembly, Component, set_as_top
from math import sin, cos

from openmdao.main.datatypes.api import Float
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.doegenerators.api import FullFactorial, Uniform
from openmdao.lib.components.api import MetaModel
from openmdao.lib.casehandlers.api import DBCaseRecorder
from openmdao.lib.surrogatemodels.api import LogisticRegression, FloatKrigingSurrogate


class Trig(Component):

    x = Float(0,iotype="in",units="rad")

    f_x_sin = Float(0.0,iotype="out")
    f_x_cos = Float(0.0,iotype="out")

    def execute(self):
        self.f_x_sin = .5*sin(self.x)
        self.f_x_cos = .5*cos(self.x)

class Simulation(Assembly):

    def configure(self):

        # Our component to be meta-modeled
        self.add("trig_calc", Trig())

        # Create meta_model for two responsese
        self.add("trig_meta_model", MetaModel(params = ('x', ),
                                              responses = ('f_x_sin', 'f_x_cos')))

        # Use Kriging for the f_x output
        self.trig_meta_model.surrogates['f_x_sin'] = LogisticRegression()
        self.trig_meta_model.surrogates['f_x_cos'] = FloatKrigingSurrogate()

        # Training the MetaModel
        self.add("DOE_Trainer", DOEdriver())
        self.DOE_Trainer.DOEgenerator = FullFactorial()
        self.DOE_Trainer.DOEgenerator.num_levels = 20
        self.DOE_Trainer.add_parameter("trig_calc.x", low=0, high=20)
        self.DOE_Trainer.add_response('trig_calc.f_x_sin')
        self.DOE_Trainer.add_response('trig_calc.f_x_cos')

        # Pass training data to the meta model.
        self.connect('DOE_Trainer.case_inputs.trig_calc.x', 'trig_meta_model.params.x')
        self.connect('DOE_Trainer.case_outputs.trig_calc.f_x_sin', 'trig_meta_model.responses.f_x_sin')
        self.connect('DOE_Trainer.case_outputs.trig_calc.f_x_cos', 'trig_meta_model.responses.f_x_cos')

        #MetaModel Validation
        self.add("DOE_Validate", DOEdriver())
        self.DOE_Validate.DOEgenerator = Uniform()
        self.DOE_Validate.DOEgenerator.num_samples = 20
        self.DOE_Validate.add_parameter(("trig_meta_model.x", "trig_calc.x"),
                                        low=0, high=20)
        self.DOE_Validate.add_response("trig_calc.f_x_sin")
        self.DOE_Validate.add_response("trig_calc.f_x_cos")
        self.DOE_Validate.add_response("trig_meta_model.f_x_sin")
        self.DOE_Validate.add_response("trig_meta_model.f_x_cos")

        #Iteration Hierarchy
        self.driver.workflow.add(['DOE_Trainer', 'DOE_Validate'])
        self.DOE_Trainer.workflow.add('trig_calc')
        self.DOE_Validate.workflow.add(['trig_calc', 'trig_meta_model'])

if __name__ == "__main__":

    sim = set_as_top(Simulation())
    sim.run()

    #This is how you can access any of the data
    train_inputs = sim.DOE_Trainer.case_inputs.trig_calc.x
    train_actual_sin = sim.DOE_Trainer.case_outputs.trig_calc.f_x_sin
    train_actual_cos = sim.DOE_Trainer.case_outputs.trig_calc.f_x_cos
    inputs = sim.DOE_Validate.case_inputs.trig_meta_model.x
    actual_sin = sim.DOE_Validate.case_outputs.trig_calc.f_x_sin
    actual_cos = sim.DOE_Validate.case_outputs.trig_calc.f_x_cos
    predicted_sin = sim.DOE_Validate.case_outputs.trig_meta_model.f_x_sin
    predicted_cos = sim.DOE_Validate.case_outputs.trig_meta_model.f_x_cos

    for a,b,c,d in zip(actual_sin, predicted_sin, actual_cos, predicted_cos):
        print "%1.3f, %1.3f, %1.3f, %1.3f"%(a, b, c, d)
