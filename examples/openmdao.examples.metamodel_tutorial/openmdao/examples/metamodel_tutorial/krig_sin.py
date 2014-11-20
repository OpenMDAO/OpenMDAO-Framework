''' Example problem - metamodel of the sine function. '''

import sys
from math import sin

from openmdao.lib.casehandlers.api import DBCaseRecorder
from openmdao.lib.components.api import MetaModel
from openmdao.lib.doegenerators.api import FullFactorial, Uniform
from openmdao.lib.drivers.api import DOEdriver
from openmdao.lib.surrogatemodels.api import FloatKrigingSurrogate
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float


class Sin(Component):
    ''' Simple sine calculation. '''

    x = Float(0.0, iotype="in", units="rad")

    f_x = Float(0.0, iotype="out")

    def execute(self):
        self.f_x = .5*sin(self.x)


class Simulation(Assembly):
    ''' Top level assembly for MetaModel of a sine component using a
    Kriging surrogate.'''

    def configure(self):

        # Our component to be meta-modeled
        self.add("sin_calc", Sin())

        # Another instance of our component for head-to-head comparison with
        # the metamodel.
        self.add("sin_verify", Sin())

        # Create meta_model for f_x as the response
        self.add("sin_meta_model", MetaModel(params = ('x', ),
                                             responses = ('f_x', )))

        # Use Kriging for the f_x output
        self.sin_meta_model.default_surrogate = FloatKrigingSurrogate()

        # Training the MetaModel
        self.add("DOE_Trainer", DOEdriver())
        self.DOE_Trainer.DOEgenerator = FullFactorial()
        self.DOE_Trainer.DOEgenerator.num_levels = 25
        self.DOE_Trainer.add_parameter("sin_calc.x", low=0, high=20)
        self.DOE_Trainer.add_response('sin_calc.f_x')

        # Pass training data to the meta model.
        self.connect('DOE_Trainer.case_inputs.sin_calc.x', 'sin_meta_model.params.x')
        self.connect('DOE_Trainer.case_outputs.sin_calc.f_x', 'sin_meta_model.responses.f_x')

        # Cross-validate the metamodel using random data
        self.add("DOE_Validate", DOEdriver())
        self.DOE_Validate.DOEgenerator = Uniform()
        self.DOE_Validate.DOEgenerator.num_samples = 100
        self.DOE_Validate.add_parameter(("sin_meta_model.x", "sin_verify.x"),
                                        low=0, high=20) # , name="combined_input"
        self.DOE_Validate.add_response("sin_verify.f_x")
        self.DOE_Validate.add_response("sin_meta_model.f_x")

        #Iteration Hierarchy
        self.driver.workflow.add(['DOE_Trainer', 'DOE_Validate'])
        self.DOE_Trainer.workflow.add('sin_calc')
        self.DOE_Validate.workflow.add(['sin_verify', 'sin_meta_model'])


if __name__ == "__main__":

    sim = set_as_top(Simulation())
    sim.run()

    #This is how you can access any of the data
    train_inputs = sim.DOE_Trainer.case_inputs.sin_calc.x
    train_actual = sim.DOE_Trainer.case_outputs.sin_calc.f_x
    inputs = sim.DOE_Validate.case_inputs.sin_meta_model.x
    actual = sim.DOE_Validate.case_outputs.sin_verify.f_x
    predicted = sim.DOE_Validate.case_outputs.sin_meta_model.f_x

    if '--noplot' not in sys.argv:
        import pylab as p

        p.scatter(train_inputs, train_actual, c='g', label="training data")
        p.scatter(inputs, predicted, c='b', label="predicted result")
        p.legend()
        p.show()

    for a,p in zip(actual, predicted):
        print "%1.3f, %1.3f"%(a,p)
