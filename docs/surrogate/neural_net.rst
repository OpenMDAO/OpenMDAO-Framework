.. index:: 

MetaModel - Tutorial
==================================

This tutorial is a demonstration of how to construsct a MetaModel using a
neural network surrogate model. A more detailed description of
how MetaModel works can be found here: [insert link here]. For this
example, a simple component was written for the Sin function, the function
whose behavior is behavior is being modeled in this case.

.. testcode:: NN_MetaModel_start

    from openmdao.main.api import Assembly, Component, SequentialWorkflow
    from math import sin
        
    from openmdao.lib.datatypes.api import Float
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import FullFactorial, Uniform
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.casehandlers.api import DBCaseRecorder
    from openmdao.lib.surrogatemodels.api import NeuralNet
       
    class Sin(Component): 
        
        x = Float(0,iotype="in",units="rad",low=0,high=6.3)
        
        f_x = Float(0.0,iotype="out")
        
        def execute(self): 
            self.f_x = .5*sin(self.x)

This Sin component has only one input and output, which will be mimicked
by the MetaModel. Had there been more inputs or outputs, access to those
would also be available. To create a MetaModel of our component, we first 
define an assembly to work in, and then instantiate the MetaModel component.

.. testcode::

    class Simulation(Assembly):            
        def __init__(self):
            super(Simulation,self).__init__()
        
            #Components
            self.add("sin_meta_model",MetaModel())
            self.sin_meta_model.surrogate = {"default":NeuralNet()}    
            self.sin_meta_model.model = Sin()
            self.sin_meta_model.recorder = DBCaseRecorder()

Here the MetaModel component is instantiated as "sin_meta_model." Once the MetaModel
component is in place, the first step is to fill the `surrogate` socket. In this case
we set the default to NeuralNet, meaning that all outputs would be modeled with NeuralNet
surrogate models. However, specific surrogate models can be specified for specific output
variables. For more details, see further documentation here [insert link here]. The next 
step is to identify the component that is being modeled by placing it in the `model` 
socket. This is the point where the Sin component is added to the MetaModel.

Once the `surrogate` and `model` sockets of the MetaModel have been filled, the MetaModel
can be run in *training* mode.  This is done by adding a DOEdriver, which is allows a DOE
to be executed for this workflow.  The type of DOEGenerator used in this MetaModel is a 
FullFactorial.  This generator creates a set of evenly spaced points across an interval. 
The number of points is 16 as defined by ``num_levels`` under the DOEGenerator, and the 
interval is the set of input values.  In this case the x value is the only input, as seen
 in the earlier Sin component.

.. testcode::

        #Training the MetaModel
        self.add("DOE_Trainer",DOEdriver())
        self.DOE_Trainer.DOEgenerator = FullFactorial()
        self.DOE_Trainer.DOEgenerator.num_levels = 16
        self.DOE_Trainer.add_parameter("sin_meta_model.x")
        self.DOE_Trainer.add_event("sin_meta_model.train_next")
        self.DOE_Trainer.case_outputs = ["sin_meta_model.f_x"]
        self.DOE_Trainer.recorder = DBCaseRecorder()
        self.DOE_Trainer.force_execute = True