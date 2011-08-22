.. index:: Mult_Outputs_Meta

Modeling Multiple Outputs
==================================

This tutorial is a short demonstration of how to construct a MetaModel of a component with
multiple outputs. This tutorial builds off of the :ref:`single_output` tutorial, with 
modifications for multiple outputs in a component.

We created a new component called ``Trig()``. This component has one input and two 
outputs, both of which will be mimicked by the MetaModel. 

.. testcode:: Mult_out_parts

    from openmdao.main.api import Assembly, Component, SequentialWorkflow
    from math import sin, cos
    
    from openmdao.lib.datatypes.api import Float
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.doegenerators.api import FullFactorial, Uniform
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.casehandlers.api import DBCaseRecorder
    from openmdao.lib.surrogatemodels.api import LogisticRegression, KrigingSurrogate
    
    
    class Trig(Component): 
        
        x = Float(0,iotype="in",units="rad")
        
        f_x_sin = Float(0.0,iotype="out")
        f_x_cos = Float(0.0,iotype="out")
        
        def execute(self): 
            self.f_x_sin = .5*sin(self.x)
            self.f_x_cos = .5*cos(self.x)

This next section differs from the the previous example in that there are two surrogate models, 
one specified for each of the outputs. Note that each of the outputs had been assinged 
a specific surrogate model, a logistic regression for sin, and a kriging surrogate for cos. In this case, 
no ``default`` was set at all. 

The parameter `x` still only needs to be added once in this case, since the same input 
is being evaluated for both outputs, thus a need for only one input.
        

.. testcode:: Mult_out_parts

    class Simulation(Assembly):
        
        def __init__(self):
            super(Simulation,self).__init__()
        
            #Components
            self.add("trig_meta_model",MetaModel())
            self.trig_meta_model.surrogate = {"f_x_sin":LogisticRegression(),
                                             "f_x_cos":KrigingSurrogate()}  
            self.trig_meta_model.model = Trig()        
            self.trig_meta_model.recorder = DBCaseRecorder()

            #Training the MetaModel
            self.add("DOE_Trainer",DOEdriver())
            self.DOE_Trainer.DOEgenerator = FullFactorial()
            self.DOE_Trainer.DOEgenerator.num_levels = 20
            self.DOE_Trainer.add_parameter("trig_meta_model.x",low=0,high=20)
            self.DOE_Trainer.case_outputs = ["trig_meta_model.f_x_sin","trig_meta_model.f_x_cos"]
            self.DOE_Trainer.add_event("trig_meta_model.train_next")
            self.DOE_Trainer.recorder = DBCaseRecorder()
            self.DOE_Trainer.force_execute = True 
            
            #MetaModel Validation
            self.add("trig_calc",Trig())
            self.add("DOE_Validate",DOEdriver())
            self.DOE_Validate.DOEgenerator = Uniform()
            self.DOE_Validate.DOEgenerator.num_samples = 20
            self.DOE_Validate.add_parameter(("trig_meta_model.x","trig_calc.x"),low=0,high=20)
            self.DOE_Validate.case_outputs = ["trig_calc.f_x_sin","trig_calc.f_x_cos","trig_meta_model.f_x_sin","trig_meta_model.f_x_cos"]
            self.DOE_Validate.recorder = DBCaseRecorder()
            self.DOE_Validate.force_execute = True
            
            #Iteration Hierarchy
            self.driver.workflow = SequentialWorkflow()
            self.driver.workflow.add(['DOE_Trainer','DOE_Validate'])
            self.DOE_Trainer.workflow.add('trig_meta_model')    
            self.DOE_Validate.workflow.add('trig_meta_model')
            self.DOE_Validate.workflow.add('trig_calc')

        
The iteration hierarchy is structurally the same as it would be with one output.  Even 
though there's multiple surrogate models for multiple outputs, they are still contained 
within only one MetaModel component.  So once again there is the MetaModel component seperately 
added to each workflow, and the ``trig_calc`` component being added to the validation 
stage so that comparitive values may be generated.


In the printing of the information, we have now included all four of the outputs. 
For the kriging surrogate model, the answer returned as a normal distribution 
(kriging surrogate predicts both a mean and a standard deviation for a given input).
When comparing the data, we just look at the mean here.  This is why there is a ``.mu`` appended to the 
cos case under ``predicted_cos``.  An 
alternative would be to append ``.sigma`` which would return the standard deviation.
        
.. testcode:: Mult_out_parts

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
    
        
        for a,b,c,d in zip(actual_sin,predicted_sin,actual_cos,predicted_cos):
            print "%1.3f, %1.3f, %1.3f, %1.3f"%(a,b,c,d)
            
To view this example, and try running and modifying the code for yourself, you can download it here:
:download:`multi_outs.py </../examples/openmdao.examples.metamodel_tutorial/openmdao/examples/metamodel_tutorial/multi_outs.py>`.    