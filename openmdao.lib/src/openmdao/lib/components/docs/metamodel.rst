.. index:: MetaModel

.. _MetaModel:

*MetaModel*
~~~~~~~~~~~

MetaModel is a class which supports generalized meta modeling capabilities. It has two  slots, one
for surrogate model generators and a second for the  model that is being approximated. The first
slot, named `surrogate`, must  always be filled before anything else is done. This slot gets
filled with  a dictionary that specifies which surrogate model generator should be used for  which
outputs. The keys of the dictionary are the variable names, and the values are the particular
surrogate model generators which adhere to the ISurrogate interface. A special key, ``'default'``,
can be used to indicate a surrogate model generator to be used if no specific one is given for a
particular variable.  Any specified variables will override the default. OpenMDAO provides some
surrogate modelers in ``openmdao.lib.surrogatemodels``. 

.. testcode:: MetaModel_slots
        
   from openmdao.main.api import Assembly
   from openmdao.lib.components.api import MetaModel
   from openmdao.lib.surrogatemodels.api import KrigingSurrogate,LogisticRegression

   class Simulation(Assembly):
       def __init__(self): 
           super(Simulation,self).__init__(self)

           self.add('meta_model',MetaModel())
           #using KriginSurrogate for all outputs                
           self.meta_model.surrogate = {'default':KrigingSurrogate()}

           #alternately, overiding the default for a specific variable
           self.meta_model.surrogate = {'default':LogisticRegression(),
                                        'f_xy':KrigingSurrogate()}

Once the surrogate dictionary has been specified, the model slot, called 
`model`, can be filled with a component. As soon as a component is put in the
slot, MetaModel will automatically mirror the inputs and outputs of that 
component. In other words, MetaModel will have the same inputs and 
outputs as whatever component is put into the model slot. 

.. testcode:: MetaModel_model

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def __init__(self): 
            super(Simulation,self).__init__(self)

            self.add('meta_model',MetaModel())
            self.meta_model.surrogate = {'default':KrigingSurrogate()}

            #component has two inputs: x,y
            self.meta_model.model = BraninComponent()

            #meta_model now has two inputs: x,y
            self.meta_model.x = 9
            self.meta_model.y = 9


Depending on the component being approximated, you may not want to generate 
approximations for all the outputs. Alternatively, you may want to exclude some 
of the inputs from consideration when the surrogate models are generated
if the inputs are going to be held constant for a given study. MetaModel
provides two I/O-Traits to handle this situation: `includes` and `excludes`.
Only one of these traits can be used at a time, and both inputs and outputs
are specified at the same time. 

If you are specifying the includes, then only the I/O-Traits in that list will
be used. If you are specifying the excludes, then everything *but* the I/O-Traits
in the list will be mirrored by MetaModel.

.. testcode:: MetaModel_excludes

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def __init__(self):
            super(Simulation,self).__init__(self)

            self.add('meta_model',MetaModel())
            self.meta_model.surrogate = {'default':KrigingSurrogate()}

            #component has two inputs: x,y
            self.meta_model.model = BraninComponent()

            #exclude the x input 
            self.meta_model.excludes=['x']


or 

.. testcode:: MetaModel_includes

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):

        def __init__(self): 
            super(Simulation,self).__init__(self)

            self.add('meta_model',MetaModel())
            self.meta_model.surrogate = {'default': KrigingSurrogate()}

            #component has two inputs: x,y
            self.meta_model.model = BraninComponent()

            #include only the y input
            self.meta_model.includes=['y']

MetaModel treats inputs and outputs a little differently. All the inputs, regardless of which ones
are being included/excluded, will be mirrored by a MetaModel. But if inputs are excluded, then
MetaModel won't pass down their values to the surrogate models as inputs to training cases. 

When outputs are excluded, they no longer get mirrored by MetaModel. They won't get
surrogate models fit to them, and consequently, they won't be available to the simulation from
MetaModel. 

Now you have set up your MetaModel with a specific surrogate model, and you have 
put a model into the `model` slot. The input and output 
inclusions/exclusions have been specified. The next step is to actually start
training and executing the MetaModel in simulations. 

MetaModel has two operating modes: *training* and *prediction.* When run in *training* mode, 
MetaModel passes its given inputs down to the model in the model slot and runs 
it. Then it stores the outputs from the model to use for generating a
surrogate model later. When run in *predict* mode, MetaModel will check for 
any new training data and, if present, will generate a surrogate model for 
each model output with the data. Then it will make a prediction of the model 
outputs for the given inputs. A MetaModel instance must always be run in training mode 
before executing it in predict mode.

To put an instance of MetaModel into the training mode, you must set the ``train_next`` event trait
before executing the component. This event trait automatically resets itself after the execution, 
so it must be set again before each training case. An event trait is just a trigger mechanism, and
it will trigger its behavior regardless of the value you set it to. 

.. testcode:: MetaModel

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def __init__(self): 
            super(Simulation,self).__init__()

            self.add('meta_model',MetaModel())
            self.meta_model.surrogate = {'default':KrigingSurrogate()}

            #component has two inputs: x,y
            self.meta_model.model = BraninComponent()

            self.meta_model.train_next = True
            self.meta_model.x = 2
            self.meta_model.y = 3

            self.meta_model.execute()


In a typical iteration hierarchy, a Driver is responsible for setting the
``train_next`` event when appropriate. This is accomplished via the
IHasEvents Driver sub-interface. The ``train_next`` event is added to a
Driver, which will then automatically set ``train_next`` prior to each
iteration of the model. A simple code snippet is presented below, while a
more detailed example can be found in the ``single_objective_ei`` example under the
``openmdao.examples.expected_improvement`` package.

.. testcode:: MetaModel_Assembly

    from openmdao.main.api import Assembly
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly): 
        def __init__(self,doc=None): 
            super(Simulation,self).__init__()

            self.add('meta_model',MetaModel())
            self.meta_model.surrogate = {'default':KrigingSurrogate()}

            #component has two inputs: x,y
            self.meta_model.model = BraninComponent()

            self.add('driver',DOEdriver())
            self.driver.workflow.add('meta_model')
            self.driver.add_event('meta_model.train_next')

When the ``train_next`` event is not set, MetaModel automatically runs in predict mode. 
When in predict mode, the outputs provided are the result of predicted outputs from the 
surrogate model inside of MetaModel. 

Before being able to predict the surrogate model response
for any of the outputs of MetaModel, the surrogate model must be trained with the 
recorded training data. This will happen automatically whenever MetaModel is run in predict mode and 
new training data is available. This makes MetaModel more efficient, because it is not trying
to retrain the model constantly when running large sets of training cases. Instead, the actual
surrogate model training is only done when a prediction is needed and new training data is available. 

*Source Documentation for metamodel.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
