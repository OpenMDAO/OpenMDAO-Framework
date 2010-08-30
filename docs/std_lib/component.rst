
.. index:: Components

.. _Components:

Components
==========

.. index:: MetaModel

.. _MetaModel:

*MetaModel*
~~~~~~~~~~~

    A class which supports generalized meta modeling capabilities. There are two 
    sockets, one for a particular surrogate model generator and a second for the 
    model that is being approximated. The first socket, named `surrogate`, must 
    always be filled before anything else is done. This socket gets filled with 
    a surrogate model generator which adhears to the `ISurrogate` interface. 
    OpenMDAO provides some surrogate modelers in openmdao.lib.surrogatemodels. 
    
    .. testcode:: MetaModel_sockets
        
        from openmdao.main.api import Assembly
        from openmdao.lib.api import MetaModel
        from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
        
        class simulation(Assembly):
            def __init__(self): 
                super(simulation,self).__init__(self)
                
                self.add('meta_model',MetaModel())
                self.meta_model.surrogate = KrigingSurrogate()
        
    Once a specific surrogate model has been specified the model socket, called 
    `model`, can be filled with a component. As soon as a component is put in the
    socket, MetaModel will automatically mirror the inputs and outputs of that 
    component. In other words, MetaModel will have the same inputs and 
    outputs as whatever component is put into the `model` socket. 
    
    .. testcode:: MetaModel_model
        
        from openmdao.main.api import Assembly
        from openmdao.lib.api import MetaModel
        from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
        from openmdao.examples.singleEI.branin_component import BraninComponent
        
        class simulation(Assembly):
            def __init__(self): 
                super(simulation,self).__init__(self)
                
                self.add('meta_model',MetaModel())
                self.meta_model.surrogate = KrigingSurrogate()
        
                #component has two inputs: x,y
                self.meta_model.model = BraninComponent()
                
                #meta_model now has two inputs: x,y
                self.meta_model.x = 9
                self.meta_model.y = 9

        
    Depending on the component being approximated, you may not want to generate 
    approximations for all the outputs. Alternately you may want to exclude some 
    of the inputs from consideration in the generation of the surrogate models 
    if the inputs were going to be held constant for a given study. MetaModel
    provides two I/O-Traits to handle this situtaion: `includes` and `excludes`.
    Only one of these traits can be used at a time, and both inputs and outputs
    are specified at the same time. 
    
    If you are specifying the incudes, then only the I/O-Traits in that list will
    be used. If you are specifying the exludes, then everything but the I/O-Traits
    in the list will be mirrored by MetaModel
    
    .. testcode:: MetaModel_excludes
        
        from openmdao.main.api import Assembly
        from openmdao.lib.api import MetaModel
        from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
        from openmdao.examples.singleEI.branin_component import BraninComponent
        
        class simulation(Assembly):
            def __init__(self):
                super(simulation,self).__init__(self)
                
                self.add('meta_model',MetaModel())
                self.meta_model.surrogate = KrigingSurrogate()
                
                #component has two inputs: x,y
                self.meta_model.model = BraninComponent()
                
                #exclude the x input 
                self.meta_model.excludes=['x']

        
    or 
     
    .. testcode:: MetaModel_includes
        
        from openmdao.main.api import Assembly
        from openmdao.lib.api import MetaModel
        from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
        from openmdao.examples.singleEI.branin_component import BraninComponent
        
        class simulation(Assembly):
            
            def __init__(self): 
                super(simulation,self).__init__(self)
                
                self.add('meta_model',MetaModel())
                self.meta_model.surrogate = KrigingSurrogate()
                
                #component has two inputs: x,y
                self.meta_model.model = BraninComponent()
                
                #include only the y input
                self.meta_model.includes=['y']
        
    MetaModel treats inputs and outputs slightly differently. For inputs, regardless 
    of which ones are being included/excluded, all the inputs will be mirrored by 
    a MetaModel. But if they are excluded, then MetaModel won't pass their values 
    down to the surrogate models as inputs to training cases. 
    
    For outputs, when they are excluded they no longer get mirrored by MetaModel. 
    This means that those outputs won't get surrogate models fit to them and they
    consequently won't be available to the simulation from MetaModel. 
    
    Now you have setup your metamodel with a specific surrogate model, and you have 
    put a model into the `model` socket. The input and output 
    inclutions/exclusions have been specified. The next step is to actually start
    training and executing the MetaModel in simulations. 
    
    MetaModel has two operating modes: training and prediction. When run in training mode, 
    Metamodel passes its given inputs down to the model in the model socket and runs 
    it. Then it stores the outputs from the model to use for generating a
    surrogate model later. When run in predict mode, MetaModel will check for 
    any new training data and, if present, will generate a surrogate model for 
    each model output with the data. Then it will make a prediction of the model 
    outputs for the given inputs. A MetaModel instance must always be run in training mode 
    first, before executing it in predict mode.
    
    To put the an instance of MetaModel into the training mode, users must set the `train_next` 
    event trait before executing the component. This event trait automatically resets itself after the 
    execution, so it must be set again before each training case. This is shown in the example below:
    
    .. testcode:: MetaModel
        
        from openmdao.main.api import Assembly
        from openmdao.lib.api import MetaModel
        from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
        from openmdao.examples.singleEI.branin_component import BraninComponent
        
        class simulation(Assembly):
            def __init__(self): 
                super(simulation,self).__init__()
                
                self.add('meta_model',MetaModel())
                self.meta_model.surrogate = KrigingSurrogate()
                
                #component has two inputs: x,y
                self.meta_model.model = BraninComponent()
                
                self.meta_model.train_next = True
                self.meta_model.x = 2
                self.meta_model.y = 3
                
                self.meta_model.execute()
        
        
        
    
    In a typical iteration heirarchy, a Driver is responsible for setting
    the `train_next` event when appropriate. This is accomplished via the `IHasEvents` 
    Driver sub-interface. The `train_next` event is added to a Driver and then it will automatically set 
    `train_next` each time before it executes the model. A simple code snippet is 
    presented below, and a more detailed example can be found in the SingleEI
    example under the openmdao.examples.singleEI package. 
    
    .. testcode:: MetaModel_Assembly
        
        from openmdao.main.api import Assembly
        from openmdao.lib.api import DOEdriver, MetaModel
        from openmdao.examples.singleEI.branin_component import BraninComponent
        
        class Analysis(Assembly): 
            def __init__(self,doc=None): 
                super(Analysis,self).__init__()
                
                self.add('branin_meta_model',MetaModel())
                self.branin_meta_model.surrogate = KrigingSurrogate()
                self.branin_meta_model.model = BraninComponent()
                
                self.add('driver',DOEdriver())
                self.driver.workflow.add(self.branin_meta_model)
                self.driver.add_event('branin_meta_model.train_next')
                
    When the `train_next` event is not set, MetaModel automatically runs in predict mode. 
    When in predict mode, the outputs provided are the result of predicted outputs from the 
    surrogate model inside of MetaModel. 
    
    Before being able to predict the surrogate model response
    for any of the outputs of MetaModel, the surrogate model must actually be trained with the 
    recorded training data. This will happen automatically whenever MetaModel is run in predict mode and 
    new training data is available. This makes MetaModel more efficient, because it is not trying
    to retrain the model constantly when running large sets of training cases. Instead, the actual
    surrogate model training is only done when a prediction is needed and new training data is available. 