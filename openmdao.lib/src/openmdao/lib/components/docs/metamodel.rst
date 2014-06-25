.. index:: MetaModel

.. _MetaModel:

*MetaModel*
~~~~~~~~~~~

MetaModel is a class which supports generalized meta modeling
(a.k.a. surrogate modeling) capabilities. The Metamodel can
be used to create a reduced order (and faster) respresentation
of any input-output relationship in your model. The inputs to
a metamodel are called params, and the outputs are called responses. When
you instantiate a metamodel, you give it a tuple of params and a tuple of
responses that you want it to model.

Let's start by creating an assembly with a component (the `BraninComponent`)
and a metamodel of its response with respect to its two inputs.

.. testcode:: MetaModel_model

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def configure(self):

            #component has two inputs (x, y) and one output (f_xy)
            self.add('model', BraninComponent())
            self.model.x = 9.
            self.model.y = 9.

            #create meta_model for f_xy as the response
            self.add('meta_model', MetaModel(params=('x', 'y'),
                                             responses=('f_xy')))

Metamodel contains a ``slot``, called ``default_surrogate``, which can be
filled with a surrogate model generator instance. Copies of this default
surrogate model generator will be used for any outputs that don't have a
specific surrogate model generator associated with them.

You can also associate a surrogate model generator with a specific output by
finding that output's key in the dictionary named `surrogates` and assigning
the specific surrogate model there. All surrogate model generators must
implement the ISurrogate interface. OpenMDAO provides some surrogate model
generators in the ``openmdao.lib.surrogatemodels`` directory.

.. testcode:: MetaModel_slots

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def configure(self):

            #component has two inputs (x, y) and one output (f_xy)
            self.add('model', BraninComponent())
            self.model.x = 9
            self.model.y = 9

            #create meta_model for f_xy as the response
            self.add('meta_model', MetaModel(params=('x', 'y'),
                                             responses=('f_xy')))

            #The meta_model contains a dictionary named 'surrogates' whose keys are
            #the variable names, and whose values are slots where a surrogate
            #model instance can be filled. In this case we'll just put
            #another KrigingSurrogate in there, just to show how it's done.
            #Since the default_surrogate is also a KrigingSurrogate, this
            #will give us the same results we would have had if we'd only
            #used the default_surrogate.

            # use Kriging for the f_xy output
            self.meta_model.surrogates['f_xy'] = KrigingSurrogate()

Now you have set up your MetaModel with a specific surrogate model, and you have
put a model into the `model` slot. The next step is to actually start
training and executing the MetaModel in simulations.

When you create a MetaModel instance, a selection of input and output variables are
created based on the names you give it in the params and responses tuples. These variables
allow you to operate the metamodel in *training* and *prediction*.

For training, a Metamodel is created with two variables trees ``params`` and ``responses``,
which contain inputs for all of the params and responses that were specified
when instantiated. You can connect these variables to some other component
(like a ``DOEDriver``) that provides a list of training points. Each training point in the list
is a list of the values of all the params or responses.

For prediction, a Metamodel is also created with input and output variables
that match the params and responses that were specified. Once a MetaModel has
been trained, it will run in *predict* mode, and predict its outputs based on
what is passed into the input variables.

.. testcode:: MetaModel_Assembly

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.doegenerators.api import FullFactorial
    from openmdao.lib.drivers.api import DOEdriver
    from openmdao.lib.optproblems.branin import BraninComponent
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate

    class Simulation(Assembly):
        def configure(self):

            #component has two inputs (x, y) and one output (f_xy)
            self.add('model', BraninComponent())
            self.model.x = 9
            self.model.y = 9

            #create meta_model for f_xy as the response
            self.add('meta_model', MetaModel(params=('x', 'y'),
                                             responses=('f_xy')))

            # use Kriging for the f_xy output
            self.meta_model.surrogates['f_xy'] = KrigingSurrogate()

            # Generate training data for the meta_model
            self.add("DOE_Trainer", DOEdriver())
            self.DOE_Trainer.DOEgenerator = FullFactorial()
            self.DOE_Trainer.DOEgenerator.num_levels = 25
            self.DOE_Trainer.add_parameter("model.x", low=0, high=20)
            self.DOE_Trainer.add_parameter("model.y", low=0, high=20)
            self.DOE_Trainer.add_response('model.f_x')

            # Pass training data to the meta model.
            self.connect('DOE_Trainer.case_inputs.model.x', 'meta_model.params.x')
            self.connect('DOE_Trainer.case_inputs.model.y', 'meta_model.params.y')
            self.connect('DOE_Trainer.case_outputs.model.f_x', 'meta_model.responses.f_x')

            # Iteration Hierarchy
            self.driver.workflow.add(['DOE_Trainer','model'])
            self.DOE_Trainer.workflow.add('model')


The first time a MetaModel runs, it trains using the data in the params and
responses variable trees, and then predicts a new response. Thereafter, it
always predicts. However, if new training data is passed in, then it will
train on the new data, and predict. This makes MetaModel more efficient,
because it is not trying to retrain the model constantly when running large
sets of training cases. Instead, the actual surrogate model training is only
done when a prediction is needed and new training data is available.



*Source Documentation for metamodel.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
