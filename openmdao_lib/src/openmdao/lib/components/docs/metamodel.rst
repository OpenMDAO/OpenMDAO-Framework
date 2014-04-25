.. index:: MetaModel

.. _MetaModel:

*MetaModel*
~~~~~~~~~~~

MetaModel is a class which supports generalized meta modeling
(a.k.a. surrogate modeling) capabilities. It has a :term:`slot` called
`model` for the component that is being approximated. As soon as a component is put in the
slot, MetaModel will automatically mirror the inputs and outputs of that
component. In other words, MetaModel will have the same inputs and
outputs as whatever component is put into the model slot. If you fill
only the `model` slot, the MetaModel instance will mimic
the underlying component.

.. testcode:: MetaModel_model

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def configure(self):

            self.add('meta_model',MetaModel())

            #component has two inputs: x,y
            self.meta_model.add('model', BraninComponent())

            #meta_model now has two inputs: x,y
            self.meta_model.x = 9
            self.meta_model.y = 9

A second slot, called ``default_surrogate`` can be filled with a surrogate
model generator instance. Copies of this default surrogate model generator
will be used for any outputs that don't have a specific surrogate model
generator associated with them. To associate a surrogate model generator with
a specific output, you must first fill the `model` slot so that MetaModel can
determine what your outputs are. When `model` is filled, MetaModel will
create a dictionary named `surrogates` where the keys are the variable names in
the model, and the values are slots. To override the default surrogate model
generator for a specific output, just drop the new surrogate model generator
into the slot whose key is the output of interest. All surrogate model
generators must implement the ISurrogate interface. OpenMDAO provides some
surrogate model generators in the ``openmdao.lib.surrogatemodels`` directory.

.. testcode:: MetaModel_slots

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def configure(self):

            self.add('meta_model', MetaModel())

            #component has two inputs: x,y
            self.meta_model.add('model', BraninComponent())

            #once the 'model' slot has been filled, a dictionary named 'surrogates' will
            #be created. The dictionary's keys are the variable names, and the values are
            #slots where a surrogate model instance can be filled. In this case we'll just put
            #another KrigingSurrogate in there, just to show how it's done. Since
            #the default_surrogate is also a KrigingSurrogate, this will give us
            #the same results we would have had if we'd only used the default_surrogate.

            # use Kriging for the f_xy output
            self.meta_model.surrogates['f_xy'] = KrigingSurrogate()

            #meta_model now has two inputs: x,y
            self.meta_model.x = 9
            self.meta_model.y = 9

Depending on the component being approximated, you may not want to generate
approximations for all the outputs. MetaModel provides two variables to give
you control over which inputs and outputs to mirror: `includes` and `excludes`. Only one
of these variables can have a non-empty value at a time.

If you are specifying the includes, then only the variables in that list will
be used. If you are specifying the excludes, then everything *but* the variables
in the list will be mirrored by MetaModel.

.. testcode:: MetaModel_excludes

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def configure(self):

            self.add('meta_model', MetaModel())
            self.meta_model.default_surrogate = KrigingSurrogate()

            #component has two inputs: x,y
            self.meta_model.add('model', BraninComponent())

            #exclude the x input
            self.meta_model.excludes = ['x']


or

.. testcode:: MetaModel_includes

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):

        def configure(self):

            self.add('meta_model',MetaModel())
            self.meta_model.default_surrogate = KrigingSurrogate()

            #component has two inputs: x,y
            self.meta_model.add('model', BraninComponent())

            #include only the y input
            self.meta_model.includes=['y']

When outputs are excluded, they no longer get mirrored by MetaModel. They won't get
surrogate models fit to them, and consequently, they won't be available to the simulation from
MetaModel. Similarly, if inputs are excluded, they won't be visible in the MetaModel, nor
will they be passed down to the simulation.  In addition, if a given input is constant for a
given training set, its value won't be passed down to the surrogate model generators as an input
to training cases.

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

To put an instance of MetaModel into the training mode, you must set the ``train_next`` event
before executing the component. This event automatically resets itself after the execution,
so it must be set again before each training case. An event is just a trigger mechanism, and
it will trigger its behavior regardless of the value you set it to.

.. testcode:: MetaModel

    from openmdao.main.api import Assembly
    from openmdao.lib.components.api import MetaModel
    from openmdao.lib.surrogatemodels.api import KrigingSurrogate
    from openmdao.lib.optproblems.branin import BraninComponent

    class Simulation(Assembly):
        def configure(self):

            self.add('meta_model',MetaModel())
            self.meta_model.default_surrogate = KrigingSurrogate()

            #component has two inputs: x,y
            self.meta_model.add('model', BraninComponent())

            self.meta_model.train_next = True
            self.meta_model.x = 2
            self.meta_model.y = 3

            self.meta_model.execute()


In a typical iteration hierarchy, a Driver is responsible for setting the
``train_next`` event when appropriate. The ``train_next`` event is added to a
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
        def configure(self):

            self.add('meta_model',MetaModel())
            self.meta_model.default_surrogate = KrigingSurrogate()

            #component has two inputs: x,y
            self.meta_model.add('model', BraninComponent())

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
