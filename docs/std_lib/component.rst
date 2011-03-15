
.. index:: Components

.. _Components:

Components
==========

.. index:: MetaModel

.. _MetaModel:

MetaModel
~~~~~~~~~~~

    MetaModel is a class which supports generalized meta modeling capabilities. There are two 
    sockets, one for surrogate model generators and a second for the 
    model that is being approximated. The first socket, named `surrogate`, must 
    always be filled before anything else is done. This socket gets filled with 
    a dictionary that specifies which surrogate model generator should be used for 
    which outputs. The keys of the dictionary are the variable names, and the values
    are the particular surrogate model generators which adhere to the ISurrogate
    interface. A special key, 'default', can be used to indicate a surrogate model
    generator to be used if no specific one is given for a particular variable. 
    Any specific variables specified will override the default. 
    OpenMDAO provides some surrogate modelers in ``openmdao.lib.surrogatemodels``. 
    
    .. testcode:: MetaModel_sockets
        
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
                                             
    Once the surrogate dictionary has been specified, the model socket, called 
    `model`, can be filled with a component. As soon as a component is put in the
    socket, MetaModel will automatically mirror the inputs and outputs of that 
    component. In other words, MetaModel will have the same inputs and 
    outputs as whatever component is put into the model socket. 
    
    .. testcode:: MetaModel_model
        
        from openmdao.main.api import Assembly
        from openmdao.lib.components.api import MetaModel
        from openmdao.lib.surrogatemodels.api import KrigingSurrogate
        from openmdao.examples.expected_improvement.branin_component import BraninComponent
        
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
        from openmdao.examples.expected_improvement.branin_component import BraninComponent
        
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
        from openmdao.examples.expected_improvement.branin_component import BraninComponent
        
        class Simulation(Assembly):
            
            def __init__(self): 
                super(Simulation,self).__init__(self)
                
                self.add('meta_model',MetaModel())
                self.meta_model.surrogate = {'default': KrigingSurrogate()}
                
                #component has two inputs: x,y
                self.meta_model.model = BraninComponent()
                
                #include only the y input
                self.meta_model.includes=['y']
        
    MetaModel treats inputs and outputs a little differently. All the inputs, regardless of which ones are
    being included/excluded, will be mirrored by a MetaModel. But if inputs are excluded, then MetaModel won't
    pass down their values to the surrogate models as inputs to training cases. 

    When outputs are excluded, they no longer get mirrored by MetaModel. They won't get
    surrogate models fit to them, and consequently, they won't be available to the simulation from
    MetaModel. 

    Now you have set up your MetaModel with a specific surrogate model, and you have 
    put a model into the `model` socket. The input and output 
    inclusions/exclusions have been specified. The next step is to actually start
    training and executing the MetaModel in simulations. 
    
    MetaModel has two operating modes: *training* and *prediction.* When run in *training* mode, 
    MetaModel passes its given inputs down to the model in the model socket and runs 
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
        from openmdao.examples.expected_improvement.branin_component import BraninComponent
        
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
        from openmdao.examples.expected_improvement.branin_component import BraninComponent
        
        class Analysis(Assembly): 
            def __init__(self,doc=None): 
                super(Analysis,self).__init__()
                
                self.add('branin_meta_model',MetaModel())
                self.branin_meta_model.surrogate = KrigingSurrogate()
                self.branin_meta_model.model = BraninComponent()
                
                self.add('driver',DOEdriver())
                self.driver.workflow.add('branin_meta_model')
                self.driver.add_event('branin_meta_model.train_next')
                
    When the ``train_next`` event is not set, MetaModel automatically runs in predict mode. 
    When in predict mode, the outputs provided are the result of predicted outputs from the 
    surrogate model inside of MetaModel. 
    
    Before being able to predict the surrogate model response
    for any of the outputs of MetaModel, the surrogate model must be trained with the 
    recorded training data. This will happen automatically whenever MetaModel is run in predict mode and 
    new training data is available. This makes MetaModel more efficient, because it is not trying
    to retrain the model constantly when running large sets of training cases. Instead, the actual
    surrogate model training is only done when a prediction is needed and new training data is available. 
    
    (See the source documentation for more information on :ref:`MetaModel<openmdao.lib.components.metamodel.py>`.)


.. index:: Nastran, NastranComponent, MSC Nastran

.. _`NastranComponent`:

MSC NastranComponent
~~~~~~~~~~~~~~~~~~~~~~

The following documentation refers to the MSC (MacNeal-Schwendler Corporation) Nastran Component. This component is
a wrapper for MSC Nastran, but it does not include the MSC Nastran executable. You need to have installed MSC Nastran
with a valid license before this wrapper will work. 

*Overview*
----------

If you are creating a component that is supposed to call Nastran to calculate your component's outputs,
you must do four things: 

#) Point your component to the Nastran executable, by setting the ``nastran_command`` input
#) Make your component a subclass of NastranComponent 
#) Specify how Nastran will deal with your inputs 
#) Specify how Nastran will deal with your outputs 

Once you do these things, NastranComponent will worry about setting up Nastran's input file (for the
correct input variables), running Nastran, and parsing the output values out of Nastran's output. The MSC
Nastran Component has been tested exclusively with MSC Nastran 2005, although as long as the input and
output don't change, it should work for any version. 

.. index:: NastranComponent

*Subclassing NastranComponent* 
------------------------------

All of NastranComponent's logic is in the ``execute`` function. The ``execute`` function reads the traits that are
connected to it (both input and output variables). It uses NastranReplacer and then NastranMaker to update
the Nastran file for the current input variables. It runs the Nastran command by calling its superclass,
``ExternalCode``. Finally, it parses the output two ways: first, by calling the output variable's
``nastran_func`` function in order to parse out the value from the ``FileParser`` and the ``NastranOutput`` object,
and second, by calling ``NastranParser``.

What all these classes do will be explained when we discuss how to tell NastranComponent how to process
the input and output variables. Additional details on all of the inputs to NastranComponent can be found in the
source documentation, :ref:`here<openmdao.lib.components.nastran.nastran.py>`.

.. index:: NastranMaker

*Controlling Nastran's Input*
------------------------------

To control what Nastran solves, you have to change certain variables in the Nastran input file.
NastranComponent can only insert the correct variables in the right places if you tell it where to insert
the variables. You can specify the input variables in two ways: via Nastran Replacer or NastranMaker.


.. index:: NastranReplacer

NastranReplacer (the Crude Way) 
+++++++++++++++++++++++++++++++++ 

NastranReplacer looks at the Nastran input file and replaces all instances of ``%varname`` with the current
value of the design variable. The length of ``varname`` is limited to seven characters since, along with the
percent sign, it must fit in an eight-character block. You can use the same placeholder in multiple places,
but it will give you a warning.

The main shortcoming, and the reason why it is the crude way, is that the input variable is placed in the
same block as the placeholder variable, which limits its precision. When using an optimizer with a very
small step size, it's possible that eight characters aren't enough to distinguish between iterations.

There is a secondary mode of operation. If you specify a variable that starts with an asterisk (e.g.,
``%*myvar``), NsatranReplacer will overwrite the variable and keep on overwriting for the length of the
value. This is useful when you want to insert a value that doesn't correspond to an eight-character wide
block. The best example is if you wanted to replace the number in the line ``METHOD 103``. If you tried
replacing it with a normal variable (if you insert ``XXXXXXXX``), you would get either ``METHOD 1XXXXXXXX``
or ``XXXXXXXX03``. Using overwrite variables you can insert ``104`` in the expression ``METHOD %*n``, and it
will yield ``METHOD 104``.

The asterisk variables are very useful when replacing variables that aren't in the bulk data section. When
you want to replace a bulk value (in a card), NastranMaker is much more appropriate since it understands
the bulk data format. Replacing bulk data with NastranReplacer is highly discouraged.


.. index:: NastranMaker

NastranMaker
++++++++++++++

Using NastranMaker does not rely on placeholder variables; instead, you must provide the `keyword`, the `id`, and
the `fieldnum` to change a card. NastranMaker will find the right card to modify and will convert the entire
card to long form. This way, you get 16 characters to express numbers. It also allows you to keep the Nastran
input unmodified, instead of littering it with placeholder variables. Below is an example:

::

    >>> t1 = Float(10., desc="Thickness of pshell #1",
               iotype="in",
               nastran_card="PSHELL",
               nastran_id="1",
               nastran_fieldnum=3)

Note that the ``Nastran_card`` (the keyword) and the id must be strings, while the fieldnum must be an
integer. To make sense of which fields to change, an understanding of Nastran is required. Each field
specifies a different attribute that can be modified. To find out which fields modify which attributes,
consult the Nastran documentation. (See the `MSC.Nastran 2004 Quick Reference Guide
<http://www.google.com/search?source=ig&hl=en&rlz=1G1GGLQ_ENUS331&q=MSC.Nastran+2004+Quick+Reference+Guide&aq=f&aqi=&aql=&oq=&gs_rfai=CECsdPelqTJjaLozmNJ_-vcwGAAAAqgQFT9AJml8>`_.)


In general, a sample input line will look something like this:

::

    PSHELL         8       4       3

Here, ``PSHELL`` is the keyword, or the type of thing that you're modifying. The first number is usually the
id, so in this case, it is 8. In this example, there are two attributes, with values 4 and 3, that control
something about this ``PSHELL``. As an example, for a ``PSHELL``, the second argument (4) dictates which
material card you're referencing, and the third argument (3) specifies the thickness.

*Parsing Nastran's Output*
---------------------------
The goal is to set output variables to certain values in Nastran's output. As with Nastran's input, there
are two ways of going about it: one involves instructing the parser to pick out a certain location denoted
by its distance from a certain anchor; the other way attempts to intelligently parse the grid structure that
most pages of output have. The second way will not work for every case, but it's a much cleaner solution if
it works.

NastranOutput (the Crude Way)
+++++++++++++++++++++++++++++++ 
 
Although this method is generally not recommended, sometimes it is necessary to use it. When specifying the
design variable, you also specify a ``nastran_func`` attribute. You will specify a function that takes one
variable: a `FileParser` (from ``openmdao.util.filewrap``). The idea is that the function you specify will be
able to parse out the value you want from the FileParser. The FileParser is a convenient way of looking for
something in the text. You can specify an anchor in the text (such as ``D I S P L A C E M E N T   V E C T O
R``) and then take the value that is x lines down and y fields across the line. You can also access the
output text itself in ``filewrap.data``.

This method is not recommended because it is not very sturdy. If the data in the output file changes
significantly, and you specify the values you want by the number of fields they are away from the beginning of
the line, you may unknowingly get bad data. The other problem is that if you define two functions in your class
(perhaps a helper function and another one that returns the results), when you pass the function that returns
the results in through ``nastran_func``, it will not know where the helper function is and  will break.

.. index:: NastranParser

NastranParser
+++++++++++++++

NastranParser tries to parse the grid out of each page of output. It identifies 1) a header for the page, then
2) the grid's headers, and finally 3) its values. If it parses a page correctly, the query for information is
much like querying a database, but much simpler. See the following example.

::

    >>> a = Float(0.0, iotype="out",
              nastran_header="displacement vector",
              nastran_subcase=1, # this must be an integer
              nastran_constraints={"column name" : "value"},
              nastran_columns=["column name"])

Once these values are specified, NastranParser will try to find the header in the output, then apply
the constraints to the grid, and yield a smaller grid with the viable rows and the acceptable columns
(specified by ``nastran_columns``). Note that ``a`` is a two-dimensional Python array. Each row will be a row
in a grid and will contain only the columns listed in ``nastran_columns``.

NastranParser accepts the name of the header as a string of all lower case letters with sane spacing as
well as the header presented in the output file (stripped of spaces at the beginning and end). 

.. note:: As of this writing, if it cannot find the header, it will break. If it cannot find the column names
   you specify, it will break. Right now, even though you specify a smaller grid of values than you want
   returned, the value of the variable will be only ``result[0][0]``. This will change in future versions.

One of the main reasons to support retrieving multiple columns is that you can access the parser
outside of design variable declaration. NastranComponent has an attribute ``parser``, which is the
NastranParser after it's run Nastran. After you call ``super(...).execute()``, you could retrieve values by
calling the parser's ``get`` function, in an identical fashion to the design variable declaration:

::

    >>> displacement_vector = self.parser.get("displacement vector",
                                              1,
                                              {"POINT ID." : "443"},
                                              ["T2"])

Do note that ``displacement_vector`` is a two-dimensional array. In this example, it has one value
(``[[value]]``), but if more columns or more rows were allowed, you would get a bit bigger two-dimensional
array. 


``self.parser.get`` has an optional argument that is useful in parsing grids that have more than one value
per column. A good example can be found in ``test/practice-grid.row-width.txt``. As you can see, if you
wanted to select the data for element id 1, you'd actually want those 15 rows of data. So, you invoke ``get``
with the optional argument ``row_width``. By using ``row_width``, once you find a row that satisfies your
constraints, it'll include the remaining (``row_width-1``) rows in the output.

It is important to understand how NastranParser works. It is a heuristic-based parser. This means that the
developers have built something that correctly identifies most grids that they have thrown at it. Since
there is no official Nastran output specification, it might not work on your grid. This is a known problem
without a known solution.

Another, perhaps more pressing, problem is that NastranParser uses the data in the grid to help the parsing
task. This means that if the data changes significantly, you *could* get different parses. While this is
not very likely, it is a possibility. Currently, if this happens, the hope is that the ``get`` function
will break because you'll try to access a column that NastranParser doesn't recognize. While this is a real
problem, it is not showstopping because most of the time NastranParser will parse the grid correctly
regardless and because, under most runs, the data doesn't undergo drastic changes. One example of a drastic
change would be omitting an entire column of values during one execution and then having values in the
next iteration. Another example would be going from a floating point number to ``0.0``. The problem is that the
floating point numbers are long and usually block unnecessary columns from forming. But if there is a
column of ``0.0``, the parsing problem might think there's an extra column. If you are worried about
inconsistencies in parsing, you could isolate the particular grid you are parsing and change.

.. index:: NastranComponent

*NastranComponent*
------------------

We've gone over the parts that make NastranComponent work, but what about NastranComponent itself?
Essentially, it just passes off the work to its subparts. You should be aware of some additional information
to take maximum advantage of its utilities. 

.. index:: nastran_make_hook

To use NastranMaker without actually defining the traits in your subclass, you can implement the function
``nastran_maker_hook`` in your subclass. This function will be called with one argument, the ``NastranMaker``
object. It is called after it has processed all the input variables that are visible on traits. The
function's return is ignored. Right after it finishes, ``NastranMaker`` writes out the Nastran file that will
be run.   
