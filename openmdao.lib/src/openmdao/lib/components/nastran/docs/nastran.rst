.. index:: Nastran, NastranComponent, MSC Nastran


.. _NastranComponent:

*NastranComponent*
~~~~~~~~~~~~~~~~~~~

The NastranComponent documentation refers to MSC (MacNeal-Schwendler Corporation) Nastran. This
component is a wrapper for MSC Nastran but does not include the MSC Nastran executable. *MSC Nastran
must be installed with a valid license before this wrapper will work.* 

**Overview**

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

**Subclassing NastranComponent** 

All of NastranComponent's logic is in the ``execute`` function. The ``execute`` function reads the
traits that are connected to it (both input and output variables). It uses NastranReplacer and then
NastranMaker to update the Nastran file for the current input variables. It runs the Nastran command
by calling its superclass, ``ExternalCode``. Finally, it parses the output two ways: first, by
calling the output variable's ``nastran_func`` function in order to parse out the value from the
``FileParser`` and the ``NastranOutput`` object, and second, by calling ``NastranParser``.

What all these classes do will be explained when we discuss how to tell NastranComponent how to
process the input and output variables. 

*Controlling Nastran's Input*

To control what Nastran solves, you have to change certain variables in the Nastran input file.
NastranComponent can only insert the correct variables in the right places if you tell it where to
insert the variables. You can specify the input variables in two ways: via :ref:`NastranReplacer` or
:ref:`NastranMaker`.


**Parsing Nastran's Output**

The goal is to set output variables to certain values in Nastran's output. As with Nastran's input,
there are two ways of going about it: one involves instructing the parser to pick out a certain
location denoted by its distance from a certain anchor; the other way attempts to intelligently
parse the grid structure that most pages of output have. The second way will not work for every
case, but it's a much cleaner solution if it works.


*NastranOutput (the Crude Way)*
 
Although this method is generally not recommended, sometimes it is necessary to use it. When
specifying the design variable, you also specify a ``nastran_func`` attribute. You will specify a
function that takes one variable: a `FileParser` (from ``openmdao.util.filewrap``). The idea is that
the function you specify will be able to parse out the value you want from the FileParser. The
FileParser is a convenient way of looking for something in the text. You can specify an anchor in
the text (such as ``D I S P L A C E M E N T   V E C T O R``) and then take the value that is x lines
down and y fields across the line. You can also access the output text itself in ``filewrap.data``.

This method is not recommended because it is not very sturdy. If the data in the output file changes
significantly, and you specify the values you want by the number of fields they are away from the
beginning of the line, you may unknowingly get bad data. The other problem is that if you define two
functions in your class (perhaps a helper function and another one that returns the results), when
you pass the function that returns the results in through ``nastran_func``, it will not know where
the helper function is and  will break. (For more information on Nastran output, see
:ref:`NastranParser`.)

*Source Documentation for nastran.py*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
