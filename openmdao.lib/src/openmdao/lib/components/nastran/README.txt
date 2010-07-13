
The Nastran Component
---------------------

 * Overview

Say you are creating a component Q that is supposed to call nastran in order to calculate its outputs. You must do three things. First, your component must subclass NastranComponent. Second, you must specify how nastran will deal with your inputs. Third, you must specify how nastran will deal with your outputs. Once you do those three things, NastranComponent will worry about setting up nastran's input file (for the correct input variables), running nastran, and parsing the output values out of nastran's output.

 * Subclassing NastranComponent

NastranComponent's logic is all in the execute function. It reads the traits that are connected to it (both input and output variables). It uses NastranReplacer and then NastranMaker to update the nastran file for the current input variables. It runs the nastran command by calling its superclass, ExternalCode. Finally, it parses the output two ways: first by calling the output variable's ``nastran_func'' function in order to parse out the value out of the FileParser and the NastranOutput object, and secondly, by calling NastranParser.

I will explain what all these classes do while talking about how to tell NastranComponent how to process the input and output variables.

 - Controlling nastran's input

In order to control what Nastran solves, we have to change certain variables in the nastran input file. NastranComponent can only do insert the correct variables in the right places if you tell it where to insert the variables. There are two ways of specifying the input variables.

 * NastranReplacer (the crude way)

NastranReplacer looks at the nastran input file and replaces all instances of ``%varname'' with the current value of the design variable. The length of varname is limited to 7 characters since, along with the percent sign, it must fit in an eight character block. You can use the same placeholder in multiple places, but it will give you a warning.

The main shortcoming, and the reason why it is the crude way, is that the input variable is placed in the same block as the placeholder variable, which limits its precision. When using an optimizer with a very small step size, it's possible that the 8 characters aren't enough to distinguish between iterations.

There is a secondary mode of operation. If you specify a variable that starts with an asterisk (ie, ``%*myvar''), NsatranReplacer will overwrite the variable and keep on overwritting for the length of the value. This is useful when you want to insert a value that doesn't correspond to an 8 character wide block. The best example is that if you wanted to replace the number in the line ``METHOD 103'', if you tried replacing it with a normal variable, you would either get ``METHOD 1XXXXXXXX'' or ``XXXXXXXX03'' if we insert ``XXXXXXXX''. Using overwrite variables, we can insert ``104'' in the expression ``METHOD %*n'' and it will yield ``METHOD 104''.

 * NastranMaker

This way does not rely on placeholder variables, instead you must give it the keyword, the id, and the fieldnum to change. NastranMaker finds the right card to modify and will convert the entire card to long form. That way, you get 16 characters to express numbers. It also allows you to keep the nastran input unmodified, instead of littering it with placeholder variables.

An example:

    >>> t1 = Float(10., desc="Thickness of pshell #1",
               iotype="in",
               nastran_card="PSHELL",
               nastran_id="1",
               nastran_fieldnum=3)

Note that the nastran_card (the keyword) and the id must be strings, while the fieldnum must be an integer.

 - Parsing nastran's output

The goal is to set output variables to certain values in nastran's output. As with nastran's input, there are two ways of going about it: one involves instructing the parser to pick out a certain location denoted by its distance from a certain anchor; the other attempts to intelligently parse the grid structure that most pages of output have. The second way will not work for every case, but is a much cleaner solution if it works.

 * NastranOutput (the crude way)

This method is rather unclassy. I don't recommend using it, but sometimes it is necessary. When specifying the design variable, you also specify a nastran_func attribute. You will specify a function that takes two variables: a FileParser (from openmdao.util.filewrap) and a NastranOutput variable. The idea is that the function you specify will be able to parse out the value you want from these two objects. The first, the FileParser is a convenient way of looking for something in a text. You can specify an anchor in the text (such as ``D I S P L A C E M E N T   V E C T O R'') and then take the value that is x lines down and y fields into the line.

Using FileParser is sufficient, but in order to make most things easier, a NastranOutput variable is also handed to your function. The NastranOutput variable is essentially a dictionary of common attributes you might want. The nice thing about it is that you can access any value, and if it hasn't been previously calculated, it will be calculated at the time. This way you can make a tree of data dependencies. For example, if you want to get the displacement for all nodes, you could find ``displacement vector'' with FileParser, but in order to get all the nodes' values, you need to know how many nodes. Luckily, I've already provided an attribute to NastranOutput, ``number_of_grid_points'' that gives you want you need.

There are a few downsides about NastranOutput which make it almost unusable for complicated tasks. First, the function that you pass in via nastran_func is run in the context of NastranComponent, so you cannot call helper functions that you define elsewhere (including your own subclassed Component). A solution that might work is to have NastranComponent include some functions or classes specified along with nastran_func before running the function. The other problem is that the only helper functions you can use are defined in nastran_output_helpers.py. Currently, there aren't many since NastranParser is a much better approach. Another problem is that you cannot add helper functions unless you modify nastran_output_helpers.py. (Again, (I think) this be solved by specifying a set of functions to be treated as NastranOutput helpers, but I haven't tried it).

 * NastranParser

NastranParser tries to parse the grid out of each page of output. It identifies a header for the page, then the grid's headers, and finally its values. If it parses a page correctly, the query for information is much like querying a database, but much simpler.

    >>> a = Float(0.0, iotype="in",
              nastran_header="displacement vector",
              nastran_subcase=1, # this must be an integer
              nastran_constraints={"column name" : "value"},
              nastran_columns=["column name"])

Once these values are specified, NastranParser will try and find the header in the output, and then apply the constraints to the grid and yield a smaller grid with the viable rows and the acceptable columns (specified by nastran_columns)

NastranParser accepts the name of the header as a string of all lower case letters with sane spacing as well as the header presented in the output file (stripped of spaces at the beginning and end). Do note that as of this writing, if it cannot find the header, it will break. If it cannot find the column names you specify, it will break. Right now, even though the user specifies a smaller grid of values they want returned, the value of the variable will only be result[0][0]. This will change in future versions.

One of the main reasons for supporting retrieving multiple columns is that you can access the parser outside of design variable declaration. NastranComponent has an attribute ``parser'' which is the NastranParser after it's run nastran. After you call super(...).execute(), you could retrieve values by calling the parser's get function, in an identical fashion to the design variable declaration:

    >>> displacement_vector = self.parser.get("displacement vector",
                                              1,
                                              {"POINT ID." : "443"},
                                              ["T2"])

Do note that displacement_vector is a two dimensional array. In this example it has one value (so [[value]]), but if allowed more columns or more rows, we would get a bit bigger two dimensional array.