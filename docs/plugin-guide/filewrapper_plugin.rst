
.. _Building-a-Plugin-Using-a-File-Wrapper:

Building a Plugin that Contains a File Wrapper
==============================================

For many legacy codes, the only viable way to include them in an MDAO process
is through file-wrapping. In a file-wrapped component, the inputs are passed
and the outputs are extracted via input and output files. In order to
facilitate this process, OpenMDAO includes several utilities that will be
described in this section.

There are three phases during the execution of a file-wrapped component:

- Generating an input file that contains OpenMDAO variable values.
- Executing the external code
- Parsing the output file and extracting values to place in OpenMDAO

A file-wrapped component must perform all three of these tasks in its 'execute'
function.

Presently, the only way to create a file-wrapped component involves writing some
Python code, so a basic knowledge of Python is required to proceed.


.. _`A-Note-on-Precision`:

A Note on Precision
---------------------

In a file-wrapped component, all key inputs for the external code come from
an intermediate file that must be written. It is important to be careful to
prevent the loss of precision when generating the input file. Consider a 
Variable with 15 digits of precision.

.. doctest:: precision

    >>> val = 3.1415926535897932
    >>>
    >>> val
    3.1415926535897931
    >>>
    >>> print val
    3.14159265359
    >>>
    >>> print "%s" % str(val)
    3.14159265359    
    >>>
    >>> print "%f" % val
    3.141593
    >>> 
    >>> print "%.16f" % val
    3.1415926535897931
    
If the variable's value in the input file is created using the `print`
statement, only 11 digits of precision are in the generated output. The same
is true if we convert the value to a string and use string output formatting.
Printing the variable as a floating point number with no format string gives
even less precision. In order to output the full precision of a variable, the
decimal precision must be specified using formatted output (i.e., '%.16f').

All of this quibbling over the 11th-15th decimal place may sound unnecessary,
but some applications are sensitive to changes of this magnitude. Moreover, it
is important to consider how your component may be used during optimization. A
gradient optimizer will often use a finite-difference scheme to calculate the
gradients for a model, and this means that some component inputs might be
subjected to small increments and decrements. A loss of precision here can
completely change the calculated gradient and prevent the optimizer from
reaching a correct minimum value.

The file-wrapping utilities in OpenMDAO use '%.16g'. If you write your own
custom input-file generator for a new component, you should use this format
for the floating point variables.

Precision is also important when parsing the output, although the file parsing
utilities always grab the entire number. However, some codes limit the number
of digits of precision in their output files for human-readability. In such a
case, you should check the manual for your external application to see if
there is a flag to tell the code to output the full precision.
    

.. _`Running-the-External-Code`:

Running the External Code
-------------------------

The ExternalCode component takes care of the mundane tasks associated with the
execution of the external application. These include:

- Making the system call using the Subprocess module
- Redirecting stdin, stdout, and stderr to the user's specification
- Capturing error codes
- Defining environment variables
- Handling timeout and polling
- Running the code on a remote server if required (forthcoming)

So, we recommended that you always derive your file-wrapped component from
the `ExternalCode` base class. The following example shows how you would
do this for a simple component.

.. testcode:: External_Code

    from openmdao.lib.datatypes.api import Float
    from openmdao.lib.components.api import ExternalCode
    
    # The following will be used for file wrapping, see the next sections
    from openmdao.util.filewrap import InputFileGenerator, FileParser    
    
    class WrappedComp(ExternalCode):
        """A simple file wrapper."""
        
        # Variables go here
        var_input = Float(3.612, iotype='in', desc='A floating point input')
        var_output = Float(3.612, iotype='out', desc='output')
        
        def execute(self):
            """ Executes our file-wrapped component. """
            
            # (Prepare input file here)

            #Execute the component
            super(Wrapped_Comp, self).execute()
            
            # (Parse output file here)

This component still needs one more piece of information -- the command
string that runs the external code. The ExternalCode object has an attribute
named `command` which takes the command string. So, if we want to execute a
code that we normally would run by typing this at the command prompt:

::

        /usr/bin/externalcode -v -r1

then we need to set the command attribute as follows:

.. testcode:: External_Code

    MyComp = WrappedComp()
    MyComp.command = '/usr/bin/externalcode -v -r1'
    
Note, you could also declare this in the `__init__` function of WrappedComp,
if it is something that you don't expect will need to be changed by the user.
The same is true of the other attributes described below.

This example is ready to execute, although it is missing the code that writes
out the input file and parse the output file. Subsequent sections explain how
to write these.

The ExternalCode object also allows you to specify stdout, stdin, and stderr. For
example, what if your application handled input and output on the command line
using stdout and stdin as such:
            
::

        /usr/bin/externalcode -v -r1 < myinput.txt > myoutput.txt
        
We can tell ExternalCode to append these to our command line by setting the
following attributes:

.. testcode:: External_Code

    MyComp.command = '/usr/bin/externalcode -v -r1'
    MyComp.stdin = 'myinput.txt'
    MyComp.stdout = 'myoutput.txt'
    MyComp.stderr = 'myerror.log'
    
Note that we don't just paste it all into the command string, particularly if
we want to assure cross-platform compatibility. We also went ahead and
captured the stderr output into a file called myerror.log.

If you would like to redirect stderr to stdout, you can use the following:

.. testcode:: External_Code

    MyComp.stderr = MyComp.STDOUT
    
Note the capital letters in STDOUT. We've saved the special symbol STDOUT that
is given in the subprocess module as a convenience.

Sometimes execution of a code requires you to set environment variables,
possibly to define paths that are needed to search for dynamic libraries, etc.
The ExternalCode allows you to define variables for the execution environment
using the dictionary `env_vars`.

.. testcode:: External_Code

    MyComp.env_vars = { 'LIBRARY_PATH' : '/usr/local/lib' }
    
The ExternalCode component also allows you to manage the polling rate and the
timeout values. Timeout is a measure of maximum time to wait for the code to
complete its execution. If a component takes longer than the given timeout value,
then the process will end with a timeout error. Note that the default
timeout is 0, which means no timeout. The polling rate can also be adjusted
by setting the poll_delay attribute. Note that if it is not set, an internally
computed value is used (and this value is most likely fine.)

.. testcode:: External_Code

    MyComp.timeout = 120
    MyComp.poll_delay = 10

This capability proved useful in a recent case with an analysis code that
occasionally got caught in an infinite loop. It was known that a single
execution of that code never exceeded 1 minute, so a 60 second timeout was
used to terminate the execution so that the inputs could be tweaked and tried
again. The poll_delay attribute is mainly useful for reducing the rate that
the process is polled. There is no reason to poll every second if the code
normally takes hours to run.

Finally, if your code returns some kind of error or status code, you should
check it with this attribute.

.. testcode:: External_Code

    print MyComp.return_code
    
.. testoutput:: External_Code
    :hide:

    0

Generating the Input File - Templated File I/O
----------------------------------------------

There are two different ways to generate an input file for an external
application. The first way is to write the file completely from scratch
using the new values that are contained in the component's variables. There
is not much that can be done to aid with this task, as it requires knowlege
of the file format and can be completed using Python's standard formatted
output. One exception to this is the FORTRAN namelist, which is more of a
standard output format. Some tools to help create namelist input files
are given in the next section.

The second way to generate an input file is by templating. A template file is
a sample input file which can be processed by a templating engine to insert
new values in the appropriate locations. Often the template file is a valid
input file before being processed, though other times it contains directives
or conditional logic to guide the generation. Obvously this method works well
for cases where only a small number of the possible variables and settings are
being exposed to manipulation by outside components.

OpenMDAO includes a basic templating capability that allows a template file to
be read, fields to be replaced with new values, and an input file to be
generated so that the external application can read it. Suppose we have an
input file that contains some integer, floating point, and string inputs:

::

    INPUT
    1 2 3
    INPUT
    10.1 20.2 30.3
    A B C
    
This is a valid input file for our application, and it can also be used as a
template file. The templating object is called `InputFileGenerator`, and it
includes methods that can replace specific fields as measured by their row
and field numbers. 

To use the InputFileGenerator object, first instantiate it and give it the
name of the template file, and the name of the output file that we want to
produce. (Note that this code will need to be placed in the execute function
of your component *before* the external code has been run. See :ref:`Running the
External Code`.) The code will generally look like this:

::

    from openmdao.util.filewrap import InputFileGenerator

    parser = FileParser()
    parser.set_template_file('mytemplate.txt')
    parser.set_generated_file('myinput.txt')
    
    # (Call functions to poke new values here)
    
    parser.generate()

When the template file is set, it is read into memory so that all subesquent
replacements are done without writing the intermediate file to the disk. Once
all replacements have been made, the generate method is called to create the
input file.

.. testcode:: Parse_Input
    :hide:
    
    from openmdao.util.filewrap import InputFileGenerator
    parser = InputFileGenerator()
    from openmdao.main.api import Component
    self = Component()
    
    # A way to "cheat" and do this without a file.
    parser.data = []
    parser.data.append("INPUT")
    parser.data.append("1 2 3")
    parser.data.append("INPUT")
    parser.data.append("10.1 20.2 30.3")
    parser.data.append("A B C")

Let's say we want to grab the replace the second integer with a 7. The code
would look like this.
    
.. testcode:: Parse_Input

    parser.mark_anchor("INPUT")
    parser.transfer_var(7, 1, 2)
    
.. testcode:: Parse_Input
    :hide:
    
    for datum in parser.data:
        print datum
    
.. testoutput:: Parse_Input

    INPUT
    1 7 3
    INPUT
    10.1 20.2 30.3
    A B C
    
The method `mark_anchor` is used to define an anchor, which becomes the
starting point for the `transfer_var` method. Here, we find the 2nd field in
the 1st line down from the anchor, and replace it with the new value.

Now, what if we want to replace the third value of the floating point numbers
after the second INPUT statement. An additional argument can be passed to the
`mark_anchor` method to tell it to start at the 2nd instance of the text
fragment "INPUT".

.. testcode:: Parse_Input

    parser.mark_anchor("INPUT", 2)
    
    my_var = 3.1415926535897932
    parser.transfer_var(my_var, 1, 3)
    
.. testcode:: Parse_Input
    :hide:
    
    for datum in parser.data:
        print datum
    
.. testoutput:: Parse_Input

    INPUT
    1 7 3
    INPUT
    10.1 20.2 3.141592653589793
    A B C
    
Note that we are able to pass a floating point value to `transfer_var` and still
keep 15 digits of precision. See :ref:`A-Note-on-Precision` for a discussion on
why this is important.
    
We can also count backwards from the bottom of the file by passing a negative
number. Here, the second instance of "INPUT" from the bottom brings us
back to the first one.

.. testcode:: Parse_Input

    parser.mark_anchor("INPUT", -2)
    parser.transfer_var("99999", 1, 1)
    
.. testcode:: Parse_Input
    :hide:
    
    for datum in parser.data:
        print datum
    
.. testoutput:: Parse_Input

    INPUT
    99999 7 3
    INPUT
    10.1 20.2 3.141592653589793
    A B C
    
There is also a method for replacing an entire array of values. Let's try
replacing the set of three integers.

.. testcode:: Parse_Input

    from numpy import array
    
    array_val = array([123, 456, 789])

    parser.mark_anchor("INPUT")
    parser.transfer_array(array_val, 1, 1, 3)
    
.. testcode:: Parse_Input
    :hide:
    
    for datum in parser.data:
        print datum.rstrip()
    
.. testoutput:: Parse_Input

    INPUT
    123 456 789
    INPUT
    10.1 20.2 3.141592653589793
    A B C

The method `transfer_array` takes 4 required inputs. The first is an array
of values that will become the new values in the file. The second is the
starting row after the anchor. The third is the starting field that will be
replaced, and the fourth is the ending field. The new array replaces the
block of fields spanned by starting field and ending field.

It is also possible to use the transfer_array method to 'stretch' an existing
array in a template to add more terms.

.. testcode:: Parse_Input

    from numpy import array
    
    array_val = array([11, 22, 33, 44, 55, 66])

    parser.mark_anchor("INPUT")
    parser.transfer_array(array_val, 1, 1, 3, sep=' ')
    
.. testcode:: Parse_Input
    :hide:
    
    for datum in parser.data:
        print datum.rstrip()
    
.. testoutput:: Parse_Input

    INPUT
    11 22 33 44 55 66
    INPUT
    10.1 20.2 3.141592653589793
    A B C

The named argument 'sep' is used to define what separate to include between the
additional terms of the array. Future revisions of InputFileGenerator will
hopefully be able to detect this automatically.

The input file templating capability that comes with OpenMDAO is basic but
quite functional. If you need a more powerful templating engine, particularly
one that allows the inclusion of logic in your template files, then it may be
beneficial to consider one of the community-developed engines such as mako_
or django_.

.. _mako: http://www.makotemplates.org/

.. _django: http://docs.djangoproject.com/en/dev/topics/templates/

.. todo:: Include some examples with one of the templating engines.

Generating the Input File - FORTRAN Namelists
---------------------------------------------

Since legacy FORTRAN are expected to be a frequent candidate for
file-wrapping, a library for reading and generating FORTRAN namelist files has
been included. The syntax for a namelist varies somewhat, depending on the
FORTRAN implementation, but the format generally looks like this:

::

   NAME
   ! Comment string
   &GROUP1 
    XREAL =  1.0e33,
    XINT = 2,
    XCHAR = 'namelist', 
    XBOOL = T/
   &GROUP2
    AREAL =  1.  1.  2.  3., 
    AINT = 2 2 3 4, 
    ACHAR = 'aaa' 'bbb' 'ccc' ' ddd', 
    ABOOL = T T F F/

The namelist utility includes functions to generate a valid namelist file from
a component's set of input variables. There are also functions that can be
used to parse a namelist file, and load the variable data back into an
OpenMDAO component's variables (which can be useful for populating a component
with new values.)

For example, let's consider a component whose inputs include 5 variables of
various types. A component that writes out the an input file as a single
namelist called `MAIN` would look like this:

.. testcode:: Namelist

    from numpy import array
    
    from openmdao.lib.datatypes.api import Float, Int, Str, Bool, Array
    from openmdao.lib.components.api import ExternalCode
    
    from openmdao.util.namelist_util import Namelist
    
    class WrappedComp(ExternalCode):
        """A simple file wrapper."""
        
        xreal = Float(35.6, iotype='in', desc='A floating point input')
        xint = Int(88, iotype='in', desc='An integer input')
        xchar = Str("Hello", iotype='in', desc='A string input')
        xbool = Bool("True", iotype='in', desc='A boolean input')
        areal = Array(array([1.0, 1.0, 2.0, 3.0]), iotype='in', desc='An array input')
        
        def execute(self):
            """ Executes our file-wrapped component. """
            
            self.stdin = "FileWrapTemplate.txt"
            sb = Namelist(self)
            sb.set_filename(self.stdin)

            # Add a Title Card
            sb.set_title("My Title")
            
            # Add a group. Subsequent variables are in this group
            sb.add_group('main')
            
            # Toss in a comment
            sb.add_comment(' ! Comment goes here')
            
            # Add all the variables
            sb.add_var("xreal")
            sb.add_var("xint")
            sb.add_var("xchar")
            sb.add_var("xbool")
            sb.add_var("areal")
            
            # Add an internal variable
            sb.add_new_var("Py", 3.14)
            
            # Generate the input file for FLOPS
            sb.generate()

Note that this component is derived from ExternalCode, and uses a few of its
features, so it is important to read :ref:`Running the External Code` before
proceeding.

In the `execute` method, a *Namelist* object is instantiated. This object
allows you to sequentially build up a namelist input file. The only argument
is 'self', which is passed because the Namelist object needs to access your
component's OpenMDAO variables in order to automatically determine the data
type. The `set_filename` method is used to set the name of the input file that
will be written. Here, we just pass it the variable self.stdin, which is part
of the ExternalCode API.

The first card we create for the Namelist is the title card, which is
optionally assigned with the `set_title` method. After this, the first
namelist group is declared with the `add_group` method. Subsequent variables
are added to this namelist grouping. If `add_group` is called again, the
current group is closed and any further variables are added to the new one.

The `add_var` method is used to add a variable to the Namelist. The only
needed argument is the variable's name in the component. The variable's type
is used to determine what kind of namelist variable to output. If you need to
add something to the namelist that isn't contained in one of the component's
variables, then use the add_new_var method, giving it a name and a value as
arguments.

There is also an add_comment method that let's you add a comment to the
namelist. Of course, this isn't an essential function, but there are time you
may want to add comments to enhance readability. The comment text should
include the comment character. Note that the namelist format doesn't require a
comment character, but it's still a good practice.

Finally, once every variable, group, and comment has been assigned, use the
`generate` method to create the input file. If a variable was entered
incorrectly, or if you have given it a variable type that it doesn't know how
to handle (e.g., and Instance or a custom variable), an exception will be
raised. Otherwise, the input file is created, and your `execute` function can
move on to running your code.

Parsing a Namelist File
~~~~~~~~~~~~~~~~~~~~~~~~

The Namelist object also includes some functions to parse a namelist file and
load the variable values into a component's list of variables. This can be
useful for loading in models that were developed when your code was executed
standalone.

.. todo:: Write about the namelist parsing functions.

Parsing the Output File
-----------------------

When an external code is executed, it typically outputs the results into a
file. OpenMDAO includes a few things to ease the task of extracting the
important information out of a file.

Basic Extraction
~~~~~~~~~~~~~~~~

Let's consider an application that produces the following as part of its
text-file output:

::

    LOAD CASE 1
    STRESS 1.3334e7 3.9342e7 NaN 2.654e5
    DISPLACEMENT 2.1 4.6 3.1 2.22234
    LOAD CASE 2
    STRESS 11 22 33 44 55 66
    DISPLACEMENT 1.0 2.0 3.0 4.0 5.0

As part of our file wrap, we need to reach into this file and grab the
information that is needed by other downstream components in the model.
OpenMDAO includes an object called `FileParser`, which contains functions to
parse a file, grab the fields you specify, and apply them to the appropriate
data type. For this to work, the file has to have some general format that
would allow us to locate the piece of data we need relative to some constant
feature in the file. In other words, the main capability of the FileParser is
to locate and extract a set of characters that is some number of lines and
some number of fields away from an 'anchor' point.

::

    from openmdao.util.filewrap import FileParser

    parser = FileParser()
    parser.set_file('myoutput.txt')
    
To use the FileParser object, first instantiate it and give it the name of the
output file. (Note that this code will need to be placed in the execute
function of your component *after* the external code has been run. See
:ref:`Running the External Code`.)

.. testcode:: Parse_Output
    :hide:
    
    from openmdao.util.filewrap import FileParser
    parser = FileParser()
    from openmdao.main.api import Component
    self = Component()
    
    # A way to "cheat" and do this without a file.
    parser.data = []
    parser.data.append("LOAD CASE 1")
    parser.data.append("STRESS 1.3334e7 3.9342e7 NaN 2.654e5")
    parser.data.append("DISPLACEMENT 2.1 4.6 3.1 2.22234")
    parser.data.append("LOAD CASE 2")
    parser.data.append("STRESS 11 22 33 44 55 66")
    parser.data.append("DISPLACEMENT 1.0 2.0 3.0 4.0 5.0")

Let's say we want to grab the first "STRESS" value from each load case in the
file snippet shown above. The code would look like this. (Note: the print
statement is only there for display in this example.)
    
.. testcode:: Parse_Output

    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_var(1, 2)
    
    print "%g is a %s" % (var, type(var))
    self.xreal = var

.. testoutput:: Parse_Output

    1.3334e+07 is a <type 'float'>
    
The method `mark_anchor` is used to define an anchor, which becomes the
starting point for the `transfer_var` method. Here, we grab the value from the
2nd field in the 1st line down from the anchor. The parser is smart enough to
recognize the number as floating point, and to create a Python float variable.
The final statement assigns this value to the component variable `xreal`.

The third value of stress is NaN. If we want to grab that element:

.. testcode:: Parse_Output

    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_var(1, 4)
    
    print "%g" % var

.. testoutput:: Parse_Output

    nan
    
Python also has built-in values for nan and inf that are valid for *float*
variables. The parser can recognize them when it encounters them in a file.
This gives you the ability to catch numerical numerical overflows, underflows,
etc. and take action. Numpy includes the functions `isnan` and `isinf` to test
for "nan" and "inf" respectively.

.. testcode:: Parse_Output

    from numpy import isnan, isinf
    
    print isnan(var)
    
.. testoutput:: Parse_Output

    True

When the data is not a number, it is recognized as a string. Let's grab the
word "DISPLACEMENT".
    
.. testcode:: Parse_Output

    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_var(2, 1)
    
    print var

.. testoutput:: Parse_Output

    DISPLACEMENT
    
Now, what if we want to grab the value of stress from the second load case. An
additional argument can be passed to the `mark_anchor` method to tell it to
start at the 2nd instance of the text fragment "LOAD CASE".

.. testcode:: Parse_Output

    parser.mark_anchor("LOAD CASE", 2)
    var = parser.transfer_var(1, 2)
    
    print var

.. testoutput:: Parse_Output

    11
    
We can also count backwards from the bottom of the file by passing a negative
number. Here, the second instance of "LOAD CASE" from the bottom brings us
back to the first one.

.. testcode:: Parse_Output

    parser.mark_anchor("LOAD CASE", -2)
    var = parser.transfer_var(1, 2)
    
    print "%g" % var

.. testoutput:: Parse_Output

    1.3334e+07
    

Array Extraction
~~~~~~~~~~~~~~~~

Let's consider the same application that produces the following as part of its
text-file output:

::

    LOAD CASE 1
    STRESS 1.3334e7 3.9342e7 NaN 2.654e5
    DISPLACEMENT 2.1 4.6 3.1 2.22234
    LOAD CASE 2
    STRESS 11 22 33 44 55 66
    DISPLACEMENT 1.0 2.0 3.0 4.0 5.0

This time, we'd like to grab all of the displacements in one read, and store
them as an array. This can be done with the `transfer_array` method.

.. testcode:: Parse_Output

    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_array(2, 2, 2, 5)
    
    print var

.. testoutput:: Parse_Output

    [ 2.1      4.6      3.1      2.22234]

The `transfer_array` method takes 4 arguments -- starting row, starting field,
ending row, and ending field. The parser extracts all values from the starting
row and field and continuing until it hits the ending field in the ending row.
These values are all placed in a 1D array. When extracting multiple lines, if
a line break is hit, the parser continues reading from the next line until the
last line is hit. The following extraction illustrates this:

.. testcode:: Parse_Output

    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_array(1, 3, 2, 4)
    
    print var

.. testoutput:: Parse_Output

    ['39342000.0' 'nan' '265400.0' 'DISPLACEMENT' '2.1' '4.6' '3.1']
    
With the inclusion of "DISPLACEMENT", this is returned as an array of strings, so
care must be taken.

Functions to extract multi-dimensional arrays are forthcoming. For now, please
use `transfer_var` and `transfer_array` to read the data and load it into your
array.

Delimiters
~~~~~~~~~~

When the parser counts fields in a line of output, it determines the field boundaries
by comparing against a set of delimiters. These delimiters can be changed using the
`set_delimiters` method. By default, the delimiters are the general white space
characters space (' ') and tab ('\t'). The newline characters ('\n' and '\r') are also
always removed regardless of the delimiter status.

One common case that will require a change in the default delimiter is the comma
separated file (i.e, csv). Here's an example of such an output file:

::

    CASE 1
    3,7,2,4,5,6

.. testcode:: Parse_Output
    :hide:
    
    parser.data = []
    parser.data.append("CASE 1")
    parser.data.append("3,7,2,4,5,6")
    
If we try grabbing the first element without changing the delimiters:

.. testcode:: Parse_Output

    parser.mark_anchor("CASE")
    var = parser.transfer_var(1, 2)
    
    print var

.. testoutput:: Parse_Output

    ,7,2,4,5,6
    
What happend here is slightly confusing, but the main point is that the parser
did not handle this as expected because commas were not in the set of
delimiters. Now let's specify commas as our delimiter.

.. testcode:: Parse_Output

    parser.mark_anchor("CASE")
    parser.set_delimiters(", ")
    var = parser.transfer_var(1, 2)
    
    print var

.. testoutput:: Parse_Output

    7

With the correct delimiter set, we extract the 2nd integer as expected.
    
Special Case Delimiter - Columns
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
