.. index:: plugin; building using a file wrapper

.. index:: file wrapper

.. _Building-a-Plugin-Using-a-File-Wrapper:

Building a Plugin that Contains a File Wrapper
==============================================

For many legacy codes, the only viable way to include them in an MDAO process
is through file wrapping. In a file-wrapped component, the inputs are passed
and the outputs are extracted via input and output files. To
facilitate this process, OpenMDAO includes several utilities that will be
described in this section.

The execution of a file-wrapped component consists of the following three phases:

- Generating an input file that contains OpenMDAO variable values
- Executing the external code
- Parsing the output file and extracting values to place in OpenMDAO

A file-wrapped component must perform all three of these tasks in its ``execute``
function.

Presently, to create a file-wrapped component you must write some
Python code, so you need a basic knowledge of Python to proceed.

.. index:: precision in file-wrapped component


.. _`A-Note-on-Precision`:

A Note on Precision
---------------------

In a file-wrapped component, all key inputs for the external code come from an intermediate file
that must be written. When generating the input file, it is important to prevent the loss of
precision. Consider a variable with 15 digits of precision.

.. doctest:: precision

    >>> val = 3.1415926535897932
    >>>
    >>> val
    3.141592653589793...
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
    3.141592653589793...
    
If the variable's value in the input file is created using the ``print``
statement, only 11 digits of precision are in the generated output. The same
is true if you convert the value to a string and use string output formatting.
Printing the variable as a floating point number with no format string gives
even less precision. To output the full precision of a variable, you must specify
decimal precision using formatted output (i.e., ``"%.16f"``).

Quibbling over the 11th--15th decimal place may sound unnecessary,
but some applications are sensitive to changes of this magnitude. Moreover, it
is important to consider how your component may be used during optimization. A
gradient optimizer will often use a finite-difference scheme to calculate the
gradients for a model, and this means that some component inputs might be
subjected to small increments and decrements. A loss of precision here can
completely change the calculated gradient and prevent the optimizer from
reaching a correct minimum value.

The file-wrapping utilities in OpenMDAO use ``"%.16g"``. If you write your own
custom input-file generator for a new component, you should use this format
for the floating point variables.

Precision is also important when parsing the output, although the file-parsing
utilities always grab the entire number. However, some codes limit the number of
digits of precision in their output files for human readability. In such a case,
you should check your external application's manual to see if there is a flag for
telling the code to output the full precision.

.. index:: ExternalCode component

.. _`Running-the-External-Code`:

Running the External Code
-------------------------

The ExternalCode component takes care of the mundane tasks associated with 
executing the external application. These include:

- Making the system call using the Subprocess module
- Redirecting `stdin, stdout,` and `stderr` to the user's specification
- Capturing error codes
- Defining environment variables
- Handling timeout and polling
- Running the code on a remote server if required

So we recommend that you always derive your file-wrapped component from
the ExternalCode base class. The following example shows how to do this for a simple component.

Let's consider a simple application called `externalcode`, which takes one
input and returns one output. The input is specified with an input file called
``myinput.txt``, and the output is printed in a file called ``myoutput.txt``. The
name of the input file that externalcode expects is hard-coded and cannot be
changed. We want to create an OpenMDAO component that generates the input
file, runs the code, and reads the data from the output file every time the
component runs.

To do this, we create a component that looks like this:

.. testcode:: External_Code

    from openmdao.lib.datatypes.api import Float
    from openmdao.lib.components.api import ExternalCode
    from openmdao.main.api import FileMetadata
    
    # The following will be used for file wrapping, see the next sections
    from openmdao.util.filewrap import InputFileGenerator, FileParser    
    
    class WrappedComp(ExternalCode):
        """A simple file wrapper."""
        
        # Variables go here
        var_input = Float(3.612, iotype='in', desc='A floating point input')
        var_output = Float(6.312, iotype='out', desc='output')

        def __init__(self):
            """Constructor for the PdcylComp component"""
    
            super(WrappedComp, self).__init__()
    
            # External Code public variables
            self.input_file = 'myinput.txt'
            self.output_file = 'myoutput.txt'
            self.stderr = 'myerror.log'
            
            self.external_files = [
                FileMetadata(path=self.input_file, input=True),
                FileMetadata(path=self.output_file),
                FileMetadata(path=self.stderr),
            ]
            
        def execute(self):
            """ Executes our file-wrapped component. """
            
            # (Prepare input file here)

            # Execute the component
            super(WrappedComp, self).execute()
            
            # (Parse output file here)

Instead of deriving our OpenMDAO component from ``component``, we derive
it from ``ExternalCode`` to take advantage of the features listed above. Just
like in other components, we define variables for inputs and outputs.

In the ``__init__`` function, we can define the names of the input and output
files that the external code needs to run. Here we create two new private
variables ``self.input_file`` and ``self.output_file`` to store these filenames.
The name of these variables is not important. We also define a file for
redirection of `stderr`. The variable name `stderr` is inherited from the
ExternalCode component. Finally, the ``__init__`` function also contains a
block of code where these three files are added to the component's
FileMetadata. This assures that when the model containing this component is
saved to an egg, these files are always packed up and included in that egg.
This is also necessary to support running the code on a remote server.

As with other components, the actual component execution occurs in the
``execute`` method. Notice that the ExternalCode component takes care of
running the external code, so all we have to do is call the ``execute``
method of our base class via the *Super* call. We would still need to generate
the input file and parse the output file. This example includes a placeholder
comment for each of these tasks. More detail about what goes there can be
found in the sections that follow.

To run, this component still needs one more piece of information --
the command string that runs the external code. The ExternalCode object has an
attribute named `command` which takes the command as a list of strings.
So, if you want to execute a code that you normally run by typing

::

        /usr/bin/externalcode -v -r1
        /usr/bin/externalcode -v -r1

at the command prompt, then you need to set the command attribute as follows:

.. testcode:: External_Code

    MyComp = WrappedComp()
    MyComp.command = ['/usr/bin/externalcode', '-v', '-r1']
    
Note that you could also declare this in the ``__init__`` method of ``WrappedComp`` if it
is something that you don't expect the user will need to change. The same is true
of the other attributes described below.

This example is ready to execute, although it is missing the code that writes
out the input file and parses the output file. Subsequent sections explain how
to write these.

The ExternalCode object also allows you to specify `stdout, stdin,` and `stderr`.
For example, if your application handled input and output on the command line
using a redirection of `stdout` or `stdin` as such:

::

        /usr/bin/externalcode -v -r1 < myinput.txt > myoutput.txt
        
you can let ExternalCode handle the redirection for you. When the following
attributes are set, the redirection is automatically appended to your command
line.

.. testcode:: External_Code

    MyComp.command = ['/usr/bin/externalcode', '-v', '-r1']
    MyComp.stdin = 'myinput.txt'
    MyComp.stdout = 'myoutput.txt'
    
Note that you don't just paste everything into the command string, particularly if you
want to assure cross-platform compatibility.

In the example, we captured the `stderr` output into a file called
``myerror.log``. If you would like to redirect `stderr` to `stdout,` you can
use the following:

.. testcode:: External_Code

    MyComp.stderr = MyComp.STDOUT
    
Note the capital letters in ``STDOUT``. We've saved the special symbol ``STDOUT`` that
is given in the subprocess module as a convenience.

Sometimes for code to execute you must set environment variables,
possibly to define paths that are needed to search for dynamic libraries, etc.
The ExternalCode allows you to define variables for the execution environment
using the dictionary ``env_vars``.

.. testcode:: External_Code

    MyComp.env_vars = { 'LIBRARY_PATH' : '/usr/local/lib' }
    
The ExternalCode component also allows you to manage the polling rate and the
timeout values. `Timeout` is a measure of the maximum time to wait for the code to
complete its execution. If a component takes longer than the given timeout value,
then the process will end with a timeout error. Note that the default
timeout is 0, which means no timeout. The polling rate can also be adjusted
by setting the ``poll_delay`` attribute. Note that if it is not set, an internally
computed value is used (and this value will most likely be fine.)

.. testcode:: External_Code

    MyComp.timeout = 120
    MyComp.poll_delay = 10

This capability proved useful in a recent case with an analysis code that
occasionally got caught in an infinite loop. A single execution of that code had
never exceeded 1 minute, so a 60-second timeout was used to terminate the execution
so that the inputs could be tweaked and tried again. The ``poll_delay`` attribute is
mainly useful for reducing the polling rate. There is no reason
to poll every second if the code normally takes hours to run.

Finally, if your code returns some kind of error or status code, you should
check it with this attribute.

.. testcode:: External_Code

    print MyComp.return_code
    
.. testoutput:: External_Code
    :hide:

    0
    
.. index:: input file; for external application


Generating the Input File - Templated File I/O
----------------------------------------------

You can generate an input file for an external application two different ways. The
first way is to write the file completely from scratch using the new values that are
contained in the component's variables. Not much can be done to aid with this task, as
it requires knowledge of the file format and can be completed using Python's standard
formatted output. One exception to this is the Fortran namelist, which is more of a
standard output format. The next section mentions some tools to help create namelist
input files.


.. index:: templating; for generating input file

The second way to generate an input file is by templating. A *template* file is
a sample input file which can be processed by a templating engine to insert
new values in the appropriate locations. Often the template file is a valid
input file before being processed, although other times it contains directives
or conditional logic to guide the generation. Obviously this method works well
for cases where only a small number of the possible variables and settings are
being manipulated by outside components.

OpenMDAO includes a basic templating capability that allows a template file to
be read, fields to be replaced with new values, and an input file to be
generated so that the external application can read it. Suppose you have an
input file that contains some integer, floating point, and string inputs:

::

    INPUT
    1 2 3
    INPUT
    10.1 20.2 30.3
    A B C
    
This is a valid input file for your application, and it can also be used as a
template file. The templating object is called `InputFileGenerator`, and it
includes methods that can replace specific fields as measured by their row
and field numbers. 

To use the InputFileGenerator object, first instantiate it and give it the name of
the template file and the name of the output file that you want to produce. (Note
that this code must be placed in the ``execute`` method of your component
*before* the external code is run. See :ref:`Running-the-External-Code`.) The
code will generally look like this:

::

    from openmdao.util.filewrap import InputFileGenerator

    parser = InputFileGenerator()
    parser.set_template_file('mytemplate.txt')
    parser.set_generated_file('myinput.txt')
    
    # (Call functions to poke new values here)
    
    parser.generate()

When the template file is set, it is read into memory so that all subsequent
replacements are done without writing the intermediate file to the disk. Once
all replacements have been made, the ``generate`` method is called to create the
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

Let's say you want to grab and replace the second integer with a 7. The code
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
 
.. index:: mark_anchor
   
The method ``mark_anchor`` is used to define an anchor, which becomes the
starting point for the ``transfer_var`` method. Here you find the second field in
the first line down from the anchor and replace it with the new value.

Now, what if you want to replace the third value of the floating point numbers
after the second ``INPUT`` statement. An additional argument can be passed to the
``mark_anchor`` method to tell it to start at the second instance of the text
fragment ``"INPUT"``.

.. testcode:: Parse_Input

    parser.reset_anchor()    
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
    
Note that you are able to pass a floating point value to ``transfer_var`` and still
keep 15 digits of precision. See :ref:`A-Note-on-Precision` for a discussion of
why this is important.

Note also that we used the method ``reset_anchor`` to return the anchor to the
beginning of the file before marking our new anchor. Subsequent calls to
``mark_anchor`` start at the previous anchor and find the next instance of the
anchor text. It is a good practice to reset your anchor unless you are looking for
an instance of "B" that follows an instance of "A".

You can also count backwards from the bottom of the file by passing a negative
number. Here, the second instance of ``"INPUT"`` from the bottom brings you
back to the first one.

.. testcode:: Parse_Input

    parser.reset_anchor()    
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
    
There is also a method for replacing an entire array of values. Try
replacing the set of three integers as follows:

.. testcode:: Parse_Input

    from numpy import array
    
    array_val = array([123, 456, 789])

    parser.reset_anchor()    
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

.. index:: transfer_array

The method ``transfer_array`` takes four required inputs. The first is an array
of values that will become the new values in the file. The second is the
starting row after the anchor. The third is the starting field that will be
replaced, and the fourth is the ending field. The new array replaces the
block of fields spanned by the starting field and the ending field.

You can also use the ``transfer_array`` method to `stretch` an existing
array in a template to add more terms.

.. testcode:: Parse_Input

    from numpy import array
    
    array_val = array([11, 22, 33, 44, 55, 66])

    parser.reset_anchor()    
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

The named argument ``sep`` defines which separator to include between the
additional terms of the array. Future revisions of InputFileGenerator will
hopefully be able to detect this automatically.

The input file templating capability that comes with OpenMDAO is basic but quite
functional. If you need a more powerful templating engine, particularly one that
allows the inclusion of logic in your template files, then you may want to consider
one of the community-developed engines, such as mako_ or django_.

.. _mako: http://www.makotemplates.org/

.. _django: https://docs.djangoproject.com/en/dev/topics/templates/

.. todo:: Include some examples with one of the templating engines.


.. index:: Fortran namelists

Generating the Input File - Fortran Namelists
---------------------------------------------

Since legacy Fortran codes are expected to be frequent candidates for
file wrapping, OpenMDAO includes a library for reading and generating Fortran
namelist. The syntax for a namelist varies somewhat depending on the
Fortran implementation, but the format generally looks like this:

::

   NAMEIn 
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

The namelist utility includes methods to generate a valid namelist file from a
component's set of input variables. Other methods can parse a
namelist file and load the variable data back into an OpenMDAO component's
variables (which can be useful for populating a component with new values).

For example, consider a component whose inputs include five variables of
various types. A component that writes out an input file as a single
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
            sb.add_newvar("Py", 3.14)
            
            # Generate the input file for FLOPS
            sb.generate()

Note that this component is derived from ``ExternalCode`` and uses a few of its
features, so it is important to read :ref:`Running-the-External-Code` before
proceeding.

In the ``execute`` method, a Namelist object is instantiated. This object
allows you to sequentially build up a namelist input file. The only argument
is `self`, which is passed because the Namelist object needs to access your
component's OpenMDAO variables to automatically determine the data
type. The ``set_filename`` method is used to set the name of the input file that
will be written. Here, you just pass it the variable ``self.stdin``, which is part
of the ExternalCode API.

The first card you create for the ``Namelist`` is the title card, which is
optionally assigned with the ``set_title`` method. After this, the first
namelist group is declared with the ``add_group`` method. Subsequent variables
are added to this namelist grouping. If ``add_group`` is called again, the
current group is closed, and any further variables are added to the new one.

The ``add_var`` method is used to add a variable to the ``Namelist``. The only
needed argument is the variable's name in the component. The variable's type
is used to determine what kind of namelist variable to output. If you need to
add something to the namelist that isn't contained in one of the component's
variables, then use the ``add_newvar`` method, giving it a name and a value as
arguments. This method is what you will use if your variable has a different
name in your component than in the namelist file (i.e., you may have decided
to use a more descriptive name in Openmdao instead of the original cryptic
6-character Fortran name.)

Another method, ``add_comment``, lets you add a comment to the
namelist. Of course, this isn't an essential function, but there are times you
may want to add comments to enhance readability. The comment text should
include the comment character. Note that the namelist format doesn't require a
comment character, but it's still a good practice.

Finally, once every variable, group, and comment have been assigned, use the
``generate`` method to create the input file. If a variable was entered
incorrectly, or if you have given it a variable type that it doesn't know how
to handle (e.g., an Instance or a custom variable), an exception will be
raised. Otherwise, the input file is created, and your ``execute`` method can
move on to running your code.

*Parsing a Namelist File*
~~~~~~~~~~~~~~~~~~~~~~~~~~

The Namelist object also includes some functions for parsing a namelist file and
loading the variable values into a component's list of variables. Doing this 
can be useful for loading in models that were developed when your code was executed
standalone.

.. todo:: Write about the namelist parsing functions.

.. index:: parsing output file (for external code)

Parsing the Output File
-----------------------

When an external code is executed, it typically outputs the results into a
file. OpenMDAO includes a few things to ease the task of extracting the
important information from a file.

*Basic Extraction*
~~~~~~~~~~~~~~~~~~~

Consider an application that produces the following as part of its
text-file output:

::

    LOAD CASE 1
    STRESS 1.3334e7 3.9342e7 NaN 2.654e5
    DISPLACEMENT 2.1 4.6 3.1 2.22234
    LOAD CASE 2
    STRESS 11 22 33 44 55 66
    DISPLACEMENT 1.0 2.0 3.0 4.0 5.0

As part of the file wrap, you need to reach into this file and grab the information
that is needed by downstream components in the model. OpenMDAO includes an
object called `FileParser`, which contains functions for parsing a file, grabbing
the fields you specify, and applying them to the appropriate data type. For this to
work, the file must have some general format that would allow you to locate the
piece of data you need relative to some constant feature in the file. In other
words, the main capability of the FileParser is to locate and extract a set of
characters that is some number of lines and some number of fields away from an
`anchor` point.

::

    from openmdao.util.filewrap import FileParser

    parser = FileParser()
    parser.set_file('myoutput.txt')
    
To use the FileParser object, first instantiate it and give it the name of the
output file. (Note that this code must be placed in your component's
``execute`` function *after* the external code has been run. See
:ref:`Running-the-External-Code`.)

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

Say you want to grab the first ``STRESS`` value from each load case in the file
snippet shown above. The code would look like this. (Note: in this example the print
statement is there only for display.)

.. testcode:: Parse_Output

    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_var(1, 2)
    
    print "%g is a %s" % (var, type(var))
    self.xreal = var

.. testoutput:: Parse_Output

    1.3334e+07 is a <type 'float'>
    
The method ``mark_anchor`` is used to define an anchor, which becomes the
starting point for the ``transfer_var`` method. Here, you grab the value from the
second field in the first line down from the anchor. The parser is smart enough to
recognize the number as floating point and to create a Python float variable.
The final statement assigns this value to the component variable `xreal`.

The third value of ``STRESS`` is `NaN`. If you want to grab that element, you can type
this:

::

    parser.reset_anchor()    
    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_var(1, 4)
    
    print "%g" % var

::

    nan
    
Python also has built-in values for `nan` and `inf` that are valid for float variables. The parser
recognizes them when it encounters them in a file. This allows you to catch numerical overflows,
underflows, etc., and take action. NumPy includes the functions ``isnan`` and ``isinf`` to test for
`nan` and `inf` respectively.

::

    from numpy import isnan, isinf
    
    print isnan(var)
    
::

    True

When the data is not a number, it is recognized as a string. Grab the
word ``DISPLACEMENT``.
    
.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_var(2, 1)
    
    print var

.. testoutput:: Parse_Output

    DISPLACEMENT
    
Now, what if you want to grab the value of stress from the second load case? An
additional argument can be passed to the ``mark_anchor`` method telling it to
start at the second instance of the text fragment ``"LOAD CASE"``.

.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("LOAD CASE", 2)
    var = parser.transfer_var(1, 2)
    
    print var

.. testoutput:: Parse_Output

    11
    
Note also that we used the method ``reset_anchor`` to return the anchor to the
beginning of the file before marking our new anchor. Subsequent calls to
``mark_anchor`` start at the previous anchor and find the next instance of the
anchor text. It is a good practice to reset your anchor unless you are looking for
an instance of "B" that follows an instance of "A".

You can also count backwards from the bottom of the file by passing a negative
number. Here, the second instance of ``"LOAD CASE"`` from the bottom brings us
back to the first one.

.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("LOAD CASE", -2)
    var = parser.transfer_var(1, 2)
    
    print "%g" % var

.. testoutput:: Parse_Output

    1.3334e+07

There is a shortcut for extracting data that is stored as ``Key Value`` or 
``"Key Value Value....``.

.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("LOAD CASE 1")
    var = parser.transfer_keyvar("DISPLACEMENT", 1)
    
    print "%g" % var

.. testoutput:: Parse_Output

    2.1
    
The method ``transfer_keyvar`` finds the first occurrence of the *key* string
after the anchor (in this case, the word ``DISPLACEMENT``), and grabs the
specified field value. This can be useful in cases where variables are found
on lines that are uniquely named, particularly where you don't always know how
many lines the key will occur past the anchor location. There are two optional
arguments to ``transfer_keyvar``. The first lets you specify the `nth` occurrence
of the key, and the second lets you specify a number of lines to offset from
the line where the key is found (negative numbers are allowed).

*Array Extraction*
~~~~~~~~~~~~~~~~~~

Now consider the same application that produces the following as part of its
text-file output:

::

    LOAD CASE 1
    STRESS 1.3334e7 3.9342e7 NaN 2.654e5
    DISPLACEMENT 2.1 4.6 3.1 2.22234
    LOAD CASE 2
    STRESS 11 22 33 44 55 66
    DISPLACEMENT 1.0 2.0 3.0 4.0 5.0

This time, grab all of the displacements in one read and store
them as an array. You can do this with the ``transfer_array`` method.

.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_array(2, 2, 2, 5)
    
    print var

.. testoutput:: Parse_Output

    [ 2.1      4.6      3.1      2.22234]

The ``transfer_array`` method takes four arguments: *starting row, starting field,
ending row,* and *ending field.* The parser extracts all values from the starting
row and field and continues until it hits the ending field in the ending row.
These values are all placed in a 1D array. When extracting multiple lines, if
a line break is hit, the parser continues reading from the next line until the
last line is hit. The following extraction illustrates this:

.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("LOAD CASE")
    var = parser.transfer_array(1, 3, 2, 4)
    
    print var

.. testoutput:: Parse_Output

    ['39342000.0' 'nan' '265400.0' 'DISPLACEMENT' '2.1' '4.6' '3.1']
    
With the inclusion of ``'DISPLACEMENT'``, this is returned as an array of strings,
so you must be careful.

There is also a method to extract a 2-dimensional array from tabulated data.
Consider an output table that looks like this:

.. testcode:: Parse_Output2D
    :hide:
    
    from openmdao.util.filewrap import FileParser
    parser = FileParser()
    from openmdao.main.api import Component
    self = Component()
    
    # A way to "cheat" and do this without a file.
    parser.data = []
    parser.data.append('FREQ  DELTA  -8.5  -8.5  -8.5  -8.5  -8.5  -8.5  -8.5  -8.5  -8.5  -8.5')
    parser.data.append(' Hz')
    parser.data.append(' 50.   1.0   30.0  34.8  36.3  36.1  34.6  32.0  28.4  23.9  18.5  12.2')
    parser.data.append(' 63.   1.0   36.5  41.3  42.8  42.6  41.1  38.5  34.9  30.4  25.0  18.7')
    parser.data.append(' 80.   1.0   42.8  47.6  49.1  48.9  47.4  44.8  41.2  36.7  31.3  25.0')
    parser.data.append('100.   1.0   48.4  53.1  54.7  54.5  53.0  50.4  46.8  42.3  36.9  30.6')


::

        FREQ  DELTA   A     B     C     D     E     F     G     H     I     J
         Hz
         50.   1.0   30.0  34.8  36.3  36.1  34.6  32.0  28.4  23.9  18.5  12.2
         63.   1.0   36.5  41.3  42.8  42.6  41.1  38.5  34.9  30.4  25.0  18.7
         80.   1.0   42.8  47.6  49.1  48.9  47.4  44.8  41.2  36.7  31.3  25.0
        100.   1.0   48.4  53.1  54.7  54.5  53.0  50.4  46.8  42.3  36.9  30.6
        
We would like to extract the relevant numerical data from this table, which
amounts to all values contained in columns labeled "A" through "J" and rows
labeled "50 Hz" through "100 Hz." We would like to save these values in a
two-dimensional numpy array. This can be accomplished using the ``transfer_2Darray``
method.

.. testcode:: Parse_Output2D

    parser.reset_anchor()    
    parser.mark_anchor("Hz")
    var = parser.transfer_2Darray(1, 3, 4, 12)
    
    print var

.. testoutput:: Parse_Output2D

    [[ 30.   34.8  36.3  36.1  34.6  32.   28.4  23.9  18.5  12.2]
     [ 36.5  41.3  42.8  42.6  41.1  38.5  34.9  30.4  25.   18.7]
     [ 42.8  47.6  49.1  48.9  47.4  44.8  41.2  36.7  31.3  25. ]
     [ 48.4  53.1  54.7  54.5  53.   50.4  46.8  42.3  36.9  30.6]]

The arguments to ``transfer_2Darray`` are the starting row number, the starting field
number, the ending row number, and the ending field number. If the end field is
omitted, then all values to the end of the line are extracted. In that case, care
must be taken to make sure that all lines have the same number of values.

Note that if the delimiter is set to ``'columns'``, then the column number should be
entered instead of the field number. Delimiters are discussed in the next section.

.. index:: delimiters

*Delimiters*
~~~~~~~~~~~~

When the parser counts fields in a line of output, it determines the field
boundaries by comparing against a set of delimiters. These delimiters can be
changed using the ``set_delimiters`` method. By default, the delimiters are the
general white space characters space (``" "``) and tab (``"\\t"``). The newline characters
(``"\\n"`` and ``"\\r"``) are always removed regardless of the delimiter status.

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
    parser.reset_anchor()    
    
Try grabbing the first element without changing the delimiters:

.. testcode:: Parse_Output

    parser.mark_anchor("CASE")
    var = parser.transfer_var(1, 2)
    
    print var

.. testoutput:: Parse_Output

    ,7,2,4,5,6
    
What happened here is slightly confusing, but the main point is that the parser
did not handle this as expected because commas were not in the set of
delimiters. Now specify commas as your delimiter.

.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("CASE")
    parser.set_delimiters(", ")
    var = parser.transfer_var(1, 2)
    
    print var

.. testoutput:: Parse_Output

    7

With the correct delimiter set, you extract the second integer as expected.

While the ability to set the delimiters adds flexibility for parsing many
different types of input files, you may find cases that are too complex to
parse (e.g., a field with separator characters inside of quotes.) In such cases
you may need to read and extract the data manually.
    
*Special Case Delimiter - Columns*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One special-case value of the delimiter, ``'columns'``, is useful when the
data fields have defined column location, as is the case in certain formatted
output from Fortran or C. When the delimiter is set to ``'columns'``, the
behavior of some of the methods is slightly different. Consider the following 
output file:

::

    CASE 1
    12345678901234567890
    TTF    3.7-9.4434967
    
.. testcode:: Parse_Output
    :hide:
    
    parser.data = []
    parser.data.append("CASE 1")
    parser.data.append("12345678901234567890")
    parser.data.append("TTF    3.7-9.4434967")
    parser.reset_anchor() 

The second line is a comment that helps the reader identify the column
number (particularly on a printout) and does not need to be parsed.

In the third line, the first three columns contain flags that are either ``'T'``
or ``'F'``. Columns 4-10 contain a floating point number, and columns 11
through 20 contain another floating point number. Note that there isn't
always a space between the two numbers in this format, particularly when the
second number has a negative sign. We can't parse this with a regular
separator, but we can use the special separator ``'columns'``.

Let's parse this file to extract the third boolean flag and the two numbers.

.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("CASE")
    parser.set_delimiters("columns")
    var1 = parser.transfer_var(2, 3, 3)
    var2 = parser.transfer_var(2, 4, 10)
    var3 = parser.transfer_var(2, 11, 20)
    
    print var1
    print var2
    print var3

When the delimiters are in column mode, ``transfer_var`` takes the starting
field and the ending field as its second and third arguments. Since we just
want one column for the boolean flag, the starting field and ending field are
the same. This gives us the output:

.. testoutput:: Parse_Output

    F
    3.7
    -9.4434967

which is what we wanted to extract.

The ``transfer_array`` method can also be used with columns, but it is used
differently than ``transfer_var``. Consider this output file:

::

    CASE 2
    123456789012345678901234567890
    NODE 11 22 33 COMMENT
    NODE 44 55 66 STUFF
  
.. testcode:: Parse_Output
    :hide:
    
    parser.data = []
    parser.data.append("CASE 2")
    parser.data.append("12345678901234567890")
    parser.data.append("NODE 11 22 33 COMMENT")
    parser.data.append("NODE 44 55 66 STUFF")
    parser.reset_anchor() 
    
In this example, we want to extract the six numerical values and place them in
an array. When the delimiter is set to columns, we can define a rectangular
box from which all elements are parsed into an array. Note that the numbers
inside of the box are parsed assuming standard separator characters (``" \t"``).
      
.. testcode:: Parse_Output

    parser.reset_anchor()    
    parser.mark_anchor("CASE 2")
    parser.set_delimiters("columns")
    var = parser.transfer_array(2, 6, 3, 13)
    
    print var
    
So here we've called ``transfer_array`` with four arguments: `starting row,
starting column, ending row, ending column`. This results in the following
value for var:

.. testoutput:: Parse_Output

    [ 11.  22.  33.  44.  55.  66.]
    
You can always exit column mode and return to normal delimiter parsing by setting the
delimiters back to the default:

.. testcode:: Parse_Output

    parser.set_delimiters(" \t")
