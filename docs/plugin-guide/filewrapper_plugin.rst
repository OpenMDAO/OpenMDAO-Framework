
.. _Building-a-Plugin-Using-a-File-Wrapper:

Building a Plugin that Contains a File Wrapper
==============================================

For many legacy codes, the only viable way to include them in an MDAO process
is through file-wrapping. In a file-wrapped component, the inputs are passed
and the outputs are extracted via input and output files.

There are three phases during the execution of a file-wrapped component:

- Generating an input file that contains Openmdao variable values.
- Executing the external code
- Parsing the output file and extracing values to place in OpenMDAO

A file-wrapped component must perform all three of these tasks in its 'execute'
function.


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
statement, only 11 digits of precision are in the generated output. The
same is true if we convert the value to a string and use string output
formatting. Printing the variable as a floating point number with no format
string gives even less precision. In order to output the full precision of a
variable, the decimal precision must be specified in the format (i.e., '%.16f').

All of this quibbling over the 11th-15th decimial place may sound unnecessary,
but some applications are sensitive to changes of this magnitude. Moreover, it
is important to consider how your component may be used during optimization. A
gradient optimizer will often use a finite-difference scheme to calculate the
gradients for a model, and this means that some component inputs might be
subjected to small increments and decrements. A loss of precision here can 
competely change the calculated gradient and prevent the optimizer from reaching
a correct minimum value.

The filewrapping utilities in OpenMDAO use '%.16g'. If you write
your own custom input-file generator for a new component, you should use this
format for the floating point variables.

Precision is also important when parsing the output, although the file parsing
utilities always grab the entire number. However, some codes limit the number
of digits of precision in their output files for human-readability. In such a
case, you should check the manual for your external application to see if there
is a flag to tell the code to output the full precision.
    

.. _`Running-the-External-Code`:

Running the External Code
-------------------------

Generating the Input File - Templated File I/O
----------------------------------------------

Generating the Input File - FORTRAN Namelists
---------------------------------------------

Since legacy FORTRAN are expected to be a frequent candidate for file-wrapping,
a library for reading and generating FORTRAN namelist files has been included.
The syntax for a namelist varies somewhat, depending on the FORTRAN implementation,
but the format generally looks like this:

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
a component's set of input variables. There are also functions that can be used 
to parse a namelist file, and load the variable data back into an OpenMDAO
component's variables (which can be useful for populating a component with new values.)

For example, let's consider a component whose inputs include 5 variables of various
types. A component that writes out the an input file as a single namelist called
`MAIN` would look like this:

.. testcode:: Namelist

    from numpy import array
    
    from openmdao.lib.datatypes.api import Float, Int, Str, Bool, Array
    from openmdao.lib.components.api import ExternalCode
    from openmdao.main.api import Component
    
    from openmdao.util.namelist_util import Namelist
    
    class Wrapped_Comp(ExternalCode):
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

Note that this component is derived from ExternalComp, and uses a few of its features, so it
is important to read :ref:`Running the External Code` before proceeding.

In the `execute` method, a *Namelist* object is instantiated. This object allows you to
sequentially build up a namelist input file. The only argument is 'self', which is passed
because the Namelist object needs to access your component's OpenMDAO variables in order to
automatically determine the data type. The `set_filename` method is used to set the name
of the input file that will be written. Here, we just pass it the variable self.stdin, which
is part of the ExternalComp API.

The first card we create for the Namelist is the title card, which is optionally assigned with
the `set_title` method. After this, the first namelist group is declared with the `add_group`
method. Subsequent variables are added to this namelist grouping. If `add_group` is called again,
the current group is closed and any further variables are added to the new one.

The `add_var` method is used to add a variable to the Namelist. The only needed argument is the
variable's name in the component. The variable's type is used to determine what kind of 
namelist variable to output. If you need to add something to the namelist that isn't contained
in one of the component's variables, then use the add_new_var method, giving it a name
and a value as arguments.

Finally, once every variable, group, and comment has been assigned, use the `generate` method
to create the input file.

Parsing a Namelist File
~~~~~~~~~~~~~~~~~~~~~~~~

The Namelist object also includes some functions to parse a namelist file and load the variable
values into a component's list of variables. This can be useful for loading in models that
were developed when your code was executed standalone.

.. todo:: Write about the namelist parsing functions.

Parsing the Output File
-----------------------


.. todo:: File wrapping procedure
    
.. todo:: Convenience functions (templating, locating data in a file, etc.)
