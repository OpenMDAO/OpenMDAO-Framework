
.. _Building-a-Plugin-Using-a-File-Wrapper:

Building a Plugin that Contains a File Wrapper
==============================================



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
case, there may be a flag to tell the code to output the full precision.
    

Generating the Input File - Templated File I/O
----------------------------------------------

Generating the Input File - FORTRAN Namelists
---------------------------------------------

Since legacy FORTRAN are expected to be a frequent candidate for file-wrapping,
a library for reading and generating FORTRAN namelist files has been included.

Running the External Code
-------------------------

Parsing the Output File
-----------------------


.. todo:: File wrapping procedure
    
.. todo:: Convenience functions (templating, locating data in a file, etc.)
