.. index:: case_recorder case_iterator CSV db

Recording your Inputs and Outputs
=====================================

The previous section showed you how you can record output from OpenMDAO using a case recorder. This
next lesson will show you the available case recorders in more detail.

OpenMDAO contains the following case recorders:

================== ==================================================================
Name                Output Type
================== ==================================================================
CSVCaseRecorder     CSV file, defaults to cases.csv
------------------ ------------------------------------------------------------------
DBCaseRecorder      SQLite database, default ':memory:'; can also be stored in file
------------------ ------------------------------------------------------------------
DumpCaseRecorder    File-like object, defaults to sys.stdout
------------------ ------------------------------------------------------------------
ListCaseRecorder    Python List
================== ==================================================================

The recorders are interchangable, so you can use any of them in a slot that can accept them. All
drivers contain a slot that can accept a list of case recorders. Why a list? That is so you can have the same
case data recorded in multiple ways if you want to. For example, you could have use the DumpCaseRecorder 
to output data to the screen, and use the DBCaseRecorder to save the same data to database. 

Each driver determines what it needs to write to the recorders in it's list. 
In the previous example, the ``DOEdriver`` saves each point in the DOE as a
case. However, a single-objective optimizer such as the ``SLSQPdriver``openm saves the state of the model
at each iteration in the optimization so that the convergence history can be observed. The
state of the model includes the parameters, objective, constraints, and any other data that
the user chooses to include by listing them in the ``printvars`` variable.

The ``CSVCaseRecorder`` outputs the selected variables into a file in the csv
(Comma Seperated Value) format. The ``DBCaseRecorder`` stores the selected variables in an
SQLite database, which can be stored in memory or on disc as a binary file. The ``DumpCaseRecorder``
is used to output the selected variables into a file-like object. The default object is sys.stdout,
which redirects the output to STDOUT. Finally, the ``ListCaseRecorder`` stores the cases in a Python
list. Of these recorders, the CSVCaseRecorder is the most useful for passing data to other applications
such as an external post-processing tool. The DBCaseRecorder is the most useful for saving data
for later use.

Let's consider our simple unconstrained optimization of the Paraboloid component with SLSQP. We would
like to print out the convergence history of the variables, objective, and constraint into a csv
file, which we can read into Excel for some post processing. Additionally, we would like to save an
SQLite database for future use. The code for this should look like:

.. literalinclude:: ../../examples/openmdao.examples.simple/openmdao/examples/simple/case_recorders.py

Here, we set ``opt_problem.driver.recorders`` to be a list that contains the csv and db case recorders. The
``CSVCaseRecorder`` takes a filename as an argument, as does the ``DBCaseRecorder``. These files will be
written in the directory where you execute this python file.

OpenMDAO has a data structure for storing case information. This structure includes the variable names, their status
as an input or output, and a number of other metadata fields. Run the above code, and inspect the resulting file
``converge.csv``.

::

"label","uuid","/INPUTS","paraboloid.y","paraboloid.x","/OUTPUTS","objective","Constraint0","/METADATA","retries","max_retries","parent_uuid","msg"
"","ef56c020-686a-11e1-94f8-34159e027f06","",0.0,0.0,"",22.0,-15.0,"","","",""
"","ef57b322-686a-11e1-94f8-34159e027f06","",-8.50000000486,6.50000000636,"",-25.7499999974,1.12260067908e-08,"","","",""
"","ef583c84-686a-11e1-94f8-34159e027f06","",-6.7370023896,8.26299760727,"",-23.4775087306,-3.13171355515e-09,"","","",""
"","ef587384-686a-11e1-94f8-34159e027f06","",-7.83333333577,7.16666667003,"",-27.0833333304,5.79672487788e-09,"","","",""


This file should be readable into an application that accepts a csv input file. The first line is a header that contains
the variable names for the values that are printed. Notice that the objective and constraints are printed for an optimizer
driver. The first column is a case label, which is currently empty for cases generated from a driver. The second column
is a string that contains a unique identifier for this case. Columns with a section header ("/INPUTS", "/OUTPUTS",
"/METADATA") do not contain any data. The final columns in the file contain some metadata associated with the case. None
of these are set by ``SLSQPdriver.`` Note that in OpenMDAO's flavor of csv, all string will always be enclosed in double
quotes.

The ``CSVCaseRecorder`` supports simple data types -- integers, floats, and strings. It also supports single elements of an array.
The chosen element becomes a column in the csv file. Some of the more complicated data types -- dictionaries, lists, multi-dimensional
arrays, custom data objects -- are not yet supported by the CSVCaseRecorder, and it is not clear how some of these could best be
represented in a comma-separated format. However, the other case recorders should support every type of variable, provided that
it is serializable.