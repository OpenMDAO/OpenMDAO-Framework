.. index:: case_recorder case_iterator CSV db

Recording Your Inputs and Outputs
=====================================

The previous section showed you how you can record output from OpenMDAO using a case recorder. This
next lesson will show you the available case recorders in more detail.

OpenMDAO contains the following case recorders:

==================== ====================================================================
Name                  Output Type
==================== ====================================================================
``CSVCaseRecorder``   CSV file, defaults to cases.csv
-------------------- --------------------------------------------------------------------
``DBCaseRecorder``    SQLite database, default ``':memory:'``; can also be stored in file
-------------------- --------------------------------------------------------------------
``DumpCaseRecorder``  File-like object, defaults to ``sys.stdout``
-------------------- --------------------------------------------------------------------
``ListCaseRecorder``  Python List
==================== ====================================================================

The recorders are interchangeable, so you can use any of them in a :term:`Slot` that can accept them. All
drivers contain a Slot that can accept a list of case recorders. Why a list? It's so you can have the same
case data recorded in multiple ways if you want to. For example, you could use the DumpCaseRecorder to 
output data to the screen and use the DBCaseRecorder to save the same data to a database. 

Each driver determines what it needs to write to the recorders in its list. 
In the previous example, the DOEdriver saves each point in the DOE as a
case. However, a single-objective optimizer, such as the SLSQPdriver, saves the state of the model
at each iteration in the optimization so that the convergence history can be observed. The
state of the model includes the parameters, objective, constraints, and any other data that
you choose to include by listing them in the ``printvars`` variable.

The CSVCaseRecorder outputs the selected variables into a file in the csv
(Comma Separated Value) format. The DBCaseRecorder stores the selected
variables in an SQLite database, which can be stored in memory or on disc as
a binary file. The DumpCaseRecorder is used to output the selected
variables into a file-like object in a human-readable format. The default
object is ``sys.stdout``, which redirects the output to STDOUT. It can also take
a filename as an argument. Finally, the ListCaseRecorder stores the cases
in a Python list. Of these recorders, the CSVCaseRecorder is the most useful
for passing data to other applications, such as an external post-processing
tool. The DBCaseRecorder is the most useful for saving data for later use.

At the end of the top-level assembly's ``run()``, all case recorders are closed.
Each type of recorder defines its own implementation of ``close()``,
but the general idea is to specify that the recording process is complete.
For example, the CSVCaseRecorder will close the file being written so that
other applications can use it. Note that in some cases you cannot record to
a closed recorder.

Let's consider our simple unconstrained optimization of the Paraboloid component with SLSQP. We would
like to print out the convergence history of the variables, objective, and constraint into a csv
file, which we can read into Excel for some post processing. Additionally, we'd like to save an
SQLite database for future use. The code for this should look like:

.. literalinclude:: ../../../examples/openmdao.examples.simple/openmdao/examples/simple/case_recorders.py

Here, we set ``opt_problem.driver.recorders`` to be a list that contains the csv and db case
recorders. The CSVCaseRecorder takes a filename as an argument, as does the DBCaseRecorder.
These files will be written in the directory where you execute this Python file.

OpenMDAO has a data structure for storing case information. This structure includes the variable
names, their status as an input or output, and a number of other metadata fields. Run the above
code, and inspect the resulting file ``converge.csv``.

::

"label","/INPUTS","paraboloid.y","paraboloid.x","/OUTPUTS","Objective","Constraint ( paraboloid.x-paraboloid.y >= 15.0 )","/METADATA","retries","max_retries","parent_uuid","msg"
"1","",0.0,0.0,"",22.0,-15.0,"","","",""
"2","",-8.50000000486,6.50000000636,"",-25.7499999974,1.12260067908e-08,"","","","",""
"3","",-6.7370023896,8.26299760727,"",-23.4775087306,-3.13171355515e-09,"","","","",""
"4","",-7.83333333577,7.16666667003,"",-27.0833333304,5.79672487788e-09,"","","","",""


This file should be readable into an application that accepts a csv input file. The first line is a
header that contains the variable names for the values that are printed. Notice that the objective
and constraints are printed for an optimizer driver. The first column is a case label, which
contains the iteration count. Columns with a section header (``"/INPUTS", "/OUTPUTS", "/METADATA"``)
do not contain any data. The final columns in the file contain  some metadata associated with the
case. None of these are set by SLSQPdriver. Note that in OpenMDAO's flavor of csv, string data
will always be enclosed in double quotes.

The CSVCaseRecorder supports simple data types -- integers, floats, and strings. It also supports
single elements of an array. The chosen element becomes a column in the csv file. Some of the more
complicated data types -- dictionaries, lists, multi-dimensional arrays, custom data objects -- are
not yet supported by the CSVCaseRecorder, and it is not clear how some of these could best be
represented in a comma-separated format. However, the other case recorders should support every type
of variable, provided that it can be serialized.

The CSVCaseRecorder also saves the csv output files from previous runs. These backup files are given
unique names that contain the current date and time. The user can specify the number of backups
to keep for each CSVCaseRecorder object by adding the following line to the above example.

::

   opt_problem.driver.recorders[0].num_backups = 3

If you set the number of backups to 0, no backup files are saved. The default number of backup
files is 5. Note that it is a rolling save, so the oldest files are deleted as the newest ones 
are saved so that the total is kept at the desired number.

The DumpCaseRecorder is generally used to write readable text to a
file or to STDOUT. Let's try using a DumpCaseRecorder to output a history
of our parameters, constraints, and objectives to a file named ``'data.txt'``.

::

    from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
    from openmdao.lib.casehandlers.api import DumpCaseRecorder
    
    opt_problem = OptimizationConstrained()
    
    outfile = open('data.txt', 'w')
    opt_problem.driver.recorders = [DumpCaseRecorder(outfile)]
    opt_problem.run()

            
You should now have a file called ``'data.txt'`` that contains output that looks
like this:

::

   Case: 3
      uuid: 409e0790-6f91-11e1-b85e-005056b50025
      inputs:
         paraboloid.x: 7.16666667003
         paraboloid.y: -7.83333333577
      outputs:
         Constraint ( paraboloid.x-paraboloid.y>=15.0 ): 5.79672487788e-09
         Objective: -27.0833333304

We can also choose to print out all framework variables from components in
this driver's workflow using the wildcard "*" in the printvars list and
rerunning the model. 

::

      opt_problem.driver.printvars = ['*']
      opt_problem.run()

The output produced is more detailed:

::

   Case: 14
      uuid: c32f6c1c-6f91-11e1-bebc-005056b50025
      inputs:
         driver.accuracy: 1e-06
         driver.differentiator: <openmdao.lib.differentiators.finite_difference.FiniteDifference object at 0x1cbad350>
         driver.directory: 
         driver.force_execute: True
         driver.iout: 6
         driver.iprint: 0
         driver.maxiter: 50
         driver.output_filename: slsqp.out
         driver.printvars: ['*']
         paraboloid.directory: 
         paraboloid.force_execute: False
         paraboloid.x: 7.16666667003
         paraboloid.y: -7.83333333577
      outputs:
         Constraint ( paraboloid.x-paraboloid.y>=15.0 ): 5.79672487788e-09
         Objective: -27.0833333304
         driver.error_code: 0
         paraboloid.f_xy: -27.0833333304
         
You can also use partial wildcard matches and include multiple wildcards in the 
``printvars`` list, so scenarios like this

::

      opt_problem.driver.printvars = ['comp1.*', 'comp2.*', *error*]

are possible. This will return a set of cases with all variables from ``comp1,
comp2`` as well as any variable with "error" in its name.

The wildcard "?" is also supported for matching single characters, so you could
rewrite the previous line like this:

::

      opt_problem.driver.printvars = ['comp?.*', *error*]

