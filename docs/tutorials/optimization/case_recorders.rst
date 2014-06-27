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
-------------------- --------------------------------------------------------------------
``JSONCaseRecorder``  JSON file, defaults to cases.json
-------------------- --------------------------------------------------------------------
``BSONCaseRecorder``  BSON file, defaults to cases.bson
==================== ====================================================================

The recorders are interchangeable, so you can use any of them in the top-level
assembly's ``recorders`` list. All assemblies contain a ``recorders`` variable,
however only the top-level assembly's recorders are used.
Why a list? It's so you can have the same case data recorded in multiple ways if you want to. For example, you could use the DumpCaseRecorder to 
output data to the screen and use the DBCaseRecorder to save the same data to a database. 

At the end of each workflow's execution, output variables in that workflow
satisfying the top-level assembly's ``includes`` and ``excludes`` specification
are recorded. These variables hold lists of patterns of variables to be
included or excluded from the data recorded.
By default ``includes`` is ``['*']``, including everything.
By default ``excludes`` is ``[]``, excluding nothing.

The CSVCaseRecorder outputs the selected variables into a file in the csv
(Comma Separated Value) format. The DBCaseRecorder stores the selected
variables in an SQLite database, which can be stored in memory or on disc as
a binary file. The DumpCaseRecorder is used to output the selected
variables into a file-like object in a human-readable format. The default
object is ``sys.stdout``, which redirects the output to STDOUT. It can also take
a filename as an argument. The ListCaseRecorder stores the cases in a Python
list. The JSONCaseRecorder and BSONCaseRecorders store variable, expression,
and driver configuration as well as case data to a file.

Of these recorders, the CSVCaseRecorder is the most useful
for passing data to other applications, such as an external post-processing
tool. The DBCaseRecorder is the most useful for saving data for later use.
The BSONCaseRecorder will record everything JSONCaseRecorder does, but in a
more compact form. To perform JSON and BSON case recorder post-processing,
:ref:`CaseDataset <openmdao.lib.casehandlers.query.py>` is typically used.

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

Here, we set ``opt_problem.recorders`` to be a list that contains the csv and db case
recorders. The CSVCaseRecorder takes a filename as an argument, as does the DBCaseRecorder.
These files will be written in the directory where you execute this Python file.

OpenMDAO has a data structure for storing case information. This structure includes the variable
names, their status as an input or output, and a number of other metadata fields. Run the above
code, and inspect the resulting file ``converge.csv``.

::

   "timestamp","/INPUTS","paraboloid.x","paraboloid.y","/OUTPUTS","_pseudo_0","_pseudo_1","driver.workflow.itername","paraboloid.derivative_exec_count","paraboloid.exec_count","paraboloid.f_xy","paraboloid.itername","/METADATA","uuid","parent_uuid","msg"
   1403884288.956539,"",0.0,0.0,"",22.0,15.0,"1",0,1,22.0,"1-paraboloid","","e6fe4819-fe12-11e3-8001-005056000100","",""
   1403884288.961565,"",0.0,0.0,"",22.0,15.0,"2",0,2,22.0,"2-paraboloid","","e6ff6478-fe12-11e3-8002-005056000100","",""
   1403884288.976972,"",6.4999990006594981,-8.5000009993404984,"",-25.749996002635005,3.5527136788005009e-15,"3",0,5,-25.749996002635005,"3-paraboloid","","e701c311-fe12-11e3-8003-005056000100","",""
   1403884288.983514,"",8.2629979802763209,-6.7370020197236782,"",-23.477506285730119,0.0,"4",0,8,-23.477506285730119,"4-paraboloid","","e702c3de-fe12-11e3-8004-005056000100","",""
   1403884288.986463,"",7.1666665854829459,-7.8333334145170523,"",-27.083333333333314,1.7763568394002505e-15,"5",0,9,-27.083333333333314,"5-paraboloid","","e7033878-fe12-11e3-8005-005056000100","",""


This file should be readable into an application that accepts a csv input file. The first line is a
header that contains the variable names for the values that are printed. Notice that the objective
and constraints are printed for an optimizer driver.
Columns with a section header (``"/INPUTS", "/OUTPUTS", "/METADATA"``)
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

   opt_problem.recorders[0].num_backups = 3

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
    opt_problem.recorders = [DumpCaseRecorder(outfile)]
    opt_problem.run()

            
You should now have a file called ``'data.txt'`` that contains output that looks
like this:

::

   Case:
      uuid: e704a187-fe12-11e3-800a-005056000100
      timestamp: 1403884288.995384
      inputs:
         paraboloid.x: 7.16666658548
         paraboloid.y: -7.83333341452
      outputs:
         _pseudo_0: -27.0833333333
         _pseudo_1: 1.7763568394e-15
         driver.workflow.itername: 5
         paraboloid.derivative_exec_count: 0.0
         paraboloid.exec_count: 9.0
         paraboloid.f_xy: -27.0833333333
         paraboloid.itername: 5-paraboloid


The ``_pseudo_N`` variables relate to objectives, responses, and constraints.
         
You can use partial wildcard matches and include multiple wildcards in the 
``includes`` or ``excludes`` lists, so scenarios like this

::

      opt_problem.includes = ['comp1.*', 'comp2.*', *error*]

are possible. This will return a set of cases with all variables from ``comp1,
comp2`` as well as any variable with "error" in its name.

The wildcard "?" is also supported for matching single characters, so you could
rewrite the previous line like this:

::

      opt_problem.includes = ['comp?.*', *error*]

