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
drivers contain a slot that can accept a list of case recorders. Each driver determines what it
needs to write. In the previous example, the ``DOEdriver`` saves each point in the DOE as a
case. However, a single-objective optimizer such as CONMINdriver saves the state of the model
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

Let's consider our simple unconstrained optimization of the Paraboloid component with CONMIN. We would
like to print out the convergence history of the variables, objective, and constraint into a csv
file, which we can read into Excel for some post processing. Additionally, we would like to save an
SQLite database for future use. The code for this should look like:

.. testcode:: Paraboloid_case_handlers

        from openmdao.main.api import Assembly
        from openmdao.lib.drivers.api import CONMINdriver
        
        from openmdao.examples.simple.paraboloid import Paraboloid
        
        class OptimizationConstrained(Assembly):
            """Constrained optimization of the Paraboloid with CONMIN."""
            
            def configure(self):
                """ Creates a new Assembly containing a Paraboloid and an optimizer"""
                
                # pylint: disable-msg=E1101
        
                # Create Paraboloid component instances
                self.add('paraboloid', Paraboloid())
        
                # Create CONMIN Optimizer instance
                self.add('driver', CONMINdriver())
                
                # Driver process definition
                self.driver.workflow.add('paraboloid')
                
                # CONMIN Flags
                self.driver.iprint = 0
                self.driver.itmax = 30
                self.driver.fdch = .000001
                self.driver.fdchm = .000001
                
                # Objective 
                self.driver.add_objective('paraboloid.f_xy')
                
                # Design Variables 
                self.driver.add_parameter('paraboloid.x', low=-50., high=50.)
                self.driver.add_parameter('paraboloid.y', low=-50., high=50.)
                
                # Constraints
                self.driver.add_constraint('paraboloid.x-paraboloid.y >= 15.0')
                
                
        if __name__ == "__main__": # pragma: no cover         
        
            import time
            
            opt_problem = OptimizationConstrained()
            
            #-----------------------------
            # Set up our CaseRecorders
            #-----------------------------
                
            from openmdao.lib.casehandlers.api import CSVCaseRecorder, DBCaseRecorder
                
            opt_problem.driver.recorders = [CSVCaseRecorder(filename='converge.csv'), 
                                            DBCaseRecorder(dbfile='converge.db', append=False)]
                                                
            #-----------------------------
            # Run problem
            #-----------------------------
                
            opt_problem.run()
            
            #----------------------------------------------------
            # Print out history of our objective for inspection
            #----------------------------------------------------
                
            for case in opt_problem.driver.recorders[0].get_iterator():
                print case['objective']
            
            print "\n"
            print "CONMIN Iterations: ", opt_problem.driver.iter_count
            print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                                 opt_problem.paraboloid.y)

Here, we set ``opt_problem.driver.recorders`` to be a list that contains the csv and db case recorders. The
``CSVCaseRecorder`` takes a filename as an argument, as does the ``DBCaseRecorder``. These files will be
written in the directory where you execute this python file.

OpenMDAO has a data structure for storing case information. This structure includes the variable names, their status
as an input or output, and a number of other metadata fields. Run the above code, and inspect the resulting file
``converge.csv``.

::

"label","uuid","/INPUTS","paraboloid.y","paraboloid.x","/OUTPUTS","Constraint4","objective","Constraint0","Constraint1","Constraint2
","Constraint3","/METADATA","retries","max_retries","parent_uuid","msg"
"","6be3af7e-67a6-11e1-af51-005056b50025","",0.0,1e-06,"",0.0,21.999994,14.999999,0.0,0.0,0.0,"","","",""
"","6be4adfc-67a6-11e1-af51-005056b50025","",-7.54160275589,7.4584047025,"",0.0,-26.8280028871,-7.45839724736e-06,0.0,0.0,0.0,"","","",""
"","6be59636-67a6-11e1-af51-005056b50025","",-7.81867047557,7.38483984481,"",0.0,-26.9305645958,-0.203510320387,0.0,0.0,0.0,"","","",""
"","6be684f6-67a6-11e1-af51-005056b50025","",-7.77597457081,7.22403265322,"",0.0,-27.0734583953,-7.22402542941e-06,0.0,0.0,0.0,"","","",""
"","6be79940-67a6-11e1-af51-005056b50025","",-7.81007102711,7.19970818675,"",0.0,-27.0760422275,-0.00977921386021,0.0,0.0,0.0,"","","",""
"","6be8ab0a-67a6-11e1-af51-005056b50025","",-7.82982627771,7.18204792231,"",0.0,-27.0770934079,-0.0118742000216,0.0,0.0,0.0,"","","",""
"","6be99664-67a6-11e1-af51-005056b50025","",-7.82422478018,7.17577473428,"",0.0,-27.0830846921,4.85536538974e-07,0.0,0.0,0.0,"","","",""

This file should be readable into an application that accepts a csv input file. The first line is a header that contains
the variable names for the values that are printed. Notice that the objective and constraints are printed for an optimizer
driver. The first column is a case label, which is currently empty for cases generated from a driver. The second column
is a string that contains a unique identifier for this case. Columns with a section header ("/INPUTS", "/OUTPUTS",
"/METADATA") do not contain any data. The final columns in the file contain some metadata associated with the case. None
of these are set by ``CONMINDriver.`` Note that in OpenMDAO's flavor of csv, all string data must be enclosed in double
quotes.

The ``CSVCaseRecorder`` supports simple data types -- integers, floats, and strings. It also supports single elements of an array.
The chosen element becomes a column in the csv file. Some of the more complicated data types -- dictionaries, lists, multi-dimensional
arrays, custom data objects -- are not yet supported by the CSVCaseRecorder, and it is not clear how some of these could best be
represented in a comma-separated format. However, the other case recorders should support every type of variable, provided that
it is serializable.