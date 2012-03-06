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

The recorders are interchangable, so you can use any of them in a slot that can accept them, such
as the recorders slot that drivers have. The ``CSVCaseRecorder`` outputs the selected variables
into a file in the csv (Comma Seperated Value) format. The ``DBCaseRecorder`` 

The ``ListCaseRecorder`` is mostly used 



:: testcode:: Paraboloid_case_handlers

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
            
            tt = time.time()
            opt_problem.run()
        
            print "\n"
            print "CONMIN Iterations: ", opt_problem.driver.iter_count
            print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                                 opt_problem.paraboloid.y)
            print "Elapsed time: ", time.time()-tt, "seconds"

