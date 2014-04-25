from openmdao.main.api import Assembly
from openmdao.lib.drivers.api import SLSQPdriver

from openmdao.examples.simple.paraboloid import Paraboloid

class OptimizationConstrained(Assembly):
    """Constrained optimization of the Paraboloid component."""

    def configure(self):
        """ Creates a new Assembly containing a Paraboloid and an optimizer"""

        # Create Paraboloid component instances
        self.add('paraboloid', Paraboloid())

        # Create Optimizer instance
        self.add('driver', SLSQPdriver())

        # Driver process definition
        self.driver.workflow.add('paraboloid')

        # Optimzier Flags
        self.driver.iprint = 0

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
    import os
    if os.path.exists('converge.db'):
        os.remove('converge.db')   
        
    from openmdao.lib.casehandlers.api import CSVCaseRecorder, DBCaseRecorder

    opt_problem.driver.recorders = [CSVCaseRecorder(filename='converge.csv'),
                                    DBCaseRecorder(dbfile='converge.db', append=False)]
    opt_problem.driver.printvars = ['*']

    #-----------------------------
    # Run problem
    #-----------------------------

    opt_problem.run()

    #----------------------------------------------------
    # Print out history of our objective for inspection
    #----------------------------------------------------

    for case in opt_problem.driver.recorders[0].get_iterator():
        print case

    print "\n"
    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x, \
                                         opt_problem.paraboloid.y)