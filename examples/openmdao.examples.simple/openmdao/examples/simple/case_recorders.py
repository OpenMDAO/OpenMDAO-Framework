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

    opt_problem = OptimizationConstrained()

    #-----------------------------
    # Set up our CaseRecorders
    #-----------------------------
    import os
    if os.path.exists('converge.json'):
        os.remove('converge.json')

    from openmdao.lib.casehandlers.api import JSONCaseRecorder

    opt_problem.recorders = [JSONCaseRecorder(filename='converge.json')]

    #-----------------------------
    # Run problem
    #-----------------------------

    opt_problem.run()

    #----------------------------------------------------
    # Print out history of our objective for inspection
    #----------------------------------------------------

    case_dataset = CaseDataset('converge.json', 'json')
    data = case_dataset.by_case().fetch()

    for case in data:
        print case

    print "\n"
    print "Minimum found at (%f, %f)" % (opt_problem.paraboloid.x,
                                         opt_problem.paraboloid.y)
