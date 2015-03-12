import unittest

from openmdao.main.api import set_as_top

from openmdao.lib.optproblems.api import SellarProblem, UnitScalableProblem
from openmdao.lib.architectures.api import MDF, IDF, BLISS, CO

class TestArchTestSuite(unittest.TestCase):

    def test_architectures(self):

        optproblems = [
            SellarProblem,
            UnitScalableProblem,
        ]

        architectures = [
            #BLISS,  # keep this commented out until BLISS is updated
            MDF,
            IDF,
            CO,
        ]

        optproblem = optproblems[0]
        architecture = architectures[2]

        for optproblem in optproblems:
            for architecture in architectures:
                problem_name = optproblem.__name__
                architecture_name = architecture.__name__

                problem = set_as_top(optproblem())

                problem.architecture = architecture()
                problem.architecture.parent = problem

                try:
                    print problem_name, architecture_name
                    problem._setup()
                    problem.check_config()

                except:
                    import traceback
                    traceback.print_exc()
                    self.fail('%s architecture could not be configured for %s'%(problem_name, architecture_name))

if __name__ == '__main__':
    unittest.main()
