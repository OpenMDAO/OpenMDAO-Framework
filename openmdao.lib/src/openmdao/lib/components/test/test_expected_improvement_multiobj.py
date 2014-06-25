# pylint: disable-msg=C0111,C0103

import unittest
from numpy import array
from openmdao.lib.components.expected_improvement_multiobj import MultiObjExpectedImprovement
from openmdao.lib.casehandlers.api import CaseSet, ListCaseIterator
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.main.case import Case

class MultiObjExpectedImprovementTests(unittest.TestCase):

    def test_ei_2obj(self):
        ei = MultiObjExpectedImprovement()
        ei.target = array([[1, 10], [1, -10]])
        ei.current = [NormalDistribution(mu=1, sigma=1),
                      NormalDistribution(mu=0, sigma=1)]
        ei.calc_switch = "EI"
        ei.execute()
        self.assertAlmostEqual([5.0], ei.EI,1)
        self.assertEqual(0.5, ei.PI,6)

    def test_ei_nobj(self):
        ei = MultiObjExpectedImprovement()
        ei.target = array([[1, 1, 1]])
        list_of_cases = [Case(outputs=[("y1", 1), ("y2", 1), ("y3", 1)])]
        ei.criteria = ['y1', 'y2', 'y3']
        ei.current = [NormalDistribution(mu=1, sigma=1),
                      NormalDistribution(mu=1, sigma=1),
                      NormalDistribution(mu=1, sigma=1)]
        ei.execute()
        self.assertAlmostEqual(0.875,ei.PI,1)

    def test_ei_calc_switch(self):
        ei = MultiObjExpectedImprovement()
        ei.target = array([[1, 1, 1]])
        ei.current = [NormalDistribution(mu=1, sigma=1),
                      NormalDistribution(mu=1, sigma=1),
                      NormalDistribution(mu=1, sigma=1)]
        ei.calc_switch = 'EI'
        try:
            ei.execute()
        except ValueError,err:
            self.assertEqual(str(err),': EI calculations not supported'
                             ' for more than 2 objectives')

    def test_reset_y_star_event(self):
        ei = MultiObjExpectedImprovement()
        ei.target = array([[1, 1, 1]])
        ei.current = [NormalDistribution(mu=1,sigma=1),
                      NormalDistribution(mu=1,sigma=1),
                      NormalDistribution(mu=1,sigma=1)]
        ei.execute()
        ei.target = array([[2, 2, 2]])
        ei.execute()
        self.assertEqual(ei.y_star.all(), array([2, 2, 2]).all())


if __name__ == "__main__":
    unittest.main()

