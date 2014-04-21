# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.lib.components.expected_improvement import ExpectedImprovement
from openmdao.lib.casehandlers.api import CaseSet, ListCaseIterator
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.main.case import Case

class ExpectedImprovementTests(unittest.TestCase):

    def test_ei(self):
        ei = ExpectedImprovement()
        ei.target = 1.0
        ei.current = NormalDistribution(mu=1, sigma=1)
        ei.execute()
        self.assertAlmostEqual([0.40],ei.EI,2)
        self.assertAlmostEqual(0.5,ei.PI,6)

    def test_ei_zero_division(self):
        ei = ExpectedImprovement()
        ei.target = 1.0
        ei.current = NormalDistribution(mu=1, sigma=0)
        ei.execute()
        self.assertEqual(0,ei.EI)
        self.assertEqual(0,ei.PI)

if __name__ == "__main__":
    unittest.main()

