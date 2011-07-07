# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.lib.components.expected_improvement import ExpectedImprovement
from openmdao.lib.casehandlers.listcaseiter import ListCaseIterator
from openmdao.lib.casehandlers.api import CaseSet
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.main.case import Case


class ExpectedImprovementTests(unittest.TestCase):
    
    def test_ei(self):
        ei = ExpectedImprovement()
        ei.best_case = CaseSet(Case(outputs=[("y",1)]))
        ei.criteria = "y"
        ei.predicted_value = NormalDistribution(mu=1,sigma=1)
        ei.execute()
        self.assertAlmostEqual([0.91],ei.EI,2)
        self.assertAlmostEqual(0.5,ei.PI,6)
        
    def test_ei_bad_criteria(self):
        ei = ExpectedImprovement()
        ei.best_case = CaseSet(Case(outputs=[("y",1)]))
        ei.criteria = "x"
        ei.predicted_value = NormalDistribution(mu=1,sigma=1)
        try:
            ei.execute()
        except ValueError,err:
            self.assertEqual(str(err),": best_case did not have an output which "
                 "matched the criteria, 'x'")
      
    def test_ei_zero_division(self):
        ei = ExpectedImprovement()
        ei.best_case = CaseSet(Case(outputs=[("y",1)]))
        ei.criteria = "y"
        ei.predicted_value = NormalDistribution(mu=1,sigma=0)
        ei.execute()
        self.assertEqual(0,ei.EI)
        self.assertEqual(0,ei.PI)

    """
    def test_2d_filter1(self):
        pf = ParetoFilter()
        x = [1,1,1,2,2,2,3,3,3]
        y = [1,2,3,1,2,3,1,2,3]
        cases = []
        for x_0,y_0 in zip(x,y):
            cases.append(Case(outputs=[("x",x_0),("y",y_0)]))
        
        pf.case_sets = [ListCaseIterator(cases),]
        pf.criteria = ['x','y']
        pf.execute()

        x_p,y_p = zip(*[(case['x'],case['y']) for case in pf.pareto_set])
        x_dom,y_dom = zip(*[(case['x'],case['y']) for case in pf.dominated_set])
        
        self.assertEqual((1,),x_p)
        self.assertEqual((1,),y_p)
        self.assertEqual((1, 1, 2, 2, 2, 3, 3, 3),x_dom)
        self.assertEqual((2, 3, 1, 2, 3, 1, 2, 3),y_dom)
    """

        
if __name__ == "__main__":
    unittest.main()

