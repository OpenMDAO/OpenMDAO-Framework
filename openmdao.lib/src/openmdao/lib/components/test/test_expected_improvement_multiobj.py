# pylint: disable-msg=C0111,C0103

import unittest
from numpy import array
from openmdao.lib.components.expected_improvement_multiobj import MultiObjExpectedImprovement
from openmdao.lib.casehandlers.listcaseiter import ListCaseIterator
from openmdao.lib.casehandlers.api import CaseSet
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.main.case import Case

class MultiObjExpectedImprovementTests(unittest.TestCase):
    
    def test_ei_2obj(self):
        ei = MultiObjExpectedImprovement()
        bests = CaseSet()
        list_of_cases = [Case(outputs=[("y1",1),("y2",10)]),Case(outputs=[("y1",1),("y2",-10)])]
        for case in list_of_cases:
            bests.record(case)
        ei.best_cases = bests
        ei.criteria = ["y1","y2"]
        ei.predicted_values = [NormalDistribution(mu=1,sigma=1),NormalDistribution(mu=0,sigma=1)]
        ei.calc_switch = "EI"
        ei.execute()
        self.assertAlmostEqual([5.0],ei.EI,1)
        self.assertEqual(0.5,ei.PI,6)

    def test_ei_nobj(self):
        ei = MultiObjExpectedImprovement()
        bests = CaseSet()
        list_of_cases = [Case(outputs=[("y1",1),("y2",1),("y3",1)])]
        for case in list_of_cases:
            bests.record(case)
        ei.best_cases = bests
        ei.criteria = ['y1','y2','y3']
        ei.predicted_values = [NormalDistribution(mu=1,sigma=1),
                                                    NormalDistribution(mu=1,sigma=1),
                                                    NormalDistribution(mu=1,sigma=1)]
        ei.execute()
        self.assertAlmostEqual(0.875,ei.PI,1)

    def test_ei_calc_switch(self):
        ei = MultiObjExpectedImprovement()
        bests = CaseSet()
        list_of_cases = [Case(outputs=[("y1",1),("y2",1),("y3",1)])]
        for case in list_of_cases:
            bests.record(case)
        ei.best_cases = bests
        ei.criteria = ['y1','y2','y3']
        ei.predicted_values = [NormalDistribution(mu=1,sigma=1),
                                                    NormalDistribution(mu=1,sigma=1),
                                                    NormalDistribution(mu=1,sigma=1)]
        ei.calc_switch = 'EI'
        try:
            ei.execute()
        except ValueError,err:
            self.assertEqual(str(err),': EI calculations not supported'
                                            ' for more than 2 objectives')

    def test_reset_y_star_event(self):
        ei = MultiObjExpectedImprovement()
        bests = CaseSet()
        list_of_cases = [Case(outputs=[("y1",1),("y2",1),("y3",1)])]
        for case in list_of_cases:
            bests.record(case)
        ei.best_cases = bests
        ei.criteria = ['y1','y2','y3']
        ei.predicted_values = [NormalDistribution(mu=1,sigma=1),
                                                    NormalDistribution(mu=1,sigma=1),
                                                    NormalDistribution(mu=1,sigma=1)]
        ei.execute()
        bests = CaseSet()
        list_of_cases = [Case(outputs=[("y1",2),("y2",2),("y3",2)])]
        for case in list_of_cases:
            bests.record(case)
        ei.best_cases = bests        
        ei.reset_y_star = True
        ei.execute()
        self.assertEqual(ei.y_star.all(),array([2,2,2]).all())
        
    def test_bad_criteria(self):
        ei = MultiObjExpectedImprovement()
        bests = CaseSet()
        list_of_cases = [Case(outputs=[("y1",1),("y2",1)])]
        for case in list_of_cases:
            bests.record(case)
        ei.best_cases = bests
        ei.criteria = ['y1','y3']
        ei.predicted_values = [NormalDistribution(mu=1,sigma=1),
                                                    NormalDistribution(mu=1,sigma=1)]
        try:
            ei.execute()
        except ValueError,err:
            self.assertEqual(str(err),": no cases in the provided case_set"
                                " had output matching the provided criteria, ['y1' 'y3']")

if __name__ == "__main__":
    unittest.main()

