# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.lib.components.pareto_filter import ParetoFilter
from openmdao.lib.casehandlers.listcaseiter import ListCaseIterator
from openmdao.main.case import Case


class ParetoFilterTests(unittest.TestCase):
    
    def test_1d_filter(self):
        pf = ParetoFilter()
        x = [1,2,3,4,5,6,7,8,9,10]
        cases = [Case(outputs=[("x",x_0)]) for x_0 in x]
        pf.case_sets = [ListCaseIterator(cases),]
        pf.criteria = ['x']
        pf.execute()
       
        x_p = [case['x'] for case in pf.pareto_set]
        x_dom = [case['x'] for case in pf.dominated_set]
        
        self.assertEqual([1],x_p)
        self.assertEqual([2,3,4,5,6,7,8,9,10],x_dom)
        
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

    def test_2d_filter2(self):
        pf = ParetoFilter()
        x = [1,1,2,2,2,3,3,3,]
        y = [2,3,1,2,3,1,2,3]
        cases = []
        for x_0,y_0 in zip(x,y):
            cases.append(Case(outputs=[("x",x_0),("y",y_0)]))
        
        pf.case_sets = [ListCaseIterator(cases),]
        pf.criteria = ['x','y']
        pf.execute()

        x_p,y_p = zip(*[(case['x'],case['y']) for case in pf.pareto_set])
        x_dom,y_dom = zip(*[(case['x'],case['y']) for case in pf.dominated_set])
        
        self.assertEqual((1,2),x_p)
        self.assertEqual((2,1),y_p)
        self.assertEqual((1, 2, 2, 3, 3, 3),x_dom)
        self.assertEqual((3, 2, 3, 1, 2, 3),y_dom)
        
    def test_bad_case_set(self): 
        pf = ParetoFilter()
        x = [1,1,2,2,2,3,3,3,]
        y = [2,3,1,2,3,1,2,3]
        cases = []
        for x_0,y_0 in zip(x,y):
            cases.append(Case(outputs=[("x",None,x_0),("y",None,y_0)]))
            
        pf.case_sets = [ListCaseIterator(cases),]
        pf.criteria = ['z','w']
        
        try:
            pf.execute()
        except ValueError,err: 
            self.assertEqual(str(err),": no cases in the provided case_set had output matching the provided criteria, ['z', 'w']")
        else: 
            self.fail("expected ValueError")

        
if __name__ == "__main__":
    unittest.main()

