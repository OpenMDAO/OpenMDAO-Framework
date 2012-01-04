"""
Test of the ExecComp component.
"""

import unittest

from openmdao.test.execcomp import ExecComp, ExecCompWithDerivatives
from openmdao.main.api import Assembly, set_as_top


class execCompTest(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        
    def test_execcomp(self):
            
        exp1 = ['y1 = 2.0*x1 + x2*x2',
                'y2 = 3.0*x1*x2']
        self.top.add('comp1', ExecComp(exp1))
    
        self.top.comp1.x1 = 3.0
        self.top.comp1.x2 = 5.0
        self.top.comp1.run()
        
        self.assertEqual(self.top.comp1.y1, 31.0)
        self.assertEqual(self.top.comp1.y2, 45.0)

    def test_execcomp_derivatives(self):
            
        exp1 = ['y1 = 2.0*x1 + x2*x2',
                'y2 = 3.0*x1*x2']
        deriv1 = ['dy1_dx1 = 2.0',
                  'dy1_dx2 = 2.0*x2',
                  'dy2_dx1 = 3.0*x2',
                  'dy2_dx2 = 3.0*x1']
                  
        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
    
        self.top.comp1.x1 = 3.0
        self.top.comp1.x2 = 5.0
        self.top.comp1.run()
        self.top.comp1.calc_derivatives(first=True, second=False)
        
        self.assertEqual(self.top.comp1.derivatives.first_derivatives['y1']['x1'], 2.0)
        self.assertEqual(self.top.comp1.derivatives.first_derivatives['y1']['x2'], 10.0)
        self.assertEqual(self.top.comp1.derivatives.first_derivatives['y2']['x1'], 15.0)
        self.assertEqual(self.top.comp1.derivatives.first_derivatives['y2']['x2'], 9.0)
        self.assertEqual(self.top.comp1.y2, 45.0)

        
if __name__ == '__main__':
    unittest.main()