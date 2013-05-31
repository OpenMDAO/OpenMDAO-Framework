"""
Test the MDA driver
"""

import unittest
import numpy

# pylint: disable-msg=F0401,E0611
from openmdao.lib.drivers.mda_solver import MDASolver
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives
from openmdao.main.api import Assembly, Component
from openmdao.main.datatypes.api import Float
from openmdao.util.testutil import assert_rel_error


class Sellar_MDA(Assembly):
    
    def configure(self):
        
        self.add('d1', Discipline1_WithDerivatives())
        self.add('d2', Discipline2_WithDerivatives())
        
        self.connect('d1.y1', 'd2.y1')
        self.connect('d2.y2', 'd1.y2')
        
        self.add('driver', MDASolver())
        self.driver.workflow.add(['d1', 'd2'])
        
        
class SLSPQdriverTestCase(unittest.TestCase):
    """test SLSQP optimizer component"""

    def setUp(self):
        self.top = Sellar_MDA()
        
    def tearDown(self):
        self.top = None
        
    def test_gauss_seidel(self):
        
        self.top.run()
        
        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)
        self.assertTrue(self.top.d1.exec_count < 10)
        
    def test_newton(self):
        
        self.top.run()
        self.top.driver.Newton = True
        
        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)
        self.assertTrue(self.top.d1.exec_count < 6)
        
            
if __name__ == "__main__":
    unittest.main()
        