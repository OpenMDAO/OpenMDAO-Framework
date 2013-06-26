"""
Test the MDA driver
"""

import unittest
import numpy

# pylint: disable-msg=F0401,E0611
from openmdao.lib.drivers.mda_solver import MDASolver
from openmdao.lib.optproblems.scalable import Discipline
from openmdao.lib.optproblems.sellar import Discipline1_WithDerivatives, \
                                            Discipline2_WithDerivatives
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.util.testutil import assert_rel_error


class Sellar_MDA(Assembly):
    
    def configure(self):
        
        self.add('d1', Discipline1_WithDerivatives())
        self.d1.x1 = 1.0
        self.d1.y1 = 1.0
        self.d1.y2 = 1.0
        self.d1.z1 = 5.0
        self.d1.z2 = 2.0
        
        self.add('d2', Discipline2_WithDerivatives())
        self.d2.y1 = 1.0
        self.d2.y2 = 1.0
        self.d2.z1 = 5.0
        self.d2.z2 = 2.0
        
        self.connect('d1.y1', 'd2.y1')
        self.connect('d2.y2', 'd1.y2')
        
        self.add('driver', MDASolver())
        self.driver.workflow.add(['d1', 'd2'])
        
        
class Scalable_MDA(Assembly):
    
    def configure(self):
        
        self.add('d1', Discipline(prob_size=2))
        self.add('d2', Discipline(prob_size=2))
        
        self.connect('d1.y_out', 'd2.y_in')
        self.connect('d2.y_out', 'd1.y_in')
        
        self.add('driver', MDASolver())
        self.driver.workflow.add(['d1', 'd2'])
        self.driver.newton = True
        

class SLSPQdriverTestCase(unittest.TestCase):
    """test SLSQP optimizer component"""

    def setUp(self):
        self.top = set_as_top(Sellar_MDA())

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
        
        self.top.driver.newton = True
        self.top.run()
        
        assert_rel_error(self, self.top.d1.y1,
                               self.top.d2.y1,
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y2,
                               self.top.d2.y2,
                               1.0e-4)
        print self.top.d1.exec_count
        self.assertTrue(self.top.d1.exec_count < 6)
        
    def test_scalable_newton(self):
        
        # This verifies that it works for arrays

        self.top = set_as_top(Scalable_MDA())
        
        self.top.d1.x = self.top.d2.x = numpy.array([[3.0], [-1.5]])
        self.top.d1.z = self.top.d2.z = numpy.array([[-1.3], [2.45]])
        self.top.d1.C_y = numpy.array([[1.1, 1.3], [1.05, 1.13]])
        self.top.d2.C_y = numpy.array([[0.95, 0.98], [0.97, 0.95]])        

        self.top.run()
        
        assert_rel_error(self, self.top.d1.y_out[0],
                               self.top.d2.y_in[0],
                               1.0e-4)
        assert_rel_error(self, self.top.d1.y_out[1],
                               self.top.d2.y_in[1],
                               1.0e-4)
        assert_rel_error(self, self.top.d2.y_out[0],
                               self.top.d1.y_in[0],
                               1.0e-4)
        assert_rel_error(self, self.top.d2.y_out[1],
                               self.top.d1.y_in[1],
                               1.0e-4)
        print self.top.d1.exec_count
        self.assertTrue(self.top.d1.exec_count < 4)
        
        
            
if __name__ == "__main__":
    unittest.main()
        