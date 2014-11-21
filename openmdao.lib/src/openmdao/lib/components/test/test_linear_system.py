import unittest

import numpy as np 

from openmdao.main.api import set_as_top, Assembly, ImplicitComponent
from openmdao.lib.datatypes.api import Array

from openmdao.lib.components.linear_system import LinearSystem


class LinearSystemTestCase(unittest.TestCase):
    """Solves the linear system Ax=b for x"""

    def setUp(self):
        top = set_as_top(Assembly())
        ls = LinearSystem(3)
        top.add("ls",ls)
        top.driver.workflow.add("ls")
        self.top = top

        top.ls.A = np.array([[3,2,-1],[2,-2,4],[-1,.5,-1]], dtype="float") 
        top.ls.b = np.array([1.,-2.,0.], dtype="float")

    def test_ls(self): 

        top = self.top
        top.run() 

        self.assertAlmostEqual(top.ls.x[0],1,5)
        self.assertAlmostEqual(top.ls.x[1],-2,5)
        self.assertAlmostEqual(top.ls.x[2],-2,5)
        
        self.assertAlmostEqual(top.ls.R[0],0.0,5)
        self.assertAlmostEqual(top.ls.R[1],0.0,5)
        self.assertAlmostEqual(top.ls.R[2],0.0,5)

    def test_ls_applyJ(self):

        top = self.top
        top.run() 

        ins = ['ls.A', 'ls.b', 'ls.x']
        ins = ['ls.x',]
        outs = ['ls.R',]

        top.ls.eval_only = True #needed to check residual derivatives
        
        J_forward = top.driver.calc_gradient(ins,outs, mode='forward')
        J_adjoint = top.driver.calc_gradient(ins,outs, mode='adjoint')

        top.driver.gradient_options.fd_form='complex_step'
        top.driver.gradient_options.fd_step = 1e-20
        J_cs = top.driver.calc_gradient(ins,outs, mode='fd')

        self.assertTrue(np.linalg.norm((J_forward- J_cs)) < 1e-5)
        self.assertTrue(np.linalg.norm((J_adjoint- J_cs)) < 1e-5)
        
if __name__ == "__main__":
    unittest.main()

