"""
Unit test for implicit components.
"""

import unittest

import numpy as np

from openmdao.lib.drivers.api import BroydenSolver
from openmdao.main.api import ImplicitComponent, Assembly, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.util.testutil import assert_rel_error


class MyComp(ImplicitComponent):
    ''' Single implicit component with 3 states and residuals. 
    
    For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
    '''

    # External inputs
    c = Float(2.0, iotype="in", 
              desc="arbitrary constant that is not iterated on but does affect the results")
    
    # States
    x = Float(0.0, iotype="state")
    y = Float(0.0, iotype="state")
    z = Float(0.0, iotype="state")

    # Residuals
    r0 = Float(iotype="residual")
    r1 = Float(iotype="residual")
    r2 = Float(iotype="residual")
    
    # Outputs
    y_out = Float(iotype='out')

    def evaluate(self): 
        """run a single step to calculate the residual 
        values for the given state var values"""

        c, x, y, z = self.c, self.x, self.y, self.z

        self.r0 = self.c*(3*x + 2*y - z) - 1
        self.r1 = 2*x - 2*y + 4*z + 2
        self.r2 = -x + y/2. - z 
        
        self.y_out = c + x + y + z

    def linearize(self): 
        #partial w.r.t c 
        c, x, y, z = self.c, self.x, self.y, self.z

        dc = [3*x + 2*y - z, 0, 0]
        dx = [3*c, 2, -1]
        dy = [2*c, -2, .5]
        dz = [-c, 4, -1]

        self.J_res_state = np.array([dz, dy, dz]).T
        self.J_res_input = np.array([dc]).T
        
        self.J_output_state = np.array([1.0])
        self.J_output_res = np.array([1.0, 1.0, 1.0])

    def provideJ(self): 

        return ('x', 'y', 'z'), ('r0', 'r1', 'r2'), self.J_res_state

    #note, these methods should be implemented in the ImplicitComp baseclass in a more general manner
    def _func(self, X): 
        """Map the results of evaluate into something that scipy.root can use""" 

        self.x = X[0]
        self.y = X[1]
        self.z = X[2]
        #TODO: List all the state variables and set them automatically, instead of hard coding them

        self.evaluate()

        #TODO: list all the residuals and return them automatically
        return np.array([self.r0, self.r1, self.r2])

    def _jac(self, X):
        """Map the analytic derivatives of evaluate into something that scipy.root can use""" 
        
        self.linearize()
        return self.J_res_state

    def execute(self): 
        x0 = [self.x, self.y, self.z]
        sol = root(self._func, x0, jac=self._jac)
        #TODO: use GMRES based approach with apply_deriv or apply_derivT if those are given


class Testcase_implicit(unittest.TestCase):
    """A variety of tests for implicit components. """
    
    def test_single_comp_self_solve(self):
        
        model = set_as_top(Assembly())
        model.add('comp', MyComp())
        model.add('driver', BroydenSolver())
        model.driver.workflow.add('comp')
        
        model.driver.add_parameter('comp.x', low=-100, high=100)
        model.driver.add_parameter('comp.y', low=-100, high=100)
        model.driver.add_parameter('comp.z', low=-100, high=100)
       
        model.driver.add_constraint('comp.r0 = 0')
        model.driver.add_constraint('comp.r1 = 0')
        model.driver.add_constraint('comp.r2 = 0')
        
        model.run()
        
        assert_rel_error(model.comp.x, 1.0, 1e-5)
        assert_rel_error(model.comp.y, -2.33333333, 1e-5)
        assert_rel_error(model.comp.z, -2.16666667, 1e-5)
        
        assert_rel_error(model.comp.y_out, -1.5, 1e-5)

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()