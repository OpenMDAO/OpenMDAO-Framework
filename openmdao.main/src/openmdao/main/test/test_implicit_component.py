"""
Unit test for implicit components.
"""

import unittest

import numpy as np

from openmdao.lib.drivers.api import BroydenSolver, MDASolver
from openmdao.main.api import ImplicitComponent, Assembly, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.main.mp_support import has_interface
from openmdao.util.testutil import assert_rel_error


class MyComp_No_Deriv(ImplicitComponent):
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

class MyComp_Deriv(MyComp_No_Deriv):
    ''' This time with derivatives.
    '''
    
    def linearize(self): 
        #partial w.r.t c 
        c, x, y, z = self.c, self.x, self.y, self.z

        dc = [3*x + 2*y - z, 0, 0]
        dx = [3*c, 2, -1]
        dy = [2*c, -2, .5]
        dz = [-c, 4, -1]

        self.J_res_state = np.array([dx, dy, dz]).T
        self.J_res_input = np.array([dc]).T
        
        self.J_output_input = np.array([[1.0]])
        self.J_output_state = np.array([[1.0, 1.0, 1.0]])

    def apply_deriv(self, arg, result):
        
        # Residual Equation derivatives
        for j, res in enumerate(self.list_residuals()):
            if res in result:
                
                # wrt States
                for k, state in enumerate(self.list_states()):
                    if state in arg:
                        result[res] += self.J_res_state[j, k]*arg[state]

                # wrt External inputs
                for k, state in enumerate(['c']):
                    if state in arg:
                        result[res] += self.J_res_input[j, k]*arg[state]
                        
        # Output Equation derivatives
        for j, res in enumerate(['y_out']):
            if res in result:
                
                # wrt States
                for k, state in enumerate(self.list_states()):
                    if state in arg:
                        result[res] += self.J_output_state[j, k]*arg[state]

                # wrt External inputs
                for k, state in enumerate(['c']):
                    if state in arg:
                        result[res] += self.J_output_input[j, k]*arg[state]
                        

class Coupled1(ImplicitComponent):
    ''' This comp only has the first 2 states (x, y). 
    
    For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
    '''

    # External inputs
    c = Float(2.0, iotype="in", 
              desc="arbitrary constant that is not iterated on but does affect the results")
    z = Float(0.0, iotype="in")
    
    # States
    x = Float(0.0, iotype="state")
    y = Float(0.0, iotype="state")

    # Residuals
    r0 = Float(iotype="residual")
    r1 = Float(iotype="residual")
    
    # Outputs
    y_out = Float(iotype='out')

    def evaluate(self): 
        """run a single step to calculate the residual 
        values for the given state var values"""

        c, x, y, z = self.c, self.x, self.y, self.z

        self.r0 = self.c*(3*x + 2*y - z) - 1
        self.r1 = 2*x - 2*y + 4*z + 2
        
        self.y_out = c + x + y + z

    def linearize(self): 
        #partial w.r.t c 
        c, x, y, z = self.c, self.x, self.y, self.z

        dc = [3*x + 2*y - z, 0, 0]
        dx = [3*c, 2, -1]
        dy = [2*c, -2, .5]
        dz = [-c, 4, -1]

        self.J_res_state = np.array([dx, dy]).T
        self.J_res_input = np.array([dc, dz]).T
        
        self.J_output_input = np.array([[1.0, 1.0]])
        self.J_output_state = np.array([[1.0, 1.0]])

    def apply_deriv(self, arg, result):
        
        # Residual Equation derivatives
        for j, res in enumerate(self.list_residuals()):
            if res in result:
                
                # wrt States
                for k, state in enumerate(self.list_states()):
                    if state in arg:
                        result[res] += self.J_res_state[j, k]*arg[state]

                # wrt External inputs
                for k, state in enumerate(['c', 'z']):
                    if state in arg:
                        result[res] += self.J_res_input[j, k]*arg[state]
                        
        # Output Equation derivatives
        for j, res in enumerate(['y_out']):
            if res in result:
                
                # wrt States
                for k, state in enumerate(self.list_states()):
                    if state in arg:
                        result[res] += self.J_output_state[j, k]*arg[state]

                # wrt External inputs
                for k, state in enumerate(['c', 'z']):
                    if state in arg:
                        result[res] += self.J_output_input[j, k]*arg[state]

class Coupled2(ImplicitComponent):
    ''' This comp only has the last state (z). 
    
    For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
    '''

    # External inputs
    c = Float(2.0, iotype="in", 
              desc="arbitrary constant that is not iterated on but does affect the results")
    x = Float(0.0, iotype="in")
    y = Float(0.0, iotype="in")
    
    # States
    z = Float(0.0, iotype="state")

    # Residuals
    r2 = Float(iotype="residual")
    
    # Outputs
    y_out = Float(iotype='out')

    def evaluate(self): 
        """run a single step to calculate the residual 
        values for the given state var values"""

        c, x, y, z = self.c, self.x, self.y, self.z

        self.r2 = -x + y/2. - z 
        
        self.y_out = c + x + y + z

    def linearize(self): 
        #partial w.r.t c 
        c, x, y, z = self.c, self.x, self.y, self.z

        dc = [3*x + 2*y - z, 0, 0]
        dx = [3*c, 2, -1]
        dy = [2*c, -2, .5]
        dz = [-c, 4, -1]

        self.J_res_state = np.array([dz]).T
        self.J_res_input = np.array([dc, dx, dy]).T
        
        self.J_output_input = np.array([[1.0, 1.0, 1.0]])
        self.J_output_state = np.array([[1.0]])

    def apply_deriv(self, arg, result):
        
        # Residual Equation derivatives
        for j, res in enumerate(self.list_residuals()):
            if res in result:
                
                # wrt States
                for k, state in enumerate(self.list_states()):
                    if state in arg:
                        result[res] += self.J_res_state[j, k]*arg[state]

                # wrt External inputs
                for k, state in enumerate(['c', 'x', 'y']):
                    if state in arg:
                        result[res] += self.J_res_input[j, k]*arg[state]
                        
        # Output Equation derivatives
        for j, res in enumerate(['y_out']):
            if res in result:
                
                # wrt States
                for k, state in enumerate(self.list_states()):
                    if state in arg:
                        result[res] += self.J_output_state[j, k]*arg[state]

                # wrt External inputs
                for k, state in enumerate(['c', 'x', 'y']):
                    if state in arg:
                        result[res] += self.J_output_input[j, k]*arg[state]

class Testcase_implicit(unittest.TestCase):
    """A variety of tests for implicit components. """
    
    def test_single_comp_self_solve(self):
        
        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv())
        model.driver.workflow.add('comp')
        
        model.run()
        
        assert_rel_error(self, model.comp.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp.z, -2.16666667, 1e-5)
        
        assert_rel_error(self, model.comp.y_out, -1.5, 1e-5)

    def test_single_comp_self_solve_no_deriv(self):
        
        model = set_as_top(Assembly())
        model.add('comp', MyComp_No_Deriv())
        model.driver.workflow.add('comp')
        
        model.run()
        
        assert_rel_error(self, model.comp.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp.z, -2.16666667, 1e-5)
        
        assert_rel_error(self, model.comp.y_out, -1.5, 1e-5)

    def test_single_comp_external_solve(self):
        
        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv())
        model.add('driver', BroydenSolver())
        model.driver.workflow.add('comp')
        
        model.driver.add_parameter('comp.x', low=-100, high=100)
        model.driver.add_parameter('comp.y', low=-100, high=100)
        model.driver.add_parameter('comp.z', low=-100, high=100)
       
        model.driver.add_constraint('comp.r0 = 0')
        model.driver.add_constraint('comp.r1 = 0')
        model.driver.add_constraint('comp.r2 = 0')
        
        model.comp.eval_only = True
        model.run()
        
        assert_rel_error(self, model.comp.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp.z, -2.16666667, 1e-5)
        
        assert_rel_error(self, model.comp.y_out, -1.5, 1e-5)

    def test_coupled_comps_internal_solve(self):
        
        model = set_as_top(Assembly())
        model.add('comp1', Coupled1())
        model.add('comp2', Coupled2())
        model.add('driver', MDASolver())
        model.driver.workflow.add(['comp1', 'comp2'])
        
        model.connect('comp1.x', 'comp2.x')
        model.connect('comp1.y', 'comp2.y')
        model.connect('comp2.z', 'comp1.z')
        
        d_edges = model._depgraph.get_directional_interior_edges('comp1', 'comp2')
        print d_edges
        
        self.assertTrue( ('comp1.x', 'comp2.x') in d_edges)
        self.assertTrue( ('comp1.y', 'comp2.y') in d_edges)
        
        model.run()
        
        assert_rel_error(self, model.comp1.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp1.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp2.z, -2.16666667, 1e-5)
        
        assert_rel_error(self, model.comp1.y_out, -1.5, 1e-5)

    def test_coupled_comps_external_solve(self):
        
        model = set_as_top(Assembly())
        model.add('comp1', Coupled1())
        model.add('comp2', Coupled2())
        model.add('driver', MDASolver())
        model.driver.workflow.add(['comp1', 'comp2'])
        
        model.connect('comp1.x', 'comp2.x')
        model.connect('comp1.y', 'comp2.y')
        model.connect('comp2.z', 'comp1.z')
        
        model.driver.add_parameter('comp1.x', low=-100, high=100)
        model.driver.add_parameter('comp1.y', low=-100, high=100)
        model.driver.add_parameter('comp2.z', low=-100, high=100)
       
        model.driver.add_constraint('comp1.r0 = 0')
        model.driver.add_constraint('comp1.r1 = 0')
        model.driver.add_constraint('comp2.r2 = 0')
        
        model.comp1.eval_only = True
        model.comp2.eval_only = True
        model.run()
        
        assert_rel_error(self, model.comp1.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp1.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp2.z, -2.16666667, 1e-5)
        
        assert_rel_error(self, model.comp1.y_out, -1.5, 1e-5)

    def test_derivative(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv())
        model.driver.workflow.add('comp')
        
        model.run()

        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'])
        print J
        edges = model.driver.workflow._edges
        print edges
        
    def test_list_states(self):
        comp = MyComp_Deriv()
        self.assertEqual(set(comp.list_states()), set(['x','y','z']))

    def test_list_residuals(self):
        comp = MyComp_Deriv()
        self.assertEqual(set(comp.list_residuals()), set(['r0','r1','r2']))

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()