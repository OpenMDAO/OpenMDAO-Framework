"""
Unit test for implicit components, and for the implicit behavior of solvers in a 
derivatives solve.
"""

import unittest
from nose import SkipTest
import numpy as np

from openmdao.lib.drivers.api import BroydenSolver, MDASolver, \
                                     FixedPointIterator
from openmdao.main.api import ImplicitComponent, Assembly, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array
from openmdao.test.execcomp import ExecCompWithDerivatives
from openmdao.main.mp_support import has_interface
from openmdao.main.test.test_derivatives import SimpleDriver
from openmdao.util.testutil import assert_rel_error

import openmdao.main.pseudocomp as pcompmod  # used to keep pseudocomp names consistent in tests


class MyComp_No_Deriv(ImplicitComponent):
    ''' Single implicit component with 3 states and residuals. 
    
    For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
    '''

    # External inputs
    c = Float(2.0, iotype="in", fd_step = .00001,
              desc="arbitrary constant that is not iterated on but does affect the results")
    
    # States
    x = Float(0.0, iotype="state")
    y = Float(0.0, iotype="state")
    z = Float(0.0, iotype="state")

    # Residuals
    res = Array(np.zeros((3)), iotype="residual")
    
    # Outputs
    y_out = Float(iotype='out')

    def evaluate(self): 
        """run a single step to calculate the residual 
        values for the given state var values"""

        c, x, y, z = self.c, self.x, self.y, self.z

        self.res[0] = self.c*(3*x + 2*y - z) - 1
        self.res[1] = 2*x - 2*y + 4*z + 2
        self.res[2] = -x + y/2. - z 
        
        self.y_out = c + x + y + z
        #print c, x, y, z, self.res
        
class MyComp_Explicit(Driver):
    ''' Single implicit component with 3 states and residuals. 
    
    For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
    '''

    # External inputs
    c = Float(2.0, iotype="in", fd_step = .00001,
              desc="arbitrary constant that is not iterated on but does affect the results")
    
    # States
    x = Float(0.0, iotype="in")
    y = Float(0.0, iotype="in")
    z = Float(0.0, iotype="in")

    # Residuals
    res = Array(np.zeros((3)), iotype="out")
    
    # Outputs
    y_out = Float(iotype='out')

    def evaluate(self): 
        """run a single step to calculate the residual 
        values for the given state var values"""

        c, x, y, z = self.c, self.x, self.y, self.z

        self.res[0] = self.c*(3*x + 2*y - z) - 1
        self.res[1] = 2*x - 2*y + 4*z + 2
        self.res[2] = -x + y/2. - z 
        
        self.y_out = c + x + y + z
        #print c, x, y, z, self.res
        
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
        res = self.list_residuals()[0]
        if res in result:
            
            # wrt States
            for k, state in enumerate(self.list_states()):
                if state in arg:
                    result[res] += self.J_res_state[:, k]*arg[state]

            # wrt External inputs
            for k, inp in enumerate(['c']):
                if inp in arg:
                    result[res] += self.J_res_input[:, k]*arg[inp]
                        
        # Output Equation derivatives
        for j, outp in enumerate(['y_out']):
            if outp in result:
                
                # wrt States
                for k, state in enumerate(self.list_states()):
                    if state in arg:
                        result[outp] += self.J_output_state[j, k]*arg[state]

                # wrt External inputs
                for k, inp in enumerate(['c']):
                    if inp in arg:
                        result[outp] += self.J_output_input[j, k]*arg[inp]
                        
    def apply_derivT(self, arg, result):
        
        # wrt States
        for k, state in enumerate(self.list_states()):
            if state in result:
                
                # Residual Equation derivatives
                res = self.list_residuals()[0]
                if res in arg:
                    result[state] += self.J_res_state.T[k, :].dot(arg[res])

                # Output Equation derivatives
                for j, outp in enumerate(['y_out']):
                    if outp in arg:
                        result[state] += self.J_output_state.T[k, j]*arg[outp]
                        
        # wrt External inputs
        for k, inp in enumerate(['c']):
            if inp in result:

                # Residual Equation derivatives
                res = self.list_residuals()[0]
                if res in arg:
                    result[inp] += self.J_res_input.T[k, :].dot(arg[res])

                # Output Equation derivatives
                for j, outp in enumerate(['y_out']):
                    if outp in arg:
                        result[inp] += self.J_output_input.T[k, j]*arg[outp]
                        
class MyComp_Deriv_ProvideJ(MyComp_No_Deriv):
    ''' This time with derivatives.
    '''
    
    def linearize(self): 
        #partial w.r.t c 
        c, x, y, z = self.c, self.x, self.y, self.z

        dc = [3*x + 2*y - z, 0, 0]
        dx = [3*c, 2, -1]
        dy = [2*c, -2, .5]
        dz = [-c, 4, -1]

        J_res = np.array([dx, dy, dz, dc]).T
        J_output = np.array([[1.0, 1.0, 1.0, 1.0]])
        
        self.J = np.vstack((J_res, J_output))
        
    def provideJ(self):
        input_keys = ('x', 'y', 'z', 'c')
        output_keys = ('res', 'y_out')
        return input_keys, output_keys, self.J 

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
    res = Array(np.zeros((2)), iotype="residual")
    
    # Outputs
    y_out = Float(iotype='out')

    def evaluate(self): 
        """run a single step to calculate the residual 
        values for the given state var values"""

        c, x, y, z = self.c, self.x, self.y, self.z

        self.res[0] = self.c*(3*x + 2*y - z) - 1
        self.res[1] = 2*x - 2*y + 4*z + 2
        
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
        res = self.get_residuals()[0]
        if res in result:
            
            # wrt States
            for k, state in enumerate(self.list_states()):
                if state in arg:
                    result[res] += self.J_res_state[:, k]*arg[state]

            # wrt External inputs
            for k, state in enumerate(['c']):
                if state in arg:
                    result[res] += self.J_res_input[:, k]*arg[state]
                        
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
    res = Array(np.zeros((1)), iotype="residual")
    
    # Outputs
    y_out = Float(iotype='out')

    def evaluate(self): 
        """run a single step to calculate the residual 
        values for the given state var values"""

        c, x, y, z = self.c, self.x, self.y, self.z

        self.res[0] = -x + y/2. - z 
        
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
        res = self.get_residuals()[0]
        if res in result:
            
            # wrt States
            for k, state in enumerate(self.list_states()):
                if state in arg:
                    result[res] += self.J_res_state[:, k]*arg[state]

            # wrt External inputs
            for k, state in enumerate(['c']):
                if state in arg:
                    result[res] += self.J_res_input[:, k]*arg[state]
                        
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
    
    def setUp(self):
        pcompmod._count = 0  # reset pseudocomp numbering
        
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
       
        model.driver.add_constraint('comp.res[0] = 0')
        model.driver.add_constraint('comp.res[1] = 0')
        model.driver.add_constraint('comp.res[2] = 0')
        
        model.comp.eval_only = True

        self.assertEqual(set(model.driver.workflow.get_implicit_info()),
                         set())

        model.run()
        
        assert_rel_error(self, model.comp.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp.z, -2.16666667, 1e-5)
        
        assert_rel_error(self, model.comp.y_out, -1.5, 1e-5)

    def test_coupled_comps_internal_solve(self):
        
        raise SkipTest('Param/Con not supported on MDA solver yet')
    
        model = set_as_top(Assembly())
        model.add('comp1', Coupled1())
        model.add('comp2', Coupled2())
        model.add('driver', MDASolver())
        model.driver.workflow.add(['comp1', 'comp2'])
        model.driver.newton = True
        
        model.connect('comp1.x', 'comp2.x')
        model.connect('comp1.y', 'comp2.y')
        model.connect('comp2.z', 'comp1.z')
        
        d_edges = model._depgraph.get_directional_interior_edges('comp1', 'comp2')
        self.assertTrue( ('comp1.x', 'comp2.x') in d_edges)
        self.assertTrue( ('comp1.y', 'comp2.y') in d_edges)
        
        model.run()
        
        assert_rel_error(self, model.comp1.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp1.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp2.z, -2.16666667, 1e-5)
        
        assert_rel_error(self, model.comp1.y_out, -1.5, 1e-5)

    def test_coupled_comps_external_solve(self):

        raise SkipTest('Param/Con not supported on MDA solver yet')
    
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
       
        model.driver.add_constraint('comp1.res[0] = 0')
        model.driver.add_constraint('comp1.res[1] = 0')
        model.driver.add_constraint('comp2.res[2] = 0')
        
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
        info = model.driver.workflow.get_implicit_info()
        edges = model.driver.workflow._edges
        #print edges
        #print info
        self.assertEqual(set(info[('comp.res',)]),
                         set(['comp.x', 'comp.y', 'comp.z']))
        self.assertEqual(len(info), 1)

        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        edges = model.driver.workflow._edges
        self.assertEqual(set(edges['@in0']), set(['comp.c']))
        self.assertEqual(set(edges['comp.y_out']), set(['@out0']))

        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
    def test_derivative_state_connection_internal_solve_ProvideJ(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv_ProvideJ())
        model.comp.add('c', Float(2.0, iotype="in", fd_step = .001))
        
        model.add('comp2', ExecCompWithDerivatives(["y=2*x"],
                                                   ["dy_dx=2"]))
        model.driver.workflow.add(['comp', 'comp2'])
        model.connect('comp.z', 'comp2.x')
        
        model.run()
        #print model.comp.x, model.comp.y, model.comp.z, model.comp.res
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'])
        info = model.driver.workflow.get_implicit_info()
        #print info
        self.assertEqual(set(info[('comp.res',)]),
                         set(['comp.x', 'comp.y', 'comp.z']))
        self.assertEqual(len(info), 1)

        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(set(edges['@in0']), set(['comp.c']))
        self.assertEqual(set(edges['comp2.y']), set(['@out0']))

        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='adjoint')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='fd')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
    def test_derivative_state_connection_internal_solve_apply_deriv(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv())
        model.comp.add('c', Float(2.0, iotype="in", fd_step = .001))
        
        model.add('comp2', ExecCompWithDerivatives(["y=2*x"],
                                                   ["dy_dx=2"]))
        model.driver.workflow.add(['comp', 'comp2'])
        model.connect('comp.z', 'comp2.x')
        
        model.run()
        #print model.comp.x, model.comp.y, model.comp.z, model.comp.res
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'])
        info = model.driver.workflow.get_implicit_info()
        #print info
        self.assertEqual(set(info[('comp.res',)]),
                         set(['comp.x', 'comp.y', 'comp.z']))
        self.assertEqual(len(info), 1)

        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(set(edges['@in0']), set(['comp.c']))
        self.assertEqual(set(edges['comp2.y']), set(['@out0']))

        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='adjoint')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='fd')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
    def test_derivative_state_connection_external_solve_ProvideJ(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv_ProvideJ())
        model.comp.add('c', Float(2.0, iotype="in", fd_step = .001))
        
        model.add('comp2', ExecCompWithDerivatives(["y=2*x"],
                                                   ["dy_dx=2"]))
        
        model.add('solver', BroydenSolver())
        model.solver.workflow.add(['comp', 'comp2'])
        model.driver.workflow.add(['solver'])
        model.connect('comp.z', 'comp2.x')
        
        model.solver.add_parameter('comp.x', low=-100, high=100)
        model.solver.add_parameter('comp.y', low=-100, high=100)
        model.solver.add_parameter('comp.z', low=-100, high=100)
       
        model.solver.add_constraint('comp.res[0] = 0')
        model.solver.add_constraint('comp.res[1] = 0')
        model.solver.add_constraint('comp.res[2] = 0')
        
        model.comp.eval_only = True
        
        model.run()
        #print model.comp.x, model.comp.y, model.comp.z, model.comp.res
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'])
        info = model.driver.workflow.get_implicit_info()
        #print info
        self.assertEqual(set(info[('_pseudo_0.out0', '_pseudo_1.out0','_pseudo_2.out0')]),
                         set([('comp.x',), ('comp.y',), ('comp.z',)]))
        self.assertEqual(len(info), 1)

        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(set(edges['@in0']), set(['comp.c']))
        self.assertEqual(set(edges['comp2.y']), set(['@out0']))

        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='adjoint')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='fd')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
    def test_derivative_state_connection_external_solve_ProvideJ(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv_ProvideJ())
        model.comp.add('c', Float(2.0, iotype="in", fd_step = .001))
        
        model.add('comp2', ExecCompWithDerivatives(["y=2*x"],
                                                   ["dy_dx=2"]))
        
        model.add('solver', BroydenSolver())
        model.solver.workflow.add(['comp', 'comp2'])
        model.driver.workflow.add(['solver'])
        model.connect('comp.z', 'comp2.x')
        
        model.solver.add_parameter('comp.x', low=-100, high=100)
        model.solver.add_parameter('comp.y', low=-100, high=100)
        model.solver.add_parameter('comp.z', low=-100, high=100)
       
        model.solver.add_constraint('comp.res[0] = 0')
        model.solver.add_constraint('comp.res[1] = 0')
        model.solver.add_constraint('comp.res[2] = 0')
        
        model.comp.eval_only = True
        
        model.run()
        #print model.comp.x, model.comp.y, model.comp.z, model.comp.res
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'])
        info = model.driver.workflow.get_implicit_info()
        #print info
        self.assertEqual(set(info[('_pseudo_0.out0', '_pseudo_1.out0','_pseudo_2.out0')]),
                         set([('comp.x',), ('comp.y',), ('comp.z',)]))
        self.assertEqual(len(info), 1)

        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(set(edges['@in0']), set(['comp.c']))
        self.assertEqual(set(edges['comp2.y']), set(['@out0']))

        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='adjoint')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='fd')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
    def test_derivative_state_connection_external_solve_apply_deriv_not_implicit(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Explicit())
        model.comp.add('c', Float(2.0, iotype="in", fd_step = .001))
        
        model.add('comp2', ExecCompWithDerivatives(["y=2*x"],
                                                   ["dy_dx=2"]))
        
        model.add('solver', BroydenSolver())
        model.solver.workflow.add(['comp'])
        model.driver.workflow.add(['solver', 'comp2'])
        model.connect('comp.z', 'comp2.x')
        
        model.solver.add_parameter('comp.x', low=-100, high=100)
        model.solver.add_parameter('comp.y', low=-100, high=100)
        model.solver.add_parameter('comp.z', low=-100, high=100)
       
        model.solver.add_constraint('comp.res[0] = 0')
        model.solver.add_constraint('comp.res[1] = 0')
        model.solver.add_constraint('comp.res[2] = 0')
        
        model.comp.eval_only = True
        
        model.run()
        #print model.comp.x, model.comp.y, model.comp.z, model.comp.res
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'])
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='adjoint')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp2.y'],
                                                mode='fd')
        print J
        assert_rel_error(self, J[0][0], -0.1666, 1e-3)
        
    def test_derivative_no_deriv(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_No_Deriv())
        model.driver.workflow.add('comp')
        
        model.run()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'])
        info = model.driver.workflow.get_implicit_info()
        edges = model.driver.workflow._edges

        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
    def test_derivative_nested_solver(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv())
        model.add('solver', BroydenSolver())
        model.driver.workflow.add('solver')
        model.solver.workflow.add('comp')
        model.solver.tol = 0.0000001
        
        model.solver.add_parameter('comp.x', low=-100, high=100)
        model.solver.add_parameter('comp.y', low=-100, high=100)
        model.solver.add_parameter('comp.z', low=-100, high=100)
       
        model.solver.add_constraint('comp.res[0] = 0')
        model.solver.add_constraint('comp.res[1] = 0')
        model.solver.add_constraint('comp.res[2] = 0')
        
        model.comp.eval_only = True
        model.run()

        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'])
        
        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(edges['@in0'], ['comp.c'])
        self.assertEqual(edges['comp.y_out'], ['@out0'])
        self.assertEqual(edges['comp.res[0]'], ['_pseudo_0.in0'])
        self.assertEqual(edges['comp.res[1]'], ['_pseudo_1.in0'])
        self.assertEqual(edges['comp.res[2]'], ['_pseudo_2.in0'])
        self.assertTrue('_pseudo_0.out0' in model.driver.workflow._derivative_graph)
        self.assertTrue('_pseudo_1.out0' in model.driver.workflow._derivative_graph)
        self.assertTrue('_pseudo_2.out0' in model.driver.workflow._derivative_graph)
        self.assertTrue('comp.x' in model.driver.workflow._derivative_graph)
        self.assertTrue('comp.y' in model.driver.workflow._derivative_graph)
        self.assertTrue('comp.z' in model.driver.workflow._derivative_graph)
        
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
    def test_derivative_nested_solver_no_deriv(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_No_Deriv())
        model.add('solver', BroydenSolver())
        model.driver.workflow.add('solver')
        model.solver.workflow.add('comp')
        model.solver.tol = 0.0000001
        
        model.solver.add_parameter('comp.x', low=-100, high=100)
        model.solver.add_parameter('comp.y', low=-100, high=100)
        model.solver.add_parameter('comp.z', low=-100, high=100)
       
        model.solver.add_constraint('comp.res[0] = 0')
        model.solver.add_constraint('comp.res[1] = 0')
        model.solver.add_constraint('comp.res[2] = 0')
        
        model.comp.eval_only = True
        model.run()

        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'])
        
        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(edges['@in0'], ['~0.comp|c'])
        self.assertEqual(edges['~0.comp|y_out'], ['@out0'])
        self.assertEqual(edges['~0.comp|res[0]'], ['_pseudo_0.in0'])
        self.assertEqual(edges['~0.comp|res[1]'], ['_pseudo_1.in0'])
        self.assertEqual(edges['~0.comp|res[2]'], ['_pseudo_2.in0'])
        
        
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
    def test_solver_nested_under_double_nested_driver(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv())
        model.add('subdriver', SimpleDriver())
        model.add('solver', BroydenSolver())
        model.driver.workflow.add('subdriver')
        model.subdriver.workflow.add('solver')
        model.solver.workflow.add('comp')
        model.solver.tol = 0.0000001
        
        model.solver.add_parameter('comp.x', low=-100, high=100)
        model.solver.add_parameter('comp.y', low=-100, high=100)
        model.solver.add_parameter('comp.z', low=-100, high=100)
       
        model.solver.add_constraint('comp.res[0] = 0')
        model.solver.add_constraint('comp.res[1] = 0')
        model.solver.add_constraint('comp.res[2] = 0')
        
        model.subdriver.add_parameter('comp.c', low=-100, high=100)
        model.subdriver.add_objective('comp.y_out')

        model.comp.eval_only = True
        model.run()

        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'])
        
        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(edges['@in0'], ['~subdriver.comp|c'])
        self.assertEqual(edges['~subdriver.comp|y_out'], ['@out0'])
        
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
    def test_solver_nested_under_double_nested_driver_no_deriv(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_No_Deriv())
        model.add('subdriver', SimpleDriver())
        model.add('solver', BroydenSolver())
        model.driver.workflow.add('subdriver')
        model.subdriver.workflow.add('solver')
        model.solver.workflow.add('comp')
        model.solver.tol = 0.0000001
        
        model.solver.add_parameter('comp.x', low=-100, high=100)
        model.solver.add_parameter('comp.y', low=-100, high=100)
        model.solver.add_parameter('comp.z', low=-100, high=100)
       
        model.solver.add_constraint('comp.res[0] = 0')
        model.solver.add_constraint('comp.res[1] = 0')
        model.solver.add_constraint('comp.res[2] = 0')
        
        model.subdriver.add_parameter('comp.c', low=-100, high=100)
        model.subdriver.add_objective('comp.y_out')

        model.comp.eval_only = True
        model.run()

        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'])
        
        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(edges['@in0'], ['~subdriver.comp|c'])
        self.assertEqual(edges['~subdriver.comp|y_out'], ['@out0'])
        
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
        model.driver.workflow.config_changed()
        J = model.driver.workflow.calc_gradient(inputs=['comp.c'],
                                                outputs=['comp.y_out'],
                                                mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)
        
    def test_solver_nested_under_double_nested_driver_boundary_var_no_deriv(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_No_Deriv())
        model.add('bvar', Float(0.0, iotype='in'))
        model.add('driver', SimpleDriver())
        model.add('subdriver', SimpleDriver())
        
        model.driver.workflow.add('subdriver')
        model.subdriver.workflow.add('comp')
        
        model.subdriver.add_parameter('comp.c', low=-100, high=100)
        model.subdriver.add_objective('comp.y_out - bvar')

        model.driver.add_parameter('bvar', low=-100, high=100)
        model.driver.add_objective('bvar - comp.y_out')
        model.comp.eval_only = True
        model.run()

        J = model.driver.workflow.calc_gradient()
        edges = model.driver.workflow._edges
        #print edges
        self.assertEqual(edges['@in0'], ['_pseudo_1.in0', '~subdriver._pseudo_0|in1'])
        self.assertEqual(edges['_pseudo_1.out0'], ['@out0'])
        
    def test_list_states(self):
        comp = MyComp_Deriv()
        self.assertEqual(set(comp.list_states()), set(['x','y','z']))

    def test_list_residuals(self):
        comp = MyComp_Deriv()
        self.assertEqual(set(comp.list_residuals()), set(['res']))

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()