"""
Unit test for implicit components, and for the implicit behavior of solvers in a
derivatives solve.
"""

import unittest
import numpy as np
from scipy.optimize import fsolve
from mock import Mock

import openmdao.main.implicitcomp
from openmdao.lib.drivers.api import BroydenSolver, NewtonSolver
from openmdao.main.api import ImplicitComponent, Assembly, set_as_top, Component
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecCompWithDerivatives
from openmdao.util.testutil import assert_rel_error


class ImplicitWithSolver(ImplicitComponent):

    def solve(self):
        """Calculates the states that satisfy residuals using scipy.fsolve.
        You can override this function to provide your own internal solve."""
        #print "start solve", self.name
        x0 = self.get_state()
        fsolve(self._solve_callback, x0, xtol=1e-8)
        #print "end solve", self.name

    def _solve_callback(self, X):
        """This function is passed to the internal solver to set a new state,
        evaluate the residuals, and return them."""

        self.set_state(X)
        self.evaluate()

        return self.get_residuals()


class MyComp_No_Deriv(ImplicitWithSolver):
    ''' Single implicit component with 3 states and residuals.

    For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
    '''

    # External inputs
    c = Float(2.0, iotype="in", fd_step=.00001,
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


class MyComp_Explicit(Component):
    ''' Single implicit component with 3 states and residuals.

    For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
    '''

    # External inputs
    c = Float(2.0, iotype="in", fd_step=.00001,
              desc="arbitrary constant that is not iterated on but does affect the results")

    # States
    x = Float(0.0, iotype="in")
    y = Float(0.0, iotype="in")
    z = Float(0.0, iotype="in")

    # Residuals
    res = Array(np.zeros((3)), iotype="out")

    # Outputs
    y_out = Float(iotype='out')

    def execute(self):
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

    def provideJ(self):
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

    def list_deriv_vars(self):
        input_keys = ('x', 'y', 'z', 'c')
        output_keys = ('res', 'y_out')
        return input_keys, output_keys

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

    def provideJ(self):
        #partial w.r.t c
        c, x, y, z = self.c, self.x, self.y, self.z

        dc = [3*x + 2*y - z, 0, 0]
        dx = [3*c, 2, -1]
        dy = [2*c, -2, .5]
        dz = [-c, 4, -1]

        J_res = np.array([dx, dy, dz, dc]).T
        J_output = np.array([[1.0, 1.0, 1.0, 1.0]])

        self.J = np.vstack((J_res, J_output))
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', 'y', 'z', 'c')
        output_keys = ('res', 'y_out')
        return input_keys, output_keys


class Coupled1(ImplicitWithSolver):
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
        #print x, y, z, c, self.y_out, self.res

    def list_deriv_vars(self):
        return ('x', 'y', 'z', 'c'), ('res', 'y_out')

    def provideJ(self):
        #partial w.r.t c
        c, x, y, z = self.c, self.x, self.y, self.z

        dc = [3*x + 2*y - z, 0]
        dx = [3*c, 2]
        dy = [2*c, -2]
        dz = [-c, 4]

        self.J_res_state = np.array([dx, dy]).T
        self.J_res_input = np.array([dc, dz]).T

        self.J_output_input = np.array([[1.0, 1.0]])
        self.J_output_state = np.array([[1.0, 1.0]])

    def apply_deriv(self, arg, result):

        # Residual Equation derivatives
        if 'res' in result:

            # wrt States
            for k, state in enumerate(self.list_states()):
                if state in arg:
                    result['res'] += self.J_res_state[:, k]*arg[state]

            # wrt External inputs
            for k, state in enumerate(['c', 'z']):
                if state in arg:
                    result['res'] += self.J_res_input[:, k]*arg[state]

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


    def apply_derivT(self, arg, result):
        """ Not using it, so let's just define it. """
        self.raise_exception("Should never get here.", RunTimeError)


class Coupled2(ImplicitWithSolver):
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
        #print x, y, z, c, self.y_out, self.res

    def provideJ(self):
        #partial w.r.t c
        c, x, y, z = self.c, self.x, self.y, self.z

        dc = [0]
        dx = [-1.0]
        dy = [.5]
        dz = [-1.0]

        self.J_res_state = np.array([dz])
        self.J_res_input = np.array([dc, dx, dy]).T

        self.J_output_input = np.array([[1.0, 1.0, 1.0]])
        self.J_output_state = np.array([[1.0]])

    def list_deriv_vars(self):
        return ('x', 'y', 'z', 'c'), ('res', 'y_out')

    def apply_deriv(self, arg, result):

        # Residual Equation derivatives
        if 'res' in result:

            # wrt States
            for k, state in enumerate(self.list_states()):
                if state in arg:
                    result['res'] += self.J_res_state[:, k]*arg[state]

            # wrt External inputs
            for k, state in enumerate(['c', 'x', 'y']):
                if state in arg:
                    result['res'] += self.J_res_input[:, k]*arg[state]

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

    def apply_derivT(self, arg, result):
        """ Not using it, so let's just define it. """
        self.raise_exception("Should never get here.", RunTimeError)


class MyComp_Full_Array(ImplicitWithSolver):
    ''' Single implicit component with 3 states and residuals, all as arrays.

    For c=2.0, (x,y,z) = (1.0, -2.333333, -2.1666667)
    '''

    # External inputs
    c = Float(2.0, iotype="in", fd_step=.00001,
              desc="arbitrary constant that is not iterated on but does affect the results")

    # States
    xx = Array(np.zeros((3)), iotype="state")

    # Residuals
    res = Array(np.zeros((3)), iotype="residual")

    # Outputs
    y_out = Float(iotype='out')

    def evaluate(self):
        """run a single step to calculate the residual
        values for the given state var values"""

        c, x, y, z = self.c, self.xx[0], self.xx[1], self.xx[2]

        self.res[0] = self.c*(3*x + 2*y - z) - 1
        self.res[1] = 2*x - 2*y + 4*z + 2
        self.res[2] = -x + y/2. - z

        self.y_out = c + x + y + z
        #print c, x, y, z, self.res


class Testcase_implicit(unittest.TestCase):
    """A variety of tests for implicit components. """

    def test_single_comp_self_solve(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv())
        model.driver.workflow.add('comp')
        model.comp.eval_only = False

        model.run()

        assert_rel_error(self, model.comp.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp.z, -2.16666667, 1e-5)

        assert_rel_error(self, model.comp.y_out, -1.5, 1e-5)

    def test_single_array_comp_self_solve(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Full_Array())
        model.driver.workflow.add('comp')
        model.comp.eval_only = False

        model.run()

        assert_rel_error(self, model.comp.xx[0], 1.0, 1e-5)
        assert_rel_error(self, model.comp.xx[1], -2.33333333, 1e-5)
        assert_rel_error(self, model.comp.xx[2], -2.16666667, 1e-5)

        assert_rel_error(self, model.comp.y_out, -1.5, 1e-5)

    def test_single_comp_self_solve_no_deriv(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_No_Deriv())
        model.driver.workflow.add('comp')
        model.comp.eval_only = False

        model.run()

        assert_rel_error(self, model.comp.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp.z, -2.16666667, 1e-5)

        assert_rel_error(self, model.comp.y_out, -1.5, 1e-5)

    def test_single_comp_external_solve(self):

        model = set_as_top(Assembly())
        model.add('comp', MyComp_Deriv())
        #model.add('driver', BroydenSolver())
        model.add('driver', NewtonSolver())
        model.driver.workflow.add('comp')

        model.driver.add_parameter('comp.x', low=-100, high=100)
        model.driver.add_parameter('comp.y', low=-100, high=100)
        model.driver.add_parameter('comp.z', low=-100, high=100)

        model.driver.add_constraint('comp.res[0] = 0')
        model.driver.add_constraint('comp.res[1] = 0')
        model.driver.add_constraint('comp.res[2] = 0')

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
        model.add('driver', BroydenSolver())
        model.driver.workflow.add(['comp1', 'comp2'])
        model.comp1.eval_only = False
        model.comp2.eval_only = False
        model.comp1.force_fd = False
        model.comp2.force_fd = False

        #model.connect('comp1.x', 'comp2.x')
        #model.connect('comp1.y', 'comp2.y')
        #model.connect('comp2.z', 'comp1.z')
        model.driver.add_parameter('comp1.z', low=-100, high=100)
        model.driver.add_parameter('comp2.x', low=-100, high=100)
        model.driver.add_parameter('comp2.y', low=-100, high=100)

        model.driver.add_constraint('comp1.z = comp2.z')
        model.driver.add_constraint('comp2.x = comp1.x')
        model.driver.add_constraint('comp2.y = comp1.y')
        model.driver.tol

        model.run()

        assert_rel_error(self, model.comp1.x, 1.0, 1e-5)
        assert_rel_error(self, model.comp1.y, -2.33333333, 1e-5)
        assert_rel_error(self, model.comp2.z, -2.16666667, 1e-5)

        assert_rel_error(self, model.comp1.y_out, -1.5, 1e-5)

    def test_coupled_comps_external_solve(self):

        model = set_as_top(Assembly())
        model.add('comp1', Coupled1())
        model.add('comp2', Coupled2())
        model.add('driver', NewtonSolver())
        model.driver.workflow.add(['comp1', 'comp2'])

        model.connect('comp1.x', 'comp2.x')
        model.connect('comp1.y', 'comp2.y')
        model.connect('comp2.z', 'comp1.z')

        model.driver.add_parameter('comp1.x', low=-100, high=100)
        model.driver.add_parameter('comp1.y', low=-100, high=100)
        model.driver.add_parameter('comp2.z', low=-100, high=100)

        model.driver.add_constraint('comp1.res[0] = 0')
        model.driver.add_constraint('comp1.res[1] = 0')
        model.driver.add_constraint('comp2.res[0] = 0')

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
        model.add('driver', SimpleDriver())
        model.driver.workflow.add('comp')
        model.driver.add_parameter('comp.c', low=-1000, high=1000)
        model.driver.add_objective('comp.y_out')
        model.comp.eval_only = False

        model.run()
        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'])
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 2e-3)

    def test_derivative_state_connection_internal_solve_ProvideJ(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

        model.add('comp', MyComp_Deriv_ProvideJ())
        model.comp.add('c', Float(2.0, iotype="in"))

        model.add('comp2', ExecCompWithDerivatives(["y=2*x"],
                                                   ["dy_dx=2"]))
        model.driver.workflow.add(['comp', 'comp2'])
        model.connect('comp.z', 'comp2.x')
        model.comp.eval_only = False

        model.run()
        #print model.comp.x, model.comp.y, model.comp.z, model.comp.res
        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='forward')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='adjoint')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='fd')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

    def test_derivative_state_connection_internal_solve_apply_deriv(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

        model.add('comp', MyComp_Deriv())
        model.comp.add('c', Float(2.0, iotype="in"))
        model.comp.eval_only = False

        model.add('comp2', ExecCompWithDerivatives(["y=2*x"],
                                                   ["dy_dx=2"]))
        model.driver.workflow.add(['comp', 'comp2'])
        model.connect('comp.z', 'comp2.x')

        model.run()
        #print model.comp.x, model.comp.y, model.comp.z, model.comp.res
        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'])
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='adjoint')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='fd')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

    def test_derivative_state_connection_external_solve_ProvideJ(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

        model.add('comp', MyComp_Deriv_ProvideJ())
        model.comp.add('c', Float(2.0, iotype="in", fd_step=.001))

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
        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='forward')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='adjoint')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='fd')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

    def test_derivative_state_connection_external_solve_apply_deriv_not_implicit(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

        model.add('comp', MyComp_Explicit())
        model.comp.add('c', Float(2.0, iotype="in", fd_step=.001))

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

        model.run()
        #print model.comp.x, model.comp.y, model.comp.z, model.comp.res
        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'])
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='adjoint')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp2.y'],
                                       mode='fd')
        assert_rel_error(self, J[0][0], -0.1666, 2e-3)

    def test_derivative_no_deriv(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

        model.add('comp', MyComp_No_Deriv())
        model.driver.workflow.add('comp')
        model.comp.eval_only = False

        model.run()
        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'])

        #print J
        assert_rel_error(self, J[0][0], 0.75, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 2e-3)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 2e-3)

    def test_derivative_nested_solver(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

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

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'])

        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

    def test_derivative_nested_solver_no_deriv(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

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

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'])

        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

    def test_solver_nested_under_double_nested_driver(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

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

        model.driver.add_parameter('comp.c', low=-100, high=100)
        model.driver.add_objective('comp.y_out')

        model.comp.eval_only = True
        model.run()

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'])

        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='fd')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

    def test_solver_nested_under_double_nested_driver_no_deriv(self):

        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())

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

        model.driver.add_parameter('comp.c', low=-100, high=100)
        model.driver.add_objective('comp.y_out')

        model.comp.eval_only = True
        model.run()

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'])

        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
                                       outputs=['comp.y_out'],
                                       mode='adjoint')
        #print J
        assert_rel_error(self, J[0][0], 0.75, 1e-5)

        J = model.driver.calc_gradient(inputs=['comp.c'],
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

        J = model.driver.calc_gradient()

    def test_list_states(self):
        comp = MyComp_Deriv()
        self.assertEqual(set(comp.list_states()), set(['x', 'y', 'z']))

    def test_list_residuals(self):
        comp = MyComp_Deriv()
        self.assertEqual(set(comp.list_residuals()), set(['res']))

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
