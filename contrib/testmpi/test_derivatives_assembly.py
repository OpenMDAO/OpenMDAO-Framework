"""
Basic new method to calculate derivatives across assembly.
"""

import unittest

import numpy as np

from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.main.test.test_derivatives import ArrayComp2D
from openmdao.util.testutil import assert_rel_error

class SimpleComp(Component):

    x = Float(1.0, iotype='in')
    y = Float(1.0, iotype='out')

    def execute(self):
        """ run """

        self.y = 5.3 * self.x

    def provideJ(self):
        """ Calculate the Jacobian """

        J = np.zeros([1, 1])
        J[0, 0] = 5.3
        return J

    def list_deriv_vars(self):
        input_keys = ('x', )
        output_keys = ('y',)

        return input_keys, output_keys


class Testcase_derivatives_assembly(unittest.TestCase):
    """ Test derivative across assemblies. """

    def test_simple(self):

        top = set_as_top(Assembly())
        nest = top.add('nest', Assembly())
        top.driver.workflow.add('nest')
        top.driver.gradient_options.lin_solver = 'petsc_ksp'

        nest.add('comp', SimpleComp())
        nest.driver.workflow.add('comp')
        nest.create_passthrough('comp.x')
        nest.create_passthrough('comp.y')

        top.run()

        top.nest.driver.gradient_options.derivative_direction = 'forward'
        J = top.driver.calc_gradient(inputs = ['nest.x'], outputs=['nest.y'])
        assert_rel_error(self, J[0][0], 5.3, .000001)

        J = top.driver.calc_gradient(inputs = ['nest.x'], outputs=['nest.y'], mode='adjoint')
        assert_rel_error(self, J[0][0], 5.3, .000001)

    def test_array(self):

        top = set_as_top(Assembly())
        nest = top.add('nest', Assembly())
        top.driver.workflow.add('nest')
        top.driver.gradient_options.lin_solver = 'petsc_ksp'

        nest.add('comp', ArrayComp2D())
        nest.driver.workflow.add('comp')
        nest.create_passthrough('comp.x')
        nest.create_passthrough('comp.y')

        top.run()

        J = top.driver.calc_gradient(inputs = ['nest.x'], outputs=['nest.y'])
        diff = J - top.nest.comp.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs = ['nest.x'], outputs=['nest.y'], mode='adjoint')
        diff = J - top.nest.comp.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

if __name__ == '__main__':
    unittest.main()