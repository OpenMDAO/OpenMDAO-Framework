"""
Testing differentiation of user-defined datatypes.
"""

import nose
import unittest

import numpy as np

from openmdao.lib.geometry.geom_data import GeomData
from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree
from openmdao.util.testutil import assert_rel_error


class GeomComponent(Component):

    x = Float(1.0, iotype='in')

    geom_out = VarTree(GeomData(2, 1), iotype='out')

    def list_deriv_vars(self):
        return ('x'), ('geom_out.points',)

    def provideJ(self):
        self.J = np.array([[2, 0, 1],
                           [0, 2, 1]], dtype=np.float)

    def apply_deriv(self, arg, result):
        result['geom_out.points'][0,:] += self.J[0,:]*arg['x']
        result['geom_out.points'][1,:] += self.J[1,:]*arg['x']

    def apply_derivT(self, arg, result):

        result['x'] += np.sum(self.J.T[:,0]*arg['geom_out.points'][0,:])
        result['x'] += np.sum(self.J.T[:,1]*arg['geom_out.points'][1,:])

    def execute(self):
        x = self.x

        J = np.array([[2, 0, 1],
                      [0, 2, 1]])

        self.geom_out.points[0, :] = J[0, :]*x
        self.geom_out.points[1, :] = J[1, :]*x


class GeomRecieve(Component):

    geom_in = VarTree(GeomData(2, 1), iotype='in')
    out = Array(np.zeros((2, 3)), iotype='out')

    def execute(self):

        self.out = self.geom_in.points


class GeomRecieveDerivProvideJ(GeomRecieve):

    def provideJ(self):
        self.J = np.eye(6)
        return self.J

    def list_deriv_vars(self):
        return ('geom_in.points',), ('out',)


class GeomRecieveDerivApplyDeriv(GeomRecieve):

    def provideJ(self):
        self.J = np.eye(6)

    def list_deriv_vars(self):
        return ('geom_in.points',), ('out',)

    def apply_deriv(self, arg, result):
        if 'geom_in.points' in arg:
            result['out'] += self.J.dot(arg['geom_in.points'].flatten()).reshape((2,3))

    def apply_derivT(self, arg, result):
        if 'out' in arg:
            result['geom_in.points'] += self.J.T.dot(arg['out'].flatten()).reshape((2,3))


class Testcase_deriv_obj(unittest.TestCase):

    def _check_J(self, J):
        assert_rel_error(self, J[0, 0], 2.0, .00001)
        assert_rel_error(self, J[1, 0], 0.0, .00001)
        assert_rel_error(self, J[2, 0], 1.0, .00001)
        assert_rel_error(self, J[3, 0], 0.0, .00001)
        assert_rel_error(self, J[4, 0], 2.0, .00001)
        assert_rel_error(self, J[5, 0], 1.0, .00001)

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('c1', GeomComponent())
        self.top.add('c2', GeomRecieve())
        self.top.connect('c1.geom_out.points', 'c2.geom_in.points')
        self.top.driver.workflow.add(['c1', 'c2'])

        self.top.c1.x = 4.0

        self.inputs = ['c1.x']
        self.outputs = ['c2.out']

    def tearDown(self):
        self.top = None

    def _check_derivs(self):

        top = self.top
        inputs = self.inputs
        outputs = self.outputs

        J = top.driver.calc_gradient(inputs, outputs, mode='forward')
        self._check_J(J)

        J = top.driver.calc_gradient(inputs, outputs, mode='adjoint')
        self._check_J(J)

        J = top.driver.calc_gradient(inputs, outputs, mode='fd')
        self._check_J(J)


    def test_geom_provide_deriv_check_fd_tail(self):
        self.top.run()
        self._check_derivs()

    def test_geom_provide_deriv_check_analytic_tail_provideJ(self):

        self.top.replace('c2', GeomRecieveDerivProvideJ())
        self.top.run()
        self._check_derivs()

    def test_geom_provide_deriv_check_analytic_tail_apply_deriv(self):
        self.top.replace('c2', GeomRecieveDerivApplyDeriv())
        self.top.run()
        self._check_derivs()


if __name__ == '__main__':
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
