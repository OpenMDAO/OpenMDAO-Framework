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
    y = Float(2.0, iotype='in')
    z = Float(3.5, iotype='in')

    geom_out = VarTree(GeomData(2, 1), iotype='out')
    
    def list_deriv_vars(self):
        return ('x', 'y', 'z'), ('geom_out.points')

    def provideJ(self):
        self.J = np.array([[2, 0, 1],
                           [0, 2, 1]])

    def apply_deriv(self, arg, result):
        if 'x' in arg:
            result['geom_out'] += self.J[:, 0]*arg['x']
        if 'y' in arg:
            result['geom_out'] += self.J[:, 1]*arg['y']
        if 'z' in arg:
            result['geom_out'] += self.J[:, 2]*arg['z']

        return result

    def apply_derivT(self, arg, result):

        if 'x' in arg:
            result['x'] += self.J[:, 0]*arg['geom_out']
        if 'y' in arg:
            result['y'] += self.J[:, 1]*arg['geom_out']
        if 'z' in arg:
            result['z'] += self.J[:, 2]*arg['geom_out']

        return result

    def execute(self):
        x = self.x
        y = self.y
        z = self.z

        self.geom_out.points[0, :] = J[0, 0]*x + J[0, 1]*y + J[0, 2]*z
        self.geom_out.points[1, :] = J[1, 0]*x + J[1, 1]*y + J[1, 2]*z
        

class GeomRecieve(Component):

    geom_in = VarTree(GeomData(2, 1), iotype='in')
    out = Array([0, 0, 0], iotype='out')

    def execute(self):
        self.out = self.geom_in.points


class GeomRecieveDerivProvideJ(GeomRecieve):

    def provideJ(self):
        self.J = np.eye(2)
        return self.J

    def list_deriv_vars(self):
        return ('geom_in',), ('out',)

class GeomRecieveDerivApplyDeriv(GeomRecieve):

    def provideJ(self):
        self.J = np.eye(2)

    def list_deriv_vars(self):
        return ('geom_in',), ('out',)

    def apply_deriv(self, arg, result):
        if 'geom_in' in arg:
            result['out'] += self.J.dot(arg['geom_in'])

    def apply_derivT(self, arg, result):
        if 'out' in arg:
            result['geom_in'] += self.J.T.dot(arg['out'])

class Testcase_deriv_obj(unittest.TestCase):

    def _check_J(self, J):
        assert_rel_error(self, J[0, 0], 2.0, .00001)
        assert_rel_error(self, J[0, 1], 0.0, .00001)
        assert_rel_error(self, J[0, 2], 1.0, .00001)

        assert_rel_error(self, J[1, 0], 0.0, .00001)
        assert_rel_error(self, J[1, 1], 2.0, .00001)
        assert_rel_error(self, J[1, 2], 1.0, .00001)

        assert_rel_error(self, J[2, 0], 2.0, .00001)
        assert_rel_error(self, J[2, 1], 0.0, .00001)
        assert_rel_error(self, J[2, 2], 1.0, .00001)

        assert_rel_error(self, J[3, 0], 0.0, .00001)
        assert_rel_error(self, J[3, 1], 2.0, .00001)
        assert_rel_error(self, J[3, 2], 1.0, .00001)

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('c1', GeomComponent())
        self.top.add('c2', GeomRecieve())
        self.top.connect('c1.geom_out', 'c2.geom_in')
        self.top.driver.workflow.add(['c1', 'c2'])

        self.top.c1.x = 4.0
        self.top.c1.y = 10
        self.top.c1.z = 3.0

        self.inputs = ['c1.x', 'c1.y']
        self.outputs = ['c1.z','c2.out']

    def tearDown(self):
        self.top = None

    def _check_derivs(self):
        top = self.top
        inputs = self.inputs
        outputs = self.outputs

        J = top.driver.workflow.calc_gradient(inputs, outputs, mode='forward')
        self._check_J(J)

        J = top.driver.workflow.calc_gradient(inputs, outputs, mode='adjoint')
        self._check_J(J)

        J = top.driver.workflow.calc_gradient(inputs, outputs, mode='fd')
        self._check_J(J)

    def test_geom_provide_deriv_check_fd_tail(self):

        raise nose.SkipTest("OpenMDAO can't identify when half a connection is non-differntiable yet")
        #self.top.run()
        #self._check_derivs()

    def test_geom_provide_deriv_check_analytic_tail_provideJ(self):

        raise nose.SkipTest('ProvideJ not supported for non-differentiable conections yet')

        #self.top.replace('c2', GeomRecieveDerivProvideJ())
        #self.top.run()
        #self._check_derivs()

    def test_geom_provide_deriv_check_analytic_tail_apply_deriv(self):
        self.top.replace('c2', GeomRecieveDerivApplyDeriv())
        self.top.run()
        self._check_derivs()


if __name__ == '__main__':
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
