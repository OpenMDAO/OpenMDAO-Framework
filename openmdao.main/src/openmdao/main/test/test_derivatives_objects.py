"""
Testing differentiation of user-defined datatypes.
"""

from nose import SkipTest
import unittest

import numpy as np

from openmdao.lib.components.geomcomp import GeomComponent
from openmdao.lib.geometry.box import BoxParametricGeometry
from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, Str
from openmdao.main.interfaces import IParametricGeometry, implements, \
                                     IStaticGeometry
from openmdao.main.variable import Variable
from openmdao.util.testutil import assert_rel_error


class DataObject(object):

    def __init__(self):
        self.x = 0
        self.y = 0
        self.z = 0

    def set(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def get(self):
        return self.x, self.y, self.z

class Comp_Send(Component):
    '''Passes a data object as output.'''

    p1 = Float(0.0, iotype='in')
    p2 = Float(0.0, iotype='in')

    data = Variable(DataObject(), iotype='out', data_shape=(3, ))
    dummy = Float(1.0, iotype='out')

    def execute(self):
        ''' Load computation result into self.data.'''

        p1 = self.p1
        p2 = self.p2

        x = p1*p1 + p2
        y = p2*p2 - p1
        z = 2.0*p1 + 3.0*p2

        self.data.set(x, y, z)

    def provideJ(self):
        ''' Jacobian'''

        dxdp1 = 2.0*self.p1
        dxdp2 = 1.0
        dydp1 = -1.0
        dydp2 = 2.0*self.p2
        dzdp1 = 2.0
        dzdp2 = 3.0

        self.J = np.array([[dxdp1, dxdp2], [dydp1, dydp2], [dzdp1, dzdp2]])

    def apply_deriv(self, arg, result):

        if 'data' in result:

            result['data'] += self.J[:, 0]*arg['p1']
            result['data'] += self.J[:, 1]*arg['p2']

    def apply_derivT(self, arg, result):

        if 'data' in arg:

            result['p1'] += self.J.T[0, :].dot(arg['data'])
            result['p2'] += self.J.T[1, :].dot(arg['data'])

class Comp_Receive(Component):
    '''Takes a data object as input.'''

    data = Variable(iotype='in')

    q1 = Float(0.0, iotype='out')
    q2 = Float(0.0, iotype='out')
    q3 = Float(0.0, iotype='out')

    dummy = Float(0.0, iotype='in')

    def execute(self):

        x, y, z = self.data.get()

        self.q1 = -1.0*x
        self.q2 = 2.0*y
        self.q3 = 3.0*z


class Comp_Receive_ProvideJ(Comp_Receive):
    '''Takes a data object as input.'''

    def provideJ(self):
        ''' Jacobian'''

        self.J = np.array([[-1.0, 0.0, 0.0],
                           [0.0, 2.0, 0.0],
                           [0.0, 0.0, 3.0]])
        return self.J

    def list_deriv_vars(self):
        return ('data',), ('q1','q2','q3')

class Comp_Receive_ApplyDeriv(Comp_Receive):
    '''Takes a data object as input.'''

    def provideJ(self):
        ''' Jacobian'''

        self.J = np.array([[-1.0, 0.0, 0.0],
                           [0.0, 2.0, 0.0],
                           [0.0, 0.0, 3.0]])

    def apply_deriv(self, arg, result):

        if 'data' in arg:

            result['q1'] += self.J[:, 0].dot(arg['data'])
            result['q2'] += self.J[:, 1].dot(arg['data'])
            result['q3'] += self.J[:, 2].dot(arg['data'])

    def apply_derivT(self, arg, result):

        if 'data' in result:

            result['data'] += self.J.T[0, :]*arg['q1']
            result['data'] += self.J.T[1, :]*arg['q2']
            result['data'] += self.J.T[2, :]*arg['q3']

class TestcaseDerivObj(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        self.top = set_as_top(Assembly())
        self.top.add('c1', Comp_Send())
        self.top.add('c2', Comp_Receive())
        self.top.connect('c1.data', 'c2.data')
        self.top.driver.workflow.add(['c1', 'c2'])

        self.top.c1.p1 = 3.0
        self.top.c1.p2 = 5.0

        self.inputs = ['c1.p1', 'c1.p2']
        self.outputs = ['c2.q1', 'c2.q2', 'c2.q3']

    def tearDown(self):
        self.top = None

    def _check_derivs(self):
        """ Called after each test. """
        inputs = self.inputs
        outputs = self.outputs
        top = self.top

        J = top.driver.workflow.calc_gradient(inputs, outputs, mode='fd')
        self._check_J(J)

        top.driver.workflow.config_changed()
        J = top.driver.workflow.calc_gradient(inputs, outputs, mode='forward')
        self._check_J(J)

        top.driver.workflow.config_changed()
        J = top.driver.workflow.calc_gradient(inputs, outputs, mode='adjoint')
        self._check_J(J)

        edges = top.driver.workflow._edges
        self.assertTrue(edges['c1.data'] == ['c2.data'])

    def _check_J(self, J):
        assert_rel_error(self, J[0, 0], -6.0, .00001)
        assert_rel_error(self, J[0, 1], -1.0, .00001)
        assert_rel_error(self, J[1, 0], -2.0, .00001)
        assert_rel_error(self, J[1, 1], 20.0, .00001)
        assert_rel_error(self, J[2, 0], 6.0, .00001)
        assert_rel_error(self, J[2, 1], 9.0, .00001)

    def test_analytic_tail_provideJ(self):

        raise SkipTest('ProvideJ not supported for non-differentiable conections yet')

        self.top.replace('c2', Comp_Receive_ProvideJ())
        self.top.run()
        self._check_derivs()

    def test_analytic_tail_apply_deriv(self):
        self.top.replace('c2', Comp_Receive_ApplyDeriv())
        self.top.run()
        self._check_derivs()


class GeoWithDerivatives(BoxParametricGeometry):
    '''Adds derivative functions to the famous box geometry.'''

    implements(IParametricGeometry, IStaticGeometry)

    def apply_deriv(self, arg, result):
        pass

    def apply_derivT(self, arg, result):
        pass

    def provideJ(self):
        pass

class Testcase_geom_deriv(unittest.TestCase):
    """ Test a simple object that passes a geometry. """

    def setUp(self):
        """ Called before each test. """
        pass

    def tearDown(self):
        """ Called after each test. """
        pass

    def test_basic_delegation(self):

        top = Assembly()
        top.add('geo', GeomComponent())

        # Function not there before we slot
        self.assertTrue(not hasattr(top.geo, 'apply_deriv'))
        self.assertTrue(not hasattr(top.geo, 'apply_derivT'))
        self.assertTrue(not hasattr(top.geo, 'provideJ'))

        top.geo.add('parametric_geometry', GeoWithDerivatives())

        # Now they should be there.
        self.assertTrue(hasattr(top.geo, 'apply_deriv'))
        self.assertTrue(hasattr(top.geo, 'apply_derivT'))
        self.assertTrue(hasattr(top.geo, 'provideJ'))


class ND_Send(Component):
    '''Passes a data object as output.'''

    p1 = Float(5.0, iotype='in')
    z1 = Float(5.0, iotype='out')

    data = Str("Try to differentiate this!", iotype='out')

    def list_deriv_vars(self):
        return ['p1'], ['z1']

    def execute(self):
        ''' Load computation result into self.data.'''
        self.data = str(self.p1)
        self.z1 = 2.0*self.p1

    def provideJ(self):
        return np.array([[2.0]])

    def apply_deriv(self, arg, result):
        pass

    def apply_derivT(self, arg, result):
        pass

class ND_Receive(Component):
    '''Takes a data object as input.'''
    
    data = Str("Try to differentiate this!", iotype='in')

    x1 = Float(0.0, iotype='in')
    p1 = Float(0.0, iotype='out')

    def list_deriv_vars(self):
        return ['x1'], ['p1']

    def execute(self):
        ''' Load computation result into self.data.'''
        self.p1 = 2.0*float(self.data) + 3.0*self.x1
        print self.data, self.x1, self.p1

    def provideJ(self):
        return np.array([[3.0]])

    def apply_deriv(self, arg, result):
        pass

    def apply_derivT(self, arg, result):
        pass

class TestcaseNonDiff(unittest.TestCase):
    """ Test how OpenMDAO handles differentiation. """

    def test_non_diff(self):
        # Test grouping comps with non-differentiable connections.
        model = set_as_top(Assembly())
        model.add('comp1', ND_Send())
        model.add('comp2', ND_Receive())
        model.connect('comp1.data', 'comp2.data')
        model.driver.workflow.add(['comp1', 'comp2'])
        model.run()

        inputs = ['comp1.p1']
        outputs = ['comp2.p1']
        J = model.driver.workflow.calc_gradient(inputs, outputs, mode='forward')
        self.assertAlmostEqual(J[0, 0], 2.0)

        edges = model.driver.workflow._edges
        self.assertTrue('c1.data' not in edges)

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
