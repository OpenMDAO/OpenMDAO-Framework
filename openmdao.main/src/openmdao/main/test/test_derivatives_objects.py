"""
Testing differentiation of user-defined datatypes.
"""

from nose import SkipTest
import unittest

import numpy as np

from openmdao.main.api import Component, Assembly, set_as_top, Workflow
from openmdao.main.datatypes.api import Float, Str, Int
from openmdao.main.depgraph import simple_node_iter
from openmdao.main.test.simpledriver import SimpleDriver
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

    def get_flattened_size(self):
        return 3

    def get_flattened_value(self):
        return np.array([self.x, self.y, self.z])

    def set_flattened_value(self, val):
        self.x = val[0]
        self.y = val[1]
        self.z = val[2]

class Comp_Send(Component):
    '''Passes a data object as output.'''

    p1 = Float(0.0, iotype='in')
    p2 = Float(0.0, iotype='in')

    data = Variable(DataObject(), iotype='out')
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

    def list_deriv_vars(self):
        return ('p1', 'p2'), ('data', 'dummy')

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

    data = Variable(DataObject(), iotype='in')

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

    def list_deriv_vars(self):
        return ('data',), ('q1','q2','q3')

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

        J = top.driver.calc_gradient(inputs, outputs, mode='fd')
        self._check_J(J)

        J = top.driver.calc_gradient(inputs, outputs, mode='forward')
        self._check_J(J)

        J = top.driver.calc_gradient(inputs, outputs, mode='adjoint')
        self._check_J(J)

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


class ND_Send(Component):
    '''Passes a data object as output.'''

    x = Float(5.0, iotype='in')
    y = Float(iotype='out')
    n = Int(iotype='out')


    def list_deriv_vars(self):
        return ('x',), ('y',)

    def execute(self):
        ''' Load computation result into self.data.'''
        self.n = int(self.x)
        self.y = self.x/2.0

    def provideJ(self):
        return np.array( [[0.5]] )

class ND_Receive(Component):
    '''Receives as inpu.'''

    x = Float(5.0, iotype='in')
    n = Int(1, iotype='in')
    y = Float(iotype='out')


    def list_deriv_vars(self):
        return ('x',), ('y',)

    def execute(self):
        ''' Load computation result into self.data.'''
        self.y = self.x * float(self.n)

    def provideJ(self):
        return np.array( [[float(self.n)]] )



class TestcaseNonDiff(unittest.TestCase):
    """ Test how OpenMDAO handles differentiation. """

    def test_non_diff(self):
        # Test grouping comps with non-differentiable connections.
        model = set_as_top(Assembly())
        model.add('driver', SimpleDriver())
        model.add('comp1', ND_Send())
        model.add('comp2', ND_Receive())
        model.connect('comp1.y', 'comp2.x')
        model.connect('comp1.n', 'comp2.n')
        model.driver.workflow.add(['comp1', 'comp2'])
        model.run()

        inputs = ['comp1.x']
        outputs = ['comp2.y']
        J = model.driver.calc_gradient(inputs, outputs, mode='forward')

        self.assertAlmostEqual(J[0, 0], 2.5)
        msystem = model.driver.workflow._system
        self.assertTrue(len(msystem.subsystems()) == 2)
        self.assertTrue(len(msystem.subsystems()[1]._inner_system.subsystems()) == 3)
        self.assertTrue(msystem.subsystems()[1]._inner_system.subsystems()[1].name == 'comp1')
        self.assertTrue(msystem.subsystems()[1]._inner_system.subsystems()[2].name == 'comp2')

        J = model.driver.calc_gradient(inputs, outputs, mode='fd')

    def test_non_diff_subassy(self):
        # What about subassys?

        model = set_as_top(Assembly())
        model.add('sub', Assembly())
        model.add('comp1', ND_Send())
        model.sub.add('comp2', ND_Receive())
        model.sub.create_passthrough('comp2.x')
        model.sub.create_passthrough('comp2.y')
        model.sub.create_passthrough('comp2.n')
        model.connect('comp1.y', 'sub.x')
        model.connect('comp1.n', 'sub.n')
        model.driver.workflow.add(['comp1', 'sub'])
        model.sub.driver.workflow.add(['comp2'])
        model.run()

        inputs = ['comp1.x']
        outputs = ['sub.y']
        J = model.driver.calc_gradient(inputs, outputs, mode='forward')

        self.assertAlmostEqual(J[0, 0], 2.5)

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
