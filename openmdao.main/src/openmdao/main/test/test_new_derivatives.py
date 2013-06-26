"""
This mainly tests the CyclicWorkflow's ability to generate its topological
sort.
"""

import unittest

try:
    from numpy import zeros, array, identity
except ImportError as err:
    from openmdao.main.numpy_fallback import zeros, array

from openmdao.main.api import Component, VariableTree
from openmdao.main.datatypes.api import Array, Float, VarTree

class Tree2(VariableTree):

    d1 = Array(zeros((1, 2)))

class Tree1(VariableTree):

    a1 = Float(3.)
    vt1 = VarTree(Tree2())

class MyComp(Component):

    x1 = Float(0.0, iotype='in')
    x2 = Float(0.0, iotype='in')
    x3 = Array(zeros((2, 1)), iotype='in')
    x4 = Array(zeros((2, 2)), iotype='in')
    vt = VarTree(Tree1(), iotype='in')

    xx1 = Float(0.0, iotype='out')
    xx2 = Float(0.0, iotype='out')
    xx3 = Array(zeros((2, 1)), iotype='out')
    xx4 = Array(zeros((2, 2)), iotype='out')
    vvt = VarTree(Tree1(), iotype='out')

    def execute(self):
        """ doubler """
        pass

    def linearize(self):
        """ calculates the Jacobian """

        self.J = array([[1.5, 3.7, 2.5, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1, 11.1],
                        [7.4, 23.7, 1.1, 4.2, 5.2, 6.2, 7.2, 8.2, 9.2, 10.2, 11.2],
                        [5.5, 8.7, 1.9, 4.3, 5.3, 6.3, 7.3, 8.3, 9.3, 10.3, 11.3],
                        [1.4, 2.4, 3.4, 4.4, 5.4, 6.4, 7.4, 8.4, 9.4, 10.4, 11.4],
                        [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5],
                        [1.6, 2.6, 3.6, 4.6, 5.6, 6.6, 7.6, 8.6, 9.6, 10.6, 11.6],
                        [1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 9.7, 10.7, 11.7],
                        [1.8, 2.8, 3.8, 4.8, 5.8, 6.8, 7.8, 8.8, 9.8, 10.8, 11.8],
                        [1.9, 2.9, 3.9, 4.9, 5.9, 6.9, 7.9, 8.9, 9.9, 10.9, 11.9],
                        [1.10, 2.10, 3.10, 4.10, 5.10, 6.10, 7.10, 8.10, 9.10, 10.10, 11.10],
                        [1.11, 2.11, 3.11, 4.11, 5.11, 6.11, 7.11, 8.11, 9.11, 10.11, 11.11]])

    def provideJ(self):
        """ returns the Jacobian """

        input_keys = ('x1', 'x2', 'x3', 'x4', 'vt.a1', 'vt.vt1.d1')
        output_keys = ('xx1', 'xx2', 'xx3', 'xx4', 'vvt.a1', 'vvt.vt1.d1')

        return input_keys, output_keys, self.J


class Testcase(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        pass

    def tearDown(self):
        """ Called after each test. """
        pass
    
    def test_provideJ(self):

        comp = MyComp()
        comp.linearize()

        inputs = {}
        outputs = { 'xx1': None,
                    'xx2': None,
                    'xx3': None,
                    'xx4': None,
                    'vvt.a1': None,
                    'vvt.vt1.d1': None}

        num = 11
        ident = identity(num)

        for i in range(num):

            inputs['x1'] = ident[i, 0]
            inputs['x2'] = ident[i, 1]
            inputs['x3'] = ident[i, 2:4].reshape((2, 1))
            inputs['x4'] = ident[i, 4:8].reshape((2, 2))
            inputs['vt.a1'] = ident[i, 8]
            inputs['vt.vt1.d1'] = ident[i, 9:11].reshape((1, 2))

            inputs['xx1'] = 0
            inputs['xx2'] = 0
            inputs['xx3'] = zeros((2, 1))
            inputs['xx4'] = zeros((2, 2))
            inputs['vvt.a1'] = 0
            inputs['vvt.vt1.d1'] = zeros((1, 2))

            comp.applyJ(inputs, outputs)

            self.assertEqual(outputs['xx1'], comp.J[0, i])
            self.assertEqual(outputs['xx2'], comp.J[1, i])
            for j in range(2):
                self.assertEqual(outputs['xx3'][j], comp.J[2+j, i])
            for j in range(4):
                self.assertEqual(outputs['xx4'].flat[j], comp.J[4+j, i])
            self.assertEqual(outputs['vvt.a1'], comp.J[8, i])
            for j in range(2):
                self.assertEqual(outputs['vvt.vt1.d1'].flat[j], comp.J[9+j, i])


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

