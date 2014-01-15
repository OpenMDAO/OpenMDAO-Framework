"""
This mainly tests the CyclicWorkflow's ability to generate its topological
sort.
"""

import unittest

try:
    from numpy import zeros, array
except ImportError as err:
    from openmdao.main.numpy_fallback import zeros, array

from openmdao.main.api import Assembly, Component, CyclicWorkflow, VariableTree
from openmdao.main.datatypes.api import Array, Float, VarTree
from openmdao.main.test.test_derivatives import SimpleDriver


class MyComp(Component):

    x = Float(0.0, iotype='in')
    y = Float(0.0, iotype='out')
    xx = Float(0.0, iotype='in')
    yy = Float(0.0, iotype='out')

    def execute(self):
        """ doubler """
        self.y = 2.0*self.x


class Simple(Assembly):

    def configure(self):
        """ set it up """
        self.add('c1', MyComp())
        self.add('c2', MyComp())
        self.add('driver', SimpleDriver())

        self.driver.workflow = CyclicWorkflow()
        self.driver.workflow.add(['c1', 'c2'])

        self.connect('c1.y', 'c2.x')
        self.connect('c2.y', 'c1.x')


class MultiPath(Assembly):

    def configure(self):
        """ set it up """
        self.add('c1', MyComp())
        self.add('c2', MyComp())
        self.add('c3', MyComp())
        self.add('c4', MyComp())

        self.driver.workflow = CyclicWorkflow()
        self.driver.workflow.add(['c1', 'c2', 'c3', 'c4'])

        self.connect('c1.y', 'c2.x')
        self.connect('c2.y', 'c3.x')
        self.connect('c3.y', 'c4.x')
        self.connect('c4.y', 'c1.x')
        self.connect('c1.yy', 'c3.xx')
        self.connect('c3.yy', 'c1.xx')


class TestCase(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        self.model = None

    def tearDown(self):
        """ Called after each test. """
        pass

    def test_simple_flow(self):
        # Simple Case
        self.model = Simple()
        self.model.run()

        self.assertEqual(self.model.driver.workflow._topsort,
                         ['c2', 'c1'])

    def test_multi_flow(self):
        # 2 unique loops, 3 total loops
        self.model = MultiPath()
        self.model.run()

        self.assertEqual(self.model.driver.workflow._topsort,
                         ['c3', 'c4', 'c1', 'c2'])


class Tree2(VariableTree):

    c1 = Float(9.)
    d1 = Array([5.0, 11.0])


class Tree1(VariableTree):

    a1 = Float(3.)
    b1 = Array([3.0, 7.0])

    vt1 = VarTree(Tree2())


class TestCase_Residuals(unittest.TestCase):
    """ Test run/step/stop aspects of a simple workflow. """

    def setUp(self):
        """ Called before each test. """
        self.model = Assembly()
        self.model.add('c1', MyComp())
        self.model.add('c2', MyComp())
        self.model.add('driver', SimpleDriver())
        self.model.driver.workflow = CyclicWorkflow()
        self.model.driver.workflow.add(['c1', 'c2'])

    def tearDown(self):
        """ Called after each test. """
        self.model = None

    def test_column_vector(self):
        self.model.c1.add('y_a', Array(iotype='out'))
        self.model.c1.y_a = array([[1.0], [2.0]])

        self.model.c2.add('x_a', Array(iotype='in'))
        self.model.c2.x_a = array([[3.0], [6.0]])

        self.model.connect('c1.y_a', 'c2.x_a')
        self.model.connect('c2.y', 'c1.x')

        self.model.driver.workflow.initialize_residual()
        self.model.run()

        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 1.0)
        self.assertEqual(indep[1], 2.0)
        dep = self.model.driver.workflow.get_dependents()
        self.assertEqual(dep[0], 0.0)
        self.assertEqual(dep[1], 0.0)

        dv = array([3.0, 5.0])
        self.model.driver.workflow.set_independents(dv)
        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 3.0)
        self.assertEqual(indep[1], 5.0)

    def test_row_vector(self):
        self.model.c1.add('y_a', Array(iotype='out'))
        self.model.c1.y_a = array([[1.0, 2.0]])

        self.model.c2.add('x_a', Array(iotype='in'))
        self.model.c2.x_a = array([[3.0, 6.0]])

        self.model.connect('c1.y_a', 'c2.x_a')
        self.model.connect('c2.y', 'c1.x')

        self.model.driver.workflow.initialize_residual()
        self.model.run()

        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 1.0)
        self.assertEqual(indep[1], 2.0)
        dep = self.model.driver.workflow.get_dependents()
        self.assertEqual(dep[0], 0.0)
        self.assertEqual(dep[1], 0.0)

        dv = array([3.0, 5.0])
        self.model.driver.workflow.set_independents(dv)
        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 3.0)
        self.assertEqual(indep[1], 5.0)

    def test_array_1D(self):
        self.model.c1.add('y_a', Array(iotype='out'))
        self.model.c1.y_a = array([1.0, 2.0])

        self.model.c2.add('x_a', Array(iotype='in'))
        self.model.c2.x_a = array([3.0, 6.0])

        self.model.connect('c1.y_a', 'c2.x_a')
        self.model.connect('c2.y', 'c1.x')

        self.model.driver.workflow.initialize_residual()
        self.model.run()

        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 1.0)
        self.assertEqual(indep[1], 2.0)
        dep = self.model.driver.workflow.get_dependents()
        self.assertEqual(dep[0], 0.0)
        self.assertEqual(dep[1], 0.0)

        dv = array([3.0, 5.0])
        self.model.driver.workflow.set_independents(dv)
        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 3.0)
        self.assertEqual(indep[1], 5.0)

    def test_full_matrix(self):
        self.model.c1.add('y_a', Array(iotype='out'))
        self.model.c1.y_a = array([[1.0, 2.0], [3.0, 4.0]])

        self.model.c2.add('x_a', Array(iotype='in'))
        self.model.c2.x_a = array([[2.0, 5.0], [11.0, 17.0]])

        self.model.connect('c1.y_a', 'c2.x_a')
        self.model.connect('c2.y', 'c1.x')

        self.model.driver.workflow.initialize_residual()
        self.model.run()

        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 1.0)
        self.assertEqual(indep[1], 2.0)
        self.assertEqual(indep[2], 3.0)
        self.assertEqual(indep[3], 4.0)
        dep = self.model.driver.workflow.get_dependents()
        self.assertEqual(dep[0], 0.0)
        self.assertEqual(dep[1], 0.0)
        self.assertEqual(dep[2], 0.0)
        self.assertEqual(dep[3], 0.0)

        dv = array([3.0, 5.0, -8.0, -13.0])
        self.model.driver.workflow.set_independents(dv)
        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 3.0)
        self.assertEqual(indep[1], 5.0)
        self.assertEqual(indep[2], -8.0)
        self.assertEqual(indep[3], -13.0)

    def test_matrix_element(self):
        # Array element to scalar.

        self.model.c1.add('y_a', Array(iotype='out'))
        self.model.c1.y_a = array([[1.0, 2.0], [3.0, 4.0]])
        self.model.c2.x = 7.0

        self.model.connect('c1.y_a[0, 0]', 'c2.x')
        self.model.connect('c2.y', 'c1.x')

        self.model.driver.workflow.initialize_residual()
        self.model.run()

        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 1.0)
        dep = self.model.driver.workflow.get_dependents()
        self.assertEqual(dep[0], 0.0)

        dv = array([35.0])
        self.model.driver.workflow.set_independents(dv)
        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 35.0)

    def test_vtree(self):
        self.model.c1.add('vt_out', VarTree(Tree1(), iotype='out'))

        self.model.c2.add('vt_in', VarTree(Tree1(), iotype='in'))
        self.model.c2.vt_in.a1 = 4.
        self.model.c2.vt_in.b1 = array([7.0, 12.0])
        self.model.c2.vt_in.vt1.c1 = 13.
        self.model.c2.vt_in.vt1.d1 = array([-1.0, 2.0])

        self.model.connect('c1.vt_out', 'c2.vt_in')
        self.model.connect('c2.y', 'c1.x')

        self.model.driver.workflow.initialize_residual()

        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 1.0)
        dep = self.model.driver.workflow.get_dependents()
        self.assertEqual(dep[0], 0.0)

        dv = array([35.0])
        self.model.driver.workflow.set_independents(dv)
        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 35.0)

    def test_vtree_leaf(self):
        self.model.c1.add('vt_out', VarTree(Tree1(), iotype='out'))

        self.model.c2.add('vt_in', VarTree(Tree1(), iotype='in'))
        self.model.c2.vt_in.b1 = array([7.0, 12.0])

        self.model.connect('c1.vt_out.b1', 'c2.vt_in.b1')
        self.model.connect('c2.y', 'c1.x')

        self.model.driver.workflow.initialize_residual()

        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 7.0)
        self.assertEqual(indep[1], 12.0)
        dep = self.model.driver.workflow.get_dependents()
        self.assertEqual(dep[0], -4.0)
        self.assertEqual(dep[1], -5.0)

        dv = array([13.0, 15.0])
        self.model.driver.workflow.set_independents(dv)
        indep = self.model.driver.workflow.get_independents()
        self.assertEqual(indep[0], 13.0)
        self.assertEqual(indep[1], 15.0)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
