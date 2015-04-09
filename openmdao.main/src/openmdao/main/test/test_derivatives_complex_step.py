"""
Testing for complex step support in the framework.
"""

import unittest

from numpy import array, eye, asarray, testing, zeros

from openmdao.examples.simple.optimization_constrained import OptimizationConstrained
from openmdao.main.api import Component, VariableTree, Assembly, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree, Str
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.util.testutil import assert_rel_error

class SimpleCompFloat(Component):

    x = Float(3.0, iotype='in')
    y = Float(6.0, iotype='out')

    def execute(self):
        self.y = 2.0*self.x

class SimpleCompArray(Component):

    x = Array(array([[2.0, 4.0], [1.0, 3.0]]), iotype='in')
    y = Array(iotype='out')

    def execute(self):
        self.J = array([[1.0, 3.0, 2.0, 5.0],
                        [-1.0, 3.0, 7.0, -5.0],
                        [4.0, 4.0, 3.0, -3.0],
                        [2.0, 5.0, 1.5, 2.0]])
        self.y = self.J.dot(self.x.flatten()).reshape((2, 2))

class ComplexArray(Component):

    x = Array(array([2.0, 4.0], dtype=complex), iotype='in')
    y = Array(array([0.0, 0.0], dtype=complex), iotype='out')

    def execute(self):
        y = array([0.0, 0.0], dtype=complex)
        y[0] = 2.0*self.x[0] + self.x[1]
        y[1] = 3.0*self.x[0] - 2.0*self.x[1]
        self.y = y


class TreeWithFloat(VariableTree):

    x = Float(5.0)

class TreeWithSubTree(VariableTree):

    x = Float(3.)
    sub = VarTree(TreeWithFloat())

class CompWithVarTreeSubTree(Component):

    ins = VarTree(TreeWithSubTree(), iotype="in")
    outs = VarTree(TreeWithSubTree(), iotype="out")

    def execute(self):

        self.outs.x = 2.0*self.ins.x + 3.0*self.ins.sub.x
        self.outs.sub.x = 4.0*self.ins.x + 1.0*self.ins.sub.x


class TreeWithArray(VariableTree):

    x = Array(array([[2.0, 4.0], [1.0, 3.0]]))

class TreeWithArraySubTree(VariableTree):

    x = Array(array([[2.0, 4.0], [1.0, 3.0]]))
    sub = VarTree(TreeWithArray())

class CompWithArrayVarTreeSubTree(Component):

    ins = VarTree(TreeWithArraySubTree(), iotype="in")
    outs = VarTree(TreeWithArraySubTree(), iotype="out")

    def execute(self):
        self.J1 = array([[1.0, 3.0, 2.0, 5.0],
                        [-1.0, 3.0, 7.0, -5.0],
                        [4.0, 4.0, 3.0, -3.0],
                        [2.0, 5.0, 1.0, 2.0]])
        self.J2 = .9*self.J1
        self.J3 = 1.1*self.J1
        self.J4 = .7*self.J1

        x1 = self.ins.x.flatten()
        x2 = self.ins.sub.x.flatten()

        self.outs.x = (self.J1.dot(x1) + self.J2.dot(x2)).reshape((2, 2))
        self.outs.sub.x = (self.J3.dot(x1) + self.J4.dot(x2)).reshape((2, 2))

class Paraboloid_Mixed(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', fd_form='complex_step', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')


    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0


class OptRosenSuzukiComponent(Component):
    """ From the CONMIN User's Manual:
    EXAMPLE 1 - CONSTRAINED ROSEN-SUZUKI FUNCTION. NO GRADIENT INFORMATION.

         MINIMIZE OBJ = X(1)**2 - 5*X(1) + X(2)**2 - 5*X(2) +
                        2*X(3)**2 - 21*X(3) + X(4)**2 + 7*X(4) + 50

         Subject to:

              G(1) = X(1)**2 + X(1) + X(2)**2 - X(2) +
                     X(3)**2 + X(3) + X(4)**2 - X(4) - 8   .LE.0

              G(2) = X(1)**2 - X(1) + 2*X(2)**2 + X(3)**2 +
                     2*X(4)**2 - X(4) - 10                  .LE.0

              G(3) = 2*X(1)**2 + 2*X(1) + X(2)**2 - X(2) +
                     X(3)**2 - X(4) - 5                     .LE.0

    This problem is solved beginning with an initial X-vector of
         X = (1.0, 1.0, 1.0, 1.0)
    The optimum design is known to be
         OBJ = 6.000
    and the corresponding X-vector is
         X = (0.0, 1.0, 2.0, -1.0)
    """

    x = Array(iotype='in', low=-10, high=99)
    g = Array([1., 1., 1.], iotype='out')
    result = Float(iotype='out')
    obj_string = Str(iotype='out')
    opt_objective = Float(iotype='out')

    # pylint: disable=C0103
    def __init__(self):
        super(OptRosenSuzukiComponent, self).__init__()
        self.x = array([1., 1., 1., 1.], dtype=float)
        self.result = 0.

        self.opt_objective = 6.
        self.opt_design_vars = [0., 1., 2., -1.]

    def execute(self):
        """calculate the new objective value"""
        x = self.x

        self.result = (x[0]**2 - 5.*x[0] + x[1]**2 - 5.*x[1] +
                       2.*x[2]**2 - 21.*x[2] + x[3]**2 + 7.*x[3] + 50)

        self.obj_string = "Bad"

        # cast constraint vector according to input type
        # to deal with complex inputs
        self.g = zeros(3, dtype=self.x.dtype)
        self.g[0] = (x[0]**2 + x[0] + x[1]**2 - x[1] +
                     x[2]**2 + x[2] + x[3]**2 - x[3] - 8)
        self.g[1] = (x[0]**2 - x[0] + 2.*x[1]**2 + x[2]**2 +
                     2.*x[3]**2 - x[3] - 10)
        self.g[2] = (2*x[0]**2 + 2*x[0] + x[1]**2 - x[1] +
                     x[2]**2 - x[3] - 5)

class Asym(Assembly):

    def configure(self):

        self.add('c', OptRosenSuzukiComponent())
        self.driver.workflow.add('c')

        self.create_passthrough('c.x')
        self.create_passthrough('c.g')
        self.create_passthrough('c.result')


class NestedProblem(Assembly):

    def configure(self):

        self.add('driver', SimpleDriver())
        self.driver.iout = 1
        self.driver.iprint = 3

        self.driver.gradient_options.force_fd = True
        self.driver.gradient_options.fd_step = 1.e-20
        self.driver.gradient_options.fd_form = 'complex_step'

        self.add('c',  Asym())
        self.driver.workflow.add('c')

        # this works
        # self.driver.add_parameter('c.x', start=numpy.array([1., 1., 1., 1.]))
        # this doesn't work
        self.driver.add_parameter('c.x[0]', start=1.)
        self.driver.add_parameter('c.x[1]', start=1.)
        self.driver.add_parameter('c.x[2]', start=1.)
        self.driver.add_parameter('c.x[3]', start=1.)
        self.driver.add_constraint('c.g < 0.')
        self.driver.add_objective('c.result')


class Testcase_ComplexStep_Traits(unittest.TestCase):
    """ Make sure trait Float works for complex stepping. """


    def test_float(self):

        model = set_as_top(Assembly())
        model.add('comp', SimpleCompFloat())
        model.driver.workflow.add('comp')

        model.comp.x = 3+4j
        model.run()

        self.assertEqual(model.comp.x, 3+4j)
        self.assertEqual(model.comp.y, 6+8j)

        # Set it back

        model.comp.x = 3
        model.run()

        self.assertEqual(model.comp.x, 3.0)
        self.assertEqual(model.comp.y, 6.0)

        # Make sure we can do whole workflows.
        model.add('comp2', SimpleCompFloat())
        model.driver.workflow.add('comp2')
        model.connect('comp.y', 'comp2.x')

        model.comp.x = 3+4j
        model.run()

        # Note, this stuff is not meant to be done interactively, so we have to
        # work around a little.
        model._system.set_complex_step(True)
        model.run()

        self.assertEqual(model.comp.x, 3+4j)
        self.assertEqual(model.comp2.y, 12+16j)


    def test_float_in_vartree(self):

        model = set_as_top(Assembly())
        model.add('comp', CompWithVarTreeSubTree())
        model.driver.workflow.add('comp')

        model.comp.ins.x = 2+1j
        model.comp.ins.sub.x = 5+3j
        model.run()

        self.assertEqual(model.comp.ins.x, 2+1j)
        self.assertEqual(model.comp.ins.sub.x, 5+3j)
        self.assertEqual(model.comp.outs.x, 19+11j)
        self.assertEqual(model.comp.outs.sub.x, 13+7j)

    def test_array(self):

        model = set_as_top(Assembly())
        model.add('comp', SimpleCompArray())
        model.driver.workflow.add('comp')

        model.comp.x = model.comp.x.astype('complex')
        model.comp.x[1, 1] = 3+4j
        model.run()

        print model.comp.y

        y_check = array([[31.0+20.0j, 2.0-20.0j], [18.0-12.0j, 31.5+8.0j]])
        self.assertEqual(model.comp.x[1, 1], 3+4j)
        self.assertEqual(model.comp.y[0, 0], y_check[0, 0])
        self.assertEqual(model.comp.y[0, 1], y_check[0, 1])
        self.assertEqual(model.comp.y[1, 0], y_check[1, 0])
        self.assertEqual(model.comp.y[1, 1], y_check[1, 1])

    def test_array_in_vartree(self):

        model = set_as_top(Assembly())
        model.add('comp', CompWithArrayVarTreeSubTree())
        model.driver.workflow.add('comp')

        model.comp.ins.x = model.comp.ins.x.astype('complex')
        model.comp.ins.x[1, 1] = 1+2j
        model.comp.ins.sub.x = model.comp.ins.sub.x.astype('complex')
        model.comp.ins.sub.x[1, 1] = 3+4j
        model.run()

        y1_check = array([[48.9+28.j, 13.80-28.j], [40.20-16.8j, 54.9+11.2j]])
        y2_check = array([[44.8+25.j,  14.6-25.j], [ 39.0-15.j, 51.4+10.j]])

        self.assertEqual(model.comp.ins.x[1, 1], 1+2j)
        self.assertEqual(model.comp.ins.sub.x[1, 1], 3+4j)
        diff = abs(y1_check - model.comp.outs.x).max()
        assert_rel_error(self, diff, 0.0, .0001)
        diff = abs(y2_check - model.comp.outs.sub.x).max()
        assert_rel_error(self, diff, 0.0, .0001)

class Testcase_ComplexStep_Derivatives(unittest.TestCase):
    """ Make sure complex step works in our derivative system. """

    def test_simple_float(self):

        model = set_as_top(Assembly())
        model.add('comp', SimpleCompFloat())
        model.driver.workflow.add('comp')
        model.driver.gradient_options.fd_form = 'complex_step'

        model.run()

        J = model.driver.calc_gradient(inputs=['comp.x'],
                                       outputs=['comp.y'])

        assert_rel_error(self, J[0, 0], 2.0, .000001)
        self.assertTrue(model.comp.x is not complex)
        self.assertTrue(model.comp.y is not complex)

        # Make sure we can do whole workflows.
        model.add('comp2', SimpleCompFloat())
        model.driver.workflow.add('comp2')
        model.connect('comp.y', 'comp2.x')

        model.run()

        J = model.driver.calc_gradient(inputs=['comp.x'],
                                       outputs=['comp2.y'])

        assert_rel_error(self, J[0, 0], 4.0, .000001)
        self.assertTrue(model.comp.x is not complex)
        self.assertTrue(model.comp2.y is not complex)

    def test_simple_float_subassy(self):

        model = set_as_top(Assembly())
        model.add('sub', Assembly())
        model.sub.add('subsub', Assembly())
        model.driver.workflow.add('sub')
        model.sub.driver.workflow.add('subsub')

        model.sub.subsub.add('comp', SimpleCompFloat())
        model.sub.subsub.driver.workflow.add('comp')
        model.sub.subsub.create_passthrough('comp.x')
        model.sub.subsub.create_passthrough('comp.y')
        model.sub.create_passthrough('subsub.x')
        model.sub.create_passthrough('subsub.y')

        model.driver.gradient_options.fd_form = 'complex_step'
        model.driver.gradient_options.force_fd = True
        model.run()

        J = model.driver.calc_gradient(inputs=['sub.x'],
                                       outputs=['sub.y'])

        assert_rel_error(self, J[0, 0], 2.0, .000001)
        self.assertTrue(model.sub.subsub.comp.x is not complex)
        self.assertTrue(model.sub.subsub.comp.y is not complex)

    def test_simple_float_in_vartree(self):

        model = set_as_top(Assembly())
        model.add('comp', CompWithVarTreeSubTree())
        model.driver.workflow.add('comp')
        model.driver.gradient_options.fd_form = 'complex_step'

        model.run()

        J = model.driver.calc_gradient(inputs=['comp.ins.x', 'comp.ins.sub.x'],
                                       outputs=['comp.outs.x', 'comp.outs.sub.x'])

        assert_rel_error(self, J[0, 0], 2.0, .000001)
        assert_rel_error(self, J[0, 1], 3.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)
        assert_rel_error(self, J[1, 1], 1.0, .000001)
        self.assertTrue(model.comp.ins.x is not complex)
        self.assertTrue(model.comp.ins.sub.x is not complex)
        self.assertTrue(model.comp.outs.x is not complex)
        self.assertTrue(model.comp.outs.sub.x is not complex)

    def test_simple_array(self):

        model = set_as_top(Assembly())
        model.add('comp', SimpleCompArray())
        model.driver.workflow.add('comp')
        model.driver.gradient_options.fd_form = 'complex_step'
        model.run()

        J = model.driver.calc_gradient(inputs=['comp.x'],
                                       outputs=['comp.y'])
        diff = abs(J - model.comp.J).max()
        assert_rel_error(self, diff, 0.0, .0001)
        self.assertTrue(J[0, 0] is not complex)

        model.add('driver', SimpleDriver())
        model.driver.add_parameter('comp.x', low=-10, high=10)
        model.driver.add_objective('comp.y - comp.x')
        model.run()

        J = model.driver.calc_gradient(mode='fd')

        diff = abs(J + eye(4) - model.comp.J).max()
        assert_rel_error(self, diff, 0.0, .0001)
        self.assertTrue(J[0, 0] is not complex)

    def test_complex_array_data_passing(self):

        model = set_as_top(Assembly())
        model.add('comp1', ComplexArray())
        model.add('comp2', ComplexArray())
        model.driver.workflow.add(['comp1', 'comp2'])
        model.connect('comp1.y', 'comp2.x')

        model.driver.gradient_options.fd_form = 'complex_step'
        model.run()

        J = model.driver.calc_gradient(inputs=['comp1.x'],
                                       outputs=['comp2.y'])

        assert_rel_error(self, J[0, 0], 7.0, .0001)
        assert_rel_error(self, J[0, 1], 0.0, .0001)
        assert_rel_error(self, J[1, 1], 7.0, .0001)
        assert_rel_error(self, J[1, 0], 0.0, .0001)

    def test_mixed_CS_FD(self):

        model = set_as_top(Assembly())
        model.add('comp', Paraboloid_Mixed())
        model.driver.workflow.add('comp')

        model.run()

        J = model.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                       outputs=['comp.f_xy'])

        assert_rel_error(self, J[0, 0], -6.0, .0001)
        assert_rel_error(self, J[0, 1], 8.0, .0001)


    def test_optimization(self):

        model = set_as_top(OptimizationConstrained())
        model.driver.gradient_options.fd_form = 'complex_step'
        model.run()

        assert_rel_error(self, model.paraboloid.x, 7.175775, 0.01)
        assert_rel_error(self, model.paraboloid.y, -7.824225, 0.01)

    def test_optimization2(self):

        model = set_as_top(OptimizationConstrained())
        model.driver.gradient_options.fd_form = 'complex_step'
        # Full model complex step (to test Pseudocomps)
        model.driver.gradient_options.force_fd = True
        model.run()

        assert_rel_error(self, model.paraboloid.x, 7.175775, 0.01)
        assert_rel_error(self, model.paraboloid.y, -7.824225, 0.01)

    def test_nested_rosen(self):

        top = set_as_top(NestedProblem())
        J = top.driver.calc_gradient(top.driver.list_param_targets(), top.driver.list_objective_targets())
        self.assertEqual(testing.assert_array_almost_equal(J, array([[ -3.,  -3., -17.,   9.]]), decimal=6), None)


if __name__ == '__main__':

    unittest.main()
