"""
Basic unit testing of OpenMDAO's derivative capability.
"""

from cStringIO import StringIO
from nose import SkipTest
import re
import unittest
from mock import Mock

from numpy import zeros, array, random
import numpy as np

from openmdao.lib.architectures.api import MDF, CO
from openmdao.lib.optproblems.api import UnitScalableProblem

import openmdao.main.derivatives
from openmdao.main.api import Component, VariableTree, \
                              ImplicitComponent, Assembly, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree, Int
from openmdao.main.depgraph import simple_node_iter
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecCompWithDerivatives, ExecComp
from openmdao.util.testutil import assert_rel_error

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

    def provideJ(self):
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

        return self.J

    def list_deriv_vars(self):
        input_keys = ('x1', 'x2', 'x3', 'x4', 'vt.a1', 'vt.vt1.d1')
        output_keys = ('xx1', 'xx2', 'xx3', 'xx4', 'vvt.a1', 'vvt.vt1.d1')

        return input_keys, output_keys

class IntComp(Component):
    x = Float(0.0, iotype='in')
    x_ignore = Float(0.0, iotype='in', deriv_ignore=True)
    int_in = Int(0, iotype='in', deriv_ignore=True)
    int_out = Int(0, iotype='out', deriv_ignore=True)
    y = Float(0.0, iotype='out')
    y_ignore = Float(0.0, iotype='out', deriv_ignore=True)

    def execute(self):
        self.y = 2.0*self.x
        self.int_out = self.int_in

    def list_deriv_vars(self):
        return ('x',), ('y',)

    def provideJ(self):
        return array([[2.0]])

class BadListDerivsComp(Component):
    x = Float(iotype='in')
    y = Float(iotype='out')

    def execute(self):
        self.y = self.x * 2.0

    def list_deriv_vars(self):
        return ['x', 'y']

    def provideJ(self):
        return array([[2.0]])

class VarTreeDerivVarComp(Component):
    x = VarTree(VariableTree(), iotype='in')
    y = Float(iotype='in')

    z = Float(iotype='out')

    def execute(self):
        pass

    def list_deriv_vars(self):
        return ('x', 'y'), ('z')

    def provideJ(self):
        return array([[2.0]])

class UndefinedDerivVarComp(Component):
    x = Float(iotype='in')
    y = Float(iotype='in')

    z = Float(iotype='out')

    def execute(self):
        pass

    def list_deriv_vars(self):
        return ('x', 'y', 'w'), ('z')

    def provideJ(self):
        return array([[2.0]])


class UnflattenableDerivVarComp(Component):
    x = Int(iotype='in')
    y = Float(iotype='in')

    z = Float(iotype='out')

    def execute(self):
        pass

    def list_deriv_vars(self):
        return ('x', 'y'), ('z')

    def provideJ(self):
        return array([[2.0]])

class Paraboloid(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')

    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0

    def provideJ(self):
        """Analytical first derivatives"""

        df_dx = 2.0*self.x - 6.0 + self.y
        df_dy = 2.0*self.y + 8.0 + self.x

        self.J = array([[df_dx, df_dy]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        return input_keys, output_keys

class ParaboloidNoDeriv(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')

    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + x*y + (y+4.0)**2 - 3.0


class SimpleComp(Component):

    x = Float(3.0, iotype='in')
    y = Float(6.0, iotype='out')

    def execute(self):
        self.y = 2.0*self.x

    def provideJ(self):
        return array([[2.0]])

    def list_deriv_vars(self):
        return ('x',), ('y',)

class SimpleCompMissingDeriv(Component):

    x = Float(3.0, iotype='in')
    miss_in = Float(4.0, iotype='in')
    y = Float(6.0, iotype='out')
    miss_out = Float(7.0, iotype='out')

    def execute(self):
        self.y = 2.0*self.x

    def provideJ(self):
        return array([[2.0]])

    def list_deriv_vars(self):
        return ('x',), ('y',)


class CompFoot(Component):
    """ Evaluates the equation y=2x"""

    x = Float(1.0, iotype='in', units='ft')
    y = Float(1.0, iotype='out', units='ft')

    def execute(self):
        """ Executes it """

        self.y = 2.0*self.x

    def provideJ(self):
        """Analytical first derivatives"""

        dy_dx = 2.0
        self.J = array([[dy_dx]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x',)
        output_keys = ('y',)
        return input_keys, output_keys


class CompInch(Component):
    """ Evaluates the equation y=x^2"""

    x = Float(1.0, iotype='in', units='inch')
    y = Float(1.0, iotype='out', units='inch')

    def execute(self):
        """ Executes it """

        self.y = 2.0*self.x

    def provideJ(self):
        """Analytical first derivatives"""

        dy_dx = 2.0
        self.J = array([[dy_dx]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x',)
        output_keys = ('y',)
        return input_keys, output_keys

class CompDegC(Component):
    """ Evaluates the equation y=x"""

    x = Float(1.0, iotype='in')
    y = Float(1.0, iotype='out', units='degC')

    def execute(self):
        """ Executes it """

        self.y = self.x

    def provideJ(self):
        """Analytical first derivatives"""

        dy_dx = 1.0
        self.J = array([[dy_dx]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x',)
        output_keys = ('y',)
        return input_keys, output_keys


class CompDegF(Component):
    """ Evaluates the equation y=x"""

    x = Float(1.0, iotype='in', units='degF')
    y = Float(1.0, iotype='out')

    def execute(self):
        """ Executes it """

        self.y = self.x

    def provideJ(self):
        """Analytical first derivatives"""

        dy_dx = 1.0
        self.J = array([[dy_dx]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x',)
        output_keys = ('y',)
        return input_keys, output_keys


class ArrayComp1(Component):
    '''Array component'''

    x = Array(zeros([2]), iotype='in')
    y = Array(zeros([2]), iotype='out')

    def execute(self):
        """ Executes it """

        self.y[0] = 2.0*self.x[0] + 7.0*self.x[1]
        self.y[1] = 5.0*self.x[0] - 3.0*self.x[1]
        #print "ran", self.x, self.y

    def provideJ(self):
        """Analytical first derivatives"""

        dy1_dx1 = 2.0
        dy1_dx2 = 7.0
        dy2_dx1 = 5.0
        dy2_dx2 = -3.0
        self.J = array([[dy1_dx1, dy1_dx2], [dy2_dx1, dy2_dx2]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', )
        output_keys = ('y', )
        return input_keys, output_keys


class ArrayComp1_inch(Component):
    '''Array component'''

    x = Array(zeros([2]), iotype='in', units="inch")
    y = Array(zeros([2]), iotype='out', units="inch")

    def execute(self):
        """ Executes it """

        self.y[0] = 2.0*self.x[0] + 7.0*self.x[1]
        self.y[1] = 5.0*self.x[0] - 3.0*self.x[1]
        #print "ran", self.x, self.y

    def provideJ(self):
        """Analytical first derivatives"""

        dy1_dx1 = 2.0
        dy1_dx2 = 7.0
        dy2_dx1 = 5.0
        dy2_dx2 = -3.0
        self.J = array([[dy1_dx1, dy1_dx2], [dy2_dx1, dy2_dx2]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', )
        output_keys = ('y', )
        return input_keys, output_keys

class ArrayComp1_ft(Component):
    '''Array component'''

    x = Array(zeros([2]), iotype='in', units="ft")
    y = Array(zeros([2]), iotype='out', units="ft")

    def execute(self):
        """ Executes it """

        self.y[0] = 2.0*self.x[0] + 7.0*self.x[1]
        self.y[1] = 5.0*self.x[0] - 3.0*self.x[1]
        #print "ran", self.x, self.y

    def provideJ(self):
        """Analytical first derivatives"""

        dy1_dx1 = 2.0
        dy1_dx2 = 7.0
        dy2_dx1 = 5.0
        dy2_dx2 = -3.0
        self.J = array([[dy1_dx1, dy1_dx2], [dy2_dx1, dy2_dx2]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', )
        output_keys = ('y', )
        return input_keys, output_keys


class ArrayComp1_noderiv(Component):
    '''Array component'''

    x = Array(zeros([2]), iotype='in', low=-100, high=100)
    y = Array(zeros([2]), iotype='out')

    def execute(self):
        """ Executes it """

        self.y[0] = 2.0*self.x[0] + 7.0*self.x[1]
        self.y[1] = 5.0*self.x[0] - 3.0*self.x[1]


class ArrayComp2D(Component):
    '''2D Array component'''

    x = Array(zeros((2, 2)), iotype='in')
    y = Array(zeros((2, 2)), iotype='out')

    def execute(self):
        """ Executes it """

        self.y[0][0] = 2.0*self.x[0][0] + 1.0*self.x[0][1] + \
                       3.0*self.x[1][0] + 7.0*self.x[1][1]

        self.y[0][1] = 4.0*self.x[0][0] + 2.0*self.x[0][1] + \
                       6.0*self.x[1][0] + 5.0*self.x[1][1]

        self.y[1][0] = 3.0*self.x[0][0] + 6.0*self.x[0][1] + \
                       9.0*self.x[1][0] + 8.0*self.x[1][1]

        self.y[1][1] = 1.0*self.x[0][0] + 3.0*self.x[0][1] + \
                       2.0*self.x[1][0] + 4.0*self.x[1][1]

    def provideJ(self):
        """Analytical first derivatives"""

        self.J = array([[2.0, 1.0, 3.0, 7.0],
                        [4.0, 2.0, 6.0, 5.0],
                        [3.0, 6.0, 9.0, 8.0],
                        [1.0, 3.0, 2.0, 4.0]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', )
        output_keys = ('y', )
        return input_keys, output_keys


class Array_Slice_1D(Component):
    '''1D Array with wide arrays for slicing tests'''

    x = Array(zeros((4)), iotype='in')
    y = Array(zeros((4)), iotype='out')

    def execute(self):
        """ Executes it """

        self.y[0] = 2.0*self.x[0] + 1.0*self.x[1] + \
                    3.0*self.x[2] + 7.0*self.x[3]

        self.y[1] = 4.0*self.x[0] + 2.0*self.x[1] + \
                    6.0*self.x[2] + 5.0*self.x[3]

        self.y[2] = 3.0*self.x[0] + 6.0*self.x[1] + \
                    9.0*self.x[2] + 8.0*self.x[3]

        self.y[3] = 1.0*self.x[0] + 3.0*self.x[1] + \
                    2.0*self.x[2] + 4.0*self.x[3]

    def provideJ(self):
        """Analytical first derivatives"""

        self.J = array([[2.0, 1.0, 3.0, 7.0],
                        [4.0, 2.0, 6.0, 5.0],
                        [3.0, 6.0, 9.0, 8.0],
                        [1.0, 3.0, 2.0, 4.0]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', )
        output_keys = ('y', )
        return input_keys, output_keys


class ArrayComp2D_der(Component):
    '''2D Array component'''

    x = Array(zeros((2, 2)), iotype='in')
    y = Array(zeros((2, 2)), iotype='out')

    def execute(self):
        """ Executes it """

        self.y[0][0] = 2.0*self.x[0][0] + 1.0*self.x[0][1] + \
                       3.0*self.x[1][0] + 7.0*self.x[1][1]

        self.y[0][1] = 4.0*self.x[0][0] + 2.0*self.x[0][1] + \
                       6.0*self.x[1][0] + 5.0*self.x[1][1]

        self.y[1][0] = 3.0*self.x[0][0] + 6.0*self.x[0][1] + \
                       9.0*self.x[1][0] + 8.0*self.x[1][1]

        self.y[1][1] = 1.0*self.x[0][0] + 3.0*self.x[0][1] + \
                       2.0*self.x[1][0] + 4.0*self.x[1][1]

    def provideJ(self):
        """Analytical first derivatives"""

        self.J = array([[2.0, 1.0, 3.0, 7.0],
                        [4.0, 2.0, 6.0, 5.0],
                        [3.0, 6.0, 9.0, 8.0],
                        [1.0, 3.0, 2.0, 4.0]])

    def list_deriv_vars(self):
        return ('x',), ('y',)

    def apply_deriv(self, arg, result):

        if 'x' in arg and 'y' in result:
            dy = self.J.dot(arg['x'].flatten()).reshape(2, 2)
            result['y'][:] += dy

    def apply_derivT(self, arg, result):

        if 'y' in arg and 'x' in result:
            dx = self.J.T.dot(arg['y'].flatten()).reshape(2, 2)
            result['x'][:] = dx


class GComp_noD(Component):

    x1 = Float(1.0, iotype='in')
    x2 = Float(1.0, iotype='in')
    x3 = Float(1.0, iotype='in')

    y1 = Float(1.0, iotype='out')

    def execute(self):

        self.y1 = 5.0*self.x1 + 7.0*self.x2 - 3.0*self.x3

class ABCDComp(Component):

    a = Float(1.0, iotype='in')
    b = Float(1.0, iotype='in', deriv_ignore=True)
    c = Float(2.0, iotype='out')
    d = Float(0.0, iotype='out')

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b

    def provideJ(self):
        return array([[1., 1.]]).transpose()

    def list_deriv_vars(self):
        return (('a',), ('c', 'd'))

class ABCDintComp(Component):

    a = Float(1.0, iotype='in')
    b = Int(1, iotype='in')
    c = Float(2.0, iotype='out')
    d = Float(0.0, iotype='out')

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b

    def provideJ(self):
        return array([[1., 1.]]).transpose()

    def list_deriv_vars(self):
        return (('a',), ('c', 'd'))


class Testcase_derivatives(unittest.TestCase):
    """ Test derivative aspects of a simple workflow. """

    def test_bad_list_deriv_vars(self):
        top = set_as_top(Assembly())
        top.add('comp1', BadListDerivsComp())
        top.driver.workflow.add(['comp1'])
        top.comp1.x = 1.0
        top.run()
        self.assertEqual(top.comp1.y, 2.0)
        try:
            J = top.driver.calc_gradient(['comp1.x'], ['comp1.y'])
        except Exception as err:
            self.assertEqual(str(err),
                             "comp1: The return value of list_deriv_vars() was"
                             " not a tuple of the form (invars, outvars). Value"
                             " returned was ['x', 'y']")

    def test_unflattenable_deriv_var(self):
        top = set_as_top(Assembly())
        top.add('comp', UnflattenableDerivVarComp())
        top.driver.workflow.add(['comp'])
        top.comp.x = 1
        top.comp.y = 1.0

        try:
            top.run()
        except Exception as err:
            msg = "comp: 'x', of type 'Int', was given in 'list_deriv_vars' but "\
            "variables must be of a type convertable to a 1D float array"

            self.assertEqual(str(err), msg)

    def test_vartree_deriv_var(self):
        top = set_as_top(Assembly())
        top.add('comp', VarTreeDerivVarComp())
        top.driver.workflow.add(['comp'])

        try:
            top.run()
        except Exception as err:
            msg = "comp: 'x', of type 'VarTree', was given in 'list_deriv_vars' but you must declare "\
            "sub-vars of a vartree individually"\

            self.assertEqual(str(err), msg)

    def test_undefined_deriv_var(self):
        top = set_as_top(Assembly())
        top.add('comp', UndefinedDerivVarComp())
        top.driver.workflow.add(['comp'])
        top.comp.x = 1.0
        top.comp.y = 1.0

        try:
            top.run()
        except Exception as err:
            msg = "comp: 'w' was given in 'list_deriv_vars' "\
            "but 'w' is undefined"

            self.assertEqual(str(err), msg)

    def test_int_ignore(self):

        top = set_as_top(Assembly())
        top.add('comp1', IntComp())
        top.add('comp2', IntComp())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.connect('comp1.int_out', 'comp2.int_in')
        top.connect('comp1.y', 'comp2.x')
        top.connect('comp1.y_ignore', 'comp2.x_ignore')
        top.comp1.x = 1.0
        top.run()
        self.assertEqual(top.comp2.y, 4.0)

        J = top.driver.calc_gradient(['comp1.x'], ['comp2.y'])
        assert_rel_error(self, J[0, 0], 4.0, 0.0001)

    def test_error_logging1(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.comp.x = 3
        top.comp.y = 5
        top.run()

        orig_gmres = openmdao.main.linearsolver.gmres
        orig_logger = openmdao.main.linearsolver.logger

        # wrap gmres to return an error code
        def my_gmres(A, b, x0=None, tol=1e-05, restart=None,
                     maxiter=None, xtype=None, M=None, callback=None, restrt=None):
            dx, info = orig_gmres(A, b, x0, tol, restart, maxiter,
                                  xtype, M, callback, restrt)
            return dx, 13

        openmdao.main.linearsolver.gmres = my_gmres
        openmdao.main.linearsolver.logger = mocklogger = Mock()

        try:
            top.driver.calc_gradient(outputs=['comp.f_xy'],
                                              mode='forward')

            mocklogger.error.assert_called_with(
                "ERROR in calc_gradient in '%s': gmres failed to converge after"
                " %d iterations",
                "('_pseudo_0', 'comp', 'comp.x', 'comp.y')", 13)

            top.driver.calc_gradient(outputs=['comp.f_xy'],
                                              mode='adjoint')

            mocklogger.error.assert_called_with(
                "ERROR in calc_gradient in '%s': gmres failed to"
                " converge after %d iterations",
                "('_pseudo_0', 'comp', 'comp.x', 'comp.y')", 13)

        finally:
            openmdao.main.linearsolver.gmres = orig_gmres
            openmdao.main.linearsolver.logger = orig_logger

    def test_provideJ(self):
        top = set_as_top(Assembly())
        top.add('comp', SimpleComp())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.comp.x = 14
        top.comp.run()

        top.comp.x = 14

        top.comp.run()
        top.driver.calc_gradient(outputs=['comp.y'])

    def test_non_2d_jacobian(self):
        comp = SimpleComp()
        comp.provideJ = lambda : np.array([2.0])

        top = set_as_top(Assembly())
        top.add('comp', comp)
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.comp.x = 14

        try:
            top.comp.run()
            top.driver.calc_gradient(outputs=['comp.y'])
        except ValueError as err:
            expected = "Jacobian has the wrong dimensions. Expected 2D but got 1D."
            self.assertEqual(str(err), expected)
        else:
            self.fail("Should have caught error because Jacobian is not 2D")

    def test_bad_sized_jacobian(self):
        comp = SimpleComp()
        comp.provideJ = lambda : np.array([[2.0, 2.0]])

        top = set_as_top(Assembly())
        top.add('comp', comp)
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.comp.x = 14

        try:
            top.comp.run()
            top.driver.calc_gradient(outputs=['comp.y'])
        except RuntimeError as err:
            expected = "comp: Jacobian is the wrong size. Expected (1x1) but got (1x2)"
            self.assertEqual(str(err), expected)
        else:
            self.fail("Should have caught error because Jacobian is the wrong size")

    def test_error_logging2(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)

        top.comp.x = 3
        top.comp.y = 5
        top.comp.run()

        orig_gmres = openmdao.main.linearsolver.gmres
        orig_logger = openmdao.main.linearsolver.logger

        # wrap gmres to return an error code
        def my_gmres(A, b, x0=None, tol=1e-05, restart=None,
                     maxiter=None, xtype=None, M=None, callback=None, restrt=None):
            dx, info = orig_gmres(A, b, x0, tol, restart, maxiter,
                                  xtype, M, callback, restrt)
            return dx, -13

        openmdao.main.linearsolver.gmres = my_gmres
        openmdao.main.linearsolver.logger = mocklogger = Mock()

        try:
            top.driver.calc_gradient(outputs=['comp.f_xy'],
                                     mode='forward')
            mocklogger.error.assert_called_with(
                "ERROR in calc_gradient in '%s': gmres failed",
                "('comp', 'comp.x', 'comp.y')")

            top.driver.calc_gradient(outputs=['comp.f_xy'],
                                              mode='adjoint')
            mocklogger.error.assert_called_with(
                "ERROR in calc_gradient in '%s': gmres failed",
                "('comp', 'comp.x', 'comp.y')")

        finally:
            openmdao.main.derivatives.gmres = orig_gmres
            openmdao.main.derivatives.logger = orig_logger

    def test_single_comp(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.comp.x = 3
        top.comp.y = 5
        top.run()

        self.assertEqual(top.comp.f_xy, 93.)
        self.assertEqual(top._pseudo_0.out0, 93.)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              outputs=['comp.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp.x', 'comp.y'],
                                              mode='fd')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

    def test_multi_non_relevant_path(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2']
        deriv1 = ['dy1_dx1 = 4.0*x1']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1 + x1*x2*2.0']
        deriv3 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0 + 2.0*x2',
                  'dy2_dx2 = 2.0*x1']

        exp4 = ['y1 = 3.5*x1']
        deriv4 = ['dy1_dx1 = 3.5']

        exp5 = ['y1 = 3.0*x1']
        deriv5 = ['dy1_dx1 = 3.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.add('driver', SimpleDriver())
        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])
        self.top.driver.add_parameter('comp1.x1', low=-100, high=100)
        self.top.driver.add_parameter('comp2.x1', low=-100, high=100)
        self.top.driver.add_constraint('comp4.y1 < 0')
        self.top.driver.add_constraint('comp5.y1 < 0')

        self.top.connect('comp1.y1', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp3.x2')
        self.top.connect('comp3.y1', 'comp4.x1')
        self.top.connect('comp3.y2', 'comp5.x1')

        # Case 1 - differentiable (comp4)

        self.top.comp1.x1 = 2.0
        self.top.comp2.x1 = 1.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'],
                                                   mode='forward')
        assert_rel_error(self, J[0, 0], 96.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'],
                                                   mode='adjoint')
        assert_rel_error(self, J[0, 0], 96.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                                   outputs=['comp5.y1'],
                                                   mode='fd')
        assert_rel_error(self, J[0, 0], 96.0, .001)

    def test_multi_non_relevant_path_no_params_cnsts(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2']
        deriv1 = ['dy1_dx1 = 4.0*x1']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1 + x1*x2*2.0']
        deriv3 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0 + 2.0*x2',
                  'dy2_dx2 = 2.0*x1']

        exp4 = ['y1 = 3.5*x1']
        deriv4 = ['dy1_dx1 = 3.5']

        exp5 = ['y1 = 3.0*x1']
        deriv5 = ['dy1_dx1 = 3.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.add('driver', SimpleDriver())
        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp3.x2')
        self.top.connect('comp3.y1', 'comp4.x1')
        self.top.connect('comp3.y2', 'comp5.x1')

        self.top.comp1.x1 = 2.0
        self.top.comp2.x1 = 1.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward')
        assert_rel_error(self, J[0, 0], 96.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='adjoint')
        assert_rel_error(self, J[0, 0], 96.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='fd')
        assert_rel_error(self, J[0, 0], 96.0, .001)

    def test_first_derivative(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.comp.x = 3
        top.comp.y = 5
        top.run()

        J = top.driver.calc_gradient(outputs=['comp.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        expected = """\
------------------------
Calculated Gradient
------------------------
\[\[  5.  21.\]\]
------------------------
Finite Difference Comparison
------------------------
\[\[  5.[0-9]+[ ]+21.[0-9]+\]\]

                    Calculated         FiniteDiff         RelError[^\n]+
----------------------------------------------------------------------------
comp.f_xy / comp.x: 5.0                5.[0-9]+[ ]+[^\n]+
comp.f_xy / comp.y: 21.0               21.[0-9]+[ ]+[^\n]+

Average RelError: [^\n]+
Max RelError: [^ ]+ for comp.f_xy / comp.x

"""
        stream = StringIO()
        top.driver.check_gradient(outputs=('comp.f_xy',), stream=stream)
        actual = stream.getvalue()
        if re.match(expected, actual) is None:
            print 'Expected:\n%s' % expected
            print 'Actual:\n%s' % actual
            self.fail("check_gradient() output doesn't match expected")

        stream = StringIO()
        top.check_gradient(outputs=['comp.f_xy'], stream=stream,
                           inputs=['comp.x', 'comp.y'])
        actual = stream.getvalue()
        if re.match(expected, actual) is None:
            print 'Expected:\n%s' % expected
            print 'Actual:\n%s' % actual
            self.fail("check_gradient() output doesn't match expected")

        stream = StringIO()
        top.check_gradient(inputs=('comp.x', 'comp.y'), outputs=('comp.f_xy',),
                           stream=stream)
        actual = stream.getvalue()
        if re.match(expected, actual) is None:
            print 'Expected:\n%s' % expected
            print 'Actual:\n%s' % actual
            self.fail("check_gradient() output doesn't match expected")

        stream = StringIO()
        top.check_gradient(name='driver', outputs=['comp.f_xy'], stream=stream,
                           inputs=['comp.x', 'comp.y'])
        actual = stream.getvalue()
        if re.match(expected, actual) is None:
            print 'Expected:\n%s' % expected
            print 'Actual:\n%s' % actual
            self.fail("check_gradient() output doesn't match expected")

        stream = StringIO()
        top.check_gradient(name='comp', stream=stream)
        actual = stream.getvalue()
        if re.match(expected, actual) is None:
            print 'Expected:\n%s' % expected
            print 'Actual:\n%s' % actual
            self.fail("check_gradient() output doesn't match expected")

        stream = StringIO()
        top.comp.check_gradient(stream=stream)
        actual = stream.getvalue()
        if re.match(expected, actual) is None:
            print 'Expected:\n%s' % expected
            print 'Actual:\n%s' % actual
            self.fail("check_gradient() output doesn't match expected")

        stream = StringIO()
        comp = Paraboloid()
        comp.x = 3
        comp.y = 5
        Jbase, J, io_pairs, suspects = comp.check_gradient(stream=stream)
        actual = stream.getvalue()
        if re.match(expected, actual) is None:
            print 'Expected:\n%s' % expected
            print 'Actual:\n%s' % actual
            self.fail("check_gradient() output doesn't match expected")

        # now do it again to make sure name and parent were properly reset
        Jbase, J, io_pairs, suspects = comp.check_gradient(stream=stream)
        actual = stream.getvalue()
        if re.match(expected, actual) is None:
            print 'Expected:\n%s' % expected
            print 'Actual:\n%s' % actual
            self.fail("check_gradient() output doesn't match expected")


    def test_input_as_output(self):

        top = set_as_top(Assembly())
        top.add('comp1', ExecCompWithDerivatives(['y=2.0*x + 3.0*x2'],
                                                 ['dy_dx = 2.0', 'dy_dx2 = 3.0']))
        top.add('comp2', ExecCompWithDerivatives(['y=3.0*x'],
                                                 ['dy_dx = 3.0']))
        top.connect('comp1.y', 'comp2.x')
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])
        #top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_objective('comp1.y + comp2.y + 5*comp1.x')

        objs = top.driver.get_objectives().values()
        obj = '%s.out0' % objs[0].pcomp_name

        top.comp1.x = 1.0
        top.run()

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=[obj], mode='forward')

        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                              outputs=[obj], mode='fd')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        top.driver.run()

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x', 'comp1.x2'],
                                     outputs=[obj], mode='forward')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x', 'comp1.x2'],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=[('comp1.x',), ('comp1.x2',)],
                                     outputs=[obj], mode='forward')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=[('comp1.x',), ('comp1.x2',)],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

    def test_input_as_output_nondiff(self):

        top = set_as_top(Assembly())
        top.add('comp1', ExecComp(['y=2.0*x + 3.0*x2']))
        top.add('comp2', ExecComp(['y=3.0*x']))
        top.connect('comp1.y', 'comp2.x')
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.driver.add_objective('comp1.y + comp2.y + 5*comp1.x')

        top.comp1.x = 1.0
        top.run()
        objs = top.driver.get_objectives().values()
        obj = '%s.out0' % objs[0].pcomp_name
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=[obj], mode='forward')

        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=[obj], mode='fd')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        top.driver.run()

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x', 'comp1.x2'],
                                     outputs=[obj], mode='forward')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp1.x', 'comp1.x2'],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=[('comp1.x'), ('comp1.x2')],
                                     outputs=[obj], mode='forward')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

        J = top.driver.calc_gradient(inputs=[('comp1.x'), ('comp1.x2')],
                                     outputs=[obj], mode='adjoint')
        assert_rel_error(self, J[0, 0], 13.0, 0.0001)
        assert_rel_error(self, J[0, 1], 12.0, 0.0001)

    def test_input_as_output2(self):
        # irrelevant float edge was causing a key error
        top = set_as_top(Assembly())
        sub = top.add('sub', Assembly())
        sub.add('c1', ABCDComp())
        sub.add('c2', ABCDComp())
        sub.add('c3', ABCDComp())
        sub.connect('c1.c', 'c2.a')
        sub.connect('c1.b', 'c2.b')
        sub.create_passthrough('c1.a')
        sub.create_passthrough('c1.b')
        sub.create_passthrough('c2.c')
        sub.create_passthrough('c3.d')
        sub.driver.workflow.add(['c1', 'c2', 'c3'])
        top.driver.workflow.add('sub')
        top.run()
        J = top.driver.calc_gradient(inputs=('sub.a',),
                                     outputs=('sub.c', 'sub.d'))
        self.assertEqual(J.shape, (2, 1))
        self.assertEqual(J[0, 0], 1.)
        self.assertEqual(J[1, 0], 0.)

    def test_input_as_output3(self):
        # irrelevant int edge was causing unnecessary finite differencing
        top = set_as_top(Assembly())
        sub = top.add('sub', Assembly())
        sub.add('c1', ABCDintComp())
        sub.add('c2', ABCDintComp())
        sub.add('c3', ABCDintComp())
        sub.connect('c1.c', 'c2.a')
        sub.connect('c1.b', 'c2.b')
        sub.create_passthrough('c1.a')
        sub.create_passthrough('c1.b')
        sub.create_passthrough('c2.c')
        sub.create_passthrough('c3.d')
        sub.driver.workflow.add(['c1', 'c2', 'c3'])
        top.driver.workflow.add('sub')
        top.run()
        J = top.driver.calc_gradient(('sub.a',), ('sub.c', 'sub.d'))
        self.assertEqual(J.shape, (2, 1))
        self.assertEqual(J[0, 0], 1.)
        self.assertEqual(J[1, 0], 0.)

    def test_input_as_output_nondiff_array(self):

        top = set_as_top(Assembly())
        top.add('comp', ArrayComp1_noderiv())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x')
        top.driver.add_objective('comp.y[0]')
        top.driver.add_constraint('comp.x[0] < 1')

        top.run()
        J = top.driver.calc_gradient(mode='forward')
        #print J
        assert_rel_error(self, J[0, 0], 2., 1e-5)
        assert_rel_error(self, J[0, 1], 7., 1e-5)
        assert_rel_error(self, J[1, 0], 1., 1e-5)
        assert_rel_error(self, J[1, 1], 0., 1e-5)


    def test_nested(self):

        top = set_as_top(Assembly())
        top.add('nest', Assembly())
        top.nest.add('comp', Paraboloid())

        # We shouldn't calculate a derivative of this
        top.nest.comp.add('unwanted', Float(12.34, iotype='in'))
        top.nest.comp.add('junk', Float(9.9, iotype='out'))

        top.driver.workflow.add(['nest'])
        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')
        top.nest.create_passthrough('comp.unwanted')
        top.nest.create_passthrough('comp.f_xy')
        top.nest.x = 3
        top.nest.y = 5
        top.run()

        # Now we need to set a deriv policy to prevent squawking
        # about missing deriatives.
        top.nest.comp.missing_deriv_policy = 'assume_zero'

        J = top.driver.calc_gradient(inputs=['nest.x', 'nest.y'],
                                     outputs=['nest.f_xy'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['nest.x', 'nest.y'],
                                              outputs=['nest.f_xy'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, 0.0001)
        assert_rel_error(self, J[0, 1], 21.0, 0.0001)

        # Now, let's find the derivative of the unconnected. Behaviour depends
        # on deriv policy.
        top.nest.add('stuff', Float(1.0, iotype='in'))
        top.nest.add('junk', Float(1.0, iotype='out'))
        top.add('first', Paraboloid())
        top.add('last', Paraboloid())
        top.connect('first.f_xy', 'nest.stuff')
        top.connect('nest.junk', 'last.x')
        top.driver.workflow.clear()
        top.driver.workflow.add(['first', 'nest', 'last'])

        top.nest.comp.missing_deriv_policy = 'assume_zero'
        top.run()
        J = top.driver.calc_gradient(inputs=['nest.x', 'first.x'],
                                     outputs=['nest.f_xy', 'last.f_xy'],
                                     mode='forward')

        top.nest.missing_deriv_policy = 'assume_zero'
        J = top.driver.calc_gradient(inputs=['nest.x', 'first.x'],
                                              outputs=['nest.f_xy', 'last.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], 0.0, .001)
        assert_rel_error(self, J[1, 0], 0.0, .001)
        assert_rel_error(self, J[1, 1], 0.0, .001)

        top.nest.missing_deriv_policy = 'error'

        J = top.driver.calc_gradient(inputs=['first.x'],
                                              outputs=['last.f_xy'],
                                              mode='forward')

        top.nest.missing_deriv_policy = 'assume_zero'
        J = top.driver.calc_gradient(inputs=['first.x'],
                                              outputs=['last.f_xy'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 0.0, .001)

        J = top.driver.calc_gradient(inputs=['nest.x', 'nest.stuff'],
                                              outputs=['nest.f_xy', 'nest.junk'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], 0.0, .001)
        assert_rel_error(self, J[1, 0], 0.0, .001)
        assert_rel_error(self, J[1, 1], 0.0, .001)

        J = top.driver.calc_gradient(inputs=['nest.stuff', 'nest.x'],
                                              outputs=['nest.junk', 'nest.f_xy', ],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 0.0, .001)
        assert_rel_error(self, J[0, 1], 0.0, .001)
        assert_rel_error(self, J[1, 0], 0.0, .001)
        assert_rel_error(self, J[1, 1], 5.0, .001)

        J = top.driver.calc_gradient(inputs=['nest.stuff', 'nest.x'],
                                              outputs=['nest.junk', 'nest.f_xy', ],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 0.0, .001)
        assert_rel_error(self, J[0, 1], 0.0, .001)
        assert_rel_error(self, J[1, 0], 0.0, .001)
        assert_rel_error(self, J[1, 1], 5.0, .001)

        J = top.driver.calc_gradient(inputs=['nest.stuff', 'nest.x'],
                                              outputs=['nest.f_xy', ],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 0.0, .001)
        assert_rel_error(self, J[0, 1], 5.0, .001)

        J = top.driver.calc_gradient(inputs=['nest.x', 'nest.stuff'],
                                              outputs=['nest.f_xy', ],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], 0.0, .001)

        J = top.driver.calc_gradient(inputs=['nest.x', 'nest.stuff'],
                                              outputs=['nest.f_xy', 'nest.junk'],
                                              mode='adjoint')

        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], 0.0, .001)
        assert_rel_error(self, J[1, 0], 0.0, .001)
        assert_rel_error(self, J[1, 1], 0.0, .001)

        J = top.driver.calc_gradient(inputs=['nest.stuff'],
                                              outputs=['nest.junk'],
                                              mode='forward')

        assert_rel_error(self, J[0, 0], 0.0, .001)

        top.nest.missing_deriv_policy = 'error'
        J = top.driver.calc_gradient(inputs=['nest.stuff'],
                                              outputs=['nest.junk'],
                                              mode='forward')

    def test_broadcast_graph(self):

        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())

        equation = ['y = 2.0*x + 3.0*z']
        top.add('comp1', ExecComp(equation))
        top.add('comp2', ExecComp(equation))
        top.driver.workflow.add(['comp1', 'comp2'])

        top.run()

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp2.x')],
                                     outputs=['comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 2.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp2.x')],
                                     outputs=['comp2.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 2.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp1.z')],
                                     outputs=['comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 0.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp1.z')],
                                     outputs=['comp2.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 0.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp1.z'), ('comp2.x', 'comp2.z')],
                                     outputs=['comp2.y'],
                                     mode='adjoint')
        #print J
        assert_rel_error(self, J[0, 0], 0.0, .001)
        assert_rel_error(self, J[0, 1], 5.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp1.z'), ('comp2.x', 'comp2.z')],
                                     outputs=['comp2.y'],
                                     mode='forward')
        #print J
        assert_rel_error(self, J[0, 0], 0.0, .001)
        assert_rel_error(self, J[0, 1], 5.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp1.z'), ('comp2.x', 'comp2.z')],
                                     outputs=['comp2.y'],
                                     mode='fd')

        #from openmdao.util.dotgraph import plot_graph, plot_system_tree
        #plot_system_tree(top._system, 'newsys.pdf')

        #print J
        assert_rel_error(self, J[0, 0], 0.0, .001)
        assert_rel_error(self, J[0, 1], 5.0, .001)

    def test_5in_1out(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 1.0*x1 + 2.0*x2 + 3.0*x3 + 4.0*x4 + 5.0*x5']
        deriv1 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy1_dx3 = 3.0',
                  'dy1_dx4 = 4.0',
                  'dy1_dx5 = 5.0']

        self.top.add('comp', ExecCompWithDerivatives(exp1, deriv1))
        self.top.driver.workflow.add(['comp'])

        self.top.run()
        J = self.top.driver.calc_gradient(inputs=['comp.x1',
                                                  'comp.x2',
                                                  'comp.x3',
                                                  'comp.x4',
                                                  'comp.x5'],
                                          outputs=['comp.y1'],
                                          mode='forward')

        assert_rel_error(self, J[0, 0], 1.0, .001)
        assert_rel_error(self, J[0, 1], 2.0, .001)
        assert_rel_error(self, J[0, 2], 3.0, .001)
        assert_rel_error(self, J[0, 3], 4.0, .001)
        assert_rel_error(self, J[0, 4], 5.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp.x1',
                                                           'comp.x2',
                                                           'comp.x3',
                                                           'comp.x4',
                                                           'comp.x5'],
                                                   outputs=['comp.y1'],
                                                   mode='adjoint')

        assert_rel_error(self, J[0, 0], 1.0, .001)
        assert_rel_error(self, J[0, 1], 2.0, .001)
        assert_rel_error(self, J[0, 2], 3.0, .001)
        assert_rel_error(self, J[0, 3], 4.0, .001)
        assert_rel_error(self, J[0, 4], 5.0, .001)

    def test_1in_5out(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 1.0*x1',
                'y2 = 2.0*x1',
                'y3 = 3.0*x1',
                'y4 = 4.0*x1',
                'y5 = 5.0*x1']
        deriv1 = ['dy1_dx1 = 1.0',
                  'dy2_dx1 = 2.0',
                  'dy3_dx1 = 3.0',
                  'dy4_dx1 = 4.0',
                  'dy5_dx1 = 5.0']

        self.top.add('comp', ExecCompWithDerivatives(exp1, deriv1))
        self.top.driver.workflow.add(['comp'])

        self.top.run()
        J = self.top.driver.calc_gradient(inputs=['comp.x1'],
                                          outputs=['comp.y1',
                                                   'comp.y2',
                                                   'comp.y3',
                                                   'comp.y4',
                                                   'comp.y5'],
                                          mode='forward')

        assert_rel_error(self, J[0, 0], 1.0, .001)
        assert_rel_error(self, J[1, 0], 2.0, .001)
        assert_rel_error(self, J[2, 0], 3.0, .001)
        assert_rel_error(self, J[3, 0], 4.0, .001)
        assert_rel_error(self, J[4, 0], 5.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp.x1'],
                                                   outputs=['comp.y1',
                                                            'comp.y2',
                                                            'comp.y3',
                                                            'comp.y4',
                                                            'comp.y5'],
                                                   mode='adjoint')

        assert_rel_error(self, J[0, 0], 1.0, .001)
        assert_rel_error(self, J[1, 0], 2.0, .001)
        assert_rel_error(self, J[2, 0], 3.0, .001)
        assert_rel_error(self, J[3, 0], 4.0, .001)
        assert_rel_error(self, J[4, 0], 5.0, .001)

    def test_one_array_comp_fd(self):

        top = set_as_top(Assembly())
        top.add('comp1', ArrayComp1_noderiv())
        top.driver.workflow.add(['comp1'])

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 2.0, .001)
        assert_rel_error(self, J[0, 1], 7.0, .001)
        assert_rel_error(self, J[1, 0], 5.0, .001)
        assert_rel_error(self, J[1, 1], -3.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 2.0, .001)
        assert_rel_error(self, J[0, 1], 7.0, .001)
        assert_rel_error(self, J[1, 0], 5.0, .001)
        assert_rel_error(self, J[1, 1], -3.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 2.0, .001)
        assert_rel_error(self, J[0, 1], 7.0, .001)
        assert_rel_error(self, J[1, 0], 5.0, .001)
        assert_rel_error(self, J[1, 1], -3.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp1.y[1]'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], -3.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp1.y[1]'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], -3.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp1.y[1]'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], -3.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[-1]'],
                                     outputs=['comp1.y[-1]'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], -3.0, .001)

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 2.0, .001)
        assert_rel_error(self, J[0, 1], 7.0, .001)
        assert_rel_error(self, J[1, 0], 5.0, .001)
        assert_rel_error(self, J[1, 1], -3.0, .001)

    def test_arrays(self):

        top = set_as_top(Assembly())
        top.add('comp1', ArrayComp1())
        top.add('comp2', ArrayComp1())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.connect('comp1.y', 'comp2.x')

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 39.0, .001)
        assert_rel_error(self, J[0, 1], -7.0, .001)
        assert_rel_error(self, J[1, 0], -5.0, .001)
        assert_rel_error(self, J[1, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 39.0, .001)
        assert_rel_error(self, J[0, 1], -7.0, .001)
        assert_rel_error(self, J[1, 0], -5.0, .001)
        assert_rel_error(self, J[1, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[0]'],
                                     outputs=['comp2.y[0]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 39.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp2.y[1]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp2.y[-1]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 44.0, .001)

        # this tests the finite difference code.
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 39.0, .001)
        assert_rel_error(self, J[0, 1], -7.0, .001)
        assert_rel_error(self, J[1, 0], -5.0, .001)
        assert_rel_error(self, J[1, 1], 44.0, .001)

        # this tests a simultaneous full and indexed array conn
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[1]', 'comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[1]', 'comp2.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[1]', 'comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[-1]', 'comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[-1]', 'comp2.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[-1]', 'comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0, .001)
        assert_rel_error(self, J[1, 1], -7.0, .001)
        assert_rel_error(self, J[2, 0], -5.0, .001)
        assert_rel_error(self, J[2, 1], 44.0, .001)

    def test_arrays_units(self):

        top = set_as_top(Assembly())
        top.add('comp1', ArrayComp1_ft())
        top.add('comp2', ArrayComp1_inch())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.connect('comp1.y', 'comp2.x')

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 39.0*12, .001)
        assert_rel_error(self, J[0, 1], -7.0*12, .001)
        assert_rel_error(self, J[1, 0], -5.0*12, .001)
        assert_rel_error(self, J[1, 1], 44.0*12, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 39.0*12, .001)
        assert_rel_error(self, J[0, 1], -7.0*12, .001)
        assert_rel_error(self, J[1, 0], -5.0*12, .001)
        assert_rel_error(self, J[1, 1], 44.0*12, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[0]'],
                                     outputs=['comp2.y[0]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 39.0*12, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp2.y[1]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 44.0*12, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp2.y[-1]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 44.0*12, .001)

        # this tests the finite difference code.
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 39.0*12, .001)
        assert_rel_error(self, J[0, 1], -7.0*12, .001)
        assert_rel_error(self, J[1, 0], -5.0*12, .001)
        assert_rel_error(self, J[1, 1], 44.0*12, .001)

        # this tests a simultaneous full and indexed array conn
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[1]', 'comp2.y'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0*12, .001)
        assert_rel_error(self, J[1, 1], -7.0*12, .001)
        assert_rel_error(self, J[2, 0], -5.0*12, .001)
        assert_rel_error(self, J[2, 1], 44.0*12, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[1]', 'comp2.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0*12, .001)
        assert_rel_error(self, J[1, 1], -7.0*12, .001)
        assert_rel_error(self, J[2, 0], -5.0*12, .001)
        assert_rel_error(self, J[2, 1], 44.0*12, .001)

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[1]', 'comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0*12, .001)
        assert_rel_error(self, J[1, 1], -7.0*12, .001)
        assert_rel_error(self, J[2, 0], -5.0*12, .001)
        assert_rel_error(self, J[2, 1], 44.0*12, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[-1]', 'comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0*12, .001)
        assert_rel_error(self, J[1, 1], -7.0*12, .001)
        assert_rel_error(self, J[2, 0], -5.0*12, .001)
        assert_rel_error(self, J[2, 1], 44.0*12, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[-1]', 'comp2.y'],
                                     mode='adjoint')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0*12, .001)
        assert_rel_error(self, J[1, 1], -7.0*12, .001)
        assert_rel_error(self, J[2, 0], -5.0*12, .001)
        assert_rel_error(self, J[2, 1], 44.0*12, .001)

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y[-1]', 'comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], -3.0, .001)
        assert_rel_error(self, J[1, 0], 39.0*12, .001)
        assert_rel_error(self, J[1, 1], -7.0*12, .001)
        assert_rel_error(self, J[2, 0], -5.0*12, .001)
        assert_rel_error(self, J[2, 1], 44.0*12, .001)

    def test_arrays_mixed(self):

        top = set_as_top(Assembly())
        top.add('comp1', ArrayComp1())
        top.add('comp2', ArrayComp1_noderiv())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.connect('comp1.y', 'comp2.x')

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 39.0, .001)
        assert_rel_error(self, J[0, 1], -7.0, .001)
        assert_rel_error(self, J[1, 0], -5.0, .001)
        assert_rel_error(self, J[1, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                              outputs=['comp2.y'],
                                              mode='adjoint')
        assert_rel_error(self, J[0, 0], 39.0, .001)
        assert_rel_error(self, J[0, 1], -7.0, .001)
        assert_rel_error(self, J[1, 0], -5.0, .001)
        assert_rel_error(self, J[1, 1], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[0]'],
                                     outputs=['comp2.y[0]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 39.0, .001)

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp2.y[1]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 44.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1]'],
                                     outputs=['comp2.y[-1]'],
                                     mode='forward')

        assert_rel_error(self, J[0, 0], 44.0, .001)

        # this tests the finite difference code.
        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp2.y'],
                                     mode='fd')
        assert_rel_error(self, J[0, 0], 39.0, .001)
        assert_rel_error(self, J[0, 1], -7.0, .001)
        assert_rel_error(self, J[1, 0], -5.0, .001)
        assert_rel_error(self, J[1, 1], 44.0, .001)

    def test_arrays_broadcast_fd(self):

        top = set_as_top(Assembly())
        top.add('comp1', MyComp())
        top.driver.workflow.add(['comp1'])

        top.comp1.x3 = zeros((4, 1))
        top.comp1.x4 = zeros((4, 1))
        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x1', 'comp1.x2', ('comp1.x3', 'comp1.x4')],
                                     outputs=['comp1.xx1'],
                                     mode='fd')

        self.assertEqual(0.0, abs(J).max())

    def test_array2D(self):

        top = set_as_top(Assembly())
        top.add('comp1', ArrayComp2D())
        top.driver.workflow.add(['comp1'])

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y'],
                                     mode='forward')

        diff = J - top.comp1.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                              outputs=['comp1.y'],
                                              mode='adjoint')
        diff = J - top.comp1.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                              outputs=['comp1.y'],
                                              mode='fd')
        diff = J - top.comp1.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['comp1.x'],
                                     outputs=['comp1.y'],
                                     mode='fd',
                                     return_format='dict')
        diff = J['comp1.y']['comp1.x'] - top.comp1.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        top.run()
        Jsub = top.comp1.J[2:3, 2:3]
        J = top.driver.calc_gradient(inputs=['comp1.x[1][:]'],
                                     outputs=['comp1.y[1][:]'],
                                     mode='forward')

        diff = J - Jsub
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1][:]'],
                                     outputs=['comp1.y[1][:]'],
                                     mode='adjoint')
        diff = J - Jsub
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['comp1.x[1][:]'],
                                     outputs=['comp1.y[1][:]'],
                                     mode='fd')
        diff = J - Jsub
        assert_rel_error(self, diff.max(), 0.0, .000001)

    def test_array2D_with_apply_deriv(self):

        top = set_as_top(Assembly())
        top.add('comp1', ArrayComp2D_der())
        top.driver.workflow.add(['comp1'])

        top.run()
        inputs = ['comp1.x[0, 0]', 'comp1.x[0, 1]']
        outputs = ['comp1.y[1, 0]', 'comp1.y[1, 1]']

        J = top.driver.calc_gradient(inputs=inputs,
                                     outputs=outputs,
                                     mode='forward')
        Jsub = top.comp1.J[2:4, 0:2]
        diff = J - Jsub
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=inputs,
                                              outputs=outputs,
                                              mode='adjoint')
        diff = J - Jsub
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=inputs,
                                              outputs=outputs,
                                              mode='fd')
        diff = J - Jsub
        assert_rel_error(self, diff.max(), 0.0, .000001)

    def test_array_slice_1D(self):

        top = set_as_top(Assembly())
        top.add('comp1', Array_Slice_1D())
        top.driver.workflow.add(['comp1'])

        top.run()
        J = top.driver.calc_gradient(inputs=['comp1.x[0::2]'],
                                     outputs=['comp1.y[0::2]'],
                                     mode='forward')

        diff = J - top.comp1.J[0::2, 0::2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['comp1.x[0::2]'],
                                     outputs=['comp1.y[0::2]'],
                                     mode='adjoint')

        diff = J - top.comp1.J[0::2, 0::2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['comp1.x[0::2]'],
                                     outputs=['comp1.y[0::2]'],
                                     mode='fd')

        diff = J - top.comp1.J[0::2, 0::2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

    def test_nested_2Darray(self):

        top = set_as_top(Assembly())
        top.add('nest', Assembly())
        top.nest.add('comp', ArrayComp2D())

        top.driver.workflow.add(['nest'])
        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')
        top.run()

        J = top.driver.calc_gradient(inputs=['nest.x',],
                                     outputs=['nest.y'],
                                     mode='forward')

        diff = J - top.nest.comp.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x',],
                                     outputs=['nest.y'],
                                     mode='adjoint')
        diff = J - top.nest.comp.J
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 0]',],
                                     outputs=['nest.y[0, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[0, 0]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 0]',],
                                     outputs=['nest.y[0, 0]'],
                                     mode='adjoint')

        diff = J - top.nest.comp.J[0, 0]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 1]',],
                                     outputs=['nest.y[1, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 1]',],
                                     outputs=['nest.y[1, 0]'],
                                     mode='adjoint')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, 1]',],
                                     outputs=['nest.y[1, 0]'],
                                     mode='fd')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, -1]',],
                                     outputs=['nest.y[-1, 0]'],
                                     mode='forward')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, -1]',],
                                     outputs=['nest.y[-1, 0]'],
                                     mode='adjoint')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        J = top.driver.calc_gradient(inputs=['nest.x[0, -1]',],
                                     outputs=['nest.y[-1, 0]'],
                                     mode='fd')

        diff = J - top.nest.comp.J[1, 2]
        assert_rel_error(self, diff.max(), 0.0, .000001)

        Jsub = top.nest.comp.J[2:3, 2:3]
        J = top.driver.calc_gradient(inputs=['nest.x[1][:]',],
                                     outputs=['nest.y[1][:]'],
                                     mode='forward')

        diff = J - Jsub

        J = top.driver.calc_gradient(inputs=['nest.x[1][:]',],
                                     outputs=['nest.y[1][:]'],
                                     mode='adjoint')

        diff = J - Jsub

        top.run()
        J = top.driver.calc_gradient(inputs=['nest.x[1][:]',],
                                     outputs=['nest.y[1][:]'],
                                     mode='fd')
        diff = J - Jsub

    def test_nested_2Darray_gradient_sub(self):

        # This tests the
        top = Assembly()
        top.add('nest', Assembly())
        top.add('driver', SimpleDriver())
        top.nest.add('comp', ArrayComp2D())

        top.driver.workflow.add(['nest'])
        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')

        top.driver.add_parameter('nest.x[0][0]', low=-100, high=100)
        top.driver.add_objective('nest.y[0][1]')

        # Force_fd on the assy
        options = top.nest.driver.gradient_options
        options.force_fd = True

        top.run()
        J = top.driver.calc_gradient()

        self.assertTrue('x[0][0]' in top.nest._system.vec['u']._info)

        assert_rel_error(self, J[0][0], 4.0, .000001)

    def test_nested_2Darray_simul_element_and_full_connection(self):

        top = Assembly()
        top.add('comp', ArrayComp2D())
        top.add('nest', Assembly())
        top.nest.add('comp', ArrayComp2D())

        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest', 'comp'])
        top.connect('nest.y', 'comp.x')
        top.driver.add_parameter('nest.x[0][0]', low=-10, high=10)
        top.driver.add_objective('comp.y[0][0]')
        top.driver.add_constraint('nest.y[0][1] < 0')
        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

    def test_nested_2Darray_simul_element_and_full_connection2(self):
        # Slightly different config

        top = Assembly()
        top.add('nest', Assembly())
        top.nest.add('comp1', ArrayComp2D())
        top.nest.add('comp2', ArrayComp2D())

        top.nest.driver.workflow.add(['comp1', 'comp2'])
        top.nest.connect('comp1.y', 'comp2.x')
        top.nest.create_passthrough('comp1.x')
        top.nest.create_passthrough('comp1.y')
        top.nest.add('yy', Array(iotype='out'))
        top.nest.connect('comp2.y', 'yy')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest'])
        top.driver.add_parameter('nest.x[0][0]', low=-10, high=10)
        top.driver.add_objective('nest.yy[0][0]')
        top.driver.add_constraint('nest.y[0][1] < 0')
        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

    def test_nested_2Darray_simul_element_and_full_connection_apply_derivs(self):
        # Do it all over with apply_deriv defined derivatives

        top = Assembly()
        top.add('comp', ArrayComp2D_der())
        top.add('nest', Assembly())
        top.nest.add('comp', ArrayComp2D_der())

        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest', 'comp'])
        top.connect('nest.y', 'comp.x')
        top.driver.add_parameter('nest.x[0][0]', low=-10, high=10)
        top.driver.add_objective('comp.y[0][0]')
        top.driver.add_constraint('nest.y[0][1] < 0')
        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

    def test_nested_2Darray_simul_element_and_full_connection_apply_derivs2(self):
        # Slightly different config

        top = Assembly()
        top.add('nest', Assembly())
        top.nest.add('comp1', ArrayComp2D_der())
        top.nest.add('comp2', ArrayComp2D_der())

        top.nest.driver.workflow.add(['comp1', 'comp2'])
        top.nest.connect('comp1.y', 'comp2.x')
        top.nest.create_passthrough('comp1.x')
        top.nest.create_passthrough('comp1.y')
        top.nest.add('yy', Array(iotype='out'))
        top.nest.connect('comp2.y', 'yy')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest'])
        top.driver.add_parameter('nest.x[0][0]', low=-10, high=10)
        top.driver.add_objective('nest.yy[0][0]')
        top.driver.add_constraint('nest.y[0][1] < 0')
        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)

    def test_nested_2Darray_simul_element_and_full_connection_multi_param(self):
        # Multi param

        top = Assembly()
        top.add('comp', ArrayComp2D_der())
        top.add('nest', Assembly())
        top.nest.add('comp', ArrayComp2D_der())

        top.nest.driver.workflow.add(['comp'])
        top.nest.create_passthrough('comp.x')
        top.nest.create_passthrough('comp.y')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest', 'comp'])
        top.connect('nest.y', 'comp.x')
        top.driver.add_parameter('nest.x[0][0]', low=-10, high=10)
        top.driver.add_parameter('nest.x[1][0]', low=-10, high=10)
        top.driver.add_objective('comp.y[0][0]')
        top.driver.add_constraint('nest.y[0][1] < 0')
        top.driver.add_constraint('3.0*nest.y[0][0] < 0')
        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)
        assert_rel_error(self, J[2, 0], 6.0, .000001)
        assert_rel_error(self, J[0, 1], 53.0, .000001)
        assert_rel_error(self, J[1, 1], 6.0, .000001)
        assert_rel_error(self, J[2, 1], 9.0, .000001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)
        assert_rel_error(self, J[2, 0], 6.0, .000001)
        assert_rel_error(self, J[0, 1], 53.0, .000001)
        assert_rel_error(self, J[1, 1], 6.0, .000001)
        assert_rel_error(self, J[2, 1], 9.0, .000001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 24.0, .000001)
        assert_rel_error(self, J[1, 0], 4.0, .000001)
        assert_rel_error(self, J[2, 0], 6.0, .000001)
        assert_rel_error(self, J[0, 1], 53.0, .000001)
        assert_rel_error(self, J[1, 1], 6.0, .000001)
        assert_rel_error(self, J[2, 1], 9.0, .000001)

    def test_nested_array_full_and_partial_passthrough(self):

        top = Assembly()
        top.add('nest', Assembly())
        top.nest.add('comp1', ArrayComp2D())
        top.nest.add('comp2', ArrayComp2D())

        top.nest.driver.workflow.add(['comp1', 'comp2'])
        top.nest.create_passthrough('comp1.x', 'x')
        top.nest.create_passthrough('comp1.y', 'y1')
        top.nest.create_passthrough('comp2.y', 'y2')
        top.nest.connect('x[-1, -1]', 'comp2.x[-1, -1]')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest'])
        top.run()

        Jbase = top.nest.comp1.provideJ()

        J = top.driver.calc_gradient(inputs=['nest.x'],
                                     outputs=['nest.y1', 'nest.y2'],
                                     mode='fd')
        diff = abs(J[0:4, :] - Jbase)
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[0:4, -1] - Jbase[:, -1])
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[4:, :-1])
        assert_rel_error(self, diff.max(), 0.0, .00001)

        J = top.driver.calc_gradient(inputs=['nest.x'],
                                     outputs=['nest.y1', 'nest.y2'],
                                     mode='forward')
        diff = abs(J[0:4, :] - Jbase)
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[0:4, -1] - Jbase[:, -1])
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[4:, :-1])
        assert_rel_error(self, diff.max(), 0.0, .00001)

        Jdict = top.driver.calc_gradient(inputs=['nest.x'],
                                         outputs=['nest.y1', 'nest.y2'],
                                         mode='forward',
                                         return_format='dict')
        diff = Jdict['nest.y1']['nest.x'] - Jbase
        assert_rel_error(self, diff.max(), 0.0, .00001)

        diff = Jdict['nest.y2']['nest.x'] - J[4:, :]
        assert_rel_error(self, diff.max(), 0.0, .00001)

        J = top.driver.calc_gradient(inputs=['nest.x'],
                                     outputs=['nest.y1', 'nest.y2'],
                                     mode='adjoint')
        diff = abs(J[0:4, :] - Jbase)
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[0:4, -1] - Jbase[:, -1])
        assert_rel_error(self, diff.max(), 0.0, .00001)
        diff = abs(J[4:, :-1])
        assert_rel_error(self, diff.max(), 0.0, .00001)

        Jdict = top.driver.calc_gradient(inputs=['nest.x'],
                                         outputs=['nest.y1', 'nest.y2'],
                                         mode='adjoint',
                                         return_format='dict')
        diff = Jdict['nest.y1']['nest.x'] - Jbase
        assert_rel_error(self, diff.max(), 0.0, .00001)

        diff = Jdict['nest.y2']['nest.x'] - J[4:, :]
        assert_rel_error(self, diff.max(), 0.0, .00001)

    def test_large_dataflow(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('1.0*comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        #self.top.connect('comp4.y3', 'comp5.x3')

        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1', 'comp4.y3'],
                                          mode='forward')
        assert_rel_error(self, J[0, 0], 61.0, .001)
        assert_rel_error(self, J[1, 0], 126.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1', 'comp4.y3'],
                                          mode='adjoint')

        assert_rel_error(self, J[0, 0], 61.0, .001)
        assert_rel_error(self, J[1, 0], 126.0, .001)


    def test_bug(self):

        self.top = set_as_top(Assembly())

        self.top.add('driver', SimpleDriver())
        self.top.add('dis2', SimpleComp())
        self.top.driver.add_objective('(dis2.y)**2')
        self.top.driver.add_parameter('dis2.x', low=-10.0, high=10.0)
        self.top.driver.add_constraint('dis2.y < 24.0')

        self.top.run()

        J = self.top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .001)
        assert_rel_error(self, J[1, 0], 2.0, .001)

        J = self.top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 24.0, .001)
        assert_rel_error(self, J[1, 0], 2.0, .001)

    def test_equality_constraint(self):

        self.top = set_as_top(Assembly())

        self.top.add('driver', SimpleDriver())
        self.top.add('dis', ArrayComp1())
        self.top.driver.add_parameter('dis.x[0]', low=-10.0, high=10.0)
        self.top.driver.add_constraint('dis.y[0] = 2.0*dis.y[-1]')

        self.top.run()

        J = self.top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], -8.0, .001)

        J = self.top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], -8.0, .001)

        J = self.top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], -8.0, .001)

    def test_three_way(self):
        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 50.0*x1',
                'y2 = 1.0*x1']
        deriv1 = ['dy1_dx1 = 50.0',
                  'dy2_dx1 = 1.0']

        exp2 = ['y1 = 1.2*x1']
        deriv2 = ['dy1_dx1 = 1.2']

        exp3 = ['y1 = 100.0*x1*x2 + 30*x1 + 0.3*x2']
        deriv3 = ['dy1_dx1 = 100.0*x2 + 30',
                  'dy1_dx2 = 100.0*x1 + 0.3']

        self.top.add('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.add('comp2', ExecCompWithDerivatives(exp2, deriv2))
        self.top.add('comp3', ExecCompWithDerivatives(exp3, deriv3))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp3.x2')

        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp3.y1'],
                                          mode='forward')

        Jfd = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                            outputs=['comp3.y1'],
                                            mode='fd')

        diff = Jfd-J
        assert_rel_error(self, diff.max(), 0.0, 0.1)


    def test_nondifferentiable_blocks(self):

        self.top = set_as_top(Assembly())

        exp1 = ['y1 = 2.0*x1**2',
                'y2 = 3.0*x1']
        deriv1 = ['dy1_dx1 = 4.0*x1',
                  'dy2_dx1 = 3.0']

        exp2 = ['y1 = 0.5*x1']
        deriv2 = ['dy1_dx1 = 0.5']

        exp3 = ['y1 = 3.5*x1']
        deriv3 = ['dy1_dx1 = 3.5']

        exp4 = ['y1 = x1 + 2.0*x2',
                'y2 = 3.0*x1',
                'y3 = x1*x2']
        deriv4 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 2.0',
                  'dy2_dx1 = 3.0',
                  'dy2_dx2 = 0.0',
                  'dy3_dx1 = x2',
                  'dy3_dx2 = x1']

        exp5 = ['y1 = x1 + 3.0*x2 + 2.0*x3']
        deriv5 = ['dy1_dx1 = 1.0',
                  'dy1_dx2 = 3.0',
                  'dy1_dx3 = 2.0']

        self.top.add('comp1', ExecComp(exp1))
        self.top.add('comp2', ExecComp(exp2))
        self.top.add('comp3', ExecComp(exp3))
        self.top.add('comp4', ExecCompWithDerivatives(exp4, deriv4))
        self.top.add('comp5', ExecComp(exp5))

        self.top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4', 'comp5'])

        self.top.connect('comp1.y1', 'comp2.x1')
        self.top.connect('comp1.y2', 'comp3.x1')
        self.top.connect('comp2.y1', 'comp4.x1')
        self.top.connect('comp3.y1', 'comp4.x2')
        self.top.connect('comp4.y1', 'comp5.x1')
        self.top.connect('comp4.y2', 'comp5.x2')
        self.top.connect('comp4.y3', 'comp5.x3')

        # Case 1 - differentiable (comp4)

        self.top.comp1.x1 = 2.0
        self.top.run()

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward')

        assert_rel_error(self, J[0, 0], 313.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='adjoint')

        assert_rel_error(self, J[0, 0], 313.0, .001)

        comp_list = simple_node_iter(self.top.driver.workflow._system.subsystems()[1]._nodes)
        self.assertTrue(len(comp_list) == 3)
        self.assertTrue('comp1' in comp_list)
        self.assertTrue('comp2' in comp_list)
        self.assertTrue('comp3' in comp_list)
        comp_list = simple_node_iter(self.top.driver.workflow._system.subsystems()[3]._nodes)
        self.assertTrue(len(comp_list) == 1)
        self.assertTrue('comp5' in comp_list)

        # Case 2 - differentiable (none)

        self.top.replace('comp4', ExecComp(exp4))

        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward')

        assert_rel_error(self, J[0, 0], 313.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='adjoint')

        assert_rel_error(self, J[0, 0], 313.0, .001)

        comp_list = simple_node_iter(self.top.driver.workflow._system.subsystems()[1]._nodes)
        self.assertTrue(len(comp_list) == 5)
        self.assertTrue('comp1' in comp_list)
        self.assertTrue('comp2' in comp_list)
        self.assertTrue('comp3' in comp_list)
        self.assertTrue('comp4' in comp_list)
        self.assertTrue('comp5' in comp_list)


        # Piggyback testing of the is_variable_local function -- make sure it
        # pokes through opaque systems.
        system = self.top.driver.workflow._system
        self.assertTrue(system.is_variable_local('comp2.y1') is True)

        # Case 3 - differentiable (comp5)

        self.top.replace('comp5', ExecCompWithDerivatives(exp5, deriv5))

        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward')

        assert_rel_error(self, J[0, 0], 313.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='adjoint')

        assert_rel_error(self, J[0, 0], 313.0, .001)

        comp_list = simple_node_iter(self.top.driver.workflow._system.subsystems()[1]._nodes)
        self.assertTrue(len(comp_list) == 4)
        self.assertTrue('comp1' in comp_list)
        self.assertTrue('comp2' in comp_list)
        self.assertTrue('comp3' in comp_list)
        self.assertTrue('comp4' in comp_list)

        # Case 4 - differentiable (comp1, comp3, comp5)

        self.top.replace('comp1', ExecCompWithDerivatives(exp1, deriv1))
        self.top.replace('comp3', ExecCompWithDerivatives(exp3, deriv3))

        self.top.comp1.x1 = 2.0
        self.top.run()
        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='forward')

        assert_rel_error(self, J[0, 0], 313.0, .001)

        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='adjoint')

        assert_rel_error(self, J[0, 0], 313.0, .001)

        comp_list = simple_node_iter(self.top.driver.workflow._system.subsystems()[3]._nodes)
        self.assertTrue(len(comp_list) == 2)
        self.assertTrue('comp2' in comp_list)
        self.assertTrue('comp4' in comp_list)


        # Put everything in a single pseudo-assy, and run fd.
        J = self.top.driver.calc_gradient(inputs=['comp1.x1'],
                                          outputs=['comp5.y1'],
                                          mode='fd')
        assert_rel_error(self, J[0, 0], 313.0, .001)


    def test_boundary_variables(self):

        top = set_as_top(Assembly())
        top.add('comp', Paraboloid())

        top.add('target', Float(1.0, iotype='in'))
        top.add('atarget', Array([2.0, 3.0], iotype='in'))

        top.add('driver', SimpleDriver())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('target', low=-100., high=100.)
        top.driver.add_parameter('atarget[1]', low=-100., high=100.)
        top.driver.add_objective('7.0*target + comp.f_xy - 3.5*atarget[1]')
        top.driver.add_constraint('target + 2.0*comp.f_xy - 4.5*atarget[1] < 0')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 7.0, .001)
        assert_rel_error(self, J[0, 1], -3.5, .001)
        assert_rel_error(self, J[1, 0], 1.0, .001)
        assert_rel_error(self, J[1, 1], -4.5, .001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 7.0, .001)
        assert_rel_error(self, J[0, 1], -3.5, .001)
        assert_rel_error(self, J[1, 0], 1.0, .001)
        assert_rel_error(self, J[1, 1], -4.5, .001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 7.0, .001)
        assert_rel_error(self, J[0, 1], -3.5, .001)
        assert_rel_error(self, J[1, 0], 1.0, .001)
        assert_rel_error(self, J[1, 1], -4.5, .001)

        # Do it all again without analytic derivs

        top = set_as_top(Assembly())
        top.add('comp', ParaboloidNoDeriv())

        top.add('target', Float(1.0, iotype='in'))
        top.add('atarget', Array([2.0, 3.0], iotype='in'))

        top.add('driver', SimpleDriver())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('target', low=-100., high=100.)
        top.driver.add_parameter('atarget[1]', low=-100., high=100.)
        top.driver.add_objective('7.0*target + comp.f_xy - 3.5*atarget[1]')
        top.driver.add_constraint('target + 2.0*comp.f_xy - 4.5*atarget[1] < 0')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 7.0, .001)
        assert_rel_error(self, J[0, 1], -3.5, .001)
        assert_rel_error(self, J[1, 0], 1.0, .001)
        assert_rel_error(self, J[1, 1], -4.5, .001)

        J = top.driver.calc_gradient(mode='fd')
        assert_rel_error(self, J[0, 0], 7.0, .001)
        assert_rel_error(self, J[0, 1], -3.5, .001)
        assert_rel_error(self, J[1, 0], 1.0, .001)
        assert_rel_error(self, J[1, 1], -4.5, .001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 7.0, .001)
        assert_rel_error(self, J[0, 1], -3.5, .001)
        assert_rel_error(self, J[1, 0], 1.0, .001)
        assert_rel_error(self, J[1, 1], -4.5, .001)

        top.add('driver', SimpleDriver())
        top.driver.workflow.add('comp')
        top.driver.add_parameter('comp.x', low=-100., high=100.)
        top.driver.add_parameter('atarget[1]', low=-100., high=100.)
        top.driver.add_objective('3.9*atarget[1]+comp.f_xy')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], -6.0, .001)
        assert_rel_error(self, J[0, 1], 3.9, .001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], -6.0, .001)
        assert_rel_error(self, J[0, 1], 3.9, .001)

    def test_first_derivative_with_units(self):
        top = set_as_top(Assembly())

        top.add('comp1', CompFoot())
        top.add('comp2', CompInch())

        top.connect('comp1.y', 'comp2.x')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])

        top.driver.add_parameter('comp1.x', low=-50., high=50., fd_step=.0001)
        top.driver.add_objective('comp2.y')

        top.comp1.x = 2.0
        top.run()

        J = top.driver.calc_gradient(outputs=['comp2.y'],
                                              mode='forward')
        assert_rel_error(self, J[0, 0], 48.0, .001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 48.0, .001)

    def test_unit_with_offset(self):
        top = set_as_top(Assembly())

        top.add('comp1', CompDegC())
        top.add('comp2', CompDegF())

        top.connect('comp1.y', 'comp2.x')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])

        top.driver.add_parameter('comp1.x', low=-50., high=50., fd_step=.0001)
        top.driver.add_objective('comp2.y')

        top.comp1.x = 55.0
        top.run()

        J = top.driver.calc_gradient(outputs=['comp2.y'],
                                     mode='forward')
        assert_rel_error(self, J[0, 0], 1.8, .001)

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 1.8, .001)

    def test_paramgroup(self):

        top = set_as_top(Assembly())
        top.add('comp1', GComp_noD())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1'])

        top.run()

        J = top.driver.calc_gradient(inputs=[('comp1.x1', 'comp1.x2')],
                                              outputs=['comp1.y1'],
                                              mode='forward')
        assert_rel_error(self, J[0, 0], 12.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x1', 'comp1.x2')],
                                              outputs=['comp1.y1'],
                                              mode='adjoint')
        assert_rel_error(self, J[0, 0], 12.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x1', 'comp1.x2')],
                                              outputs=['comp1.y1'],
                                              mode='fd')
        assert_rel_error(self, J[0, 0], 12.0, .001)

        J = top.driver.calc_gradient(inputs=['comp1.x1', ('comp1.x2',)],
                                              outputs=['comp1.y1'],
                                              mode='forward')
        assert_rel_error(self, J[0, 0], 5.0, .001)
        assert_rel_error(self, J[0, 1], 7.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x1', 'comp1.x2', 'comp1.x3')],
                                              outputs=['comp1.y1'],
                                              mode='forward')
        assert_rel_error(self, J[0, 0], 9.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x1', 'comp1.x2', 'comp1.x3')],
                                              outputs=['comp1.y1'],
                                              mode='adjoint')
        assert_rel_error(self, J[0, 0], 9.0, .001)

    def test_paramgroup_deriv(self):

        top = set_as_top(Assembly())
        top.add('comp1', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1'])

        top.run()

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp1.y')],
                                              outputs=['comp1.f_xy'],
                                              mode='forward')
        assert_rel_error(self, J[0, 0], 2.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp1.y')],
                                              outputs=['comp1.f_xy'],
                                              mode='adjoint')
        assert_rel_error(self, J[0, 0], 2.0, .001)

        J = top.driver.calc_gradient(inputs=[('comp1.x', 'comp1.y')],
                                              outputs=['comp1.f_xy'],
                                              mode='fd')
        assert_rel_error(self, J[0, 0], 2.0, .001)

    def test_paramgroup_with_scaler(self):

        top = set_as_top(Assembly())
        top.add('comp1', GComp_noD())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1'])

        top.driver.add_parameter(['comp1.x1', 'comp1.x2'], low=-100, high=100, scaler=2.0)
        top.driver.add_objective('comp1.y1')
        top.run()

        J = top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 12.0*2.0, .001)

        # Piggyback testing of the is_variable_local function -- make sure it
        # pokes through opaque systems.
        system = top.driver.workflow._system
        self.assertTrue(system.is_variable_local('comp1.x1') is True)
        self.assertTrue(system.is_variable_local('comp1.x2') is True)


    def test_missing_derivs_error(self):
        self.top = set_as_top(Assembly())

        self.top.add('driver', SimpleDriver())
        self.top.add('dis2', SimpleCompMissingDeriv())
        self.top.driver.add_objective('(dis2.y)**2')
        self.top.driver.add_parameter('dis2.x', low=-10.0, high=10.0)
        self.top.driver.add_constraint('dis2.miss_out < 24.0')

        self.top.run()
        try:
            J = self.top.driver.calc_gradient(mode='forward')
        except Exception as err:
            self.assertEqual(str(err),
                             "'dis2 (1-dis2): does not provide analytical derivatives for"
                             " miss_out'")
        else:
            self.fail("exception expected")

        self.top.driver.remove_constraint('dis2.miss_out < 24.0')
        self.top.driver.add_constraint('dis2.y < 24.0')
        self.top.driver.remove_parameter('dis2.x')
        self.top.driver.add_parameter('dis2.miss_in', low=-10.0, high=10.0)

        try:
            J = self.top.driver.calc_gradient(mode='forward')
        except Exception as err:
            self.assertEqual(str(err),
                             "'dis2 (1-dis2): does not provide analytical derivatives for"
                             " miss_in'")
        else:
            self.fail("exception expected")

    def test_missing_derivs_assume_zero(self):
        self.top = set_as_top(Assembly())

        self.top.add('driver', SimpleDriver())
        self.top.add('dis2', SimpleCompMissingDeriv())
        self.top.dis2.missing_deriv_policy = 'assume_zero'

        self.top.driver.add_objective('(dis2.y)**2')
        self.top.driver.add_parameter('dis2.x', low=-10.0, high=10.0)
        self.top.driver.add_constraint('dis2.miss_out < 24.0')

        self.top.run()

        J = self.top.driver.calc_gradient(mode='forward')
        assert_rel_error(self, J[0, 0], 24.0, .001)
        assert_rel_error(self, J[1, 0], 0.0, .001)

        # This will error unless we ignore missing derivs
        derivs = self.top.check_gradient(name='dis2', stream=None)
        self.assertTrue('dis2.y / dis2.x' in derivs[2])

        self.top.driver.run_iteration()
        J = self.top.driver.calc_gradient(inputs=['dis2.miss_in'],
                                          mode='fd')
        assert_rel_error(self, J[0, 0], 0.0, .001)
        assert_rel_error(self, J[1, 0], 0.0, .001)


    def test_fd_param_group_arrays_sharing_memory(self):

        class CompSource(Component):

            y = Array(array([0.0, 0.0, 0.0, 0.0]), iotype='out')

            def execute(self):
                self.y = array([1.0, 1.0, 1.0, 1.0])

        class CompSink(Component):

            a = Array(array([0.0, 0.0, 0.0, 0.0]), iotype='in')
            b = Array(array([0.0, 0.0, 0.0, 0.0]), iotype='in')
            y = Float(0.0, iotype='out')

            def execute(self):
                self.y = 2.0*sum(self.a) + 3.0*sum(self.b)
                print "Sink", self.a, self.b, self.y

        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())
        top.add('c1', CompSource())
        top.add('c2', CompSink())
        top.driver.workflow.add(['c1', 'c2'])

        top.connect('c1.y', 'c2.a')
        top.connect('c1.y', 'c2.b')

        top.run()

        top.driver.workflow.clear()
        top.driver.workflow.add('c2')
        J = top.driver.calc_gradient(inputs=[('c2.a', 'c2.b')],
                                              outputs=['c2.y'], mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)

        J = top.driver.calc_gradient(inputs=[('c2.a[0:2]', 'c2.b[0:2]')],
                                              outputs=['c2.y'], mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)

        J = top.driver.calc_gradient(inputs=[('c2.a[:2]', 'c2.b[:2]')],
                                              outputs=['c2.y'], mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)

        J = top.driver.calc_gradient(inputs=[('c2.a[1]', 'c2.b[1]')],
                                              outputs=['c2.y'], mode='fd')
        assert_rel_error(self, J[0, 0], 5.0, .001)

        top.disconnect('c1.y', 'c2.b')
        top.c2.b = array([0.0, 0.0, 0.0, 0.0])
        top.run()
        J = top.driver.calc_gradient(inputs=[('c2.a[0]', 'c2.a[2]')],
                                              outputs=['c2.y'], mode='fd')
        assert_rel_error(self, J[0, 0], 4.0, .001)

        J = top.driver.calc_gradient(inputs=[('c2.a[0:2]', 'c2.a[2:4]')],
                                              outputs=['c2.y'], mode='fd')
        assert_rel_error(self, J[0, 0], 4.0, .001)
        assert_rel_error(self, J[0, 1], 4.0, .001)

    def test_jacobian_size_error(self):

        top = set_as_top(Assembly())
        top.add('comp1', Array_Slice_1D())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1'])

        top.comp1.x = zeros((3, 2))
        try:
            J = top.driver.calc_gradient(inputs=[('comp1.x')],
                                         outputs=['comp1.y'],
                                         mode='forward')
        except RuntimeError as err:
            msg = 'comp1: Jacobian is the wrong size. Expected (4x6) but got (4x4)'
            self.assertEqual(str(err), msg)
        else:
            self.fail("exception expected")


class Comp2(Component):
    """ two-input, two-output"""

    x1 = Float(1.0, iotype='in', units='ft')
    x2 = Float(1.0, iotype='in', units='ft')
    y1 = Float(1.0, iotype='out', units='ft')
    y2 = Float(1.0, iotype='out', units='ft')

    def execute(self):
        """ Executes it """
        pass

    def provideJ(self):
        """Analytical first derivatives"""

        self.J = array([[3.0, 5.0], [7.0, 11.0]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x1', 'x2')
        output_keys = ('y1', 'y2')
        return input_keys, output_keys

class Comp2_array(Component):
    """ two-input, two-output"""

    x = Array(zeros((2, 2)), iotype='in')
    y = Array(zeros((2, 2)), iotype='out')

    def execute(self):
        """ Executes it """
        pass

    def provideJ(self):
        """Analytical first derivatives"""

        self.J = array([[3.0, 133.0, 7.0, 11.0],
                        [8.1, -5.9, 13.3, 1.23],
                        [4.11, 5.0, 17.0, -5.0],
                        [7.77, 6.12, -3.5, 11.0]])

        self.JT = self.J.T

    def apply_deriv(self, arg, result):

        if 'y' in result and 'x' in arg:
            dx = self.J.dot(arg['x'].flatten())

            result['y'] = dx.reshape((2, 2))

    def apply_derivT(self, arg, result):

        if 'y' in arg and 'x' in result:
            dy = self.JT.dot(arg['y'].flatten())

            result['x'] = dy.reshape((2, 2))

class Comp3_array(Component):
    """ two-input, two-output"""

    x = Array(zeros((3, 3)), iotype='in')
    y = Array(zeros((3, 3)), iotype='out')

    def execute(self):
        """ Executes it """
        pass

    def provideJ(self):
        """Analytical first derivatives"""

        self.J = random.random((9, 9))

        self.JT = self.J.T

    def list_deriv_vars(self):
        return ('x',), ('y',)

    def apply_deriv(self, arg, result):

        if 'y' in result and 'x' in arg:
            dx = self.J.dot(arg['x'].flatten())

            result['y'] += dx.reshape((3, 3))

    def apply_derivT(self, arg, result):

        if 'y' in arg and 'x' in result:
            dy = self.JT.dot(arg['y'].flatten())

            result['x'] += dy.reshape((3, 3))


class CompBase(Component):

    x = Float(1.0, iotype='in', units='ft')
    y = Float(1.0, iotype='out', units='ft')

    def execute(self):
        """ Executes it """
        self.y = self.x
        pass

    def provideJ(self):
        """Analytical first derivatives"""

        self.J = array([[1.0]])

    def list_deriv_vars(self):
        ''' What we have'''
        return ('x',), ('y',)


class CompForward(CompBase):

    def apply_deriv(self, arg, result):

        if 'y' in result and 'x' in arg:
            result['y'] += arg['x']

class CompAdjoint(CompBase):

    def apply_derivT(self, arg, result):

        if 'y' in arg and 'x' in result:
            result['x'] += arg['y']

class Testcase_applyJT(unittest.TestCase):
    """ Unit test for conversion of provideJ to applyJT """

    def test_forward_adjoint_error(self):
        # Test our error messages for when you have are missing one
        # of (apply_deriv, apply_derivT) and try to run the other

        model = set_as_top(Assembly())
        model.add('comp', CompForward())
        model.driver.workflow.add('comp')

        try:
            model.run()
        except Exception as err:
            msg = "comp: method 'apply_derivT' must be also specified " + \
                  " if 'apply_deriv' is specified"
            self.assertEqual(str(err), msg)
        else:
            self.fail("exception expected")

        model = set_as_top(Assembly())
        model.add('comp', CompAdjoint())
        model.driver.workflow.add('comp')

        try:
            model.run()
        except Exception as err:
            msg = "comp: method 'apply_deriv' must be also specified " + \
                  " if 'apply_derivT' is specified"
            self.assertEqual(str(err), msg)
        else:
            self.fail("exception expected")

class PreComp(Component):
    '''Comp with preconditioner'''

    x1 = Float(1.0, iotype='in', units='inch')
    x2 = Float(1.0, iotype='in', units='inch')
    y1 = Float(1.0, iotype='out', units='inch')
    y2 = Float(1.0, iotype='out', units='inch')

    def execute(self):
        """ Executes it """

        self.y1 = 2.0*self.x1 + 7.0*self.x2
        self.y2 = 13.0*self.x1 - 3.0*self.x2

    def provideJ(self):
        """Analytical first derivatives"""

        dy1_dx1 = 2.0
        dy1_dx2 = 7.0
        dy2_dx1 = 13.0
        dy2_dx2 = -3.0
        self.J = array([[dy1_dx1, dy1_dx2], [dy2_dx1, dy2_dx2]])
        return self.J

    def list_deriv_vars(self):

        input_keys = ('x1', 'x2')
        output_keys = ('y1', 'y2')
        return input_keys, output_keys

    def applyMinv(self, arg, result):

        result['y1'] = 0.03092784*arg['y1'] + 0.07216495*arg['y2']
        result['y2'] = 0.13402062*arg['y1'] - 0.02061856*arg['y2']

        return result

    def applyMinvT(self, arg, result):

        result['y1'] = 0.03092784*arg['y1'] + 0.13402062*arg['y2']
        result['y2'] = 0.07216495*arg['y1'] - 0.02061856*arg['y2']

        return result

class PreCompArray(Component):
    '''Comp with preconditioner'''

    x = Array(array([1.0, 1.0]), iotype='in')
    y = Array(array([1.0, 1.0]), iotype='out')

    def execute(self):
        """ Executes it """

        self.y[0] = 2.0*self.x[0] + 7.0*self.x[1]
        self.y[1] = 13.0*self.x[0] - 3.0*self.x[1]

    def provideJ(self):
        """Analytical first derivatives"""

        dy1_dx1 = 2.0
        dy1_dx2 = 7.0
        dy2_dx1 = 13.0
        dy2_dx2 = -3.0
        self.J = array([[dy1_dx1, dy1_dx2], [dy2_dx1, dy2_dx2]])
        return self.J

    def list_deriv_vars(self):

        input_keys = ('x', )
        output_keys = ('y', )
        return input_keys, output_keys

    def applyMinv(self, arg, result):

        if 'y' in arg:
            result['y'][0] = 0.03092784*arg['y'][0] + 0.07216495*arg['y'][1]
            result['y'][1] = 0.13402062*arg['y'][0] - 0.02061856*arg['y'][1]

        return result

    def applyMinvT(self, arg, result):

        if 'y' in arg:
            result['y'][0] = 0.03092784*arg['y'][0] + 0.13402062*arg['y'][1]
            result['y'][1] = 0.07216495*arg['y'][0] - 0.02061856*arg['y'][1]

        return result

class Testcase_preconditioning(unittest.TestCase):
    """ Unit test for applyMinv and applyMinvT """

    def test_simple(self):

        top = set_as_top(Assembly())
        top.add('comp', PreComp())
        top.driver.workflow.add('comp')

        J = top.driver.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                     outputs=['comp.y1', 'comp.y2'],
                                     mode='forward')

        #print J
        # TODO: transform back to original coords
        #assert_rel_error(self, J[0, 0], 2.0, 0.0001)
        #assert_rel_error(self, J[0, 1], 7.0, 0.0001)
        #assert_rel_error(self, J[1, 0], 13.0, 0.0001)
        #assert_rel_error(self, J[1, 1], -3.0, 0.0001)

        J = top.driver.calc_gradient(inputs=['comp.x1', 'comp.x2'],
                                     outputs=['comp.y1', 'comp.y2'],
                                     mode='adjoint')

        assert_rel_error(self, J[0, 0], 2.0, 0.0001)
        assert_rel_error(self, J[0, 1], 7.0, 0.0001)
        assert_rel_error(self, J[1, 0], 13.0, 0.0001)
        assert_rel_error(self, J[1, 1], -3.0, 0.0001)

    def test_two_comp(self):

        top = set_as_top(Assembly())

        top.add('comp1', PreComp())
        top.add('comp2', PreComp())
        top.connect('comp1.y1', 'comp2.x1')
        top.connect('comp1.y2', 'comp2.x2')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.driver.add_parameter('comp1.x1', low=-10, high=10)
        top.driver.add_parameter('comp1.x2', low=-10, high=10)
        top.driver.add_objective('comp2.y1 + comp2.y2')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        #print J

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 82.0, 0.0001)
        assert_rel_error(self, J[0, 1], 93.0, 0.0001)

    def test_two_comp_array(self):

        top = set_as_top(Assembly())

        top.add('comp1', PreCompArray())
        top.add('comp2', PreCompArray())
        top.connect('comp1.y', 'comp2.x')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2'])
        top.driver.add_parameter('comp1.x[0]', low=-10, high=10)
        top.driver.add_parameter('comp1.x[1]', low=-10, high=10)
        top.driver.add_objective('comp2.y[0]')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        #print J

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 95.0, 0.0001)
        assert_rel_error(self, J[0, 1], -7.0, 0.0001)

    def test_nested_array_element(self):

        top = set_as_top(Assembly())
        top.add('nest', Assembly())

        top.nest.add('comp1', PreCompArray())
        top.nest.add('comp2', PreCompArray())
        top.nest.add('comp3', PreCompArray())
        top.nest.connect('comp1.y', 'comp2.x')
        top.nest.driver.workflow.add(['comp1', 'comp2', 'comp3'])
        top.nest.create_passthrough('comp1.x')
        top.nest.create_passthrough('comp2.y')
        top.nest.add('yyy', Array([0.0, 0.0], iotype='out'))
        top.nest.add('dumb', Array(array([2.0, 4.0]), iotype='in'))
        top.nest.connect('comp1.y', 'yyy')
        top.nest.connect('dumb', 'comp3.x')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['nest'])
        top.driver.add_parameter('nest.x[0]', low=-10, high=10)
        top.driver.add_parameter('nest.x[1]', low=-10, high=10)
        top.driver.add_parameter('nest.dumb[0]', low=-10, high=10)
        top.driver.add_parameter('nest.dumb[1]', low=-10, high=10)
        top.driver.add_objective('nest.y[0]')
        top.driver.add_constraint('nest.yyy[0] + nest.yyy[1] < 0')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        #print J

        J = top.driver.calc_gradient(mode='adjoint')
        #print J


        assert_rel_error(self, J[0, 0], 95.0, 0.0001)
        assert_rel_error(self, J[0, 1], -7.0, 0.0001)
        assert_rel_error(self, J[0, 2], 0.0, 0.0001)
        assert_rel_error(self, J[0, 3], 0.0, 0.0001)
        assert_rel_error(self, J[1, 0], 15.0, 0.0001)
        assert_rel_error(self, J[1, 1], 4.0, 0.0001)
        assert_rel_error(self, J[1, 2], 0.0, 0.0001)
        assert_rel_error(self, J[1, 3], 0.0, 0.0001)

    def test_two_comp_bifurcation(self):

        top = set_as_top(Assembly())

        top.add('comp1', PreComp())
        top.add('comp2', PreComp())
        top.add('comp3', PreComp())
        top.add('comp4', PreComp())
        top.connect('comp1.y1', 'comp2.x1')
        top.connect('comp1.y2', 'comp2.x2')
        top.connect('comp1.y1', 'comp3.x1')
        top.connect('comp1.y2', 'comp3.x2')
        top.connect('comp2.y1 + comp3.y1', 'comp4.x1')
        top.connect('comp2.y2 - comp3.y2', 'comp4.x2')

        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp1', 'comp2', 'comp3', 'comp4'])
        top.driver.add_parameter('comp1.x1', low=-10, high=10)
        top.driver.add_parameter('comp1.x2', low=-10, high=10)
        top.driver.add_objective('comp4.y1 + comp2.y1')
        top.driver.add_constraint('comp4.y2 + comp3.y2 < 0')

        top.run()

        J = top.driver.calc_gradient(mode='forward')
        #print J

        J = top.driver.calc_gradient(mode='adjoint')
        assert_rel_error(self, J[0, 0], 475.0, 0.0001)
        assert_rel_error(self, J[0, 1], -35.0, 0.0001)
        assert_rel_error(self, J[1, 0], 2457.0, 0.0001)
        assert_rel_error(self, J[1, 1], -82.0, 0.0001)


class TestMultiDriver(unittest.TestCase):

    def test_nested_driver(self):

        top = set_as_top(Assembly())
        top.add('comp', SimpleComp())
        top.add('driver', SimpleDriver())
        top.add('inner_driver', SimpleDriver())
        top.add('target', Float(3.0, iotype='in'))

        top.driver.workflow.add('inner_driver')
        top.driver.add_parameter('target', low=-100, high=100)
        top.driver.add_objective('target + comp.x + comp.y')

        top.inner_driver.workflow.add('comp')
        top.inner_driver.add_parameter('comp.x', low=-100, high=100)
        top.inner_driver.add_objective('2.0*target + 2.0*comp.x + 2.0*comp.y')

        top.run()

        self.assertEqual( set(top._system.vec['u'].keys()),
                          set([('comp.y', ('_pseudo_0.in2', '_pseudo_1.in2')),
                               ('_pseudo_0.out0', ('_pseudo_0.out0',)),
                               ('target', ('_pseudo_0.in0', '_pseudo_1.in0', 'target')),
                               ('_pseudo_1.out0', ('_pseudo_1.out0',)),
                               ('comp.x', ('_pseudo_1.in1', '_pseudo_0.in1', 'comp.x')),
                              ]))

    def test_PA_subvar_solver_edges(self):

        # Note, this test documents a bug where the pseudoassembly didn't
        # correctly identify its solver edges because they were subvars,
        # resulting in an exception. The test runs to assure there is no
        # exception.

        sp = set_as_top(UnitScalableProblem())
        sp.architecture = MDF()

        # Make sure it runs.
        sp.run()

        # Reconverge inner solver with tight tolerance so FD is accurate.
        sp.solver.tol = 1e-12
        sp.run()

        # Test gradient
        #sp.driver.gradient_options.fd_form = 'central'
        J = sp.driver.calc_gradient()

        Jfd = sp.driver.calc_gradient(mode='fd')

        diff = J - Jfd

        assert_rel_error(self, diff.max(), 0.0, .001)

    def test_PA_subvar_driver_edges(self):

        raise SkipTest('Cannot specify options on architectures yet.')

        # There was a keyerror here too, resulting from a basevar node
        # that got removed somehow on the recursed optimizer graph.

        sp = set_as_top(UnitScalableProblem())
        sp.architecture = CO()

        # Don't run it forever
        sp.driver.maxiter = 1
        #sp.local_opt_d0.maxiter = 1
        #sp.local_opt_d1.maxiter = 1

        # Make sure it runs.
        sp.run()

        # Test gradient
        sp.driver.gradient_options.fd_form = 'central'
        J = sp.driver.calc_gradient()

        Jfd = sp.driver.calc_gradient(mode='fd')

        diff = J - Jfd
        assert_rel_error(self, diff.max(), 0.0, .001)







if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
