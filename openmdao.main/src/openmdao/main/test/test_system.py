
import unittest

import time
from numpy import array, ones, eye

from openmdao.main.api import Component, Driver, Assembly, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.util.decorators import add_delegate
from openmdao.util.testutil import assert_rel_error


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


@add_delegate(HasParameters, HasObjective, HasConstraints)
class SimpleDriver(Driver):
    """Driver with Parameters"""

    implements(IHasParameters)


class TestcaseParaboloid(unittest.TestCase):
    def setUp(self):
        self.top = top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])

    def test_single_comp(self):
        top = self.top

        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')
        top.comp.x = 3
        top.comp.y = 5

        top.run()

        # See if model gets the right answer
        self.assertEqual(top.comp.f_xy, 93.)
        self.assertEqual(top._pseudo_0.in0, 93.)
        self.assertEqual(top._pseudo_0.out0, 93.)

    def test_boundary_out(self):
        top = self.top

        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.create_passthrough('comp.f_xy')

        top.comp.x = 3
        top.comp.y = 5

        top.run()

        # See if model gets the right answer
        self.assertEqual(top.f_xy, 93.)

    def test_boundary_in_out(self):
        top = self.top

        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.create_passthrough('comp.x')
        top.create_passthrough('comp.f_xy')

        top.x = 3
        top.comp.y = 5

        top.run()

        # See if model gets the right answer
        self.assertEqual(top.f_xy, 93.)


class ABCDArrayComp(Component):
    delay = Float(0.01, iotype='in')

    def __init__(self, arr_size=9):
        super(ABCDArrayComp, self).__init__()
        self.add_trait('a', Array(ones(arr_size, float), iotype='in'))
        self.add_trait('b', Array(ones(arr_size, float), iotype='in'))
        self.add_trait('c', Array(ones(arr_size, float), iotype='out'))
        self.add_trait('d', Array(ones(arr_size, float), iotype='out'))

    def execute(self):
        time.sleep(self.delay)
        self.c = self.a + self.b
        self.d = self.a - self.b


class TestArrayComp(unittest.TestCase):
    def test_overlap_exception(self):
        size = 20   # array var size

        top = set_as_top(Assembly())
        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.add("C4", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3', 'C4'])
        top.connect('C1.c[:5]', 'C2.a')
        top.connect('C1.c[3:]', 'C3.b')
        top.connect('C2.c', 'C4.a')
        top.connect('C3.d', 'C4.b')

        top.C1.a = ones(size, float) * 3.0
        top.C1.b = ones(size, float) * 7.0

        try:
            top.run()
        except Exception as err:
            self.assertEqual(str(err), "Subvars ['C1.c[3::]', 'C1.c[:5:]'] share overlapping indices. Try reformulating the problem to prevent this.")
        else:
            self.fail("Exception expected")

class UninitializedArray(unittest.TestCase):
    class C1(Component):
        x = Array(iotype='out')

        def execute(self):
            pass

    class C2(Component):
        x = Array(iotype='in')

        def execute(self):
            pass

    class C3(Component):
        x = Array(iotype='out', noflat=True)

        def execute(self):
            pass

    class C4(Component):
        x = Array(iotype='out')

        def __init__(self, x):
            super(UninitializedArray.C4, self).__init__()
            self.x = x

        def execute(self):
            pass

    def test_uninitialized_array(self):
        expected = ": c1.x was not initialized. OpenMDAO does not support uninitialized variables."

        """
        out1.x is:
            - uninitialized
            - flattenable
            - the source of a connection
            - not a slice
        """

        top = set_as_top(Assembly())
        top.add('out1', self.C1())
        top.add('in1', self.C2())
        top.connect('out1.x', 'in1.x')
        top.driver.workflow.add(['out1', 'in1'])

        try:
            top.run()
        except ValueError as e:
            self.assertEqual(str(e), expected)
        else:
            self.fail("Should have raised error message: {}".format(expected))

        """
        out1.x is:
            - uninitialized
            - not flattenable
            - the source of a connection
            - not a slice
        """

        top = set_as_top(Assembly())
        top.add('out1', self.C3())
        top.add('in1', self.C2())
        top.connect('out1.x', 'in1.x')
        top.driver.workflow.add(['out1', 'in1'])
        
        top.run()
       
        """
        out1.x is:
            - initialized
            - flattenable
            - the source of a connection
            - not a slice
        """

        top = set_as_top(Assembly())
        top.add('out1', self.C4(eye(2)))
        top.add('in1', self.C2())
        top.connect('out1.x', 'in1.x')
        top.driver.workflow.add(['out1', 'in1'])
        
        top.run()

        """
        out1.x is:
            - initialized
            - flattenable
            - the source of a connection
            - not a slice

        in1.x[::1] is:
            - initialized
            - flattenable
            - the source of a connection
            - a slice
        """ 
        
        top = set_as_top(Assembly())
        top.add('out1', self.C4(array(range(5))))
        top.add('in1', self.C2())
        top.add('in2', self.C2())
        top.connect('out1.x', 'in1.x')
        top.connect('in1.x[::1]', 'in2.x')
        top.driver.workflow.add(['out1', 'in1', 'in2'])
        
        top.run()
       
        """
        sub.out1.x is:
            - not initialized
            - flattenable
            - source of a connection
            - not a slice
        """
        expected = "sub: out1.x was not initialized. OpenMDAO does not support uninitialized variables."
 
        top = set_as_top(Assembly())
        top.add('sub', Assembly())
        top.sub.add('out1', self.C1())
        top.sub.add('in1', self.C2())

        top.sub.connect('out1.x', 'in1.x')
        top.sub.driver.workflow.add(['out1', 'in1'])
        top.driver.workflow.add(['sub'])


        try:
            top.run()
        except ValueError as e:
            self.assertEqual(str(e), expected)
        else:
            self.fail("Should have raised error message: {}".format(expected))
