
import unittest

import time
import numpy as np

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

    def test_find_system(self):
        top = self.top

        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')
        top.comp.x = 3
        top.comp.y = 5

        top.run()

        sys = top._system.find_system('comp')
        self.assertTrue(sys.name == 'comp')
        sys = top._system.find_system("('_pseudo_0', 'comp', 'comp.x', 'comp.y')")
        self.assertTrue(sys.name == "('_pseudo_0', 'comp', 'comp.x', 'comp.y')")

        system = top.driver.workflow._system
        self.assertTrue(system.is_variable_local('comp.x') is True)

        try:
            system.is_variable_local('junk.stuff')
        except Exception as err:
            msg = 'Cannot find a system that contains varpath junk.stuff'
            self.assertEqual(str(err), msg)
        else:
            self.fail("Exception expected")


class ABCDArrayComp(Component):
    delay = Float(0.01, iotype='in')

    def __init__(self, arr_size=9):
        super(ABCDArrayComp, self).__init__()
        self.add_trait('a', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('b', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('c', Array(np.ones(arr_size, float), iotype='out'))
        self.add_trait('d', Array(np.ones(arr_size, float), iotype='out'))

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

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0

        try:
            top.run()
        except Exception as err:
            self.assertEqual(str(err), "Subvars ['C1.c[3::]', 'C1.c[:5:]'] share overlapping indices. Try reformulating the problem to prevent this.")
        else:
            self.fail("Exception expected")

    def test_no_flat_subassy_slice(self):

        # Test for a bug that Cal found.

        class BtoR(Component):
            B = Array(iotype='in', noflat=True)
            M = Array(iotype='in', noflat=True)
            R = Array(iotype='out', noflat=True)

            def execute(self):
                self.R = np.array([np.dot(self.M.T, b) for b in self.B])

        class B(Component):
            X = Array(iotype='in', noflat=True)

            def execute(self):
                self.X += 2

        class AA(Assembly):
            def configure(self):
                self.add('b_to_r', BtoR())
                self.create_passthrough('b_to_r.R')
                self.create_passthrough('b_to_r.M')
                self.create_passthrough('b_to_r.B')
                self.driver.workflow.add(['b_to_r'])

        class BB(Assembly):
            def configure(self):
                self.add('b', B())

                self.create_passthrough('b.X')

                self.driver.workflow.add(['b'])

        class Analysis(Assembly):
            def configure(self):
                self.add('aa', AA())
                self.add('bb', BB())

                self.connect('aa.R[:4]', 'bb.X')

                self.driver.workflow.add(['aa', 'bb'])

        analysis = set_as_top(Analysis())

        #Initialize camera settings
        analysis.aa.M = np.eye(3)
        analysis.aa.B = np.random.random((3,3))

        # Make sure it runs without a size error.
        analysis.run()


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
        expected = ": out1.x was not initialized. OpenMDAO does not support uninitialized variables."

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
        top.add('out1', self.C4(np.eye(2)))
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
        top.add('out1', self.C4(np.array(range(5))))
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

class Source(Component):

    s = Float(2, iotype="in")
    out = Array(iotype="out")

    def execute(self):
        self.out = self.s*np.ones(5)


class Sink(Component):

    invar = Array(iotype="in")
    out = Float(0, iotype="out")

    def execute(self):
        self.out = np.sum(self.invar)


class ArrayAsmb(Assembly):

    def configure(self):
        self.add('source', Source())
        self.add('sink', Sink())

        self.connect('source.out', 'sink.invar')

        self.driver.workflow.add(['source','sink'])


class TestArrayConnectErrors(unittest.TestCase):

    def test_wrong_initial_size(self):
        # Give a clear error message if an array variable changes size at
        # runtime compared to its initial value used to size the framework arrays

        t = set_as_top(ArrayAsmb())

        t.source.out = np.zeros(2)

        try:
            t.run()
        except RuntimeError as err:
            self.assertEqual(str(err),
                             "Array size mis-match in 'source.out'. Initial shape was (2,) but found size (5,) at runtime")
        else:
            self.fail('RuntimeError expected')

    def test_unintialized_sink_array_var(self):
        # Make sure you can run, even if the sink side of a connection is initialized
        t = set_as_top(ArrayAsmb())
        t.add('driver', SimpleDriver())
        t.driver.add_parameter('source.s', low=-10, high=10)
        t.driver.add_constraint('sink.out < 10')

        t.source.out = np.zeros((5,))

        t.run() # should run without error

if __name__ == "__main__":
    unittest.main()
