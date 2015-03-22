import unittest

from openmdao.main.api import Assembly, set_as_top, Component, Driver, ImplicitComponent
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.mp_support import has_interface
from openmdao.main.interfaces import IHasParameters, implements
from openmdao.main.hasparameters import HasParameters
from openmdao.util.decorators import add_delegate
from openmdao.main.interfaces import ISolver
from openmdao.test.execcomp import ExecComp
from openmdao.util.testutil import assert_rel_error
from numpy.testing import assert_almost_equal
from math import sin, cos, pi
from scipy.optimize import brentq
from scipy.interpolate import interp1d
import numpy as np

from openmdao.lib.drivers.brent import Brent


class TestBrentDriver(unittest.TestCase):

    def test_brent_converge(self):

        a = set_as_top(Assembly())
        comp = a.add('comp', ExecComp(exprs=["f=a * x**n + b * x - c"]))
        comp.n = 77.0/27.0
        comp.a = 1.0
        comp.b = 1.0
        comp.c = 10.0

        driver = a.add('driver', Brent())
        driver.add_parameter('comp.x', 0, 100)
        driver.add_constraint('comp.f=0')

        a.run()

        assert_rel_error(self, a.comp.x, 2.06720359226, .0001)
        assert_rel_error(self, a.comp.f, 0, .0001)

        self.assertTrue(has_interface(driver, ISolver))

    def test_errors(self):
        a = set_as_top(Assembly())
        comp = a.add('comp', ExecComp(exprs=["f=x"]))
        driver = a.add('driver', Brent())
        driver.add_parameter('comp.x')
        driver.add_constraint('comp.f=0')
        comp.n = 1.0
        comp.c = 0
        driver.lower_bound = 1.0
        try:
            a.run()
        except Exception as err:
            self.assertEqual(str(err), "driver: bounds (low=1.0, high=100.0) do not bracket a root")
        else:
            self.fail("Exception expected")

    def test_initial_run(self):

        class MyComp(Component):

            x = Float(0.0, iotype='in', low=-100000, high=100000)
            xx = Float(0.0, iotype='in', low=-100000, high=100000)
            f_x = Float(iotype='out')
            y = Float(iotype='out')

            def execute(self):
                if self.xx != 1.0:
                    self.raise_exception("xx should be 1.0, but it's %s" % self.xx, RuntimeError)
                self.f_x = 2.0*self.x
                self.y = self.x

        @add_delegate(HasParameters)
        class SpecialDriver(Driver):

            implements(IHasParameters)

            def execute(self):
                self.set_parameters([1.0])
                self.run_iteration()

        top = set_as_top(Assembly())
        top.add('comp', MyComp())
        top.add('driver', Brent())
        top.add('subdriver', SpecialDriver())
        top.driver.workflow.add('subdriver')
        top.subdriver.workflow.add('comp')

        top.subdriver.add_parameter('comp.xx')
        top.driver.add_parameter('comp.x')
        top.driver.add_constraint('comp.y = 1.0')

        top.run()



class TestBrentResizeBracket(unittest.TestCase):

    def setUp(self):
        class TestComponent(ImplicitComponent):

            # in
            a = Float(iotype='in')
            ap = Float(iotype='in')
            lambda_r = Float(iotype='in')

            # states
            phi = Float(iotype='state')

            # residuals
            residual = Float(iotype='residual')

            # outputs
            dummy = Float(iotype='out')

            eval_only = True

            def evaluate(self):

                self.residual = sin(self.phi)/(1-self.a) - cos(self.phi)/self.lambda_r/(1+self.ap)
                self.dummy = self.phi * 2



        class TestAssembly(Assembly):

            a = Float(iotype='in')
            ap = Float(iotype='in')
            lambda_r = Float(iotype='in')

            phi_star = Float(iotype='out')


            def configure(self):

                self.add('comp', TestComponent())
                self.add('brent', Brent())

                self.brent.workflow.add(['comp'])
                self.driver.workflow.add(['brent'])

                # connections to comp
                self.connect('a', 'comp.a')
                self.connect('ap', 'comp.ap')
                self.connect('lambda_r', 'comp.lambda_r')

                # setup Brent
                eps = 1e-6
                self.brent.lower_bound = eps
                self.brent.upper_bound = pi/2 - eps
                self.brent.add_parameter('comp.phi')
                self.brent.add_constraint('comp.residual = 0')

                def resize(lower, upper, iter):
                    if lower == eps and upper == pi/2 - eps:
                        return -pi/4, -eps, True
                    elif lower == -pi/4 and upper == -eps:
                        return pi/2+eps, pi-eps, True
                    else:
                        return lower, upper, False

                self.brent.f_resize_bracket = resize

                # connect outputs
                self.connect('comp.phi', 'phi_star')

        eps = 1e-6

        # for manual usage
        def f(phi, assembly):
            return sin(phi)/(1-assembly.a) - cos(phi)/assembly.lambda_r/(1+assembly.ap)

        # openmdao
        self.assembly = set_as_top(TestAssembly())
        self.manual_f = f


    def test_case1(self):
        #normal usage

        self.assembly.a = 0.3
        self.assembly.ap = 0.01
        self.assembly.lambda_r = 7.0

        # run in openmdao
        self.assembly.run()

        # run manually
        eps = 1e-6
        phi_star = brentq(self.manual_f, eps, pi/2-eps, args=self.assembly)

        assert_almost_equal(self.assembly.phi_star, phi_star, decimal=7)


    def test_case2(self):
        #alternate bracket

        self.assembly.a = 1.5
        self.assembly.ap = 0.01
        self.assembly.lambda_r = 7.0

        # openmdao
        self.assembly.run()

        # manual
        eps = 1e-6
        phi_star = brentq(self.manual_f, -pi/4.0, -eps, args=self.assembly)

        assert_almost_equal(self.assembly.phi_star, phi_star, decimal=7)



class TestBrentInvalidBracket(unittest.TestCase):

    def setUp(self):

        class TestComponent(ImplicitComponent):

            # in
            V = Array(iotype='in')
            P = Array(iotype='in')
            Prated = Float(iotype='in')

            # state
            Vrated = Float(iotype='state')

            # residual
            residual = Float(iotype='residual')

            # out
            dummy = Float(iotype='out')


            def execute(self):

                f = interp1d(self.V, self.P, kind='cubic')
                P = f(self.Vrated)
                self.residual = P - self.Prated
                self.dummy = 2 * self.Prated


        class TestAssembly(Assembly):

            V = Array(iotype='in')
            P = Array(iotype='in')
            Prated = Float(iotype='in')
            Vin = Float(iotype='in')
            Vout = Float(iotype='in')
            invalid_bracket_return = Float(iotype='in')

            Vrated = Float(iotype='out')


            def configure(self):

                self.add('comp', TestComponent())
                self.add('brent', Brent())

                self.brent.workflow.add(['comp'])
                self.driver.workflow.add(['brent'])

                # connections to comp
                self.connect('V', 'comp.V')
                self.connect('P', 'comp.P')
                self.connect('Prated', 'comp.Prated')

                # setup Brent
                self.connect('Vin', 'brent.lower_bound')
                self.connect('Vout', 'brent.upper_bound')
                self.brent.add_parameter('comp.Vrated')
                self.brent.add_constraint('comp.residual = 0')
                self.connect('invalid_bracket_return', 'brent.invalid_bracket_return')

                # connect outputs
                self.connect('comp.Vrated', 'Vrated')

        # openmdao
        self.assembly = set_as_top(TestAssembly())




    def test_case1(self):
        #normal

        Vin = 3.0
        Vout = 25.0
        self.assembly.V = np.linspace(Vin, Vout, 50)
        self.assembly.P = self.assembly.V**3
        self.assembly.Prated = 1000.0
        self.assembly.Vin = Vin
        self.assembly.Vout = Vout

        self.assembly.run()

        assert_almost_equal(self.assembly.Vrated, 10.0, decimal=7)


    def test_case2(self):
        #solution does not contain bracket

        Vin = 3.0
        Vout = 25.0
        self.assembly.V = np.linspace(Vin, Vout, 50)
        self.assembly.P = self.assembly.V**3
        self.assembly.Prated = 50**3
        self.assembly.Vin = Vin
        self.assembly.Vout = Vout
        self.assembly.invalid_bracket_return = 1.0

        self.assembly.run()

        assert_almost_equal(self.assembly.Vrated, 25.0, decimal=7)


    def test_case3(self):
        #solution does not contain bracket

        Vin = 3.0
        Vout = 25.0
        self.assembly.V = np.linspace(Vin, Vout, 50)
        self.assembly.P = self.assembly.V**3
        self.assembly.Prated = 50**3
        self.assembly.Vin = Vin
        self.assembly.Vout = Vout
        self.assembly.invalid_bracket_return = 0.5

        self.assembly.run()

        assert_almost_equal(self.assembly.Vrated, 14.0, decimal=7)


    def test_case4(self):
        #solution does not contain bracket

        Vin = 3.0
        Vout = 25.0
        self.assembly.V = np.linspace(Vin, Vout, 50)
        self.assembly.P = self.assembly.V**3
        self.assembly.Prated = 50**3
        self.assembly.Vin = Vin
        self.assembly.Vout = Vout
        self.assembly.invalid_bracket_return = 0.0

        self.assembly.run()

        assert_almost_equal(self.assembly.Vrated, 3.0, decimal=7)



if __name__ == "__main__":

    unittest.main()
