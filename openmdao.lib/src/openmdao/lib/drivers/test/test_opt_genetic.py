"""
Test the genetic optimizer driver
"""


import unittest
import random

from openmdao.main.datatypes.api import Float, Array, Enum, Int, Str
from pyevolve import Selectors

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.lib.drivers.genetic import Genetic

# pylint: disable-msg=E1101


class SphereFunction(Component):
    total = Float(0., iotype='out')
    x = Float(0, low=-5.12, high=5.13, iotype="in")
    y = Enum([-10, 0, 1, 2, 3, 4, 5], iotype="in")
    z = Int(0, low=-5, high=5, iotype="in")

    def __init__(self):
        super(SphereFunction, self).__init__()

    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = self.x**2+self.y**2+self.z**2


class Asmb(Assembly):
    def configure(self):
        self.add('sphere', SphereFunction())
        self.driver.workflow.add('sphere')
        self.create_passthrough('sphere.x')
        self.create_passthrough('sphere.y')
        self.create_passthrough('sphere.z')
        self.create_passthrough('sphere.total')


class SphereFunctionArray(Component):
    total = Float(0., iotype='out')
    x = Array([0.0, 0.0, 0.0], iotype="in")

    def __init__(self):
        super(SphereFunctionArray, self).__init__()

    def execute(self):
        """ calculate the sume of the squares for the list of numbers """
        self.total = self.x[0]**2+self.x[1]**2+self.x[2]**2


class TestCase(unittest.TestCase):
    """ test case for the genetic driver"""

    def setUp(self):
        random.seed(10)

        # pyevolve does some caching that causes failures during our
        # complete unit tests due to stale values in the cache attributes
        # below, so reset them here
        Selectors.GRankSelector.cachePopID = None
        Selectors.GRankSelector.cacheCount = None
        Selectors.GRouletteWheel.cachePopID = None
        Selectors.GRouletteWheel.cacheWheel = None

        self.top = set_as_top(Assembly())
        self.top.add('driver', Genetic())
        self.top.driver.seed = 123

    def tearDown(self):
        self.top = None

    def test_optimizeSphere_set_high_low(self):
        self.top.add('comp', SphereFunction())
        self.top.driver.workflow.add('comp')
        self.top.driver.add_objective("comp.total")

        self.top.driver.add_parameter('comp.x', high=5.13, low=-5.12)
        self.top.driver.add_parameter('comp.y')
        self.top.driver.add_parameter('comp.z', high=5, low=-5)

        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"

        self.top.run()

        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               .020, places=2)
        x, y, z = [x for x in self.top.driver.best_individual]
        self.assertAlmostEqual(x, 0.135, places=2)
        self.assertEqual(y, 0)
        self.assertEqual(z, 0)

    def test_optimizeSphere(self):
        self.top.add('comp', SphereFunction())
        self.top.driver.workflow.add('comp')
        self.top.driver.add_objective("comp.total")

        self.top.driver.add_parameter('comp.x')
        self.top.driver.add_parameter('comp.y')
        self.top.driver.add_parameter('comp.z')

        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"

        self.top.run()

        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               .02, places=1)
        x, y, z = [x for x in self.top.driver.best_individual]
        self.assertAlmostEqual(x, 0.135, places=2)
        self.assertEqual(y, 0)
        self.assertEqual(z, 0)

    def test_optimizeSpherearray_nolowhigh(self):
        self.top.add('comp', SphereFunctionArray())
        self.top.driver.workflow.add('comp')
        self.top.driver.add_objective("comp.total")

        try:
            self.top.driver.add_parameter('comp.x[0]')
        except ValueError as err:
            self.assertEqual(str(err),
                             "driver: Trying to add parameter 'comp.x[0]', "
                             "but no lower limit was found and no 'low' "
                             "argument was given. One or the other must be "
                             "specified.")
        else:
            self.fail('TypeError expected')

    def test_optimizeSphereAssemblyPassthrough(self):
        self.top.add('comp', Asmb())
        self.top.driver.workflow.add('comp')
        self.top.driver.add_objective("comp.total")

        self.top.driver.add_parameter('comp.x')
        self.top.driver.add_parameter('comp.y')
        self.top.driver.add_parameter('comp.z')

        self.top.driver.mutation_rate = .02
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"

        self.top.run()

        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               .02, places=1)
        x, y, z = [x for x in self.top.driver.best_individual]
        self.assertAlmostEqual(x, .135, places=2)
        self.assertEqual(y, 0)
        self.assertEqual(z, 0)

    def test_optimizeSpherearray(self):
        self.top.add('comp', SphereFunctionArray())
        self.top.driver.workflow.add('comp')
        self.top.driver.add_objective("comp.total")

        self.top.driver.add_parameter('comp.x[0]', low=-5.12, high=5.13)
        self.top.driver.add_parameter('comp.x[1]', low=-5.12, high=5.13)
        self.top.driver.add_parameter('comp.x[2]', low=-5.12, high=5.13)

        self.top.driver.mutation_rate = .02
        self.top.driver.population_size = 100
        self.top.driver.generations = 1
        self.top.driver.opt_type = "minimize"

        self.top.run()

        self.assertAlmostEqual(self.top.driver.best_individual.score,
                               0.22, places=2)
        x, y, z = [x for x in self.top.driver.best_individual]

    def test_list_remove_clear_params(self):
        self.top.add('comp', SphereFunction())
        self.top.driver.workflow.add('comp')
        self.top.driver.add_parameter('comp.x')
        self.top.driver.add_parameter('comp.y')

        params = self.top.driver.list_param_targets()
        self.assertEqual(set(params), set(['comp.x', 'comp.y']))
        self.assertEqual(len(params), 2)

        self.top.driver.remove_parameter('comp.x')
        params = self.top.driver.list_param_targets()
        self.assertEqual(params, ['comp.y'])

        try:
            self.top.driver.remove_parameter('xyz')
        except AttributeError as err:
            self.assertEqual(str(err),
                             "driver: Trying to remove parameter 'xyz' that is "
                             "not in this driver.")
        else:
            self.fail('RuntimeError Expected')

        self.top.driver.add_parameter('comp.x')
        self.top.driver.clear_parameters()
        params = self.top.driver.list_param_targets()
        self.assertEqual(params, [])

        self.top.driver.add_parameter('comp.y')
        try:
            self.top.driver.add_parameter('comp.y')
        except ValueError as err:
            self.assertEqual(str(err),
                             "driver: ['comp.y'] are already Parameter targets")
        else:
            self.fail('RuntimeError expected')

    def test_0_low_high(self):

        class SomeComp(Component):
            """Arbitrary component with a few variables, but which does not
            really do any calculations"""

            w = Float(0.0, low=-10, high=0.0, iotype="in")
            x = Float(0.0, low=0.0, high=100.0, iotype="in")
            y = Int(10, low=10, high=100, iotype="in")
            z = Enum([-10, -5, 0, 7], iotype="in")

        class Simulation(Assembly):
            """Top Level Assembly used for simulation"""

            def configure(self):
                """Adds the Genetic driver to the assembly"""

                opt = self.add('optimizer', Genetic())
                self.add('comp', SomeComp())
                opt.workflow.add('comp')

                self.optimizer.add_parameter('comp.x')
                self.optimizer.add_parameter('comp.y')
                self.optimizer.add_parameter('comp.z')

        s = Simulation()

    def test_improper_parameter_type(self):

        class SomeComp(Component):
            """Arbitrary component with a few variables, but which does not
            really do any calculations"""

            z = Str("test", iotype="in")

        class Simulation(Assembly):
            """Top Level Assembly used for simulation"""

            def configure(self):
                """Adds the Genetic driver to the assembly"""

                self.add('driver', Genetic())
                self.add('comp', SomeComp())
                self.driver.workflow.add('comp')

                self.driver.add_parameter('comp.z')

        try:
            s = set_as_top(Simulation())
        except ValueError as err:
            self.assertEqual(str(err),
                "driver: The value of parameter 'comp.z' must be a real or "
                "integral type, but its type is 'str'.")
        else:
            self.fail("ValueError expected")

    def test_initial_run(self):

        from openmdao.main.interfaces import IHasParameters, implements
        from openmdao.main.hasparameters import HasParameters
        from openmdao.util.decorators import add_delegate

        class MyComp(Component):

            x = Float(0.0, iotype='in', low=-10, high=10)
            xx = Float(0.0, iotype='in', low=-10, high=10)
            f_x = Float(iotype='out')
            y = Float(iotype='out')

            def execute(self):
                if self.xx != 1.0:
                    self.raise_exception("Lazy", RuntimeError)
                self.f_x = 2.0*self.x
                self.y = self.x

                #print self.x, self.xx, self.f_x, self.y

        @add_delegate(HasParameters)
        class SpecialDriver(Driver):

            implements(IHasParameters)

            def execute(self):
                self.set_parameters([1.0])

        top = set_as_top(Assembly())
        top.add('comp', MyComp())
        top.add('driver', Genetic())
        top.add('subdriver', SpecialDriver())
        top.driver.workflow.add('subdriver')
        top.subdriver.workflow.add('comp')

        top.subdriver.add_parameter('comp.xx')
        top.driver.add_parameter('comp.x')
        top.driver.add_objective('comp.f_x')

        top.run()


if __name__ == "__main__":
    unittest.main()
