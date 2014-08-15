
import numpy as np

from openmdao.util.testutil import assert_rel_error
from openmdao.test.mpiunittest import MPITestCase
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.main.mpiwrap import mpiprint
from openmdao.main.test.test_derivatives import SimpleDriver

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

        self.J = np.array([[df_dx, df_dy]])
        return self.J

    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)
        return input_keys, output_keys


class MPITests(MPITestCase):

    N_PROCS = 2

    def test_simple(self):
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

        # if our rank >= required cpus, nothing will actually
        # run so the numbers will be wrong, so skip that case
        if self.comm.rank == 0:
            self.assertEqual(top.comp.f_xy, 93.)
            self.assertEqual(top._pseudo_0.out0, 93.)

            J = top.driver.workflow.calc_gradient(mode='forward')

            assert_rel_error(self, J[0, 0], 5.0, 0.0001)
            assert_rel_error(self, J[0, 1], 21.0, 0.0001)

            J = top.driver.workflow.calc_gradient(mode='adjoint')

            assert_rel_error(self, J[0, 0], 5.0, 0.0001)
            assert_rel_error(self, J[0, 1], 21.0, 0.0001)

            J = top.driver.workflow.calc_gradient(mode='fd')

            assert_rel_error(self, J[0, 0], 5.0, 0.0001)
            assert_rel_error(self, J[0, 1], 21.0, 0.0001)


if __name__ == '__main__':
    import unittest
    unittest.main()