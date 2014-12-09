
import numpy as np

from openmdao.util.testutil import assert_rel_error
from openmdao.test.mpiunittest import MPITestCase
from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.main.mpiwrap import mpiprint
from openmdao.main.test.simpledriver import SimpleDriver

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

    def setUp(self):
        # this model mimics the one in test_derivatives, test_single_comp
        self.top = top = set_as_top(Assembly())
        top.add('comp', Paraboloid())
        top.add('driver', SimpleDriver())
        top.driver.workflow.add(['comp'])
        top.driver.add_parameter('comp.x', low=-1000, high=1000)
        top.driver.add_parameter('comp.y', low=-1000, high=1000)
        top.driver.add_objective('comp.f_xy')

        top.comp.x = 3
        top.comp.y = 5

    def test_run(self):

        self.top.run()

        self.assertEqual(self.top.comp.f_xy, 93.)
        self.assertEqual(self.top._pseudo_0.out0, 93.)

    def test_calc_gradient_fwd(self):
        self.top.run()

        J = self.top.driver.calc_gradient(mode='forward',
                                          return_format='dict')
        #mpiprint("J local: %s" % J)

        J = self.top.driver.workflow._system.get_combined_J(J)
        #mpiprint("final J: %s" % J)

        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 21.0, 0.0001)

    def test_calc_gradient_adjoint(self):
        self.top.run()

        J = self.top.driver.calc_gradient(mode='adjoint',
                                          return_format='dict')
        #mpiprint("J local: %s" % J)

        J = self.top.driver.workflow._system.get_combined_J(J)
        #mpiprint("final J: %s" % J)

        if self.comm.rank == 0:
            assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 5.0, 0.0001)
            assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 21.0, 0.0001)

    def test_calc_gradient_fd(self):
        self.top.run()

        J = self.top.driver.calc_gradient(mode='fd',
                                          return_format='dict')

        assert_rel_error(self, J['_pseudo_0.out0']['comp.x'][0][0], 5.0, 0.0001)
        assert_rel_error(self, J['_pseudo_0.out0']['comp.y'][0][0], 21.0, 0.0001)


if __name__ == '__main__':
    import unittest
    unittest.main()
