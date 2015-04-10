""" Test other optimizers under MPI."""

from unittest import TestCase

import numpy as np

from openmdao.lib.drivers.api import SLSQPdriver, COBYLAdriver, CONMINdriver
from openmdao.main.api import Component, Assembly, set_as_top
from openmdao.main.datatypes.api import Float
from openmdao.test.mpiunittest import MPITestCase
from openmdao.util.testutil import assert_rel_error


class Parab1(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')


    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = (x-3.0)**2 + (y+4.0)**2

    def provideJ(self):
        """Caculate and return the Jacobian"""

        df_dx = 2.0*self.x - 6.0
        df_dy = 2.0*self.y + 8.0

        return np.array([[df_dx, df_dy]])

    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)

        return input_keys, output_keys

class Parab2(Component):
    """ Evaluates the equation f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3 """

    # set up interface to the framework
    # pylint: disable-msg=E1101
    x = Float(0.0, iotype='in', desc='The variable x')
    y = Float(0.0, iotype='in', desc='The variable y')

    f_xy = Float(iotype='out', desc='F(x,y)')


    def execute(self):
        """f(x,y) = (x-3)^2 + xy + (y+4)^2 - 3
        Optimal solution (minimum): x = 6.6667; y = -7.3333
        """

        x = self.x
        y = self.y

        self.f_xy = x*y - 3.0

    def provideJ(self):
        """Caculate and return the Jacobian"""

        df_dx = self.y
        df_dy = self.x

        return np.array([[df_dx, df_dy]])

    def list_deriv_vars(self):
        input_keys = ('x', 'y')
        output_keys = ('f_xy',)

        return input_keys, output_keys

class MPITests1(MPITestCase):

    N_PROCS = 2

    def test_SLSQP(self):

        top = set_as_top(Assembly())
        top.add('parab1', Parab1())
        top.add('parab2', Parab2())

        top.add('driver', SLSQPdriver())
        top.driver.workflow.add(['parab1', 'parab2'])
        top.driver.add_parameter(['parab1.x', 'parab2.x'], low=-100, high=100)
        top.driver.add_parameter(['parab1.y', 'parab2.y'], low=-100, high=100)
        top.driver.add_objective('parab1.f_xy + parab2.f_xy')
        top.driver.add_constraint('parab1.x-parab1.y >= 15.0')

        top.run()

        assert_rel_error(self, top.parab1.x, 7.166, 0.001)
        assert_rel_error(self, top.parab1.y, -7.833, 0.001)
        assert_rel_error(self, top._pseudo_0.out0, -27.083, 0.001)

    def test_COBYLA(self):

        top = set_as_top(Assembly())
        top.add('parab1', Parab1())
        top.add('parab2', Parab2())

        top.add('driver', COBYLAdriver())
        top.driver.workflow.add(['parab1', 'parab2'])
        top.driver.add_parameter(['parab1.x', 'parab2.x'], low=-100, high=100)
        top.driver.add_parameter(['parab1.y', 'parab2.y'], low=-100, high=100)
        top.driver.add_objective('parab1.f_xy + parab2.f_xy')
        top.driver.add_constraint('parab1.x-parab1.y >= 15.0')

        top.run()

        assert_rel_error(self, top.parab1.x, 7.166, 0.001)
        assert_rel_error(self, top.parab1.y, -7.833, 0.001)
        assert_rel_error(self, top._pseudo_0.out0, -27.083, 0.001)

    def test_CONMIN(self):

        top = set_as_top(Assembly())
        top.add('parab1', Parab1())
        top.add('parab2', Parab2())

        top.add('driver', CONMINdriver())
        top.driver.workflow.add(['parab1', 'parab2'])
        top.driver.add_parameter(['parab1.x', 'parab2.x'], low=-100, high=100)
        top.driver.add_parameter(['parab1.y', 'parab2.y'], low=-100, high=100)
        top.driver.add_objective('parab1.f_xy + parab2.f_xy')
        top.driver.add_constraint('parab1.x-parab1.y >= 15.0')

        top.run()

        assert_rel_error(self, top.parab1.x, 7.176, 0.001)
        assert_rel_error(self, top.parab1.y, -7.824, 0.001)
        assert_rel_error(self, top._pseudo_0.out0, -27.083, 0.001)

if __name__ == '__main__':
    from openmdao.test.mpiunittest import mpirun_tests
    mpirun_tests()
