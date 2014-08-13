
import time

import numpy as np

from openmdao.test.mpiunittest import MPITestCase
from openmdao.util.testutil import assert_rel_error

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.mpiwrap import MPI, mpiprint, set_print_rank
from openmdao.lib.drivers.iterate import FixedPointIterator

from openmdao.lib.optproblems import sellar

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
        #mpiprint("%s: c = %s" % (self.name, self.c))


class SellarMDF(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with FixedPointIterator.
    """
    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        self.add('driver', FixedPointIterator())

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        C1 = self.add('C1', sellar.Discipline1())
        C2 = self.add('C2', sellar.Discipline2())

        self.driver.workflow.add(['C1','C2'])

        #not relevant to the iteration. Just fixed constants
        C1.z1 = C2.z1 = 1.9776
        C1.z2 = C2.z2 = 0
        C1.x1 = 0

        # Solver settings
        self.driver.max_iteration = 5
        self.driver.tolerance = 1.e-15
        self.driver.print_convergence = False


class MPITests1(MPITestCase):

    N_PROCS = 6

    def test_sellar_params1(self):
        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C1.y2 = C2.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()

        if self.comm.rank == 0:
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, 0.001)

    def test_sellar_params2(self):
        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C2.y2 = C1.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()

        if self.comm.rank == 0:
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, 0.001)


class MPITests2(MPITestCase):

    N_PROCS = 6

    def test_sellar_cyclic(self):

        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')
        top.connect('C2.y2', 'C1.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()

        if self.comm.rank == 0:
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, 0.001)

    def test_sellar_parallel(self):
        # no direct connections
        top = set_as_top(SellarMDF())

        top.driver.add_parameter('C1.y1', low=-1e99, high=1e99)
        top.driver.add_constraint('C1.y1 = C2.y1')
        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C2.y2 = C1.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()

        if self.comm.rank == 0:
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, 0.001)

    # def test_system_layers(self):
    #     #8 comps, several layers of subsystems
    #     top = set_as_top(Assembly())
    #     top.add('driver', NTimes(1))
    #     for i in range(1,9):
    #         name = 'C%d' % i
    #         top.add(name, ABCDArrayComp(5))
    #         top.driver.workflow.add(name)
    #         getattr(top, name).mpi.requested_cpus = 1

    #     top.create_passthrough('C2.a')
    #     top.create_passthrough('C7.d')

    #     conns = [
    #         ('C1.c','C4.a'),
    #         ('C2.c','C5.a'),
    #         ('C3.c','C8.a'),
    #         ('C3.d','C6.a'),
    #         ('C5.c','C4.b'),
    #         ('C5.d','C6.b'),
    #         ('C6.c','C8.b'),
    #         ('C6.d','C7.a'),
    #     ]

    #     for u,v in conns:
    #         top.connect(u, v)

    #     top.driver.add_parameter('C3.a[1]', high=100.0, low=0.0)
    #     top.driver.add_constraint('C8.d[0]=0')

    #     top.run()


# FIXME: running this file as main currently doesn't work...
if __name__ == '__main__':
    import unittest
    unittest.main()
