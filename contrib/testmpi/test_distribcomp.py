
from unittest import TestCase
import time
import sys

import numpy as np

from openmdao.lib.drivers.iterate import FixedPointIterator
from openmdao.lib.drivers.newton_solver import NewtonSolver
from openmdao.lib.optproblems import sellar

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.interfaces import implements, ISolver
from openmdao.main.mpiwrap import MPI
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecComp
from openmdao.test.mpiunittest import MPITestCase, MPIContext
from openmdao.util.testutil import assert_rel_error



class ABCDArrayComp(Component):
    delay = Float(0.01, iotype='in')

    def __init__(self, arr_size=10):
        super(ABCDArrayComp, self).__init__()
        self.add_trait('a', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('b', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('c', Array(np.ones(arr_size, float), iotype='out'))
        self.add_trait('d', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        time.sleep(self.delay)
        self.c = self.a + self.b
        self.d = self.a - self.b

    def dump(self, comm):
        print "%d: %s.a = %s" % (comm.rank, self.name, self.a)
        print "%d: %s.b = %s" % (comm.rank, self.name, self.b)
        print "%d: %s.c = %s" % (comm.rank, self.name, self.c)
        print "%d: %s.d = %s" % (comm.rank, self.name, self.d)


class DistribCompSimple(Component):
    """Uses 2 procs but takes full input vars"""
    def __init__(self, arr_size=10):
        super(DistribCompSimple, self).__init__()
        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        if self.mpi.comm != MPI.COMM_NULL:
            if self.mpi.comm.rank == 0:
                self.outvec = self.invec * 0.25
            elif self.mpi.comm.rank == 1:
                self.outvec = self.invec * 0.5

            # now combine vecs from different processes
            both = np.zeros((2, len(self.outvec)))
            self.mpi.comm.Allgather(self.outvec, both)

            # add both together to get our output
            self.outvec = both[0,:] + both[1,:]

    def get_req_cpus(self):
        return 2


class DistribComp(Component):
    """Uses 2 procs and takes input var slices"""
    def __init__(self, arr_size=10):
        super(DistribComp, self).__init__()
        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        if self.mpi.comm != MPI.COMM_NULL:
            if self.mpi.comm.rank == 0:
                self.outvec = self.invec * 0.25
            elif self.mpi.comm.rank == 1:
                self.outvec = self.invec * 0.5

    def get_arg_indices(self, name):
        comm = self.mpi.comm
        if name in ('in'):
            num = self.a.size / comm.size
            start = comm.rank * num
            end = start + num
            if comm.rank == comm.size - 1:
                end += self.a.size % comm.size
            return list(xrange(start, end))

    def get_req_cpus(self):
        return 2

class MPITests1(MPITestCase):

    N_PROCS = 2

    def test_distrib_simple(self):
        size = 10

        top = set_as_top(Assembly())
        top.add("C1", ABCDArrayComp(size))
        top.add("C2", DistribCompSimple(size))
        top.driver.workflow.add(['C1', 'C2'])
        top.connect('C1.c', 'C2.invec')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0

        top.run()

        self.assertTrue(all(top.C2.outvec==np.ones(size, float)*7.5))


if __name__ == '__main__':
    from openmdao.test.mpiunittest import mpirun_tests
    mpirun_tests()
