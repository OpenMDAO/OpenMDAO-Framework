
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
from openmdao.main.mpiwrap import MPI, make_idx_array, to_idx_array, evenly_distrib_idxs
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecComp
from openmdao.test.mpiunittest import MPITestCase, MPIContext
from openmdao.util.testutil import assert_rel_error

def take_nth(rank, size, seq):
    """Return an iterator over the sequence that returns every
    nth element of seq based on the given rank within a group of
    the given size.  For example, if size = 2, a rank of 0 returns
    even indexed elements and a rank of 1 returns odd indexed elements.
    """
    assert(rank < size)
    it = iter(seq)
    while True:
        for proc in range(size):
            if rank == proc:
                yield it.next()
            else:
                it.next()

class InOutArrayComp(Component):
    delay = Float(0.01, iotype='in')

    def __init__(self, arr_size=10):
        super(InOutArrayComp, self).__init__()
        self.mpi.requested_cpus = 2

        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        time.sleep(self.delay)
        self.outvec = self.invec * 2.

class DistribCompSimple(Component):
    """Uses 2 procs but takes full input vars"""
    def __init__(self, arr_size=10):
        super(DistribCompSimple, self).__init__()
        self.mpi.requested_cpus = 2

        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        if self.mpi.comm == MPI.COMM_NULL:
            return
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


class DistribInputComp(Component):
    """Uses 2 procs and takes input var slices"""
    def __init__(self, arr_size=11):
        super(DistribInputComp, self).__init__()
        self.arr_size = arr_size
        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        if self.mpi.comm == MPI.COMM_NULL:
            return

        for i,val in enumerate(self.invec):
            self.local_outvec[i] = 2*val

        self.mpi.comm.Allgatherv(self.local_outvec,
                                [self.outvec, self.sizes,
                                 self.offsets, MPI.DOUBLE])

    def get_distrib_idxs(self):
        """ component declares the local sizes and sets initial values
        for all distributed inputs and outputs"""

        comm = self.mpi.comm
        rank = comm.rank

        start, end, self.sizes, self.offsets = evenly_distrib_idxs(comm, self.arr_size)

        #need to re-initialize the variable to have the correct local size
        self.invec = np.ones(self.sizes[rank], dtype=float)
        self.local_outvec = np.empty(self.sizes[rank], dtype=float)

        return {
            'invec': make_idx_array(start, end),
            'outvec': make_idx_array(start, end)
        }

    def get_req_cpus(self):
        return 2


class DistribOverlappingInputComp(Component):
    """Uses 2 procs and takes input var slices"""
    def __init__(self, arr_size=11):
        super(DistribOverlappingInputComp, self).__init__()
        self.arr_size = arr_size
        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        if self.mpi.comm == MPI.COMM_NULL:
            return

        for i,val in enumerate(self.invec):
            self.local_outvec[i] = 2*val

        outs = self.mpi.comm.allgather(self.local_outvec)

        self.outvec = np.zeros(self.arr_size, float)
        tmpout = np.zeros(self.arr_size, float)

        self.outvec[:8] = outs[0]
        tmpout[4:11] = outs[1]

        self.outvec += tmpout

    def get_distrib_idxs(self):
        """ component declares the local sizes and sets initial values
        for all distributed inputs and outputs"""

        comm = self.mpi.comm
        rank = comm.rank

        #need to re-initialize the variable to have the correct local size
        if rank == 0:
            size = 8
            start = 0
            end = 8
        else:
            size = 7
            start = 4
            end = 11

        self.invec = np.ones(size, dtype=float)
        self.local_outvec = np.empty(size, dtype=float)

        return {
            'invec': make_idx_array(start, end),
            'outvec': make_idx_array(start, end)
        }

    def get_req_cpus(self):
        return 2

class DistribInputDistribOutputComp(Component):
    """Uses 2 procs and takes input var slices and has output var slices as well"""
    def __init__(self, arr_size=11):
        super(DistribInputDistribOutputComp, self).__init__()
        self.arr_size = arr_size
        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        if self.mpi.comm == MPI.COMM_NULL:
            return

        #start = self.offsets[self.mpi.comm.rank]
        for i,val in enumerate(self.invec):
            self.outvec[i] = 2*val

    def get_distrib_idxs(self):
        """ component declares the local sizes and sets initial values
        for all distributed inputs and outputs. Returns a dict of
        index arrays keyed to variable names.
        """

        comm = self.mpi.comm
        rank = comm.rank

        start, end, sizes, offsets = evenly_distrib_idxs(comm, self.arr_size)

        self.invec = np.ones(sizes[rank], dtype=float)
        self.outvec = np.ones(sizes[rank], dtype=float)

        print self.name,".outvec",self.outvec

        return {
            'invec': make_idx_array(start, end),
            'outvec': make_idx_array(start, end)
        }

    def get_req_cpus(self):
        return 2

class DistribNoncontiguousComp(Component):
    """Uses 2 procs and takes non-contiguous input var slices and has output
    var slices as well
    """
    def __init__(self, arr_size=11):
        super(DistribNoncontiguousComp, self).__init__()
        self.arr_size = arr_size
        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        if self.mpi.comm == MPI.COMM_NULL:
            return

        for i,val in enumerate(self.invec):
            self.outvec[i] = 2*val

    def get_distrib_idxs(self):
        """ component declares the local sizes and sets initial values
        for all distributed inputs and outputs. Returns a dict of
        index arrays keyed to variable names.
        """

        comm = self.mpi.comm
        rank = comm.rank

        idxs = list(take_nth(rank, comm.size, range(self.arr_size)))

        self.invec = np.ones(len(idxs), dtype=float)
        self.outvec = np.ones(len(idxs), dtype=float)

        return {
            'invec': to_idx_array(idxs),
            'outvec': to_idx_array(idxs)
        }

    def get_req_cpus(self):
        return 2

class DistribGatherComp(Component):
    """Uses 2 procs gathers a distrib input into a full output"""
    def __init__(self, arr_size=11):
        super(DistribGatherComp, self).__init__()
        self.arr_size = arr_size
        self.add_trait('invec', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        if self.mpi.comm == MPI.COMM_NULL:
            return

        self.mpi.comm.Allgatherv(self.invec,
                                [self.outvec, self.sizes,
                                 self.offsets, MPI.DOUBLE])

    def get_distrib_idxs(self):
        """ component declares the local sizes and sets initial values
        for all distributed inputs and outputs. Returns a dict of
        index arrays keyed to variable names.
        """

        comm = self.mpi.comm
        rank = comm.rank

        start, end, self.sizes, self.offsets = evenly_distrib_idxs(comm, self.arr_size)

        #need to re-initialize the variable to have the correct local size
        self.invec = np.ones(self.sizes[comm.rank], dtype=float)

        return { 'invec': make_idx_array(start, end) }

    def get_req_cpus(self):
        return 2

class NonDistribGatherComp(Component):
    """Uses 2 procs gathers a distrib input into a full output"""
    def __init__(self):
        super(NonDistribGatherComp, self).__init__()
        self.add_trait('invec', Array(np.ones(0, float), iotype='in'))
        self.add_trait('outvec', Array(np.ones(0, float), iotype='out'))

    def execute(self):
        self.outvec = self.invec[:]


class MPITests(MPITestCase):

    N_PROCS = 2

    def test_distrib_full_in_out(self):
        size = 11

        top = set_as_top(Assembly())
        top.add("C1", InOutArrayComp(size))
        top.add("C2", DistribCompSimple(size))
        top.driver.workflow.add(['C1', 'C2'])
        top.connect('C1.outvec', 'C2.invec')

        top.C1.invec = np.ones(size, float) * 5.0

        top.run()

        self.assertTrue(all(top.C2.outvec==np.ones(size, float)*7.5))

    def test_distrib_idx_in_full_out(self):
        size = 11

        top = set_as_top(Assembly())
        top.add("C1", InOutArrayComp(size))
        top.add("C2",DistribInputComp(size))
        top.driver.workflow.add(['C1', 'C2'])
        top.connect('C1.outvec', 'C2.invec')

        top.C1.invec = np.array(range(size, 0, -1), float)

        top.run()

        self.assertTrue(all(top.C2.outvec==np.array(range(size, 0, -1), float)*4))

    def test_distrib_idx_in_distrb_idx_out(self):
        # normal comp to distrib comp to distrb gather comp
        size = 11

        top = set_as_top(Assembly())
        top.add("C1", InOutArrayComp(size))
        top.add("C2", DistribInputDistribOutputComp(size))
        top.add("C3", DistribGatherComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3'])
        top.connect('C1.outvec', 'C2.invec')
        top.connect('C2.outvec', 'C3.invec')

        top.C1.invec = np.array(range(size, 0, -1), float)

        top.run()

        self.assertTrue(all(top.C3.outvec==np.array(range(size, 0, -1), float)*4))

    def test_noncontiguous_idxs(self):
        # take even input indices in 0 rank and odd ones in 1 rank
        size = 11

        top = set_as_top(Assembly())
        top.add("C1", InOutArrayComp(size))
        top.add("C2", DistribNoncontiguousComp(size))
        top.add("C3", DistribGatherComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3'])
        top.connect('C1.outvec', 'C2.invec')
        top.connect('C2.outvec', 'C3.invec')

        top.C1.invec = np.array(range(size), float)

        top.run()

        if self.comm.rank == 0:
            self.assertTrue(all(top.C2.outvec == np.array(list(take_nth(0, 2, range(size))), 'f')*4))
        else:
            self.assertTrue(all(top.C2.outvec == np.array(list(take_nth(1, 2, range(size))), 'f')*4))

        full_list = list(take_nth(0, 2, range(size))) + list(take_nth(1, 2, range(size)))
        self.assertTrue(all(top.C3.outvec == np.array(full_list, 'f')*4))

    def test_overlapping_inputs_idxs(self):
        # distrib comp with distrib_idxs that overlap, i.e. the same
        # entries are distributed to multiple processes
        size = 11

        top = set_as_top(Assembly())
        top.add("C1", InOutArrayComp(size))
        top.add("C2",DistribOverlappingInputComp(size))
        top.driver.workflow.add(['C1', 'C2'])
        top.connect('C1.outvec', 'C2.invec')

        top.C1.invec = np.array(range(size, 0, -1), float)

        top.run()

        self.assertTrue(all(top.C2.outvec[:4]==np.array(range(size, 0, -1), float)[:4]*4))
        self.assertTrue(all(top.C2.outvec[8:]==np.array(range(size, 0, -1), float)[8:]*4))

        # overlapping part should be double size of the rest
        self.assertTrue(all(top.C2.outvec[4:8]==np.array(range(size, 0, -1), float)[4:8]*8))

    def test_nondistrib_gather(self):
        # regular comp --> distrib comp --> regular comp.  last comp should
        # automagically gather the full vector without declaring distrib_idxs
        size = 11

        top = set_as_top(Assembly())
        top.add("C1", InOutArrayComp(size))
        top.add("C2", DistribInputDistribOutputComp(size))
        top.add("C3", NonDistribGatherComp())
        top.driver.workflow.add(['C1', 'C2', 'C3'])
        top.connect('C1.outvec', 'C2.invec')
        top.connect('C2.outvec', 'C3.invec')

        top.C1.invec = np.array(range(size, 0, -1), float)

        top.run()

        if self.comm.rank == 0:
            self.assertTrue(all(top.C3.outvec==np.array(range(size, 0, -1), float)*4))


if __name__ == '__main__':
    from openmdao.test.mpiunittest import mpirun_tests
    mpirun_tests()
