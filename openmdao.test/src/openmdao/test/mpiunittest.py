
"""
Some classes to make it a little more convenient to do MPI testing.
"""

import sys
from contextlib import contextmanager
from unittest import TestCase

from mpi4py import MPI


@contextmanager
def MPIContext():
    try:
        yield
    except:
        exc_type, exc_val, exc_tb = sys.exc_info()
        if exc_val is not None:
            fail = True
        else:
            fail = False

        fails = MPI.COMM_WORLD.allgather(fail)

        if fail or not any(fails):
            raise exc_type, exc_val, exc_tb
        else:
            for i,f in enumerate(fails):
                if f:
                    raise RuntimeError("a test failed in (at least) rank %d" % i)


class MPITestCase(TestCase):
    """A base class for all TestCases that are
    intended to run under mpirun.
    """

    # A class attribute 'N_PROCS' must be defined
    # for each MPITestCase class in order to
    # know how big to make the MPI communicator.
    # N_PROCS = 4
    def __init__(self, methodName='runTest'):
        super(MPITestCase, self).__init__(methodName=methodName)
        self.comm = MPI.COMM_WORLD
