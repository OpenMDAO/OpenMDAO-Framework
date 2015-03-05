"""
A simple unit testing framework for MPI programs.
"""

import os
import sys
import types
import traceback
from inspect import getmro

from unittest import TestCase, SkipTest
from openmdao.main.mpiwrap import under_mpirun
from openmdao.util.testutil import assert_rel_error

try:
    from mpi4py import MPI
except ImportError:
    MPI = None


def mpi_fail_if_any(f):
    """In order to keep MPI tests from hanging when
    a test fails in one process and succeeds in
    another, perform an allgather to check for any
    failures in any of the processes running the test,
    and fail if any of them fail.
    """
    def failwrapper(self, *args, **kwargs):
        try:
            ret = f(self, *args, **kwargs)
        except Exception:
            exc = sys.exc_info()
            fail = True
        else:
            fail = False

        fails = MPI.COMM_WORLD.allgather(fail)

        if fail:
            raise exc[0], exc[1], exc[2]
        elif any(fails):
            self.fail("a test failed in another rank")

        return ret

    return failwrapper

collective_assert_rel_error = mpi_fail_if_any(assert_rel_error)

def wrapper(f):
    if under_mpirun():
        return f

    else:
        def wrap(self, *args, **kwargs):
            if MPI is None:
                raise SkipTest("mpi4py not installed")

            # loop over the results from all of the MPI procs we
            # spawned and keep the first failure/error.
            for i, info in enumerate(self.infos):
                if info['failures']:
                    if "a test failed in another rank" not in info['failures'][0]:
                        self.fail("{process %d} %s" % (i, info['failures'][0]))
                elif info['errors']:
                    if "a test failed in another rank" not in info['errors'][0]:
                        self.fail("{process %d} %s" % (i, info['errors'][0]))

        wrap.__name__ = f.__name__ # nose won't find it unless __name__ starts with 'test_'
        return wrap


class MPITestCaseMeta(type):
    """A metaclass to create collective versions of
    all of the 'assert*' methods used for testing.
    """
    def __init__(cls, name, bases, dct):
        super(MPITestCaseMeta, cls).__init__(name, bases, dct)
        parent = getmro(cls)[1]
        for n, obj in parent.__dict__.items():
            if n.startswith('assert') or n == 'fail':
                setattr(cls, 'collective_'+n, mpi_fail_if_any(obj))


class MPITestCase(TestCase):
    """A base class for all TestCases that are
    intended to run under MPI.  Note that these will only work properly
    when running under testflo or in a script executed via mpirun.
    """
    __metaclass__ = MPITestCaseMeta

    # A class attribute 'N_PROCS' must be defined
    # for each MPITestCase class in order to
    # know how big to make the MPI communicator.
    # N_PROCS = 4
    def __init__(self, methodName='runTest'):
        super(MPITestCase, self).__init__(methodName=methodName)
        self.comm = MPI.COMM_WORLD
