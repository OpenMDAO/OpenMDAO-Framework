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


class MPIContext(object):
    """Supports using the 'with' statement when executing code in
    multiple MPI processes so that if any of the blocks raise an
    exception, all processes sharing that communicator will fail.
    """
    def __enter__(self):
        pass
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_val is not None:
            fail = True
            traceback.print_exception(exc_type, exc_val, exc_tb)
        else:
            fail = False
        
        fails = MPI.COMM_WORLD.allgather(fail)
        
        if fail or not any(fails):
            return False  # exception will be re-raised for us
        else:
            for i,f in enumerate(fails):
                if f:
                    raise RuntimeError("a test failed in (at least) rank %d" % i)

def mpi_fail_if_any(f):
    """In order to keep MPI tests from hanging when
    a test fails in one process and succeeds in 
    another, perform an allgather to check for any
    failures in any of the processes running the test,
    and fail if any of them fail.
    """
    def wrapper(self, *args, **kwargs):
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

    return wrapper
    
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
    """A metaclass to wrap all of the test_ methods to act appropriately
    when run serially vs. under mpi.  Also creates collective versions of
    all of the 'assert*' methods used for testing.
    """
    def __new__(meta, name, bases, dct):
        newdict = {}
        for n, obj in dct.items():
            if n.startswith('test_') and isinstance(obj, types.FunctionType):
                newdict[n] = wrapper(obj)
            else:
                newdict[n] = obj
                    
        return type.__new__(meta, name, bases, newdict)

    def __init__(cls, name, bases, dct):
        super(MPITestCaseMeta, cls).__init__(name, bases, dct)
        parent = getmro(cls)[1]
        for n, obj in parent.__dict__.items():
            if n.startswith('assert') or n == 'fail':
                setattr(cls, 'collective_'+n, mpi_fail_if_any(obj))


class MPITestCase(TestCase):
    """A base class for all TestCases that are
    intended to run under MPI.
    """
    __metaclass__ = MPITestCaseMeta
    
    # A class attribute 'N_PROCS' must be defined
    # for each MPITestCase class in order to 
    # know how big to make the MPI communicator.
    # N_PROCS = 4

    def run(self, result=None):
        info = {
            'failures': [],
            'errors': [],
            'skipped': [],
        }
        self.infos = []
        junk = 0

        if result is None:
            result = self.defaultTestResult()
            startTestRun = getattr(result, 'startTestRun', None)
            if startTestRun is not None:
                startTestRun()

        try:
            exc_info = None
            if under_mpirun():
                self.comm = MPI.Comm.Get_parent()

                try:
                    super(MPITestCase, self).run(result)
                except Exception:
                    exc_info = sys.exc_info()

                for key in info.keys():
                    for tcase, data in getattr(result, key):
                        info[key].append(data)

                # send results back to the mothership
                self.comm.gather(info, root=0)
                
                self.comm.gather(junk, root=0)

                if exc_info is not None:
                    raise exc_info[0], exc_info[1], exc_info[2]
                    
            else:
                testpath = '.'.join((self.__class__.__module__, 
                                     self.__class__.__name__,
                                     self._testMethodName))

                self.comm = MPI.COMM_SELF.Spawn(sys.executable, 
                                    args=['-m', 'openmdao.test.mpiunittest', 
                                          testpath], 
                                    maxprocs=self.N_PROCS)

                # gather results from spawned MPI processes
                self.infos = self.comm.gather(info, root=MPI.ROOT)

                try:
                    super(MPITestCase, self).run(result)
                except Exception:
                    exc_info = sys.exc_info()

                self.junks = self.comm.gather(junk, root=MPI.ROOT)
                
                if exc_info is not None:
                    raise exc_info[0], exc_info[1], exc_info[2]
                
        finally:
            sys.stdout.flush()
            sys.stderr.flush()
            self.comm.Disconnect()


if __name__ == '__main__':
    if under_mpirun():
        args = sys.argv[1:]
        testpath = args[0]

        parts = testpath.split('.')

        # FIXME: LAME HACK, but it'll do for now
        tdir = os.path.dirname(os.path.abspath(__file__))
        # up 4 levels to the top of the repo
        for i in range(4):
            tdir = os.path.dirname(tdir)
        tdir = os.path.join(tdir, 'contrib')
        if tdir not in sys.path:
            sys.path.append(tdir)

        try:
            method = parts[-1]
            testcase_classname = parts[-2]
            modname = '.'.join(parts[:-2])

            __import__(modname)
            mod = sys.modules[modname]

            # Find the TestCase derived class in the 
            # specified module and create an instance
            tcase = getattr(mod, testcase_classname)(method)

            result = tcase.defaultTestResult()

        except Exception:
            exc_info = sys.exc_info()
            sys.stdout.flush()
            sys.stderr.flush()
            if under_mpirun():
                MPI.Comm.Get_parent().Disconnect()
            raise exc_info[0], exc_info[1], exc_info[2]

        # run the test case, which will report its
        # results back to the process that spawned this
        # MPI process.
        try:
            tcase.run(result)
        except Exception:
            exc_info = sys.exc_info()
            sys.stdout.flush()
            sys.stderr.flush()
            if under_mpirun():
                MPI.Comm.Get_parent().Disconnect()
            raise exc_info[0], exc_info[1], exc_info[2]
