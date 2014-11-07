"""
A simple unit testing framework for MPI programs.
"""

import os
import sys

from unittest import TestCase, SkipTest
from openmdao.main.mpiwrap import mpiprint
from openmdao.util.testutil import assert_rel_error

try:
    from mpi4py import MPI
except ImportError:
    MPI = None


def _under_mpirun():
    """Return True if we're being executed under mpirun."""
    # TODO: this is a bit of a hack and there appears to be
    # no consistent set of environment vars between MPI 
    # implementations.
    for name in os.environ.keys():
        if name.startswith('OMPI_COMM') or name.startswith('MPICH_'):
            return True
    return False


## The following decorator doesn't work, and I'm not
## sure why.  It hangs at the allgather, but I verified
## that the allgather is being called in all processes
## in the communicator.
# def mpi_fail_if_any(f):
#     """In order to keep MPI tests from hanging when
#     a test fails in one process and succeeds in 
#     another, perform an allgather to check for any
#     failures in any of the processes running the test,
#     and fail if any of them fail.
#     """
#     def wrapper(self, *args, **kwargs):
#         try:
#             ret = f(self, *args, **kwargs)
#         except Exception as exc:
#             fail = [True]
#         else:
#             fail = [False] 

#         mpiprint('about to allgather. fail=%s'%fail)
#         mpiprint("size = %d" % self.comm.size)
#         try:
#             fails = self.comm.allgather(fail)
#         except:
#             print "except"
#         mpiprint("fails = %s" % fails)

#         if fail:
#             self.fail(str(exc))
#         elif any(fails):
#             self.fail("a test failed in another rank")
        
#         return ret

#     return wrapper


class MPITestCase(TestCase):
    """A base class for all TestCases that are
    intended to run under MPI.
    """
    # A class attribute 'N_PROCS' must be defined
    # for each MPITestCase class in order to 
    # know how big to make the MPI communicator.
    # N_PROCS = 4
    def __init__(self, methodName='runTest'):
        super(MPITestCase, self).__init__(methodName)

        # save the original test method so the wrapper
        # will know what to call
        self._orig_testmethod_name = self._testMethodName

        if not _under_mpirun():
            self._testMethodName = '_test_non_mpi_method_wrapper'
            
    def _test_non_mpi_method_wrapper(self):
        """A wrapper we put around every test method
        when we're run normally (non-MPI), so that we can
        then kick off N mpi processes running that test
        method.
        """
        if MPI is None:
            raise SkipTest("mpi4py not installed")

    def run(self, result=None):
        info = {
            'failures': [],
            'errors': [],
            'skipped': [],
        }

        if result is None:
            result = self.defaultTestResult()
            startTestRun = getattr(result, 'startTestRun', None)
            if startTestRun is not None:
                startTestRun()

        try:
            if _under_mpirun():
                self.comm = MPI.Comm.Get_parent()

                try:
                    super(MPITestCase, self).run(result)
                except Exception as err:
                    print str(err)

                for key in info.keys():
                    for tcase, data in getattr(result, key):
                        info[key].append(data)

                # send results back to the mothership
                self.comm.gather(info, root=0)
                    
            else:
                testpath = '.'.join((self.__class__.__module__, 
                                     self.__class__.__name__,
                                     self._orig_testmethod_name))

                self.comm = MPI.COMM_SELF.Spawn(sys.executable, 
                                    args=['-m', 'openmdao.test.mpiunittest', testpath], 
                                    maxprocs=self.N_PROCS)

                # gather results from spawned MPI processes
                infos = self.comm.gather(info, root=MPI.ROOT)

                try:
                    super(MPITestCase, self).run(result)
                except Exception as err:
                    print str(err)

                self._testMethodName = self._orig_testmethod_name

                # for each type of error or skip, loop over
                # the results from all of the MPI procs we
                # spawned and keep the unique ones.
                for key in info.keys():
                    rset = set()
                    for i,rmap in enumerate(infos):
                        val = rmap[key]
                        for v in val:
                            if v and v not in rset:
                                rset.add(v)
                                getattr(result, key).append((self, "{%d} %s" % (i, v)))
        finally:
            self.comm.Disconnect()

    # @mpi_fail_if_any
    # def mpi_assert_rel_error(self, actual, desired, tolerance):
    #     assert_rel_error(self, actual, desired, tolerance)

    # @mpi_fail_if_any
    # def mpiAssertEqual(self, val1, val2):
    #     self.assertEqual(val1, val2)

    # @mpi_fail_if_any
    # def mpiAssertTrue(self, arg):
    #     self.assertTrue(arg)

    # @mpi_fail_if_any
    # def mpiAsserFalse(self, arg):
    #     self.assertFalse(arg)



if __name__ == '__main__':
    if _under_mpirun():
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

        except Exception as err:
            print str(err)
            if _under_mpirun():
                MPI.Comm.Get_parent().Disconnect()

        # run the test case, which will report its
        # results back to the process that spawned this
        # MPI process.
        try:
            tcase.run(result)
        except Exception as err:
            if _under_mpirun():
                MPI.Comm.Get_parent().Disconnect()
            raise

    
