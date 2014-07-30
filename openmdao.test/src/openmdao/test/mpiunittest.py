"""
A simple unit testing framework for MPI programs.
"""

import os
import sys

from unittest import TestCase, SkipTest
from openmdao.util.fileutil import get_module_path

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

_key2method = {
    'failures': 'addFailure',
    'skipped': 'addSkip',
    'errors': 'addError',
    'expectedFailures': 'addExpectedFailure',
    'unexpectedSuccesses': 'addUnexpectedSuccess'
}

class MPITestCase(TestCase):
    """A base class for all TestCases that are
    intended to run under MPI.
    """
    # A class attribute 'NCPUS' must be defined
    # for each MPITestCase class in order to 
    # know how big to make the MPI communicator.
    # NCPUS = 4
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
        if _under_mpirun():
            if result is None:
                result = self.defaultTestResult()
                startTestRun = getattr(result, 'startTestRun', None)
                if startTestRun is not None:
                    startTestRun()

            try:
                super(MPITestCase, self).run(result)
            except Exception as err:
                print str(err)

            comm = MPI.Comm.Get_parent()

            info = {
                'failures': [],
                'errors': [],
                'skipped': [],
                'expectedFailures': [],
                'unexpectedSuccesses': [],
            }
            for key in info.keys():
                for tcase, msg in getattr(result, key):
                    info[key].append(msg)

            # send results back to the mothership
            comm.gather(info, root=0)
                
        else:
            testpath = '.'.join((self.__class__.__module__, 
                                 self.__class__.__name__,
                                 self._orig_testmethod_name))

            self.comm = MPI.COMM_SELF.Spawn(sys.executable, 
                                args=['-m', 'unittest', testpath], 
                                maxprocs=self.NCPUS)
            infos = []

            # gather results from spawned MPI processes
            self.comm.gather(infos, root=MPI.ROOT)

            comm.Disconnect()

            for info in infos:
                for k,v in info.items():
                    getattr(result, _key2method[k])(self, v)

            for i,info in enumerate(infos):
                for k,v in info.items():
                    print "%d: %s: %s" % (i,k,v)

        
