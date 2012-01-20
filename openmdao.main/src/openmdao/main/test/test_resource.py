"""
Test resource allocation.
"""

import getpass
import glob
import logging
import multiprocessing
import nose
import os.path
import platform
import shutil
import sys
import tempfile
import unittest

from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator
from openmdao.util.testutil import find_python

# Users who have ssh configured correctly for testing.
SSH_USERS = []


class TestCase(unittest.TestCase):
    """ Test resource allocation. """

    def setUp(self):
        self.user = getpass.getuser()
        self.node = platform.node()
        self.name = self.node.replace('.', '_')
        self.python = find_python()
        self.cluster = None

        if sys.platform == 'win32' or self.user not in SSH_USERS:
            self.skip_ssh = True
        else:
            self.skip_ssh = False

        self.machines = []
        self.machines.append({'hostname':self.node,
                              'python':self.python})

        # Ensure we aren't held up by local host load problems.
        for allocator in ResourceAllocationManager.list_allocators():
            if allocator.name == 'LocalHost':
                self.local = allocator
                self.local.max_load = 10
                break
        else:
            raise RuntimeError('No LocalHost allocator!?')

    def tearDown(self):
# shutdown() currently causes problems (except at exit).
#        if self.cluster is not None:
#            self.cluster.shutdown()

        if self.skip_ssh:
            return

        # This cleanup *should* be OK, but it's not bulletproof.
        uid = os.getuid()
        tempdir = tempfile.gettempdir()
        for path in glob.glob(os.path.join(tempdir, 'omdao-*')):
            info = os.stat(path)
            if info.st_uid == uid:
                shutil.rmtree(path)

    def test_cluster(self):
        logging.debug('')
        logging.debug('test_cluster')

        if self.skip_ssh:
            logging.debug('    requires ssh, skipping')
            return

        self.cluster = ClusterAllocator(self.name, self.machines)
        self.assertEqual(len(self.cluster), len(self.machines))

        n_servers, criteria = \
            self.cluster.max_servers({'python_version':sys.version[:3]})
        try:
            n_cpus = multiprocessing.cpu_count()
        except (AttributeError, NotImplementedError):  # pragma no cover
            n_cpus = 1
        self.assertEqual(n_servers, len(self.cluster)*n_cpus)

        n_servers, criteria = \
            self.cluster.max_servers({'python_version':'bad-version'})
        self.assertEqual(n_servers, 0)

    def test_max_servers(self):
        logging.debug('')
        logging.debug('test_max_servers')

        n_servers, criteria = \
            self.local.max_servers({'python_version':sys.version[:3]})
        try:
            n_cpus = multiprocessing.cpu_count()
        except (AttributeError, NotImplementedError):
            n_cpus = 1
        self.assertEqual(n_servers, self.local.max_load * n_cpus)

        n_servers, criteria = \
            self.local.max_servers({'python_version':'bad-version'})
        self.assertEqual(n_servers, 0)

    def test_hostnames(self):
        logging.debug('')
        logging.debug('test_hostnames')

        hostnames = ResourceAllocationManager.get_hostnames({'n_cpus':1})
        self.assertEqual(hostnames[0], platform.node())
        
        hostnames = ResourceAllocationManager.get_hostnames({'no_such_resource':1})
        self.assertEqual(hostnames, None)
        
    def test_resources(self):
        logging.debug('')
        logging.debug('test_resources')

        result = ResourceAllocationManager.allocate({'localhost':False})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'exclude':[platform.node()]})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'n_cpus':1000000})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'orphan_modules':['xyzzy']})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'python_version':'xyzzy'})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'xyzzy':None})
        self.assertEqual(result, (None, None))

    def test_bad_host(self):
        logging.debug('')
        logging.debug('test_bad_host')

        if self.skip_ssh:
            logging.debug('    requires ssh, skipping')
            return

        self.machines.append({'hostname':'xyzzy', 'python':self.python})
        self.cluster = ClusterAllocator(self.name, self.machines)
        self.assertEqual(len(self.cluster), len(self.machines)-1)

    def test_bad_python(self):
        logging.debug('')
        logging.debug('test_bad_python')

        if self.skip_ssh:
            logging.debug('    requires ssh, skipping')
            return

        self.machines = [{'hostname':self.node, 'python':'no-such-python'}]
        self.cluster = ClusterAllocator(self.name, self.machines)
        self.assertEqual(len(self.cluster), 0)


if __name__ == '__main__':
    # Avoid any user-defined resources from causing issues.
    ResourceAllocationManager.configure('')
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()

