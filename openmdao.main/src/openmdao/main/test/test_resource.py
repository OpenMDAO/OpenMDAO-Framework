"""
Test resource allocation.
"""

import glob
import logging
import multiprocessing
import os
import platform
import shutil
import sys
import unittest

from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator
from openmdao.util.testutil import find_python

# Users who have ssh configured correctly for testing.
SSH_USERS = ('setowns1',)


class TestCase(unittest.TestCase):
    """ Test resources. """

    def setUp(self):
        try:
            self.user = os.environ['USER']
        except KeyError:
            self.user = None  # Probably Windows...
        self.node = platform.node()
        self.name = self.node.replace('.', '_')
        self.python = find_python()
        self.cluster = None

        if sys.platform == 'win32' or self.user not in SSH_USERS:
            self.skip_ssh = True
        else:
            self.skip_ssh = False

        self.machines = []
        if self.node.startswith('gxterm'):
            # User environment assumed OK on this GRC cluster front-end.
            for i in range(55):
                self.machines.append({'hostname':'gx%02d' % i,
                                      'python':self.python})
        else:
            self.machines.append({'hostname':self.node,
                                  'python':self.python})

    def tearDown(self):
# shutdown() currently causes problems (except at exit).
#        if self.cluster is not None:
#            self.cluster.shutdown()

        if self.skip_ssh or self.node.startswith('gxterm'):
            return

        # This cleanup *should* be OK, but it's not bulletproof.
        uid = os.getuid()
        for path in glob.glob('/tmp/distrib-*'):
            info = os.stat(path)
            if info.st_uid == uid:
                shutil.rmtree(path)

    def test_normal(self):
        logging.debug('')
        logging.debug('test_normal')

        if self.skip_ssh:
            return

        self.cluster = ClusterAllocator(self.name, self.machines)
        if self.node.startswith('gxterm'):
            # GX isn't particularly reliable for some reason.
            self.assertTrue(len(self.cluster) >= len(self.machines)*3/4)
        else:
            self.assertEqual(len(self.cluster), len(self.machines))

        n_servers = self.cluster.max_servers({'python_version':sys.version[:3]})
        try:
            n_cpus = multiprocessing.cpu_count()
        except AttributeError:
            n_cpus = 1
        if self.node.startswith('gxterm'):
            # GX front-end n_cpus doesn't imply node n_cpus.
            self.assertTrue(n_servers >= len(self.cluster))
        else:
            self.assertEqual(n_servers, len(self.cluster)*n_cpus)

        n_servers = self.cluster.max_servers({'python_version':'bad-version'})
        self.assertEqual(n_servers, 0)

    def test_hostnames(self):
        logging.debug('')
        logging.debug('test_hostnames')

        # Ensure we aren't held up by local host load problems.
        local = ResourceAllocationManager.get_allocator(0)
        local.max_load = 10

        hostnames = ResourceAllocationManager.get_hostnames({'n_cpus': 1})
        self.assertEqual(hostnames[0], platform.node())
        
    def test_resources(self):
        logging.debug('')
        logging.debug('test_resources')

        # Ensure we aren't held up by local host load problems.
        local = ResourceAllocationManager.get_allocator(0)
        local.max_load = 10

        result = ResourceAllocationManager.allocate({'localhost':False})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'n_cpus':1000000})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'orphan_modules':'xyzzy'})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'python_version':'xyzzy'})
        self.assertEqual(result, (None, None))

        result = ResourceAllocationManager.allocate({'xyzzy':None})
        self.assertEqual(result, (None, None))

    def test_bad_host(self):
        logging.debug('')
        logging.debug('test_bad_host')

        if self.skip_ssh:
            return

        self.machines.append({'hostname':'xyzzy',
                              'python':self.python})
        self.cluster = ClusterAllocator(self.name, self.machines)
        if self.node.startswith('gxterm'):
            # GX isn't particularly reliable for some reason.
            self.assertTrue(len(self.cluster) >= len(self.machines)*3/4)
        else:
            self.assertEqual(len(self.cluster), len(self.machines)-1)

    def test_bad_python(self):
        logging.debug('')
        logging.debug('test_bad_python')

        if self.skip_ssh:
            return

        self.machines = [{'hostname':self.node,
                          'python':'no-such-python'}]
        self.cluster = ClusterAllocator(self.name, self.machines)
        self.assertEqual(len(self.cluster), 0)


if __name__ == "__main__":
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

