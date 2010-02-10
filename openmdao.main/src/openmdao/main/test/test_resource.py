"""
Test resources.
"""

import logging
import os
import platform
import sys
import unittest

from openmdao.main.resource import ClusterAllocator
from openmdao.util.testutil import find_python

# Users who have ssh configured correctly for testing.
SSH_USERS = ('setowns1',)


class TestCase(unittest.TestCase):
    """ Test resources. """

    def setUp(self):
        self.user = os.environ['USER']
        self.node = platform.node()
        self.name = self.node.replace('.', '_')
        self.python = find_python()
        self.cluster = None

        self.machines = []
        if self.node == 'gxterm3':
            # User environment assumed OK on this GRC cluster front-end.
            for i in range(55):
                self.machines.append({'hostname':'gx%02d' % i,
                                      'python':self.python})
        else:
            self.machines.append({'hostname':self.node,
                                  'python':self.python})

    def tearDown(self):
#        if self.cluster is not None:
#            self.cluster.shutdown()
        pass

    def test_normal(self):
        logging.debug('')
        logging.debug('test_normal')

        if sys.platform == 'win32' or self.user not in SSH_USERS:
            return

        self.cluster = ClusterAllocator(self.name, self.machines)
        self.assertEqual(len(self.cluster), len(self.machines))

    def test_bad_host(self):
        logging.debug('')
        logging.debug('test_bad_host')

        if sys.platform == 'win32' or self.user not in SSH_USERS:
            return

        self.machines.append({'hostname':'xyzzy',
                              'python':self.python})
        self.cluster = ClusterAllocator(self.name, self.machines)
        self.assertEqual(len(self.cluster), len(self.machines)-1)

    def zest_bad_python(self):
        logging.debug('')
        logging.debug('test_bad_python')

        if sys.platform == 'win32' or self.user not in SSH_USERS:
            return

        self.machines.append({'hostname':self.node,
                              'python':'no-such-python'})
        self.cluster = ClusterAllocator(self.name, self.machines)
        self.assertEqual(len(self.cluster), len(self.machines))


if __name__ == "__main__":
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

