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
import socket
import sys
import tempfile
import unittest

from openmdao.main.mp_util import read_server_config
from openmdao.main.objserverfactory import connect, start_server
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.main.resource import ResourceAllocator, LocalAllocator, \
                                   ClusterAllocator
from openmdao.util.testutil import assert_raises, find_python

# Users who have ssh configured correctly for testing.
SSH_USERS = []


class TestCase(unittest.TestCase):
    """ Test resource allocation. """

    def setUp(self):
        # Save existing RAM instance and force a rebuild.
        self.orig_ram = RAM._RAM
        RAM._RAM = None
        RAM.configure('')

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
        for allocator in RAM.list_allocators():
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

        # Restore RAM.
        RAM._RAM = self.orig_ram

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
            self.local.max_servers({'python_version':sys.version[:3],
                                    'n_cpus':1})
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

        hostnames = RAM.get_hostnames({'n_cpus':1})
        self.assertEqual(hostnames[0], platform.node())
        
        hostnames = RAM.get_hostnames({'no_such_resource':1})
        self.assertEqual(hostnames, None)
        
    def test_resources(self):
        logging.debug('')
        logging.debug('test_resources')

        result = RAM.allocate({'localhost':False})
        self.assertEqual(result, (None, None))

        result = RAM.allocate({'exclude':[platform.node()]})
        self.assertEqual(result, (None, None))

        result = RAM.allocate({'n_cpus':1000000})
        self.assertEqual(result, (None, None))

        result = RAM.allocate({'orphan_modules':['xyzzy']})
        self.assertEqual(result, (None, None))

        result = RAM.allocate({'python_version':'xyzzy'})
        self.assertEqual(result, (None, None))

        result = RAM.allocate({'xyzzy':None})
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

    def test_remote(self):
        logging.debug('')
        logging.debug('test_remote')

        # Start remote server.
        server_dir = 'Factory'
        if os.path.exists(server_dir):
            shutil.rmtree(server_dir)
        os.mkdir(server_dir)
        os.chdir(server_dir)
        try:
            server, server_cfg = start_server()
            cfg = read_server_config(server_cfg)
            factory = None
            try:
                factory = connect(cfg['address'], cfg['port'],
                                  pubkey=cfg['key'])
                prefix = RAM._make_prefix(factory.host)
                remote = '%s_LocalHost' % prefix

                # Show no remotes currently in RAM.
                allocator_names = \
                    [allocator.name for allocator in RAM.list_allocators()]
                logging.debug('%s', allocator_names)
                self.assertFalse(remote in allocator_names)

                # Add remote server's allocator.
                RAM.add_remotes(factory)
                allocator_names = \
                    [allocator.name for allocator in RAM.list_allocators()]
                logging.debug('%s', allocator_names)
                self.assertTrue(remote in allocator_names)
                self.assertFalse(RAM.get_allocator(remote) is RAM.list_allocators()[0])
                self.assertTrue(RAM.get_allocator(remote) is RAM.list_allocators()[1])

                # Max servers.
                max_servers = RAM.max_servers(dict(allocator=remote))
                self.assertTrue(max_servers >= 0)  # Avoid host load issues.

                remote_alloc = RAM.get_allocator(remote)

                max_servers, info = \
                    remote_alloc.max_servers(dict(localhost=True))
                self.assertEqual(max_servers, 0)
                self.assertEqual(info, dict(localhost='requested local host'))
                
                max_servers, info = \
                    remote_alloc.max_servers(dict(allocator='LocalHost'))
                self.assertEqual(max_servers, 0)
                self.assertEqual(info, dict(allocator='wrong allocator'))
                
                estimate, info = \
                    remote_alloc.time_estimate(dict(allocator='LocalHost'))
                self.assertEqual(estimate, -2)
                self.assertEqual(info, dict(allocator='wrong allocator'))
                
                # Allocate, release.
                remote_server, info = RAM.allocate(dict(allocator=remote))
                RAM.release(remote_server)

                # Remove remote allocators.
                allocator_names = \
                    [allocator.name for allocator in RAM.list_allocators()]
                for name in allocator_names:
                    if name.startswith(prefix):
                        RAM.remove_allocator(name)
                allocator_names = \
                    [allocator.name for allocator in RAM.list_allocators()]
                logging.debug('%s', allocator_names)
                self.assertFalse(remote in allocator_names)

            finally:
                if factory is not None:
                    factory.cleanup()
                server.terminate(timeout=10)
        finally:
            os.chdir('..')
            shutil.rmtree(server_dir)

        # Access local RAM in manner it would be accessed in the server.
        self.assertEqual(RAM._get_instance().get_total_allocators(), 1)
        self.assertTrue(RAM._get_instance().get_allocator_proxy(0) \
                        is RAM.list_allocators()[0])

    def test_configure(self):
        logging.debug('')
        logging.debug('test_configure')

        # Reconfigure.
        with open('resources.cfg', 'w') as out:
            out.write("""
[LocalHost]
max_load: 100
""")
        local = RAM.get_allocator('LocalHost')
        max_load = local.max_load
        try:
            self.assertTrue(max_load < 100)
            RAM.configure('resources.cfg')
            self.assertEqual(local.max_load, 100)
            local.max_load = max_load
        finally:
            os.remove('resources.cfg')

        # Add another local.
        with open('resources.cfg', 'w') as out:
            out.write("""
[Local2]
classname: openmdao.main.resource.LocalAllocator
authkey: PublicKey
allow_shell: False
total_cpus: 42
max_load: 200
""")
        try:
            RAM.configure('resources.cfg')
            local2 = RAM.get_allocator('Local2')
            self.assertEqual(local2.factory._authkey, 'PublicKey')
            self.assertEqual(local2.factory._allow_shell, False)
            self.assertEqual(local2.total_cpus, 42)
            self.assertEqual(local2.max_load, 200)
            self.assertEqual(local2.host, socket.gethostname())
            self.assertTrue(local2.pid > 0)
            RAM.remove_allocator('Local2')
        finally:
            os.remove('resources.cfg')

        # Bad local total_cpus.
        with open('resources.cfg', 'w') as out:
            out.write("""
[Local2]
classname: openmdao.main.resource.LocalAllocator
total_cpus: 0
""")
        try:
            assert_raises(self, "RAM.configure('resources.cfg')",
                          globals(), locals(), ValueError,
                          'Local2: total_cpus must be > 0, got 0')
        finally:
            os.remove('resources.cfg')

        # Bad local max_load.
        with open('resources.cfg', 'w') as out:
            out.write("""
[Local2]
classname: openmdao.main.resource.LocalAllocator
max_load: 0
""")
        try:
            assert_raises(self, "RAM.configure('resources.cfg')",
                          globals(), locals(), ValueError,
                          'Local2: max_load must be > 0, got 0')
        finally:
            os.remove('resources.cfg')

        # Bad module.
        with open('resources.cfg', 'w') as out:
            out.write("""
[BadModule]
classname: no-such-module.Allocator
max_load: 100
""")
        try:
            assert_raises(self, "RAM.configure('resources.cfg')",
                          globals(), locals(), RuntimeError,
                          "RAM configure BadModule: can't import"
                          " 'no-such-module'")
        finally:
            os.remove('resources.cfg')

        # Bad class.
        with open('resources.cfg', 'w') as out:
            out.write("""
[BadClass]
classname: openmdao.main.resource.NoSuchAllocator
max_load: 100
""")
        try:
            assert_raises(self, "RAM.configure('resources.cfg')",
                          globals(), locals(), RuntimeError,
                          "RAM configure BadClass: no class"
                          " 'NoSuchAllocator' in openmdao.main.resource")
        finally:
            os.remove('resources.cfg')

        # Add, insert, get, remove.
        local3 = LocalAllocator('Local3')
        local4 = LocalAllocator('Local4')
        RAM.add_allocator(local3)
        try:
            allocator_names = \
                [allocator.name for allocator in RAM.list_allocators()]
            self.assertEqual(allocator_names, ['LocalHost', 'Local3'])
            self.assertTrue(RAM.get_allocator('Local3') is local3)
            self.assertTrue(RAM.get_allocator(1) is local3)
            RAM.insert_allocator(0, local4)
            try:
                allocator_names = \
                    [allocator.name for allocator in RAM.list_allocators()]
                self.assertEqual(allocator_names,
                                 ['Local4', 'LocalHost', 'Local3'])
            finally:
                RAM.remove_allocator('Local4')
        finally:
            RAM.remove_allocator(1)

        assert_raises(self, "RAM.get_allocator('Local3')",
                      globals(), locals(), ValueError,
                      "allocator 'Local3' not found")

        assert_raises(self, "RAM.remove_allocator('Local3')",
                      globals(), locals(), ValueError,
                      "allocator 'Local3' not found")

    def test_base(self):
        logging.debug('')
        logging.debug('test_base')

        assert_raises(self, "ResourceAllocator('invalid-name')",
                      globals(), locals(), NameError,
                      "name 'invalid-name' is not alphanumeric")

        allocator = ResourceAllocator('dummy')
        self.assertEqual(allocator.name, 'dummy')
        allocator.invalidate()
        allocator.configure('')

        assert_raises(self, "allocator.max_servers(dict())",
                      globals(), locals(), NotImplementedError, 'max_servers')

        assert_raises(self, "allocator.time_estimate(dict())",
                      globals(), locals(), NotImplementedError, 'time_estimate')

        assert_raises(self, "allocator.deploy('xyzzy', dict(), dict())",
                      globals(), locals(), NotImplementedError, 'deploy')

        assert_raises(self, "allocator.release(None)",
                      globals(), locals(), NotImplementedError, 'release')


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.main')
    sys.argv.append('--cover-erase')
    nose.runmodule()

