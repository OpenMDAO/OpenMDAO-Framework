import ConfigParser
import logging
import nose
import os.path
import pkg_resources
import shutil
import socket
import subprocess
import sys
import time
import unittest

from openmdao.main.resource import ResourceAllocationManager as RAM

from openmdao.util.filexfer import filexfer, pack_zipfile, unpack_zipfile
from openmdao.util.testutil import assert_raises

from nas_access import protocol, NAS_Allocator

_TST_ROOT = \
    os.path.realpath(pkg_resources.resource_filename('nas_access', 'test'))
_DMZ_ROOT = 'Fake_DMZ'
_RJE_ROOT = 'RJE'


class TestCase(unittest.TestCase):

    def setUp(self):
        # Force use of fake 'ssh' and 'scp'.
        ssh = ('python', os.path.join(_TST_ROOT, 'ssh.py'), _DMZ_ROOT)
        scp = ('python', os.path.join(_TST_ROOT, 'scp.py'), _DMZ_ROOT)

        self.orig_ssh = protocol.configure_ssh(ssh)
        self.orig_scp = protocol.configure_scp(scp)

        # Avoid lots of polling log entries.
        if logging.getLogger().getEffectiveLevel() < logging.DEBUG:
            logging.getLogger().setLevel(logging.DEBUG)

        # Start RJE server.
        hostname = socket.gethostname()
        self.proc = start_server(hostname)

        # Create NAS_Allocator referring to server.
        logging.debug('create allocator')
        self.allocator = NAS_Allocator()
        parser = ConfigParser.ConfigParser()
        section = self.allocator.name
        parser.add_section(section)
        parser.set(section, 'dmz_host', hostname)
        parser.set(section, 'server_host', hostname)
        self.allocator.configure(parser)

        # Add allocator to RAM.
        RAM.add_allocator(self.allocator)

    def tearDown(self):
        logging.debug('remove')
        RAM.remove_allocator(self.allocator.name)

        logging.debug('shutdown')
        self.allocator.shutdown()

        self.proc.terminate()

        # Restore 'ssh' and 'scp' configuration.
        protocol.configure_ssh(self.orig_ssh)
        protocol.configure_scp(self.orig_scp)

        time.sleep(2)
        for name in (_RJE_ROOT, _DMZ_ROOT):
            if os.path.exists(name):
                shutil.rmtree(name)

    def test_allocator(self):
        logging.debug('')
        logging.debug('test_allocator')

        # Since we're faking it with a remote LocalHost, we should match.
        local_servers = RAM.max_servers(dict(allocator='LocalHost'))
        max_servers = RAM.max_servers(dict(allocator=self.allocator.name))
        self.assertEqual(max_servers, local_servers)

        max_servers = RAM.max_servers(dict(allocator=self.allocator.name,
                                           localhost=True)) # Contradictory!
        self.assertEqual(max_servers, 0)

        server = self.allocator.deploy('test_server', {}, {})
        try:
            self.assertEqual(server.name, 'NAS_Allocator/test_server')
            self.assertEqual(server.host, socket.gethostname())
            self.assertTrue(server.pid > 0)
            retval = server.echo(123, 'twisty', 'narrow', 'passages')
            self.assertEqual(retval, (123, 'twisty', 'narrow', 'passages'))
            self.assertTrue(server.isdir('.'))
            self.assertEqual(sorted(server.listdir('.')),
                             ['openmdao_log.txt', 'stderr', 'stdout'])
        finally:
            self.allocator.release(server)

    def test_extcode(self):
        logging.debug('')
        logging.debug('test_extcode')

        # Run a fake job in style of ExternalCode component.
        logging.debug('allocate server')
        server, server_info = RAM.allocate(dict(allocator=self.allocator.name))
        try:
            with open('junk.dat', 'w') as out:
                out.write('just some junk')
            filename = 'inputs.zip'

            logging.debug('pack inputs')
            pfiles, pbytes = pack_zipfile(('junk.dat',), filename,
                                          logging.getLogger())
            os.remove('junk.dat')

            logging.debug('transfer inputs')
            filexfer(None, filename, server, filename, 'b')

            logging.debug('unpack inputs')
            ufiles, ubytes = server.unpack_zipfile(filename)

            logging.debug('remove inputs')
            os.remove(filename)
            server.remove(filename)

            logging.debug('execute command')
            return_code, error_msg = \
                server.execute_command(dict(job_name='Testing',
                                            remote_command='echo',
                                            args=('Hello', 'World!'),
                                            output_path='echo.out'))

            logging.debug('pack outputs')
            filename = 'outputs.zip'
            pfiles, pbytes = server.pack_zipfile(('echo.out', 'junk.dat'),
                                                 filename)
            logging.debug('transfer outputs')
            filexfer(server, filename, None, filename, 'b')

            logging.debug('unpack outputs')
            ufiles, ubytes = unpack_zipfile(filename)

            logging.debug('remove outputs')
            os.remove(filename)
            server.remove(filename)

        finally:
            logging.debug('release')
            RAM.release(server)

        self.assertEqual(return_code, 0)
        self.assertEqual(error_msg, '')

        self.assertTrue(os.path.exists('echo.out'))
        with open('echo.out', 'rU') as out:
            data = out.read()
        os.remove('echo.out')
        self.assertEqual(data, 'Hello World!\n')

        self.assertTrue(os.path.exists('junk.dat'))
        with open('junk.dat', 'rU') as out:
            data = out.read()
        os.remove('junk.dat')
        self.assertEqual(data, 'just some junk')

    def test_errors(self):
        logging.debug('')
        logging.debug('test_errors')

        logging.debug('allocate server')
        server, server_info = RAM.allocate(dict(allocator=self.allocator.name))
        try:
            logging.debug('execute bad command')
            return_code, error_msg = \
                server.execute_command(dict(remote_command='no-such-command'))
            if sys.platform == 'win32':
                self.assertEqual(return_code, 1)
                self.assertEqual(error_msg, ': Operation not permitted')
            else:
                self.assertEqual(return_code, 127)
                self.assertEqual(error_msg, ': Key has expired')

            logging.debug('open bad file')
            assert_raises(self, "server.open('../../illegal-access', 'r')",
                          globals(), locals(), protocol.RemoteError,
                          "Can't open '../../illegal-access', not within root")

            logging.debug('open missing file')
            assert_raises(self, "server.open('no-such-file', 'r')",
                          globals(), locals(), protocol.RemoteError,
                          "[Errno 2] No such file or directory: 'no-such-file'")
        finally:
            logging.debug('release')
            RAM.release(server)

        logging.debug('no server')
        # Because we're faking remote access, the DMZ host is irrelevant.
        code = "NAS_Allocator(dmz_host='dmz', server_host='no-such-server')"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Server directory 'RJE-no-such-server' not found")


def start_server(hostname):
    """ Start RJE server with DMZ host `hostname` using 'LocalHost'. """
    for name in (_DMZ_ROOT, _RJE_ROOT):
        if os.path.exists(name):
            shutil.rmtree(name)
        os.mkdir(name)

    # Configure ssh to use special test code and DMZ root.
    ssh = ('python', os.path.join(_TST_ROOT, 'ssh.py'),
                     os.path.join('..', _DMZ_ROOT))

    # Configure scp to use special test code and DMZ root, and special RJE root.
    scp = ('python', os.path.join(_TST_ROOT, 'scp.py'),
                     os.path.join('..', _DMZ_ROOT), '--rje')

    orig_dir = os.getcwd()
    os.chdir(_RJE_ROOT)
    try:
        root = protocol._server_root()
        args = ('python', '-m', 'nas_access.rje',
                '--allocator', 'LocalHost',
                '--dmz-host', hostname,
                '--poll-delay', '1',
                '--ssh', ' '.join(ssh),
                '--scp', ' '.join(scp))
        out = open('rje.stdout', 'w')
        proc = subprocess.Popen(args, stdout=out, stderr=subprocess.STDOUT)
    finally:
        os.chdir(orig_dir)

    root = os.path.join(_DMZ_ROOT, root)
    for retry in range(20):
        time.sleep(0.5)
        if os.path.exists(root):
            return proc
    raise RuntimeError('server startup timeout')


if __name__ == '__main__':
    sys.argv.append('--cover-package=nas_access.')
    sys.argv.append('--cover-erase')
    nose.runmodule()

