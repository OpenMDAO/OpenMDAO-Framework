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

from nas_access import protocol, NAS_Allocator

_TST_ROOT = \
    os.path.realpath(pkg_resources.resource_filename('nas_access', 'test'))
_DMZ_ROOT = 'Fake_DMZ'
_RJE_ROOT = 'RJE'


class TestCase(unittest.TestCase):

    def setUp(self):
        """ Force use of fake 'ssh' and 'scp'. """
        ssh = ('python', os.path.join(_TST_ROOT, 'ssh.py'), _DMZ_ROOT)
        scp = ('python', os.path.join(_TST_ROOT, 'scp.py'), _DMZ_ROOT)

        self.orig_ssh = protocol.configure_ssh(ssh)
        self.orig_scp = protocol.configure_scp(scp)

        # Avoid lots of polling log entries.
        if logging.getLogger().getEffectiveLevel() < logging.DEBUG:
            logging.getLogger().setLevel(logging.DEBUG)

    def tearDown(self):
        """ Restore 'ssh' and 'scp' configuration. """
        protocol.configure_ssh(self.orig_ssh)
        protocol.configure_scp(self.orig_scp)

        for name in ('echo.out', 'junk.dat'):
            if os.path.exists(name):
                os.remove(name)

        time.sleep(2)
        for name in (_RJE_ROOT, _DMZ_ROOT):
            if os.path.exists(name):
                shutil.rmtree(name)

    def test_nas_access(self):
        logging.debug('')
        logging.debug('test_nas_access')

        # Start RJE server.
        hostname = socket.gethostname()
        proc = start_server(hostname)

        try:
            # Add NAS_Allocator referring to server.
            logging.debug('create allocator')
            allocator = NAS_Allocator(dmz_host=hostname, server_host=hostname)
            RAM.add_allocator(allocator)
            try:
                # Run a fake job in style of ExternalCode component.
                logging.debug('allocate server')
                server, server_info = RAM.allocate(dict(allocator=allocator.name))
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

            finally:
                logging.debug('remove')
                allocator = RAM.remove_allocator(allocator.name)

                logging.debug('shutdown')
                allocator.shutdown()

        finally:
            proc.terminate()

        self.assertTrue(os.path.exists('echo.out'))
        with open('echo.out', 'rU') as out:
            data = out.read()
        self.assertEqual(data, 'Hello World!\n')

        self.assertTrue(os.path.exists('junk.dat'))
        with open('junk.dat', 'rU') as out:
            data = out.read()
        self.assertEqual(data, 'just some junk')


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

