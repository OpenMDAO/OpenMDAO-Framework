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

from nas_access import protocol, NAS_Allocator


class TestCase(unittest.TestCase):

    directory = \
        os.path.realpath(pkg_resources.resource_filename('nas_access', 'test'))

    def setUp(self):
        """ Force use of fake 'ssh' and 'scp'. """
        self.orig_ssh = protocol._SSH
        protocol.configure_ssh(
            ('python', os.path.join(TestCase.directory, 'ssh.py')))

        self.orig_scp = protocol._SCP 
        protocol.configure_scp(
            ('python', os.path.join(TestCase.directory, 'scp.py')))

    def tearDown(self):
        """ Restore 'ssh' and 'scp' configuration. """
        protocol.configure_ssh(self.orig_ssh)
        protocol.configure_scp(self.orig_scp)

    def test_nas_access(self):
        logging.debug('')
        logging.debug('test_nas_access')

        # Start RJE server.
        hostname = socket.gethostname()
        proc = start_server(hostname)

        try:
            # Add NAS_Allocator referring to server.
            allocator = NAS_Allocator(dmz_host=hostname, server_host=hostname)
            RAM.add_allocator(allocator)
            try:
                # Run a fake job.
                server = RAM.allocate(dict(allocator=allocator.name))
                logging.debug('testing...')
                time.sleep(10)
                logging.debug('    done')
                RAM.release(server)
            finally:
                RAM.remove_allocator(allocator.name)
        finally:
            # Stop server.
            stop_server(proc)


def start_server(hostname):
    """ Start RJE server with DMZ host `hostname` using 'LocalHost'. """
    if os.path.exists('RJE'):
        shutil.rmtree('RJE')
    os.mkdir('RJE')
    orig_dir = os.getcwd()
    os.chdir('RJE')
    try:
        root = protocol.server_root()
        ssh = ' '.join(protocol._SSH)
        scp = ' '.join(protocol._SCP) + ' --rje'
        args = ('python', '-m', 'nas_access.rje',
                '--allocator', 'LocalHost',
                '--dmz-host', hostname,
                '--poll-delay', '1',
                '--ssh', ssh,
                '--scp', scp)

        out = open('rje.stdout', 'w')
        proc = subprocess.Popen(args, stdout=out, stderr=subprocess.STDOUT)
    finally:
        os.chdir(orig_dir)

    root = os.path.join('Fake-DMZ', root)
    for retry in range(20):
        time.sleep(0.5)
        if os.path.exists(root):
            return proc
    raise RuntimeError('server startup timeout')

def stop_server(proc):
    """ Stop RJE server `proc`. """
    proc.terminate()
#    if os.path.exists('RJE'):
#        shutil.rmtree('RJE')


if __name__ == '__main__':
#    sys.argv.append('--cover-package=nas_access.')
#    sys.argv.append('--cover-erase')
#    nose.runmodule()

    logging.getLogger().setLevel(logging.DEBUG)

    protocol.configure_ssh(
        ('python', os.path.join(TestCase.directory, 'ssh.py')))

    protocol.configure_scp(
        ('python', os.path.join(TestCase.directory, 'scp.py')))

    logging.debug('')
    logging.debug('test_nas_access')

    # Start RJE server.
    hostname = socket.gethostname()
    proc = start_server(hostname)

    try:
        # Add NAS_Allocator referring to server.
        allocator = NAS_Allocator(dmz_host=hostname, server_host=hostname)
        RAM.add_allocator(allocator)
        try:
            # Run a fake job.
            server = RAM.allocate(dict(allocator=allocator.name))
            logging.debug('testing...')
            time.sleep(10)
            logging.debug('    done')
            RAM.release(server)
        finally:
            RAM.remove_allocator(allocator.name)
    finally:
        # Stop server.
        stop_server(proc)

