import ConfigParser
import datetime
import glob
import logging
import nose
import os.path
import pkg_resources
import shutil
import sys
import tempfile
import unittest

from openmdao.main.api import SimulationRoot
from openmdao.main.resource import HOME_DIRECTORY, WORKING_DIRECTORY
from openmdao.main.grid_engine import GridEngineAllocator, GridEngineServer
from openmdao.main.mp_support import is_instance
from openmdao.util.testutil import assert_raises
from openmdao.util.fileutil import onerror


class TestCase(unittest.TestCase):

    directory = os.path.realpath(
        pkg_resources.resource_filename('openmdao.main', 'test'))

    def setUp(self):
        self.tempdir = tempfile.mkdtemp(prefix='test_ge-')
        self.startdir = os.getcwd()
        os.chdir(self.tempdir)
        SimulationRoot.chroot(self.tempdir)

        # Force use of fake 'qsub'.
        self.orig_qsub = list(GridEngineServer._QSUB)
        GridEngineServer._QSUB[:] = \
            ['python', os.path.join(TestCase.directory, 'ge_qsub.py')]

        # Force use of fake 'qhost'.
        self.orig_qhost = list(GridEngineAllocator._QHOST)
        GridEngineAllocator._QHOST[:] = \
            ['python', os.path.join(TestCase.directory, 'ge_qhost.py')]

    def tearDown(self):
        GridEngineServer._QSUB[:] = self.orig_qsub
        GridEngineAllocator._QHOST[:] = self.orig_qhost
        os.chdir(self.startdir)
        SimulationRoot.chroot(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

    def test_allocator(self):
        logging.debug('')
        logging.debug('test_allocator')

        allocator = GridEngineAllocator()
        cfg = ConfigParser.ConfigParser()
        cfg.add_section('GridEngine')
        cfg.set('GridEngine', 'MPICH2', 'mpich')
        cfg.set('GridEngine', 'OpenMPI', 'ompi')
        allocator.configure(cfg)

        # Normal, successful allocation.
        nhosts, criteria = allocator.max_servers({})
        self.assertEqual(nhosts, 19*48)  # From canned qhost output.
        estimate, criteria = allocator.time_estimate({})
        self.assertEqual(estimate, 0)

        nhosts, criteria = allocator.max_servers({'min_cpus': 2})
        self.assertEqual(nhosts, 19*48/2)  # From canned qhost output.
        estimate, criteria = allocator.time_estimate({'min_cpus': 2})
        self.assertEqual(estimate, 0)

        # Unused deployment.
        server = allocator.deploy('GridEngineTestServer', {}, {})
        self.assertTrue(is_instance(server, GridEngineServer))
        allocator.release(server)

        # Too many CPUs.
        nhosts, criteria = allocator.max_servers({'min_cpus': 1000})
        self.assertEqual(nhosts, 0)
        estimate, criteria = allocator.time_estimate({'min_cpus': 1000})
        self.assertEqual(estimate, -2)

        # Not remote.
        nhosts, criteria = allocator.max_servers({'localhost': True})
        self.assertEqual(nhosts, 0)
        estimate, criteria = allocator.time_estimate({'localhost': True})
        self.assertEqual(estimate, -2)

        # Configure bad pattern.
        cfg = ConfigParser.ConfigParser()
        cfg.add_section('GridEngine')
        cfg.set('GridEngine', 'pattern', 'xyzzy')
        allocator.configure(cfg)
        nhosts, criteria = allocator.max_servers({})
        self.assertEqual(nhosts, 0)
        estimate, criteria = allocator.time_estimate({})
        self.assertEqual(estimate, -2)

        # Incompatible Python version.
        estimate, criteria = allocator.time_estimate({'python_version': '9.9'})
        self.assertEqual(estimate, -2)

        # Unrecognized key.
        estimate, criteria = allocator.time_estimate({'no-such-key': 0})
        self.assertEqual(estimate, -2)

        # 'qhost' failure.
        GridEngineAllocator._QHOST[:] = [os.path.join('bogus-qhost')]
        cfg.set('GridEngine', 'pattern', '*')
        allocator.configure(cfg)
        nhosts, criteria = allocator.max_servers({})
        self.assertEqual(nhosts, 0)

    def test_server(self):
        logging.debug('')
        logging.debug('test_server')

        with open('echo.in', 'w') as out:
            pass

        server = GridEngineServer()
        server.configure(dict(MPI='ompi'))

        # Try various resources.
        start_time = datetime.datetime(2012, 2, 8, 16, 42)
        resource_limits = dict(cpu_time=1, wallclock_time=2)
        server.execute_command(dict(remote_command='echo',
                                    args=['hello', 'world'],
                                    submit_as_hold=True,
                                    rerunnable=True,
                                    job_environment={'ENV_VAR': 'env_value'},
                                    working_directory='.',
                                    job_category='MPI',
                                    min_cpus=256,
                                    max_cpus=512,
                                    email=['user1@host1', 'user2@host2'],
                                    email_on_started=True,
                                    email_on_terminated=True,
                                    job_name='TestJob',
                                    input_path='echo.in',
                                    output_path='echo.out',
                                    join_files=True,
                                    reservation_id='res-1234',
                                    queue_name='debug_q',
                                    priority=42,
                                    start_time=start_time,
                                    resource_limits=resource_limits,
                                    accounting_id='CFD-R-US',
                                    native_specification=('-ac', 'name=value')))

        with open('echo.out', 'r') as inp:
            lines = []
            linez = inp.readlines()
            for line in linez:
                qsl = line.replace('a', 'zzz')
                print qsl
                lines.append(qsl)
            print "LINES:", lines
            #lines = inp.readlines()
        self.assertEqual(lines, ['hello world\n'])

        with open('qsub.out', 'r') as inp:
            lines = inp.readlines()
        actual = ''.join(lines)
        expected = """\
-V -sync yes -b yes -cwd -h -r yes -M user1@host1,user2@host2 -N TestJob -i echo.in -o echo.out -j yes -ar res-1234 -q debug_q -p 42 -a 201202081642.00 -A CFD-R-US -m be -pe ompi 256-512 -l h_cpu=0:0:1 -l h_rt=0:0:2 -ac name=value echo hello world
-V
-sync arg yes
-b arg yes
-cwd
-h
-r arg yes
-M arg user1@host1,user2@host2
-N arg TestJob
-i stdin echo.in
-o stdout echo.out
-j join yes
-ar arg res-1234
-q arg debug_q
-p arg 42
-a arg 201202081642.00
-A arg CFD-R-US
-m arg be
-pe ompi 256-512
-l resource h_cpu=0:0:1
-l resource h_rt=0:0:2
-ac arg name=value
echo hello world
"""
        logging.debug('Actual output:')
        logging.debug(actual)
        logging.debug('Expected output:')
        logging.debug(expected)
        self.assertEqual(actual, expected)

        # error_path.
        server.execute_command(dict(remote_command='echo',
                                    args=['hello', 'world'],
                                    output_path='echo.out',
                                    error_path='echo.err'))

        # HOME_DIRECTORY, WORKING_DIRECTORY.
        home_dir = os.path.expanduser('~')
        work_dir = os.getcwd()
        server.execute_command(dict(remote_command='echo',
                                    args=[HOME_DIRECTORY+'hello',
                                          WORKING_DIRECTORY+'world'],
                                    working_directory=work_dir,
                                    output_path='echo.out'))
        with open('echo.out', 'r') as inp:
            lines = []
            linez = inp.readlines()
            for line in linez:
                #Some Windows echo commands quote args with spaces
                quote_stripped_line = line.replace('"', '')
                lines.append(quote_stripped_line)

        self.assertEqual(lines,
                         ['%s %s\n' % (os.path.join(home_dir, 'hello'),
                                       os.path.join(work_dir, 'world'))])
        # Bad category.
        code = "server.execute_command(dict(remote_command='echo'," \
                                      "job_category='no-such-category'))"
        assert_raises(self, code, globals(), locals(), ValueError,
                      "No mapping for job_category 'no-such-category'")

        # 'qsub' failure.
        GridEngineServer._QSUB[:] = [os.path.join('bogus-qsub')]
        code = "server.execute_command(dict(remote_command='echo'))"
        assert_raises(self, code, globals(), locals(), OSError, '')


if __name__ == '__main__':
    sys.argv.append('--cover-package=grid_engine.')
    sys.argv.append('--cover-erase')
    nose.runmodule()
