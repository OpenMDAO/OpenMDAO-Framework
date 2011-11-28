import ConfigParser
import logging
import nose
import os.path
import pkg_resources
import sys
import unittest

from openmdao.main.mp_support import is_instance
from openmdao.util.testutil import assert_raises

from grid_engine import GridEngineAllocator, GridEngineServer


class TestCase(unittest.TestCase):

    directory = os.path.realpath(
        pkg_resources.resource_filename('grid_engine', 'test'))

    def setUp(self):
        # Force use of fake 'qsub'.
        self.orig_qsub = GridEngineServer._QSUB
        GridEngineServer._QSUB = os.path.join(TestCase.directory, 'qsub')

        # Force use of fake 'qhost'.
        self.orig_qhost = GridEngineAllocator._QHOST
        GridEngineAllocator._QHOST = os.path.join(TestCase.directory, 'qhost')

    def tearDown(self):
        GridEngineServer._QSUB = self.orig_qsub
        GridEngineAllocator._QHOST = self.orig_qhost
        for name in ('echo.in, echo.out', 'qsub.out'):
            if os.path.exists(name):
                os.remove(name)

    def test_allocator(self):
        logging.debug('')
        logging.debug('test_allocator')

        # Normal, successful allocation.
        allocator = GridEngineAllocator()
        nhosts = allocator.max_servers({})
        self.assertEqual(nhosts, 19*48)
        estimate, criteria = allocator.time_estimate({})
        self.assertEqual(estimate, 0)

        # Unused deployment.
        server = allocator.deploy('GridEngineTestServer', {}, {})
        self.assertTrue(is_instance(server, GridEngineServer))

        # Too many CPUs.
        estimate, criteria = allocator.time_estimate({'n_cpus': 1000})
        self.assertEqual(estimate, -2)

        # Not remote.
        nhosts = allocator.max_servers({'localhost': True})
        self.assertEqual(nhosts, 0)
        estimate, criteria = allocator.time_estimate({'localhost': True})
        self.assertEqual(estimate, -2)

        # Configure bad pattern.
        cfg = ConfigParser.ConfigParser()
        cfg.add_section('GridEngine')
        cfg.set('GridEngine', 'pattern', 'xyzzy')
        allocator.configure(cfg)
        nhosts = allocator.max_servers({})
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
        GridEngineAllocator._QHOST = os.path.join('bogus-qhost')
        cfg.set('GridEngine', 'pattern', '*')
        allocator.configure(cfg)
        nhosts = allocator.max_servers({})
        self.assertEqual(nhosts, 0)

    def test_server(self):
        logging.debug('')
        logging.debug('test_server')

        with open('echo.in', 'w') as out:
            pass

        server = GridEngineServer()

        # Try various resources.
        server.execute_command(dict(remote_command='echo',
                                    args=['hello', 'world'],
                                    working_directory='.',
                                    job_name='TestJob',
                                    job_environment={'ENV_VAR': 'env_value'},
                                    parallel_environment='ompi',
                                    n_cpus=256,
                                    input_path='echo.in',
                                    output_path='echo.out',
                                    join_files=True,
                                    email=['user1@host1', 'user2@host2'],
                                    block_email=True,
                                    email_events='beas',
                                    start_time='01010101.00',
                                    hard_wallclock_time_limit=1,
                                    soft_wallclock_time_limit=2,
                                    hard_run_duration_limit=3,
                                    soft_run_duration_limit=4))

        with open('echo.out', 'r') as inp:
            lines = inp.readlines()
        self.assertEqual(lines, ['hello world\n'])

        with open('qsub.out', 'r') as inp:
            lines = inp.readlines()
        self.assertEqual(''.join(lines), """\
-V -sync yes -wd . -N TestJob -pe ompi 256 -i echo.in -o echo.out -j yes -M user1@host1,user2@host2 -m n -m beas -a 01010101.00 -l h_rt=0:0:1 -l s_rt=0:0:2 -l h_cpu=0:0:3 -l s_cpu=0:0:4 echo hello world
-V
-sync arg yes
-wd arg .
-N arg TestJob
-pe ompi 256
-i stdin echo.in
-o stdout echo.out
-j join yes
-M arg user1@host1,user2@host2
-m arg n
-m arg beas
-a arg 01010101.00
-l resource h_rt=0:0:1
-l resource s_rt=0:0:2
-l resource h_cpu=0:0:3
-l resource s_cpu=0:0:4
+ '[' 1 -eq 1 ']'
+ echo hello world
""")

        # 'qsub' failure.
        GridEngineServer._QSUB = os.path.join('bogus-qsub')
        code = "server.execute_command(dict(remote_command='echo'))"
        assert_raises(self, code, globals(), locals(), OSError, '')


if __name__ == '__main__':
    sys.argv.append('--cover-package=grid_engine.')
    sys.argv.append('--cover-erase')
    nose.runmodule()

