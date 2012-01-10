import ConfigParser
import glob
import logging
import nose
import os.path
import pkg_resources
import shutil
import sys
import unittest

from openmdao.main.mp_support import is_instance
from openmdao.util.testutil import assert_raises

from pbs import PBS_Allocator, PBS_Server


class TestCase(unittest.TestCase):

    directory = os.path.realpath(pkg_resources.resource_filename('pbs', 'test'))

    def setUp(self):
        # Force use of fake 'qsub'.
        self.orig_qsub = list(PBS_Server._QSUB)
        PBS_Server._QSUB[:] = \
            ['python', os.path.join(TestCase.directory, 'qsub.py')]

    def tearDown(self):
        PBS_Server._QSUB[:] = self.orig_qsub
        for name in ('TestJob.qsub', 'TestJob.bat', 'qsub.out',
                     'echo.in', 'echo.out', 'echo.qsub'):
            if os.path.exists(name):
                os.remove(name)
        for name in glob.glob('PBS_TestServer*'):
            shutil.rmtree(name)

    def test_allocator(self):
        logging.debug('')
        logging.debug('test_allocator')

        # Normal, successful allocation.
        allocator = PBS_Allocator()
        nhosts = allocator.max_servers({})
        self.assertEqual(nhosts, allocator.n_cpus)
        estimate, criteria = allocator.time_estimate({})
        self.assertEqual(estimate, 0)

        # Unused deployment.
        server = allocator.deploy('PBS_TestServer', {}, {})
        self.assertTrue(is_instance(server, PBS_Server))
        allocator.release(server)

        # Too many CPUs.
        estimate, criteria = allocator.time_estimate({'n_cpus': 1000000})
        self.assertEqual(estimate, -2)

        # Not remote.
        nhosts = allocator.max_servers({'localhost': True})
        self.assertEqual(nhosts, 0)
        estimate, criteria = allocator.time_estimate({'localhost': True})
        self.assertEqual(estimate, -2)

        # Incompatible Python version.
        estimate, criteria = allocator.time_estimate({'python_version': '9.9'})
        self.assertEqual(estimate, -2)

        # Unrecognized key.
        estimate, criteria = allocator.time_estimate({'no-such-key': 0})
        self.assertEqual(estimate, -2)

    def test_server(self):
        logging.debug('')
        logging.debug('test_server')

        with open('echo.in', 'w') as out:
            pass

        os.environ['OPENMDAO_PBS_ACCOUNTID'] = 'test-account'
        server = PBS_Server()

        # Try various resources.
        echo = os.path.join(TestCase.directory, 'echo.py')
        server.execute_command(dict(remote_command='python',
                                    args=[echo, 'hello', 'world'],
                                    working_directory='.',
                                    account_id='my-nas-acct',
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
        if sys.platform == 'win32':
            sh1 = ' -C REM PBS'
            sh2 = '-C arg REM PBS'
        else:
            sh1 = ' -S /bin/sh'
            sh2 = '-S arg /bin/sh'
        self.assertEqual(''.join(lines), """\
-V -W block=true -j oe%s .%sTestJob.qsub
-V
-W arg block=true
-j arg oe
%s
.%sTestJob.qsub
""" % (sh1, os.sep, sh2, os.sep))

        with open('TestJob.qsub', 'r') as inp:
            lines = inp.readlines()
        if sys.platform == 'win32':
            sh1 = ''
            prefix = 'REM PBS'
        else:
            sh1 = '#!/bin/sh\n'
            prefix = '#PBS'
        self.assertTrue(''.join(lines).startswith("""\
%(sh1)s%(prefix)s -W group_list=my-nas-acct
%(prefix)s -N TestJob
%(prefix)s -l select=256:ncpus=1
%(prefix)s -M user1@host1,user2@host2
%(prefix)s -m n
%(prefix)s -m bea
%(prefix)s -a 01010101.00
%(prefix)s -l walltime=0:00:01
%(prefix)s -l walltime=0:00:02
%(prefix)s -l walltime=0:00:03
%(prefix)s -l walltime=0:00:04
""" % dict(sh1=sh1, prefix=prefix)))

# Skip varification of location-dependent working directory.

        self.assertTrue(''.join(lines).endswith("""\
python %s hello world <echo.in >echo.out 2>&1
""" % echo))

        # 'qsub' failure.
        PBS_Server._QSUB[:] = [os.path.join('bogus-qsub')]
        code = "server.execute_command(dict(remote_command='echo'))"
        assert_raises(self, code, globals(), locals(), OSError, '')


if __name__ == '__main__':
    sys.argv.append('--cover-package=pbs.')
    sys.argv.append('--cover-erase')
    nose.runmodule()

