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
from openmdao.main.mp_support import is_instance
from openmdao.main.pbs import PBS_Allocator, PBS_Server
from openmdao.main.resource import HOME_DIRECTORY, WORKING_DIRECTORY
from openmdao.util.shellproc import DEV_NULL
from openmdao.util.testutil import assert_raises
from openmdao.util.fileutil import onerror


class TestCase(unittest.TestCase):

    directory = os.path.realpath(
                    pkg_resources.resource_filename('openmdao.main', 'test'))

    def setUp(self):
        self.tempdir = tempfile.mkdtemp(prefix='test_pbs-')
        self.startdir = os.getcwd()
        os.chdir(self.tempdir)
        SimulationRoot.chroot(self.tempdir)

        # Force use of fake 'qsub'.
        self.orig_qsub = list(PBS_Server._QSUB)
        PBS_Server._QSUB[:] = \
            ['python', os.path.join(TestCase.directory, 'pbs_qsub.py')]

    def tearDown(self):
        PBS_Server._QSUB[:] = self.orig_qsub
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

        allocator = PBS_Allocator()
        cfg = ConfigParser.ConfigParser()
        cfg.add_section('PBS')
        cfg.set('PBS', 'accounting_id', 'test-account')
        allocator.configure(cfg)

        # Normal, successful allocation.
        nhosts, criteria = allocator.max_servers({})
        self.assertEqual(nhosts, allocator.n_cpus)
        estimate, criteria = allocator.time_estimate({})
        self.assertEqual(estimate, 0)

        nhosts, criteria = allocator.max_servers({'min_cpus': 2, 'max_cpus': 2})
        self.assertEqual(nhosts, allocator.n_cpus/2)
        estimate, criteria = allocator.time_estimate({'min_cpus': 2})
        self.assertEqual(estimate, 0)

        # Unused deployment.
        server = allocator.deploy('PBS_TestServer', {}, {})
        self.assertTrue(is_instance(server, PBS_Server))
        allocator.release(server)

        # Too many CPUs.
        nhosts, criteria = allocator.max_servers({'min_cpus': 1000000})
        self.assertEqual(nhosts, 0)
        estimate, criteria = allocator.time_estimate({'min_cpus': 1000000})
        self.assertEqual(estimate, -2)

        # Not remote.
        nhosts, criteria = allocator.max_servers({'localhost': True})
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

        server = PBS_Server()
        server.configure(accounting_id='test-account')

        # Try various resources.
        start_time = datetime.datetime(2012, 2, 8, 16, 42)
        resource_limits = dict(wallclock_time=2)
        echo = os.path.join(TestCase.directory, 'pbs_echo.py')
        server.execute_command(dict(remote_command='python',
                                    args=[echo, 'hello', 'world'],
                                    submit_as_hold=True,
                                    rerunnable=True,
                                    job_environment={'ENV_VAR': 'env_value'},
                                    working_directory='.',
                                    min_cpus=256,
                                    email=['user1@host1', 'user2@host2'],
                                    email_on_started=True,
                                    email_on_terminated=True,
                                    job_name='TestJob',
                                    input_path='echo.in',
                                    output_path='echo.out',
                                    join_files=True,
                                    queue_name='debug_q',
                                    priority=42,
                                    start_time=start_time,
                                    resource_limits=resource_limits,
                                    accounting_id='CFD-R-US',
                                    native_specification=('-W', 'umask=077')))

        with open('echo.out', 'r') as inp:
            lines = inp.readlines()
        self.assertEqual(lines, ['hello world\n'])

        with open('qsub.out', 'r') as inp:
            output = ''.join(inp.readlines())
        if sys.platform == 'win32':
            sh1 = ' -C "REM PBS"'
            sh2 = '-C arg "REM PBS"'
            suffix = '-qsub.bat'
        else:
            sh1 = ' -S /bin/sh'
            sh2 = '-S arg /bin/sh'
            suffix = '.qsub'
        expected = """\
-V -W block=true -j oe%(sh1)s -W umask=077 .%(sep)sTestJob%(suffix)s
-V
-W arg block=true
-j arg oe
%(sh2)s
-W arg umask=077
.%(sep)sTestJob%(suffix)s
""" % dict(sh1=sh1, sh2=sh2, suffix=suffix, sep=os.sep)
        logging.debug('qsub output:')
        logging.debug(output)
        logging.debug('Expected output:')
        logging.debug(expected)
        self.assertEqual(output, expected)

        with open('TestJob%s' % suffix, 'r') as inp:
            script = ''.join(inp.readlines())
        if sys.platform == 'win32':
            sh1 = '@echo off'
            prefix = 'REM PBS'
        else:
            sh1 = '#!/bin/sh'
            prefix = '#PBS'
        beginning = ("""\
%(sh1)s
%(prefix)s -W group_list=CFD-R-US
%(prefix)s -h
%(prefix)s -r y
%(prefix)s -l select=256:ncpus=1
%(prefix)s -M user1@host1,user2@host2
%(prefix)s -N TestJob
%(prefix)s -q debug_q
%(prefix)s -p 42
%(prefix)s -a 201202081642.00
%(prefix)s -m be
%(prefix)s -l walltime=0:00:02
""" % dict(sh1=sh1, prefix=prefix))
        logging.debug('generated script:')
        logging.debug(script)
        logging.debug('Expected beginning:')
        logging.debug(beginning)
        self.assertTrue(script.startswith(beginning))

# Skip verification of location-dependent working directory.

        generated_echo = '"%s"' % echo if ' ' in echo else echo
        self.assertTrue(script.endswith("""\
python %s hello world <echo.in >echo.out 2>&1
""" % generated_echo))

        # error_path.
        server.execute_command(dict(remote_command='python',
                                    args=[echo, 'hello', 'world'],
                                    job_name='#bogus-job-(*&^*%-123456789',
                                    output_path='echo.out',
                                    error_path='echo.err'))
        with open('Zbogus-job-(_&^%s' % suffix, 'r') as inp:
            script = ''.join(inp.readlines())
        logging.debug('generated script:')
        logging.debug(script)
        self.assertTrue(script.endswith("""\
python %s hello world <%s >echo.out 2>echo.err
""" % (generated_echo, DEV_NULL)))

        # HOME_DIRECTORY, WORKING_DIRECTORY.
        home_dir = os.path.expanduser('~')
        work_dir = os.getcwd()
        server.execute_command(dict(remote_command='python',
                                    args=[echo, HOME_DIRECTORY+'hello',
                                          WORKING_DIRECTORY+'world'],
                                    working_directory=work_dir,
                                    output_path='echo.out'))
        with open('echo.out', 'r') as inp:
            lines = inp.readlines()
        self.assertEqual(lines,
                         ['%s %s\n' % (os.path.join(home_dir, 'hello'),
                                       os.path.join(work_dir, 'world'))])
        # 'qsub' failure.
        PBS_Server._QSUB[:] = [os.path.join('bogus-qsub')]
        code = "server.execute_command(dict(remote_command='echo'))"
        assert_raises(self, code, globals(), locals(), OSError, '')


if __name__ == '__main__':
    sys.argv.append('--cover-package=pbs.')
    sys.argv.append('--cover-erase')
    nose.runmodule()
