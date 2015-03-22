"""
Test the ExternalCode component.
"""

import logging
import os.path
import pkg_resources
import shutil
import sys
import time
import tempfile
import unittest
import nose

from multiprocessing.managers import RemoteError

from openmdao.main.api import Assembly, FileMetadata, SimulationRoot, set_as_top
from openmdao.main.eggchecker import check_save_load
from openmdao.main.exceptions import RunInterrupted
from openmdao.main.objserverfactory import ObjServerFactory
from openmdao.main.rbac import Credentials, get_credentials

from openmdao.lib.components.external_code import ExternalCode
from openmdao.main.datatypes.api import Int, File, FileRef, Str

from openmdao.test.cluster import init_cluster

from openmdao.util.testutil import assert_raises
from openmdao.util.fileutil import onerror


# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# Directory where we can find sleep.py.
DIRECTORY = pkg_resources.resource_filename('openmdao.lib.components', 'test')
TMPDIR = os.getcwd()  # we'll set this for each test

ENV_FILE = 'env-data'
INP_FILE = 'input-data'
INP_DATA = 'Froboz rulz!'


class Sleeper(ExternalCode):
    """ Used to test external code functionality. """

    delay = Int(1, units='s', iotype='in')
    env_filename = Str(iotype='in')
    infile = File(iotype='in', local_path='input')
    outfile = File(FileRef('output'), iotype='out')

    def __init__(self):
        super(Sleeper, self).__init__()
        self.directory = TMPDIR
        self.external_files = [
            FileMetadata(path='sleep.py', 
                         input=True, constant=True),
        ]

    def execute(self):
        """ Runs code and sets `outfile`. """
        self.command = ['python', 'sleep.py', str(self.delay)]
        if self.env_filename:
            self.command.append(self.env_filename)
        super(Sleeper, self).execute()


class Unique(Sleeper):
    """ Used to test `create_instance_dir` functionality. """

    def __init__(self):
        super(Unique, self).__init__()
        self.create_instance_dir = True


class Model(Assembly):
    """ Run multiple `Unique` component instances. """

    infile = File(iotype='in', local_path='input')
    outfile = File(FileRef('output'), iotype='out')

    def configure(self):
        self.add('a', Unique())
        self.add('b', Unique())
        self.driver.workflow.add(['a', 'b'])
        self.connect('infile', 'a.infile')
        self.connect('a.outfile', 'b.infile')
        self.connect('b.outfile', 'outfile')


class TestCase(unittest.TestCase):
    """ Test the ExternalCode component. """

    def setUp(self):
        global TMPDIR
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_extcode-')
        TMPDIR = self.tempdir
        os.chdir(self.tempdir)
        SimulationRoot.chroot(self.tempdir)
        shutil.copy(os.path.join(DIRECTORY, 'sleep.py'), 
                    os.path.join(self.tempdir, 'sleep.py'))
        with open(INP_FILE, 'w') as out:
            out.write(INP_DATA)
        dum = Assembly()  # create this here to prevent any Assemblies in tests to be 'first'

    def tearDown(self):
        SimulationRoot.chroot(self.startdir)
        os.chdir(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

    def test_normal(self):
        logging.debug('')
        logging.debug('test_normal')

        sleeper = set_as_top(Sleeper())
        sleeper.env_filename = ENV_FILE
        sleeper.env_vars = {'SLEEP_DATA': 'Hello world!'}
        sleeper.external_files.append(
            FileMetadata(path=ENV_FILE, output=True))
        sleeper.infile = FileRef(INP_FILE, sleeper, input=True)
        sleeper.stderr = None

        sleeper.run()

        self.assertEqual(sleeper.return_code, 0)
        self.assertEqual(sleeper.timed_out, False)
        self.assertEqual(os.path.exists(ENV_FILE), True)

        with open(ENV_FILE, 'rU') as inp:
            data = inp.readline().rstrip()
        self.assertEqual(data, sleeper.env_vars['SLEEP_DATA'])

        with sleeper.outfile.open() as inp:
            result = inp.read()
        self.assertEqual(result, INP_DATA)

        # Force an error.
        sleeper.stderr = 'sleep.err'
        sleeper.delay = -1
        assert_raises(self, 'sleeper.run()', globals(), locals(), RuntimeError,
                      ': return_code = 1')
        sleeper.delay = 1

        # Redirect stdout & stderr.
        sleeper.env_vars = {'SLEEP_ECHO': '1'}
        sleeper.stdin  = 'sleep.in'
        sleeper.stdout = 'sleep.out'
        sleeper.stderr = 'sleep.err'
        with open('sleep.in', 'w') as out:
            out.write('Hello World!\n')
        sleeper.run()
        with open('sleep.out', 'r') as inp:
            self.assertEqual(inp.read(), 'stdin echo to stdout\n'
                                         'Hello World!\n')
        with open('sleep.err', 'r') as inp:
            self.assertEqual(inp.read(), 'stdin echo to stderr\n'
                                         'Hello World!\n')

        # Exercise check_files() errors.
        os.remove('input')
        assert_raises(self, 'sleeper.check_files(inputs=True)',
                      globals(), locals(), RuntimeError,
                      ": missing 'in' file 'input'")
        os.remove('output')
        assert_raises(self, 'sleeper.check_files(inputs=False)',
                      globals(), locals(), RuntimeError,
                      ": missing 'out' file 'output'")
        os.remove('sleep.in')
        assert_raises(self, 'sleeper.check_files(inputs=True)',
                      globals(), locals(), RuntimeError,
                      ": missing stdin file 'sleep.in'")
        os.remove('sleep.err')
        assert_raises(self, 'sleeper.check_files(inputs=False)',
                      globals(), locals(), RuntimeError,
                      ": missing stderr file 'sleep.err'")
        os.remove('sleep.out')
        assert_raises(self, 'sleeper.check_files(inputs=False)',
                      globals(), locals(), RuntimeError,
                      ": missing stdout file 'sleep.out'")

        # Show that non-existent expected files are detected.
        sleeper.external_files.append(
            FileMetadata(path='missing-input', input=True))
        assert_raises(self, 'sleeper.run()',
                      globals(), locals(), RuntimeError,
                      ": missing input file 'missing-input'")

    def test_remote(self):
        logging.debug('')
        logging.debug('test_remote')
        init_cluster(allow_shell=True)

        sleeper = set_as_top(Sleeper())
        sleeper.env_filename = ENV_FILE
        sleeper.env_vars = {'SLEEP_DATA': 'Hello world!'}
        sleeper.external_files.append(
            FileMetadata(path=ENV_FILE, output=True))
        sleeper.infile = FileRef(INP_FILE, sleeper, input=True)
        sleeper.timeout = 5
        sleeper.resources = {'min_cpus': 1}

        sleeper.run()

        self.assertEqual(sleeper.return_code, 0)
        self.assertEqual(sleeper.timed_out, False)
        self.assertEqual(os.path.exists(ENV_FILE), True)

        with open(ENV_FILE, 'r') as inp:
            data = inp.readline().rstrip()
        self.assertEqual(data, sleeper.env_vars['SLEEP_DATA'])

        with sleeper.outfile.open() as inp:
            result = inp.read()
        self.assertEqual(result, INP_DATA)

        # Null input file.
        sleeper.stdin = ''
        assert_raises(self, 'sleeper.run()', globals(), locals(), ValueError,
                      ": Remote execution requires stdin of DEV_NULL or"
                      " filename, got ''")

        # Specified stdin, stdout, and join stderr.
        with open('sleep.in', 'w') as out:
            out.write('froboz is a pig!\n')
        sleeper.stdin = 'sleep.in'
        sleeper.stdout = 'sleep.out'
        sleeper.stderr = ExternalCode.STDOUT
        sleeper.run()

        # Null stderr.
        sleeper.stderr = None
        sleeper.run()

    def test_bad_alloc(self):
        logging.debug('')
        logging.debug('test_bad_alloc')

        extcode = set_as_top(ExternalCode())
        extcode.command = ['python', 'sleep.py']
        extcode.resources = {'allocator': 'LocalHost',
                             'localhost': False}

        assert_raises(self, 'extcode.run()', globals(), locals(),
                      RuntimeError, ': Server allocation failed')

    def test_copy(self):
        logging.debug('')
        logging.debug('test_copy')

        extcode = set_as_top(ExternalCode())

        assert_raises(self, "extcode.copy_inputs('Inputs', '*.inp')",
                      globals(), locals(), RuntimeError,
                      ": inputs_dir 'Inputs' does not exist")

        os.mkdir('Inputs')
        try:
            shutil.copy('sleep.py', os.path.join('Inputs', 'junk.inp'))
            extcode.copy_inputs('Inputs', '*.inp')
            self.assertEqual(os.path.exists('junk.inp'), True)
        finally:
            shutil.rmtree('Inputs', onerror=onerror)
            if os.path.exists('junk.inp'):
                os.remove('junk.inp')

        assert_raises(self, "extcode.copy_results('Outputs', '*.dat')",
                      globals(), locals(), RuntimeError,
                      ": results_dir 'Outputs' does not exist")

        os.mkdir('Outputs')
        try:
            shutil.copy('sleep.py', os.path.join('Outputs', 'junk.dat'))
            extcode.copy_results('Outputs', '*.dat')
            self.assertEqual(os.path.exists('junk.dat'), True)
        finally:
            shutil.rmtree('Outputs', onerror=onerror)
            if os.path.exists('junk.dat'):
                os.remove('junk.dat')

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        sleeper = set_as_top(Sleeper())
        sleeper.name = 'Sleepy'
        sleeper.infile = FileRef(INP_FILE, sleeper, input=True)

        # Exercise check_save_load().
        retcode = check_save_load(sleeper)
        self.assertEqual(retcode, 0)

    def test_timeout(self):
        logging.debug('')
        logging.debug('test_timeout')

        # Set timeout to less than execution time.
        sleeper = set_as_top(Sleeper())
        sleeper.delay = 5
        sleeper.timeout = 1
        try:
            sleeper.run()
        except RunInterrupted as exc:
            self.assertEqual(str(exc), ': Timed out')
            self.assertEqual(sleeper.timed_out, True)
        else:
            self.fail('Expected RunInterrupted')

    def test_badcmd(self):
        logging.debug('')
        logging.debug('test_badcmd')

        # Set command to nonexistant path.
        extcode = set_as_top(ExternalCode())
        extcode.command = ['no-such-command']

        try:
            extcode.run()
        except ValueError as exc:
            msg = ": The command to be executed, 'no-such-command', cannot be found"
            self.assertEqual(str(exc), msg)
            self.assertEqual(extcode.return_code, -999999)
        else:
            self.fail('Expected OSError')

    def test_nullcmd(self):
        logging.debug('')
        logging.debug('test_nullcmd')

        # Check response to no command set.
        extcode = set_as_top(ExternalCode())
        extcode.stdout = 'nullcmd.out'
        extcode.stderr = ExternalCode.STDOUT
        try:
            extcode.run()
        except ValueError as exc:
            self.assertEqual(str(exc), ': Empty command list')
        else:
            self.fail('Expected ValueError')
        finally:
            if os.path.exists(extcode.stdout):
                os.remove(extcode.stdout)

    def test_unique(self):
        logging.debug('')
        logging.debug('test_unique')

        model = set_as_top(Model())
        for comp in (model.a, model.b):
            self.assertEqual(comp.create_instance_dir, False)
            self.assertEqual(comp.return_code, 0)
            self.assertEqual(comp.timed_out, False)
        self.assertEqual(model.a.directory, 'a')
        self.assertEqual(model.b.directory, 'b')

        model.infile = FileRef(INP_FILE, model, input=True)
        model.run()
        for comp in (model.a, model.b):
            self.assertEqual(comp.return_code, 0)
            self.assertEqual(comp.timed_out, False)

        with model.outfile.open() as inp:
            result = inp.read()
        self.assertEqual(result, INP_DATA)

    def test_rsh(self):
        logging.debug('')
        logging.debug('test_rsh')

        testdir = 'external_rsh'
        if os.path.exists(testdir):
            shutil.rmtree(testdir, onerror=onerror)
        os.mkdir(testdir)
        os.chdir(testdir)

        factory = None
        try:
            # Try to set command line on remote ExternalCode instance.
            typname = 'openmdao.lib.components.external_code.ExternalCode'
            factory = ObjServerFactory(allowed_types=[typname])
            exec_comp = factory.create(typname)
            try:
                exec_comp.command = ['this-should-fail']
            except RemoteError as exc:
                msg = "RoleError: No __setattr__ access to 'command'"
                logging.debug('msg: %s', msg)
                logging.debug('exc: %s', exc)
                self.assertTrue(msg in str(exc))
            else:
                self.fail('Expected RemoteError')

            # Try to set via set() on remote instance.
            try:
                exec_comp.set('command', ['this-should-fail'])
            except RemoteError as exc:
                fragment = ": 'command' may not be set() remotely"
                if fragment not in str(exc):
                    self.fail('%s not in %s' % (fragment, exc))
            else:
                self.fail('Expected RemoteError')
        finally:
            if factory is not None:
                factory.cleanup()
            os.chdir('..')
            if sys.platform == 'win32':
                time.sleep(2)  # Wait for process shutdown.
            keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
            if not keep_dirs:
                shutil.rmtree(testdir, onerror=onerror)


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.components')
    sys.argv.append('--cover-erase')
    nose.runmodule()
