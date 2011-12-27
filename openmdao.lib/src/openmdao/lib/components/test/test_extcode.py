"""
Test the ExternalCode component.
"""

import logging
import os.path
import pkg_resources
import shutil
import sys
import time
import unittest
import nose

from multiprocessing.managers import RemoteError

from openmdao.main.api import Assembly, FileRef, FileMetadata, SimulationRoot, \
                              set_as_top
from openmdao.main.eggchecker import check_save_load
from openmdao.main.exceptions import RunInterrupted
from openmdao.main.objserverfactory import ObjServerFactory
from openmdao.main.rbac import Credentials, get_credentials

from openmdao.lib.components.external_code import ExternalCode
from openmdao.lib.datatypes.api import Int, File, Str

from openmdao.test.cluster import init_cluster

from openmdao.util.testutil import assert_raises


# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# Directory where we can find sleep.py.
DIRECTORY = pkg_resources.resource_filename('openmdao.lib.components', 'test')

ENV_FILE = 'env-data'
INP_FILE = 'input-data'
INP_DATA = 'Froboz rulz!'


class Sleeper(ExternalCode):
    """ Used to test external code functionality. """

    delay = Int(1, units='s', iotype='in')
    env_filename = Str(iotype='in')
    infile = File(iotype='in', local_path='input')
    outfile = File(iotype='out', path='output')

    def __init__(self):
        super(Sleeper, self).__init__(directory=DIRECTORY)
        self.external_files = [
            FileMetadata(path='sleep.py', input=True, constant=True),
        ]

    def execute(self):
        """ Runs code and sets `outfile`. """
        self.command = ['python', 'sleep.py', str(self.delay)]
        if self.env_filename:
            self.command.append(self.env_filename)
        super(Sleeper, self).execute()
        self.outfile = FileRef(self.outfile.path, self, output=True)


class Unique(Sleeper):
    """ Used to test `create_instance_dir` functionality. """

    def __init__(self):
        super(Unique, self).__init__()
        self.create_instance_dir = True


class Model(Assembly):
    """ Run multiple `Unique` component instances. """

    infile = File(iotype='in', local_path='input')
    outfile = File(iotype='out', path='output')

    def __init__(self):
        super(Model, self).__init__()
        self.add('a', Unique())
        self.add('b', Unique())
        self.driver.workflow.add(['a', 'b'])
        self.connect('infile', 'a.infile')
        self.connect('a.outfile', 'b.infile')
        self.connect('b.outfile', 'outfile')


class TestCase(unittest.TestCase):
    """ Test the ExternalCode component. """

    def setUp(self):
        SimulationRoot.chroot(DIRECTORY)
        with open(INP_FILE, 'w') as out:
            out.write(INP_DATA)
        if os.path.exists(ENV_FILE):
            os.remove(ENV_FILE)
        
    def tearDown(self):
        for directory in ('a', 'b'):
            if os.path.exists(directory):
                shutil.rmtree(directory)
        for name in (ENV_FILE, INP_FILE, 'input', 'output'):
            if os.path.exists(name):
                os.remove(name)
        if os.path.exists("error.out"):
            try:
                os.remove("error.out")
                
            # Windows processes greedily clutch files. I see no
            # way to delete this file in test_timeout
            except WindowsError:
                pass
                
        SimulationRoot.chroot(ORIG_DIR)
        
    def test_normal(self):
        logging.debug('')
        logging.debug('test_normal')

        sleeper = set_as_top(Sleeper())
        sleeper.env_filename = ENV_FILE
        sleeper.env_vars = {'SLEEP_DATA': 'Hello world!'}
        sleeper.external_files.append(
            FileMetadata(path=ENV_FILE, output=True))
        sleeper.infile = FileRef(INP_FILE, sleeper, input=True)

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

        # Now show that existing outputs are removed before execution.
        sleeper.env_filename = ''
        sleeper.run()
        msg = "[Errno 2] No such file or directory: '%s'" % ENV_FILE
        assert_raises(self, "open('%s', 'rU')" % ENV_FILE,
                      globals(), locals(), IOError, msg)

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
        sleeper.resources = {'n_cpus': 1}

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

    def test_bad_alloc(self):
        logging.debug('')
        logging.debug('test_bad_alloc')

        extcode = set_as_top(ExternalCode())
        extcode.command = ['python', 'sleep.py']
        extcode.resources = {'no_such_resource': 1}

        try:
            extcode.run()
        except RuntimeError as exc:
            self.assertEqual(str(exc), ': Server allocation failed :-(')
        else:
            self.fail('Exected RuntimeError')

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
            shutil.rmtree('Inputs')
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
            shutil.rmtree('Outputs')
            if os.path.exists('junk.dat'):
                os.remove('junk.dat')

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        sleeper = set_as_top(Sleeper())
        sleeper.name = 'Sleepy'
        sleeper.infile = FileRef(INP_FILE, sleeper, input=True)
        import glob
        logging.critical('%s', glob.glob('*'))

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
        extcode.command = ['xyzzy']
        try:
            extcode.run()
        except OSError as exc:
            if sys.platform == 'win32':
                msg = '[Error 2] The system cannot find the file specified'
            else:
                msg = '[Errno 2] No such file or directory'
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
            self.assertEqual(str(exc), ': Null command line')
        else:
            self.fail('Expected ValueError')
        finally:
            if os.path.exists(extcode.stdout):
                os.remove(extcode.stdout)
    
    def test_unique(self):
        logging.debug('')
        logging.debug('test_unique')

        model = Model()
        for comp in (model.a, model.b):
            self.assertEqual(comp.create_instance_dir, True)
        self.assertNotEqual(model.a.directory, 'a')
        self.assertNotEqual(model.b.directory, 'b')

        set_as_top(model)
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
            shutil.rmtree(testdir)
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

            exec_comp.set('command', ['this-should-pass'])

            # Try to set via remote-looking access.
            creds = get_credentials()
            creds.client_creds = Credentials()
            logging.debug('    using %s', creds)
            try:
                code = "exec_comp.set('command', ['this-should-fail'])"
                assert_raises(self, code, globals(), locals(), RuntimeError,
                              ": 'command' may not be set() remotely")
            finally:
                creds.client_creds = None

        finally:
            if factory is not None:
                factory.cleanup()
            os.chdir('..')
            if sys.platform == 'win32':
                time.sleep(2)  # Wait for process shutdown.
            keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
            if not keep_dirs:
                shutil.rmtree(testdir)


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao.components')
    sys.argv.append('--cover-erase')
    nose.runmodule()

