"""
Test the ExternalCode component.
"""

import logging
import os.path
import pkg_resources
import platform
import shutil
import sys
import unittest
import nose

from openmdao.main.api import Assembly, FileMetadata, SimulationRoot, set_as_top
from openmdao.main.eggchecker import check_save_load
from openmdao.main.exceptions import RunInterrupted
from openmdao.main.resource import ResourceAllocationManager, ClusterAllocator

from openmdao.lib.components.external_code import ExternalCode

from openmdao.util.testutil import assert_raises, find_python

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# Directory where we can find sleep.py.
DIRECTORY = pkg_resources.resource_filename('openmdao.lib.components', 'test')


class Unique(ExternalCode):
    """ Used to test `create_instance_dir` functionality. """

    def __init__(self):
        super(Unique, self).__init__(directory=DIRECTORY)
        self.create_instance_dir = True
        self.external_files = [
            FileMetadata(path='sleep.py', input=True, constant=True),
        ]
        self.command = 'python sleep.py 1'


class Model(Assembly):
    """ Run multiple `Unique` component instances. """

    def __init__(self):
        super(Model, self).__init__()
        self.add('a', Unique())
        self.add('b', Unique())
        self.driver.workflow.add([self.a,self.b])


class TestCase(unittest.TestCase):
    """ Test the ExternalCode component. """

    def setUp(self):
        SimulationRoot.chroot(DIRECTORY)
        
    def tearDown(self):
        for directory in ('a', 'b'):
            if os.path.exists(directory):
                shutil.rmtree(directory)
        SimulationRoot.chroot(ORIG_DIR)
        
    def test_normal(self):
        logging.debug('')
        logging.debug('test_normal')

        dummy = 'dummy_output'
        if os.path.exists(dummy):
            os.remove(dummy)

        extcode = set_as_top(ExternalCode())
        extcode.timeout = 5
        extcode.command = 'python sleep.py 1 %s' % dummy
        extcode.env_vars = {'SLEEP_DATA': 'Hello world!'}

        extcode.run()

        self.assertEqual(extcode.return_code, 0)
        self.assertEqual(extcode.timed_out, False)
        self.assertEqual(os.path.exists(dummy), True)
        try:
            with open(dummy, 'r') as inp:
                data = inp.readline().rstrip()
            self.assertEqual(data, extcode.env_vars['SLEEP_DATA'])
        finally:
            os.remove(dummy)

    def test_remote(self):
        logging.debug('')
        logging.debug('test_remote')

        # Ensure we aren't held up by local host load problems.
        local = ResourceAllocationManager.get_allocator(0)
        local.max_load = 10

        # Exercise cluster deployment if on this GRC cluster front-end.
        node = platform.node()
        if node.startswith('gxterm'):
            name = node.replace('.', '_')
            alloc = ResourceAllocationManager.get_allocator(0)
            if alloc.name != name:  # Don't add multiple copies.
                python = find_python()
                machines = []
                for i in range(55):
                    machines.append({'hostname':'gx%02d' % i, 'python':python})
                cluster = ClusterAllocator(name, machines)
                ResourceAllocationManager.insert_allocator(0, cluster)

        dummy = 'dummy_output'
        if os.path.exists(dummy):
            os.remove(dummy)

        extcode = set_as_top(ExternalCode())
        extcode.timeout = 5
        extcode.command = 'python sleep.py 1 %s' % dummy
        extcode.env_vars = {'SLEEP_DATA': 'Hello world!'}
        extcode.external_files.extend((
            FileMetadata(path='sleep.py', input=True),
            FileMetadata(path=dummy, output=True)
        ))
        extcode.resources = {'n_cpus': 1}

        extcode.run()

        self.assertEqual(extcode.return_code, 0)
        self.assertEqual(extcode.timed_out, False)
        self.assertEqual(os.path.exists(dummy), True)
        try:
            with open(dummy, 'r') as inp:
                data = inp.readline().rstrip()
            self.assertEqual(data, extcode.env_vars['SLEEP_DATA'])
        finally:
            os.remove(dummy)

    def test_bad_alloc(self):
        logging.debug('')
        logging.debug('test_bad_alloc')

        extcode = set_as_top(ExternalCode())
        extcode.command = 'python sleep.py'
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

        extcode = set_as_top(ExternalCode())
        extcode.name = 'ExternalCode'
        extcode.timeout = 5
        extcode.command = 'python sleep.py 1'
        extcode.external_files = [
            FileMetadata(path='sleep.py', input=True, constant=True),
        ]

        # Exercise check_save_load().
        retcode = check_save_load(extcode)
        self.assertEqual(retcode, 0)

    def test_timeout(self):
        logging.debug('')
        logging.debug('test_timeout')

        # Set timeout to less than execution time.
        extcode = set_as_top(ExternalCode())
        extcode.timeout = 1
        extcode.command = 'python sleep.py 5'
        try:
            extcode.run()
        except RunInterrupted as exc:
            self.assertEqual(str(exc), ': Timed out')
            self.assertEqual(extcode.timed_out, True)
        else:
            self.fail('Expected RunInterrupted')

    def test_badcmd(self):
        logging.debug('')
        logging.debug('test_badcmd')

        # Set command to nonexistant path.
        extcode = set_as_top(ExternalCode())
        extcode.command = 'xyzzy'
        extcode.stdout = 'badcmd.out'
        extcode.stderr = ExternalCode.STDOUT
        try:
            extcode.run()
        except RuntimeError as exc:
            if sys.platform == 'win32':
                self.assertTrue('Operation not permitted' in str(exc))
                self.assertEqual(extcode.return_code, 1)
            else:
                msg = ': return_code = 127'
                self.assertEqual(str(exc).startswith(msg), True)
                self.assertEqual(extcode.return_code, 127)
            self.assertEqual(os.path.exists(extcode.stdout), True)
        else:
            self.fail('Expected RuntimeError')
        finally:
            if os.path.exists(extcode.stdout):
                os.remove(extcode.stdout)

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

        model.run()
        for comp in (model.a, model.b):
            self.assertEqual(comp.return_code, 0)
            self.assertEqual(comp.timed_out, False)


if __name__ == "__main__":
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

