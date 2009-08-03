"""
Test saving and loading of simulations as eggs.
"""

import cPickle
import logging
import os.path
import pkg_resources
import shutil
import subprocess
import sys
import unittest

from enthought.traits.api import Bool, Str, Array, Int, Instance, Callable

from openmdao.main.api import Assembly, Component, Container, \
                              SAVE_PICKLE, SAVE_CPICKLE, SAVE_LIBYAML
from openmdao.main.filevar import FileTrait

from openmdao.main.pkg_res_factory import PkgResourcesFactory

import openmdao.util.testutil

# pylint: disable-msg=E1101,E1103
# "Instance of <class> has no <attr> member"

__version__ = '1.2.3'  # Used in forming egg name.

EXTERNAL_FILES = ('xyzzy', '../sub/data2', 'hello', '../sub/data4')

SOURCE_INIT = False
SINK_INIT = False

MODULE_NAME = __name__

# Set local dir in case we're running in a different directory.
PY_DIR = pkg_resources.resource_filename('openmdao.main', 'test')


class Source(Assembly):
    """ Produces files. """

    write_files = Bool(True, iostatus='in')
    text_data = Str(iostatus='in')
    text_file = FileTrait(iostatus='out')

    def __init__(self, name='Source', *args, **kwargs):
        super(Source, self).__init__(name, *args, **kwargs)
        self.directory = self.get_directory()  # Force absolute.

        global SOURCE_INIT
        SOURCE_INIT = True

        Subcontainer('sub', parent=self)
        self.create_passthru('sub.binary_file')

        # Some custom objects that must be restored.
        self.obj_list = [DataObj(i) for i in range(3)]
        
        self.text_file.filename = 'source.txt'

        # Absolute external file that exists at time of save.
        path = os.path.join(self.directory, EXTERNAL_FILES[0])
        out = open(path, 'w')
        out.write('Twisty narrow passages.\n')
        out.close()
        self.external_files.append({'path':path, 'input':True, 'constant':True})

        # Absolute external file that exists at time of save, in separate tree.
        path = os.path.join(self.directory, EXTERNAL_FILES[1])
        leaf = os.path.dirname(path)
        if not os.path.exists(leaf):
            os.makedirs(leaf)
        out = open(path, 'w')
        out.write('Some external data.\n')
        out.close()
        self.external_files.append({'path':path})

        # Relative external file that exists at time of save.
        self.push_dir(self.get_directory())
        path = EXTERNAL_FILES[2]
        out = open(path, 'w')
        out.write('Hello world!\n')
        out.close()
        self.pop_dir()
        self.external_files.append({'path':path})

        # Relative external file that exists at time of save, in separate tree.
        self.push_dir(self.get_directory())
        path = EXTERNAL_FILES[3]
        leaf = os.path.dirname(path)
        if not os.path.exists(leaf):
            os.makedirs(leaf)
        out = open(path, 'w')
        out.write('Some more external data.\n')
        out.close()
        self.pop_dir()
        self.external_files.append({'path':path})

        # External file that doesn't exist at time of save.
        self.external_files.append({'path':'does-not-exist'})

    def execute(self):
        """ Write test data to files. """
        if self.write_files:
            cwd = os.getcwd()
            self.debug("opening file '%s' in %s" % 
                       (self.text_file.filename,cwd))
            out = open(self.text_file.filename, 'w')
            out.write(self.text_data)
            out.close()

            self.debug("opening file '%s' in %s" % 
                       (self.sub.binary_file.filename,cwd))
            out = open(self.sub.binary_file.filename, 'wb')
            cPickle.dump(self.sub.binary_data, out, 2)
            out.close()


class Subcontainer(Container):
    """ Just a subcontainer for Source. """

    binary_data = Array('d', value=[], iostatus='in')
    binary_file = FileTrait(iostatus='out', binary=True)
        
    def __init__(self, name='Subcontainer', parent=None):
        super(Subcontainer, self).__init__(name, parent)
        self.binary_file.filename = os.path.join('..', 'sub', 'source.bin')



class DataObj(object):
    """ Just a custom class for objects to save & reload. """

    def __init__(self, data):
        self.data = data


class Sink(Component):
    """ Consumes files. """

    text_data = Str(iostatus='out')
    binary_data = Array('d', value=[], iostatus='out')

    # Absolute FileTrait that exists at time of save.
    text_file = FileTrait(iostatus='in')

    executions = Int(0, iostatus='in',
                     desc='Count of Oddball instance_method() calls.')
    
    def __init__(self, name='Sink', *args, **kwargs):
        super(Sink, self).__init__(name, *args, **kwargs)

        global SINK_INIT
        SINK_INIT = True
        
        self.text_file.filename = os.path.join(self.get_directory(), 'sink.txt')

        out = open(self.text_file.filename, 'w')
        out.write('Absolute FileTrait that exists at time of save.\n')
        out.close()

        # Relative FileTrait that exists at time of save.
        self.add_trait('binary_file', FileTrait(iostatus='in'))
        self.binary_file.filename = 'sink.bin'
        self.push_dir(self.get_directory())
        out = open(self.binary_file.filename, 'w')
        out.write('Relative FileTrait that exists at time of save.\n')
        out.close()
        self.pop_dir()

    def execute(self):
        """ Read test data from files. """
        inp = open(self.text_file.filename, 'r')
        self.text_data = inp.read()
        inp.close()

        inp = open(self.binary_file.filename, 'rb')
        self.binary_data = cPickle.load(inp)
        inp.close()


class Oddball(Assembly):
    """ Just a component that needs a separate directory to be created. """

    # FIXME: I tried the built-in trait types of Callable, Method, and Function
    # for these two sockets and couldn't get them to work.  We may have to
    # create new TraitTypes for these...
    #function_socket = Instance(Callable, none_allowed=True,
    #                           desc='Just something to call.', required=False)
    #method_socket = Instance(Callable, none_allowed=True,
    #                         desc='Just something to call.', required=False)
    executions = Int(0, iostatus='out', desc='Counts instance_method() calls.')

    def __init__(self, name='Oddball', *args, **kwargs):
        super(Oddball, self).__init__(name, *args, **kwargs)
        OddballComponent('oddcomp', parent=self)
        OddballContainer('oddcont', parent=self)
        self.thing_to_call = self.instance_method
        self.function_socket = os.getpid
        self.method_socket = self.instance_method
        self.peer_class = Source  # Check that class in __main__ is handled.
        self.scratch_tuple = (1, 2)

    def execute(self):
        """ Call stuff. Empty sockets are clumsy. """
        if self.thing_to_call:
            self.debug('thing_to_call returned %s', self.thing_to_call())

        try:
            self.debug('function_socket returned %s', self.function_socket())
        except RuntimeError, exc:
            if not str(exc).find('empty'):
                raise exc
        try:
            self.debug('method_socket returned %s', self.method_socket())
        except RuntimeError, exc:
            if not str(exc).find('empty'):
                raise exc

    def instance_method(self):
        """ Called by execute(). """
        self.executions += 1
        return self.executions

    @staticmethod
    def static_method():
        """ This won't pickle. """
        return None


class OddballComponent(Component):
    """ Just a subcomponent for Oddball to test nested entry points. """

    def __init__(self, name='OddballComponent', parent=None):
        super(OddballComponent, self).__init__(name, parent)
        # Some custom objects that must be restored.
        self.obj_list = [DataObj(i) for i in range(3)]


class OddballContainer(Container):
    """ Just a subcontainer for Oddball to test nested entry points. """

    def __init__(self, name='OddballContainer', parent=None):
        super(OddballContainer, self).__init__(name, parent)
        # Some custom objects that must be restored.
        self.obj_list = [DataObj(i) for i in range(3)]


def main_function():
    """ Can't pickle references to functions defined in __main__. """
    return None


class Model(Assembly):
    """ Transfer files from producer to consumer. """

    def __init__(self, name='Egg_TestModel', *args, **kwargs):
        super(Model, self).__init__(name, *args, **kwargs)

        Source(parent=self, directory='Source')
        Oddball(parent=self, directory='Oddball')
        Sink(parent=self, directory='Sink')

        self.connect('Source.text_file', 'Sink.text_file')
        self.connect('Source.binary_file', 'Sink.binary_file')

        self.connect('Oddball.executions', 'Sink.executions')

        self.Source.text_data = 'oiuyoiuyoiuy'
        self.Source.sub.binary_data = [3.14159, 2.781828, 42]


class EggTestCase(unittest.TestCase):
    """ Test saving and loading of simulations as eggs. """

    def setUp(self):
        """ Called before each test in this class. """
        self.model = Model(directory='Egg')
        self.child_objs = [self.model.Source, self.model.Sink,
                           self.model.Oddball, self.model.Oddball.oddcomp,
                           self.model.Oddball.oddcont]
        self.egg_name = None

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        self.model = None
        if self.egg_name and os.path.exists(self.egg_name):
            os.remove(self.egg_name)
        if os.path.exists('Egg'):
            shutil.rmtree('Egg')

    def save_load(self, format, use_setuptools=False):
        global SOURCE_INIT, SINK_INIT

        # Verify initial state.
        self.assertEqual(SOURCE_INIT, True)
        self.assertEqual(SINK_INIT, True)
        self.assertNotEqual(self.model.Sink.text_data,
                            self.model.Source.text_data)
        self.assertNotEqual(self.model.Sink.binary_data,
                            self.model.Source.sub.binary_data)
        self.assertNotEqual(
            self.model.Sink.binary_file.binary, True)

        for path in EXTERNAL_FILES:
            path = os.path.join(self.model.Source.get_directory(), path)
            self.assertEqual(os.path.exists(path), True)

        for i in range(3):
            self.assertEqual(self.model.Source.obj_list[i].data, i)

        self.assertEqual(self.model.Sink.executions, 0)

        # Save to egg.
        self.egg_name = self.model.save_to_egg(py_dir=PY_DIR, format=format,
                                               child_objs=self.child_objs,
                                               use_setuptools=use_setuptools)

        # Run and verify correct operation.
        self.model.run()
        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(True,
            all(self.model.Sink.binary_data==self.model.Source.sub.binary_data))
        self.assertEqual(self.model.Sink.binary_file.binary, True)

        self.assertEqual(self.model.Sink.executions, 2)

        # Restore in test directory.
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            # Clear flags to detect if loading calls __init__.
            SOURCE_INIT = False
            SINK_INIT = False

            # Load from saved initial state in egg.
            self.model.pre_delete()
            egg_path = os.path.join('..', self.egg_name)
            self.model = Component.load_from_eggfile(egg_path, install=False)
            self.model.directory = os.path.join(os.getcwd(), self.model.name)

            # Verify initial state.
            self.assertEqual(SOURCE_INIT, False)
            self.assertEqual(SINK_INIT, False)
            self.assertNotEqual(self.model.Sink.text_data,
                                self.model.Source.text_data)
            self.assertNotEqual(self.model.Sink.binary_data,
                                self.model.Source.sub.binary_data)
            self.assertNotEqual(
                self.model.Sink.binary_file.binary, True)

            for path in EXTERNAL_FILES:
                path = os.path.join(self.model.Source.get_directory(), path)
                self.assertEqual(os.path.exists(path), True)

            for i in range(3):
                self.assertEqual(self.model.Source.obj_list[i].data, i)

            self.assertEqual(self.model.Oddball.executions, 0)

            # Run and verify correct operation.
            self.model.run()
            self.assertEqual(self.model.Sink.text_data,
                             self.model.Source.text_data)
            self.assertEqual(all(self.model.Sink.binary_data==
                             self.model.Source.sub.binary_data), True)
            self.assertEqual(
                self.model.Sink.binary_file.binary, True)

            self.assertEqual(self.model.Oddball.executions, 2)

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)

    def test_save_load_cpickle(self):
        logging.debug('')
        logging.debug('test_save_load_cpickle')
        self.save_load(SAVE_CPICKLE)

    def test_save_load_pickle(self):
        logging.debug('')
        logging.debug('test_save_load_pickle')
        self.save_load(SAVE_PICKLE)

# Fails to load. It appears you can't have more than one level of
# back-pointers when loading YAML. (A component works, but an assembly doesn't)
#    def test_save_load_yaml(self):
#        logging.debug('')
#        logging.debug('test_save_load_yaml')
#        self.save_load(SAVE_LIBYAML)

    def test_save_with_setuptools(self):
        logging.debug('')
        logging.debug('test_save_with_setuptools')
        self.save_load(SAVE_CPICKLE, use_setuptools=True)

    def test_save_bad_directory(self):
        logging.debug('')
        logging.debug('test_save_bad_directory')
        self.model.Oddball.directory = os.getcwd()
        try:
            self.model.save_to_egg(py_dir=PY_DIR)
        except Exception, exc:
            msg = "Egg_TestModel: Can't save, Egg_TestModel.Oddball.oddcomp" \
                  " directory"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected Exception')

    def test_save_bad_destination(self):
        logging.debug('')
        logging.debug('test_save_bad_destination')
        try:
            self.model.save_to_egg(py_dir=PY_DIR, dst_dir='/')
        except IOError, exc:
            msg = "Egg_TestModel: Can't save to '/', no write permission"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected IOError')

    def test_save_bad_external(self):
        logging.debug('')
        logging.debug('test_save_bad_external')

        path = os.path.join(os.getcwd(), 'bad-external')
        out = open(path, 'w')
        out.close()
        metadata = self.model.Source.external_files[0]
        metadata['path'] = path
        try:
            self.model.save_to_egg(py_dir=PY_DIR)
        except Exception, exc:
            msg = "Egg_TestModel: Can't save, Egg_TestModel.Source file"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected Exception')
        finally:
            os.remove(path)

    def test_save_bad_filevar(self):
        logging.debug('')
        logging.debug('test_save_bad_filevar')

        path = os.path.join(os.getcwd(), 'bad-file-variable')
        out = open(path, 'w')
        out.close()
        self.model.Source.text_file.filename = path
        try:
            self.model.save_to_egg(py_dir=PY_DIR)
        except Exception, exc:
            msg = "Egg_TestModel: Can't save, Egg_TestModel.Source.text_file" \
                  " path"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected Exception')
        finally:
            os.remove(path)

    def test_save_bad_format(self):
        logging.debug('')
        logging.debug('test_save_bad_format')
        try:
            self.model.save_to_egg(py_dir=PY_DIR, format='unknown')
        except RuntimeError, exc:
            self.assertEqual(str(exc),
                             "Egg_TestModel: Unknown format 'unknown'.")
        else:
            self.fail('Expected RuntimeError')

    def test_save_bad_function(self):
        logging.debug('')
        logging.debug('test_save_bad_function')
        self.model.Oddball.function_socket = main_function
        try:
            self.model.save_to_egg(py_dir=PY_DIR)
        except RuntimeError, exc:
            msg = "Egg_TestModel: Can't save: reference to function defined" \
                  " in main module"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            if MODULE_NAME == '__main__':
                self.fail('Expected RuntimeError')

    def test_save_bad_method(self):
        logging.debug('')
        logging.debug('test_save_bad_method')
        self.model.Oddball.method_socket = self.model.Oddball.static_method
        try:
            self.model.save_to_egg(py_dir=PY_DIR)
        except RuntimeError, exc:
            self.assertEqual(str(exc),
                "Egg_TestModel: Can't save, 1 object cannot be pickled.")
        else:
            self.fail('Expected RuntimeError')

    def test_save_bad_tuple(self):
        logging.debug('')
        logging.debug('test_save_bad_tuple')
        self.model.Oddball.scratch_tuple = (self.model.Oddball.instance_method,)
        try:
            self.model.save_to_egg(py_dir=PY_DIR)
        except RuntimeError, exc:
            msg = 'Egg_TestModel: _fix_im_recurse: tuple'
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected RuntimeError')

    def test_save_bad_pickle(self):
        logging.debug('')
        logging.debug('test_save_bad_pickle')

        # Code objects don't pickle.
        self.model.code = compile('3 + 4', '<string>', 'eval')

        # Problem was deletion of existing buildout.cfg.
        if os.path.exists('buildout.cfg'):
            buildout_size = os.path.getsize('buildout.cfg')
            remove_buildout = False
        else:
            out = open('buildout.cfg', 'w')
            out.close()
            buildout_size = 0
            remove_buildout = True

        try:
            try:
                self.model.save_to_egg(py_dir=PY_DIR)
            except cPickle.PicklingError, exc:
                msg = "Egg_TestModel: Can't save to" \
                      " 'Egg_TestModel/Egg_TestModel.pickle': Can't pickle" \
                      " <type 'code'>: attribute lookup __builtin__.code failed"
                self.assertEqual(str(exc), msg)
            else:
                self.fail('Expected cPickle.PicklingError')

            self.assertTrue(os.path.exists('buildout.cfg'))
            self.assertEqual(os.path.getsize('buildout.cfg'), buildout_size)
        finally:
            if remove_buildout:
                os.remove('buildout.cfg')
    def test_save_bad_child(self):
        logging.debug('')
        logging.debug('test_save_bad_child')

        orphan = Component('orphan')
        try:
            self.model.save_to_egg(py_dir=PY_DIR, child_objs=[orphan])
        except RuntimeError, exc:
            self.assertEqual(str(exc), 'Entry point object has no parent!')
        else:
            self.fail('Expected RuntimeError')

        badboy = Component('badboy', orphan)
        try:
            self.model.save_to_egg(py_dir=PY_DIR, child_objs=[badboy])
        except RuntimeError, exc:
            msg = 'Egg_TestModel: orphan.badboy is not a child of' \
                  ' Egg_TestModel.'
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected RuntimeError')

    def test_save_load_container(self):
        logging.debug('')
        logging.debug('test_save_load_container')

        # Save to egg.
        self.egg_name = self.model.Source.sub.save_to_egg(py_dir=PY_DIR)

        # Restore in test directory.
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            egg_path = os.path.join('..', self.egg_name)
            sub = Container.load_from_eggfile(egg_path, install=False)
            self.assertTrue(all(sub.binary_data == self.model.Source.sub.binary_data))
        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)

    def test_load_badfile(self):
        logging.debug('')
        logging.debug('test_load_badfile')
        try:
            Component.load_from_eggfile('.')
        except ValueError, exc:
            self.assertEqual(str(exc), "'.' is not an egg/zipfile.")
        else:
            self.fail('Expected ValueError')

    def test_load_nofile(self):
        logging.debug('')
        logging.debug('test_load_nofile')
        try:
            Component.load_from_eggfile('no-such-egg')
        except ValueError, exc:
            self.assertEqual(str(exc), "'no-such-egg' not found.")
        else:
            self.fail('Expected ValueError')

    def test_load_nopackage(self):
        logging.debug('')
        logging.debug('test_load_nopackage')
        try:
            Component.load_from_eggpkg('no-such-egg')
        except pkg_resources.DistributionNotFound, exc:
            self.assertEqual(str(exc), 'no-such-egg')
        else:
            self.fail('Expected pkg_resources.DistributionNotFound')

    def test_check_save_load(self):
        logging.debug('')
        logging.debug('test_check_save_load')

        python = openmdao.util.testutil.find_python('openmdao.main')
        retcode = self.model.check_save_load(py_dir=PY_DIR, python=python)
        self.assertEqual(retcode, 0)

    def test_install_load(self):
        logging.debug('')
        logging.debug('test_install_load')

        python = openmdao.util.testutil.find_python('openmdao.main')
        logging.debug('    Using python: %s' % python)

        # Write to egg.
        self.egg_name = self.model.save_to_egg(py_dir=PY_DIR,
                                               child_objs=self.child_objs)

        install_dir = os.path.join(os.getcwd(), 'install_dir')
        if os.path.exists(install_dir):
            shutil.rmtree(install_dir)
        os.mkdir(install_dir)
        try:
            # Create special installer script.
            installer = os.path.join(install_dir, 'installer.py')
            out = open(installer, 'w')
            out.write("""\
# EASY-INSTALL-ENTRY-SCRIPT: 'setuptools>=0.6c8','console_scripts','easy_install'
__requires__ = 'setuptools>=0.6c8'
import sys
from pkg_resources import load_entry_point

sys.exit(
   load_entry_point('setuptools>=0.6c8', 'console_scripts', 'easy_install')()
)
""")
            out.close()
            os.chmod(installer, 0755)

            # Install via subprocess with PYTHONPATH set (for easy_install).
            logging.debug('Installing via subprocess...')
            env = os.environ
            path = env.get('PYTHONPATH', '')
            if path:
                path += os.pathsep
            path += install_dir
            env['PYTHONPATH'] = path
            cmdline = '%s %s -d %s %s' % \
                      (python, installer, install_dir, self.egg_name)
            stdout = open(os.path.join(install_dir, 'installer.out'), 'w')
            retcode = subprocess.call(cmdline, env=env, shell=True,
                                      stdout=stdout, stderr=subprocess.STDOUT)
            stdout.close()
            stdout = open(os.path.join(install_dir, 'installer.out'), 'r')
            for line in stdout:
                logging.debug('    %s', line.rstrip())
            stdout.close()
            self.assertEqual(retcode, 0)

            # Load full model and run.
            package_name = self.model.name
            entry_name = ''
            retcode = self.load_n_run(python, install_dir,
                                      package_name, entry_name)
            self.assertEqual(retcode, 0)

            # Load just the Oddball component and run.
            entry_name = self.model.Oddball.get_pathname()
            retcode = self.load_n_run(python, install_dir,
                                      package_name, entry_name)
            self.assertEqual(retcode, 0)

            # Try a non-existent package.
            try:
                obj = Component.load_from_eggpkg('no-such-pkg', 'no-such-entry')
            except pkg_resources.DistributionNotFound, exc:
                self.assertEqual(str(exc), 'no-such-pkg')
            else:
                self.fail('Expected DistributionNotFound')
            
            # Try a non-existent entry point.
            egg_path = os.path.join(install_dir, self.egg_name)
            sys.path.append(egg_path)
            orig_ws = pkg_resources.working_set
            pkg_resources.working_set = pkg_resources.WorkingSet()
            try:
                obj = Component.load_from_eggpkg(package_name, 'no-such-entry')
            except RuntimeError, exc:
                msg = "No 'openmdao.components' 'no-such-entry' entry point."
                self.assertEqual(str(exc), msg)
            else:
                self.fail('Expected RuntimeError')
            finally:
                sys.path.pop()
                pkg_resources.working_set = orig_ws

        finally:
            shutil.rmtree(install_dir)

    def load_n_run(self, python, install_dir, package_name, entry_name):
        """ Load component from installed egg and run it. """
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            # Create load-n-run script.
            out = open('load-n-run.py', 'w')
            out.write("""\
import sys
sys.path.append('%(egg)s')
from openmdao.main.api import Component
import openmdao.main.log
openmdao.main.log.enable_console()
try:
    comp = Component.load_from_eggpkg('%(package)s', '%(entry)s')
    comp.run()
except Exception, err:
    openmdao.main.log.logger.exception(str(err))
    raise
    
""" % {'egg':os.path.join(install_dir, self.egg_name),
       'package':package_name, 'entry':entry_name})
            out.close()

            # Load & run in subprocess.
            logging.debug("Load and run '%s' in subprocess...", entry_name)
            cmdline = '%s load-n-run.py' % python
            stdout = open('load-n-run.out', 'w')
            retcode = subprocess.call(cmdline, shell=True, stdout=stdout,
                                      stderr=subprocess.STDOUT)
            stdout.close()
            stdout = open('load-n-run.out', 'r')
            for line in stdout:
                logging.debug('    %s'% line.rstrip())
            stdout.close()
            return retcode

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)


    def test_pkg_resources_factory(self):
        logging.debug('')
        logging.debug('test_pkg_resources_factory')

        # Write to egg.
        self.egg_name = self.model.save_to_egg(py_dir=PY_DIR,
                                               child_objs=self.child_objs)
        # Create factory.
        factory = PkgResourcesFactory([os.getcwd()], ['openmdao.components'])
        logging.debug('    loaders:')
        for key, value in factory._loaders.items():
            logging.debug('        %s:', key)
            for val in value:
                logging.debug('                name: %s', val.name)
                logging.debug('               group: %s', val.group)
                logging.debug('                dist: %s', val.dist)
                logging.debug('            entry_pt: %s', val.entry_pt)
                logging.debug('                ctor: %s', val.ctor)
                logging.debug('')

        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            # Check multiple model instances.
            self.create_and_check_model(factory, 'test_model_1',
                                        'Hello world!\n')
            self.create_and_check_model(factory, 'test_model_2',
                                        'Hello world!\n')

            # Check that reloading doesn't clobber existing files.
            file_data = 'New and interesting stuff\n'
            path = os.path.join('test_model_2', 'Source', EXTERNAL_FILES[2])
            out = open(path, 'w')
            out.write(file_data)
            out.close()
            self.create_and_check_model(factory, 'test_model_2', file_data)

            # Create a component.
            comp = factory.create('Egg_TestModel.Oddball', 'test_comp')
            if comp is None:
                self.fail('Create of test_comp failed.')
            self.assertEqual(comp.get_pathname(), 'test_comp')
            self.assertEqual(comp.executions, 0)
            comp.run()
            self.assertEqual(comp.executions, 2)
            # Create a (sub)component.
            sub = factory.create('Egg_TestModel.Oddball.oddcomp', 'test_sub')
            if sub is None:
                self.fail('Create of test_sub failed.')
            self.assertEqual(sub.get_pathname(), 'test_sub')

            # Create a (sub)container.
            sub = factory.create('Egg_TestModel.Oddball.oddcont', 'test_sub')
            if sub is None:
                self.fail('Create of test_sub failed.')
            self.assertEqual(sub.get_pathname(), 'test_sub')

            # Try a non-existent entry point.
            obj = factory.create('no-such-entry', 'xyzzy')
            self.assertEqual(obj, None)

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)

    def create_and_check_model(self, factory, name, file_data):
        """ Create a complete model instance and check it's operation. """
        model = factory.create('Egg_TestModel', name)
        if model is None:
            self.fail("Create of '%s' failed." % name)
        self.assertEqual(model.directory, os.path.join(os.getcwd(), name))
        self.assertEqual(model.Oddball.get_pathname(), name+'.Oddball')

        # Verify initial state.
        self.assertNotEqual(model.Sink.text_data,
                            model.Source.text_data)
        self.assertNotEqual(model.Sink.binary_data,
                            model.Source.sub.binary_data)
        self.assertNotEqual(model.Sink.binary_file.binary, True)

        orig_dir = os.getcwd()
        os.chdir(model.Source.get_directory())
        try:
            for path in EXTERNAL_FILES:
                self.assertEqual(os.path.exists(path), True)

            inp = open(EXTERNAL_FILES[2])
            data = inp.read()
            inp.close()
            self.assertEqual(data, file_data)
        finally:
            os.chdir(orig_dir)

        for i in range(3):
            self.assertEqual(model.Source.obj_list[i].data, i)

        self.assertEqual(model.Oddball.executions, 0)

        # Run and verify correct operation.
        model.run()
        self.assertEqual(model.Sink.text_data,
                         model.Source.text_data)
        self.assertEqual(all(model.Sink.binary_data==model.Source.sub.binary_data),
                         True)
        
        self.assertEqual(model.Sink.binary_file.binary, True)

        self.assertEqual(model.Oddball.executions, 2)


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

