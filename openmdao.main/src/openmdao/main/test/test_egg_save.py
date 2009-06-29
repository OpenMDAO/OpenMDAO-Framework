"""
Test saving and loading of simulations as eggs.
"""

import cPickle
import logging
import os
import shutil
import unittest

from openmdao.main import Assembly, Component, Container, \
                          ArrayVariable, Int, FileVariable, StringList, Bool
from openmdao.main.constants import SAVE_PICKLE, SAVE_CPICKLE, SAVE_LIBYAML
from openmdao.main.socket import Socket
from openmdao.main.variable import INPUT, OUTPUT

# pylint: disable-msg=E1101,E1103
# "Instance of <class> has no <attr> member"

__version__ = '1.2.3'  # Used in forming egg name.

EXTERNAL_FILES = ('xyzzy', '../sub/data2', 'hello', '../sub/data4')

SOURCE_INIT = False
SINK_INIT = False

MODULE_NAME = __name__


class Source(Assembly):
    """ Produces files. """

    def __init__(self, name='Source', *args, **kwargs):
        super(Source, self).__init__(name, *args, **kwargs)
        self.directory = self.get_directory()  # Force absolute.

        global SOURCE_INIT
        SOURCE_INIT = True

        Bool('write_files', self, INPUT, default=True)
        StringList('text_data', self, INPUT, default=[])
        FileVariable('text_file', self, OUTPUT, default='source.txt')

        Subcontainer('sub', parent=self)
        self.create_passthru('sub.binary_file')

        # Some custom objects that must be restored.
        self.obj_list = [DataObj(i) for i in range(3)]

        # Absolute external file that exists at time of save.
        path = os.path.join(self.directory, EXTERNAL_FILES[0])
        out = open(path, 'w')
        out.write('Twisty narrow passages.\n')
        out.close()
        self.external_files.append({'path':path})

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
            out = open(self.text_file, 'w')
            out.write(self.text_data)
            out.close()

            out = open(self.sub.binary_file, 'wb')
            cPickle.dump(self.sub.binary_data, out, 2)
            out.close()


class Subcontainer(Container):
    """ Just a subcontainer for Source. """

    def __init__(self, name='Subcontainer', parent=None):
        super(Subcontainer, self).__init__(name, parent)

        ArrayVariable('binary_data', self, INPUT, float, default=[])
        FileVariable('binary_file', self, OUTPUT,
                     default=os.path.join('..', 'sub', 'source.bin'),
                     metadata={'binary':True})


class DataObj(object):
    """ Just a custom class for objects to save & reload. """

    def __init__(self, data):
        self.data = data


class Sink(Component):
    """ Consumes files. """

    def __init__(self, name='Sink', *args, **kwargs):
        super(Sink, self).__init__(name, *args, **kwargs)

        global SINK_INIT
        SINK_INIT = True

        StringList('text_data', self, OUTPUT, default=[])
        ArrayVariable('binary_data', self, OUTPUT, float, default=[])

        # Absolute FileVariable that exists at time of save.
        FileVariable('text_file', self, INPUT,
                     default=os.path.join(self.get_directory(), 'sink.txt'))
        out = open(self.text_file, 'w')
        out.write('Absolute FileVariable that exists at time of save.\n')
        out.close()

        # Relative FileVariable that exists at time of save.
        FileVariable('binary_file', self, INPUT, default='sink.bin')
        self.push_dir(self.get_directory())
        out = open(self.binary_file, 'w')
        out.write('Relative FileVariable that exists at time of save.\n')
        out.close()
        self.pop_dir()

        Int('executions', self, INPUT, default=0,
            doc='Count of Oddball instance_method() calls.')

    def execute(self):
        """ Read test data from files. """
        inp = open(self.text_file, 'r')
        self.text_data = inp.read()
        inp.close()

        inp = open(self.binary_file, 'rb')
        self.binary_data = cPickle.load(inp)
        inp.close()


class Oddball(Assembly):
    """ Just a component that needs a separate directory to be created. """

    function_socket = Socket(None, 'Just something to call.', False)
    method_socket = Socket(None, 'Just something to call.', False)

    def __init__(self, name='Oddball', *args, **kwargs):
        super(Oddball, self).__init__(name, *args, **kwargs)
        Int('executions', self, OUTPUT, default=0,
            doc='Counts instance_method() calls.')
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
        self.egg_name = None

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        self.model = None
        if self.egg_name:
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
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

        for path in EXTERNAL_FILES:
            path = os.path.join(self.model.Source.get_directory(), path)
            self.assertEqual(os.path.exists(path), True)

        for i in range(3):
            self.assertEqual(self.model.Source.obj_list[i].data, i)

        self.assertEqual(self.model.Sink.executions, 0)

        # Save to egg.
        self.egg_name = self.model.save_to_egg(format=format,
                                               use_setuptools=use_setuptools)

        # Run and verify correct operation.
        self.model.run()
        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(self.model.Sink.binary_data,
                         self.model.Source.sub.binary_data)
        self.assertEqual(
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

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
            self.model = Component.load_from_egg(os.path.join('..',
                                                              self.egg_name),
                                                 install=False)
            self.model.directory = os.path.join(os.getcwd(), self.model.name)

            # Verify initial state.
            self.assertEqual(SOURCE_INIT, False)
            self.assertEqual(SINK_INIT, False)
            self.assertNotEqual(self.model.Sink.text_data,
                                self.model.Source.text_data)
            self.assertNotEqual(self.model.Sink.binary_data,
                                self.model.Source.sub.binary_data)
            self.assertNotEqual(
                self.model.Sink.getvar('binary_file').metadata['binary'], True)

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
            self.assertEqual(self.model.Sink.binary_data,
                             self.model.Source.sub.binary_data)
            self.assertEqual(
                self.model.Sink.getvar('binary_file').metadata['binary'], True)

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
            self.model.save_to_egg()
        except ValueError, exc:
            msg = "Egg_TestModel: Can't save, Egg_TestModel.Oddball directory"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')

    def test_save_bad_destination(self):
        logging.debug('')
        logging.debug('test_save_bad_destination')
        try:
            self.model.save_to_egg(dst_dir='/')
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
            self.model.save_to_egg()
        except ValueError, exc:
            msg = "Egg_TestModel: Can't save, Egg_TestModel.Source file"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')
        finally:
            os.remove(path)

    def test_save_bad_filevar(self):
        logging.debug('')
        logging.debug('test_save_bad_filevar')

        path = os.path.join(os.getcwd(), 'bad-file-variable')
        out = open(path, 'w')
        out.close()
        self.model.Source.text_file = path
        try:
            self.model.save_to_egg()
        except ValueError, exc:
            msg = "Egg_TestModel: Can't save, Egg_TestModel.Source.text_file path"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')
        finally:
            os.remove(path)

    def test_save_bad_format(self):
        logging.debug('')
        logging.debug('test_save_bad_format')
        try:
            self.model.save_to_egg(format='unknown')
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
            self.model.save_to_egg()
        except RuntimeError, exc:
            msg = "Egg_TestModel: Can't save: reference to function defined in main module"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            if MODULE_NAME == '__main__':
                self.fail('Expected RuntimeError')

    def test_save_bad_method(self):
        logging.debug('')
        logging.debug('test_save_bad_method')
        self.model.Oddball.method_socket = self.model.Oddball.static_method
        try:
            self.model.save_to_egg()
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
            self.model.save_to_egg()
        except RuntimeError, exc:
            msg = 'Egg_TestModel: _fix_im_recurse: tuple'
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected RuntimeError')

    def test_save_load_container(self):
        logging.debug('')
        logging.debug('test_save_load_container')

        # Save to egg.
        self.egg_name = self.model.Source.sub.save_to_egg()

        # Restore in test directory.
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            sub = Container.load_from_egg(os.path.join('..', self.egg_name),
                                          install=False)
            self.assertEqual(sub.binary_data, self.model.Source.sub.binary_data)
        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)

    def test_load_badfile(self):
        logging.debug('')
        logging.debug('test_load_badfile')
        try:
            Component.load_from_egg('.')
        except ValueError, exc:
            self.assertEqual(str(exc), "'.' is not an egg/zipfile.")
        else:
            self.fail('Expected ValueError')

    def test_load_nofile(self):
        logging.debug('')
        logging.debug('test_load_nofile')
        try:
            Component.load_from_egg('no-such-egg')
        except ValueError, exc:
            self.assertEqual(str(exc), "'no-such-egg' not found.")
        else:
            self.fail('Expected ValueError')

    def test_check_save_load(self):
        # This requires the correct pythonV.R command in PATH.
        logging.debug('')
        logging.debug('test_check_save_load')
        retcode = self.model.check_save_load()
        self.assertEqual(retcode, 0)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

