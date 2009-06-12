"""
Test saving and loading of simulations as eggs.
"""

import cPickle
import logging
import os
import shutil
import unittest

from openmdao.main import Assembly, Component, Container, \
                          ArrayVariable, FileVariable, StringList, Bool
from openmdao.main.constants import SAVE_CPICKLE, SAVE_LIBYAML
from openmdao.main.variable import INPUT, OUTPUT

# pylint: disable-msg=E1101,E1103
# "Instance of <class> has no <attr> member"

__version__ = '1.2.3'  # Used in forming egg name.

EXTERNAL_FILES = ('xyzzy', '../sub/data2', 'hello', '../sub/data4')

source_init = False
sink_init = False


class Source(Assembly):
    """ Produces files. """

    def __init__(self, name='Source', *args, **kwargs):
        super(Source, self).__init__(name, *args, **kwargs)
        self.directory = self.get_directory()  # Force absolute.

        global source_init
        source_init = True

        Bool('write_files', self, INPUT, default=True)
        StringList('text_data', self, INPUT, default=[])
        FileVariable('text_file', self, OUTPUT, default='source.txt')

        Subcontainer('sub', parent=self)
        self.create_passthru('sub.binary_file')

        # Some objects that must be restored.
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

        global sink_init
        sink_init = True

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

    def execute(self):
        """ Read test data from files. """
        inp = open(self.text_file, 'r')
        self.text_data = inp.read()
        inp.close()

        inp = open(self.binary_file, 'rb')
        self.binary_data = cPickle.load(inp)
        inp.close()


class Oddball(Component):
    """ Just a component that needs a separate directory to be created. """

    def __init__(self, name='Oddball', *args, **kwargs):
        super(Oddball, self).__init__(name, *args, **kwargs)


class Model(Assembly):
    """ Transfer files from producer to consumer. """

    def __init__(self, name='Egg_TestModel', *args, **kwargs):
        super(Model, self).__init__(name, *args, **kwargs)

        Source(parent=self, directory='Source')
        Oddball(parent=self, directory='Oddball')
        Sink(parent=self, directory='Sink')

        self.connect('Source.text_file', 'Sink.text_file')
        #self.connect('Source.sub.binary_file', 'Sink.binary_file')
        self.connect('Source.binary_file', 'Sink.binary_file')

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

    def save_load(self, format):
        global source_init, sink_init

        # Verify initial state.
        self.assertEqual(source_init, True)
        self.assertEqual(sink_init, True)
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

        # Save to egg.
        self.egg_name = self.model.save_to_egg(format=format)

        # Run and verify correct operation.
        self.model.run()
        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(self.model.Sink.binary_data,
                         self.model.Source.sub.binary_data)
        self.assertEqual(
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

        # Restore in test directory.
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            # Clear flags to detect if loading calls __init__.
            source_init = False
            sink_init = False

            # Load from saved initial state in egg.
            self.model.pre_delete()
            self.model = Component.load_from_egg(os.path.join('..',
                                                              self.egg_name),
                                                 install=False)
            self.model.directory = os.path.join(os.getcwd(), self.model.name)

            # Verify initial state.
            self.assertEqual(source_init, False)
            self.assertEqual(sink_init, False)
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

            # Run and verify correct operation.
            self.model.run()
            self.assertEqual(self.model.Sink.text_data,
                             self.model.Source.text_data)
            self.assertEqual(self.model.Sink.binary_data,
                             self.model.Source.sub.binary_data)
            self.assertEqual(
                self.model.Sink.getvar('binary_file').metadata['binary'], True)

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)

    def test_save_load_pickle(self):
        logging.debug('')
        logging.debug('test_save_load_pickle')
        self.save_load(SAVE_CPICKLE)

# Fails to load. It appears you can't have more than one level of
# back-pointers when loading YAML. (A component works, but an assembly doesn't)
#    def test_save_load_yaml(self):
#        logging.debug('')
#        logging.debug('test_save_load_yaml')
#        self.save_load(SAVE_LIBYAML)

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

    def test_load_nofile(self):
        logging.debug('')
        logging.debug('test_load_nofile')

        try:
            Component.load_from_egg('no-such-egg')
        except ValueError, exc:
            self.assertEqual(str(exc), "'no-such-egg' not found.")
        else:
            self.fail('Expected ValueError')


if __name__ == '__main__':
    unittest.main()

