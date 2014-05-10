"""
Test saving and loading of simulations as eggs.
"""

import cPickle
import glob
import logging
import os.path
import pkg_resources
import shutil
import subprocess
import sys
import unittest
import nose

from openmdao.main.api import Assembly, Component, Container, VariableTree, set_as_top
from openmdao.main.file_supp import FileMetadata

from openmdao.main.pkg_res_factory import PkgResourcesFactory

from openmdao.main.eggchecker import check_save_load
from openmdao.main.datatypes.api import Int, Bool, Str, Array, File, FileRef, VarTree
from openmdao.util.testutil import assert_raises, find_python, \
                                   make_protected_dir
from openmdao.util.fileutil import onerror

# pylint: disable-msg=E1101,E1103
# "Instance of <class> has no <attr> member"

EXTERNAL_FILES = ('xyzzy', '../sub/data2', 'hello', '../sub/data4')

SOURCE_INIT = False  # Used to verify __init__() gets called when expected.
SINK_INIT = False

# Various Pickle issues arise only when this test runs as the main module.
# This is used to detect when we're the main module or not.
MODULE_NAME = __name__

# Set local dir in case we're running in a different directory.
PY_DIR = pkg_resources.resource_filename('openmdao.main', 'test')

# Observations made by observer().
OBSERVATIONS = []

# Version counter to ensure we know which egg we're dealing with.
EGG_VERSION = 0


def next_egg():
    """ Return next egg version. """
    global EGG_VERSION
    EGG_VERSION += 1
    return str(EGG_VERSION)


class Subcontainer(VariableTree):
    """ Just a subcontainer for Source. """

    binary_data = Array(dtype='d', iotype='in')
    binary_file = File(iotype='out')


class Source(Component):
    """
    Produces files. A fair amount of stuff happens in Component.save_to_egg()
    in relation to handling external files and file variables.
    """

    write_files = Bool(True, iotype='in')
    text_data = Str(iotype='in')
    text_file = File(iotype='out')
    sub = VarTree(Subcontainer(), iotype='out')

    def __init__(self, *args, **kwargs):
        super(Source, self).__init__(*args, **kwargs)

        global SOURCE_INIT
        SOURCE_INIT = True

    def configure(self):
        """ Called once we have a valid hierarchy above us. """
        self.add('sub', Subcontainer())

        # Some custom objects that must be restored.
        self.obj_list = [DataObj(i) for i in range(3)]

        # External file that doesn't exist at time of save.
        self.external_files.append(FileMetadata(path='does-not-exist'))

        self.directory = self.get_abs_directory()  # Force absolute.
        # Absolute external file that exists at time of save.
        path = os.path.join(self.directory, EXTERNAL_FILES[0])
        with open(path, 'w') as out:
            out.write('Twisty narrow passages.\n')
        self.external_files.append(FileMetadata(path=path, input=True,
                                   constant=True))

        # Absolute external file that exists at time of save, in separate tree.
        path = os.path.join(self.directory, EXTERNAL_FILES[1])
        leaf = os.path.dirname(path)
        if not os.path.exists(leaf):
            os.makedirs(leaf)
        with open(path, 'w') as out:
            out.write('Some external data.\n')
        self.external_files.append(FileMetadata(path=path))

        # Relative external file that exists at time of save.
        with self.dir_context:
            path = EXTERNAL_FILES[2]
            with open(path, 'w') as out:
                out.write('Hello world!\n')
        self.external_files.append(FileMetadata(path=path))

        # Relative external file that exists at time of save, in separate tree.
        with self.dir_context:
            path = EXTERNAL_FILES[3]
            leaf = os.path.dirname(path)
            if not os.path.exists(leaf):
                os.makedirs(leaf)
            with open(path, 'w') as out:
                out.write('Some more external data.\n')
        self.external_files.append(FileMetadata(path=path))

    def execute(self):
        """ Write test data to files. """
        if self.write_files:
            cwd = os.getcwd()

            path = 'source.txt'
            self._logger.debug("opening file '%s' in %s", path, cwd)
            with open(path, 'w') as out:
                out.write(self.text_data)
            self.text_file = FileRef(path)

            path = os.path.join('..', 'sub', 'source.bin')
            self._logger.debug("opening file '%s' in %s", path, cwd)
            with open(path, 'wb') as out:
                cPickle.dump(self.sub.binary_data, out, 2)
            self.sub.binary_file = FileRef(path, binary=True)



class DataObj(object):
    """ Just a custom class for objects to save & reload. """

    def __init__(self, data):
        self.data = data


class Sink(Component):
    """ Consumes files. """

    text_data = Str(iotype='out')
    binary_data = Array(dtype='d', iotype='out')
    text_file = File(iotype='in')
    binary_file = File(iotype='in')
    executions = Int(0, iotype='in',
                     desc='Count of Oddball instance_method() calls.')

    def __init__(self, *args, **kwargs):
        super(Sink, self).__init__(*args, **kwargs)
        global SINK_INIT
        SINK_INIT = True

    def execute(self):
        """ Read test data from files. """
        with self.text_file.open() as inp:
            self.text_data = inp.read()

        with self.binary_file.open() as inp:
            self.binary_data = cPickle.load(inp)


class Oddball(Assembly):
    """
    Just a component that needs a separate directory to be created.
    Also has some attributes used to exercise some Pickle issues.
    """

    # FIXME: I tried the built-in trait types of Callable, Method, and Function
    # for these two sockets and couldn't get them to work.  We may have to
    # create new Variables for these...
    #function_socket = Slot(Callable, none_allowed=True,
    #                           desc='Just something to call.', required=False)
    #method_socket = Slot(Callable, none_allowed=True,
    #                         desc='Just something to call.', required=False)
    executions = Int(0, iotype='out', desc='Counts instance_method() calls.')

    def configure(self):
        self.add('oddcomp', OddballComponent())
        self.add('oddcont', OddballContainer())
        self.driver.workflow.add('oddcomp')
        self.thing_to_call = self.instance_method
        self.list_to_call = [[self.instance_method, ()],
                             [Assembly.get_pathname, (self,)]]
                             # Generate IMHolder with self == None.
        self.function_socket = os.getpid
        self.method_socket = self.instance_method
        self.peer_class = Source  # Check that class in __main__ is handled.
        self.scratch_tuple = (1, 2)

    def execute(self):
        """ Call stuff. Empty sockets are clumsy. """
        if self.thing_to_call:
            self._logger.debug('thing_to_call returned %s', self.thing_to_call())

        for thing, args in self.list_to_call:
            self._logger.debug('list-thing returned %s', thing(*args))

        try:
            self._logger.debug('function_socket returned %s', self.function_socket())
        except RuntimeError, exc:
            if not str(exc).find('empty'):
                raise exc
        try:
            self._logger.debug('method_socket returned %s', self.method_socket())
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

    def __init__(self):
        super(OddballComponent, self).__init__()
        # Some custom objects that must be restored.
        self.obj_list = [DataObj(i) for i in range(3)]


class OddballContainer(Container):
    """ Just a subcontainer for Oddball to test nested entry points. """

    def __init__(self):
        super(OddballContainer, self).__init__()
        # Some custom objects that must be restored.
        self.obj_list = [DataObj(i) for i in range(3)]


def local_getpid():
    """ Example of function defined in __main__. """
    return os.getpid()


def observer(state, string, file_fraction, byte_fraction):
    """ Observe progress. """
    if state != 'analyze':  # 'analyze' is sporadic due to re-use of analyses.
        OBSERVATIONS.append((state, string, file_fraction, byte_fraction))
    return True


class Model(Assembly):
    """ Transfer files from producer to consumer. """

    def configure(self):
        self.directory = 'Egg'
        comp = Source()
        comp.directory = 'Source'
        comp = self.add('Source', comp)
        comp = Oddball()
        comp.directory = 'Oddball'
        comp = self.add('Oddball', comp)
        comp = Sink()
        comp.directory = 'Sink'
        comp = self.add('Sink', comp)

        self.driver.workflow.add(['Source', 'Oddball', 'Sink'])

        self.connect('Source.text_file', 'Sink.text_file')
        self.connect('Source.sub.binary_file', 'Sink.binary_file')

        self.connect('Oddball.executions', 'Sink.executions')

        self.Source.text_data = 'oiuyoiuyoiuy'
        self.Source.sub.binary_data = [3.14159, 2.781828, 42]


class TestCase(unittest.TestCase):
    """ Test saving and loading of simulations as eggs. """

    def setUp(self):
        """ Called before each test in this class. """
        self.model = set_as_top(Model())
        self.model.name = 'Egg_TestModel'
        self.child_objs = [self.model.Source, self.model.Sink,
                           self.model.Oddball, self.model.Oddball.oddcomp,
                           self.model.Oddball.oddcont]
        self.egg_name = None

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()  # Paranoia.  Only needed by NPSS I think.
        self.model = None
        for path in glob.glob('Egg_TestModel*.egg'):
            os.remove(path)

        if os.path.exists('Egg'):
            # Wonderful Windows sometimes doesn't remove...
            shutil.rmtree('Egg', onerror=self.onerror)

        if os.path.exists('Oddball'):
            # Wonderful Windows sometimes doesn't remove...
            shutil.rmtree('Oddball', onerror=self.onerror)

        # Not always added, but we need to ensure the egg is not in sys.path.
        if self.egg_name is not None:
            for i, path in enumerate(sys.path):
                if path.endswith(self.egg_name):
                    del sys.path[i]
                    break

    def onerror(self, function, path, excinfo):
        """ Called by shutil.rmtree() if 'Egg' tree removal has problems. """
        logging.error('onerror: function %s, path %s, excinfo %s',
                      function, path, excinfo)
        if function == os.rmdir:
            # On Windows, sometimes get 'Directory not empty'.
            logging.error('    files: %s', os.listdir(path))

    def save_load(self):
        """ Save to egg and reload. """
        global SOURCE_INIT, SINK_INIT

        # Verify initial state.
        self.assertEqual(SOURCE_INIT, True)
        self.assertEqual(SINK_INIT, True)
        self.assertNotEqual(self.model.Sink.text_data,
                            self.model.Source.text_data)
        self.assertNotEqual(self.model.Sink.binary_data,
                            self.model.Source.sub.binary_data)

        for path in EXTERNAL_FILES:
            path = os.path.join(self.model.Source.get_abs_directory(), path)
            if not os.path.exists(path):
                self.fail("pre-save path '%s' does not exist" % path)

        for i in range(3):
            self.assertEqual(self.model.Source.obj_list[i].data, i)

        self.assertEqual(self.model.Sink.executions, 0)

        # Save to egg.
        global OBSERVATIONS
        OBSERVATIONS = []
        egg_info = self.model.save_to_egg(self.model.name, next_egg(),
                                          py_dir=PY_DIR,
                                          child_objs=self.child_objs,
                                          observer=observer)
        self.egg_name = egg_info[0]

        # Check observations.
        expected = [
            ('add', 'EGG-INFO/PKG-INFO'),
            ('add', 'EGG-INFO/dependency_links.txt'),
            ('add', 'EGG-INFO/entry_points.txt'),
            ('add', 'EGG-INFO/not-zip-safe'),
            ('add', 'EGG-INFO/requires.txt'),
            ('add', 'EGG-INFO/openmdao_orphans.txt'),
            ('add', 'EGG-INFO/top_level.txt'),
            ('add', 'EGG-INFO/SOURCES.txt'),
            ('add', 'Egg_TestModel/Egg_TestModel.pickle'),
            ('add', 'Egg_TestModel/Egg_TestModel_loader.py'),
            ('add', 'Egg_TestModel/Oddball.pickle'),
            ('add', 'Egg_TestModel/Oddball_loader.py'),
            ('add', 'Egg_TestModel/Oddball_oddcomp.pickle'),
            ('add', 'Egg_TestModel/Oddball_oddcomp_loader.py'),
            ('add', 'Egg_TestModel/Oddball_oddcont.pickle'),
            ('add', 'Egg_TestModel/Oddball_oddcont_loader.py'),
            ('add', 'Egg_TestModel/Sink.pickle'),
            ('add', 'Egg_TestModel/Sink_loader.py'),
            ('add', 'Egg_TestModel/Source.pickle'),
            ('add', 'Egg_TestModel/Source/hello'),
            ('add', 'Egg_TestModel/Source/xyzzy'),
            ('add', 'Egg_TestModel/Source_loader.py'),
            ('add', 'Egg_TestModel/__init__.py'),
            ('add', 'Egg_TestModel/sub/data2'),
            ('add', 'Egg_TestModel/sub/data4'),
        ]

        # Add our file if we're not considered part of an egg.
        #Commenting out this if as a proposed fix from SET for our release testing problems.
        #if sys.modules[self.__module__].__file__.find('.egg') < 0:
        expected.append(('add', 'Egg_TestModel/test_egg_save.py'))
        expected.append(('complete', 'Egg_TestModel-1.2.3-py%d.%d.egg' % sys.version_info[:2]))

        if len(OBSERVATIONS) != len(expected):
            logging.debug('Observed, Expected')
            for i in range(max(len(OBSERVATIONS), len(expected))):
                if i < len(OBSERVATIONS):
                    ob_state, ob_string, ffract, bfract = OBSERVATIONS[i]
                else:
                    ob_state = '---'
                    ob_string = '---'
                if i < len(expected):
                    ex_state = expected[i][0]
                    ex_string = expected[i][1]
                else:
                    ex_state = '---'
                    ex_string = '---'
                logging.debug('%s:%s\t%s:%s',
                              ob_state, ob_string, ex_state, ex_string)

        self.assertEqual(len(OBSERVATIONS), len(expected))
        for i, observation in enumerate(OBSERVATIONS):
            state, string, file_fraction, byte_fraction = observation
            self.assertEqual(state, expected[i][0])
            if expected[i][1].endswith('.egg'):  # Unique versions mess this up.
                self.assertEqual(string.startswith(self.model.name), True)
                self.assertEqual(string.endswith('.egg'), True)
            else:
                self.assertEqual(string.replace('\\', '/'), expected[i][1])
            self.assertEqual(file_fraction, float(i)/float(len(expected)-1))

        # Run and verify correct operation.
        self.model.run()
        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(True,
            all(self.model.Sink.binary_data == self.model.Source.sub.binary_data))
        self.assertEqual(self.model.Sink.binary_file.binary, True)

        self.assertEqual(self.model.Sink.executions, 3)

        # Restore in test directory.
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir, onerror=onerror)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            # Clear flags to detect if loading calls __init__.
            SOURCE_INIT = False
            SINK_INIT = False

            # Load from saved initial state in egg.
            self.model.pre_delete()
            egg_path = os.path.join('..', self.egg_name)
            OBSERVATIONS = []
            self.model = Component.load_from_eggfile(egg_path,
                                                     observer=observer)
            self.model.directory = os.path.join(os.getcwd(), self.model.name)

            # Check observations.
            expected = [
                ('extract', 'EGG-INFO/PKG-INFO'),
                ('extract', 'EGG-INFO/dependency_links.txt'),
                ('extract', 'EGG-INFO/entry_points.txt'),
                ('extract', 'EGG-INFO/not-zip-safe'),
                ('extract', 'EGG-INFO/requires.txt'),
                ('extract', 'EGG-INFO/openmdao_orphans.txt'),
                ('extract', 'EGG-INFO/top_level.txt'),
                ('extract', 'EGG-INFO/SOURCES.txt'),
                ('extract', 'Egg_TestModel/Egg_TestModel.pickle'),
                ('extract', 'Egg_TestModel/Egg_TestModel_loader.py'),
                ('extract', 'Egg_TestModel/Oddball.pickle'),
                ('extract', 'Egg_TestModel/Oddball_loader.py'),
                ('extract', 'Egg_TestModel/Oddball_oddcomp.pickle'),
                ('extract', 'Egg_TestModel/Oddball_oddcomp_loader.py'),
                ('extract', 'Egg_TestModel/Oddball_oddcont.pickle'),
                ('extract', 'Egg_TestModel/Oddball_oddcont_loader.py'),
                ('extract', 'Egg_TestModel/Sink.pickle'),
                ('extract', 'Egg_TestModel/Sink_loader.py'),
                ('extract', 'Egg_TestModel/Source.pickle'),
                ('extract', 'Egg_TestModel/Source/hello'),
                ('extract', 'Egg_TestModel/Source/xyzzy'),
                ('extract', 'Egg_TestModel/Source_loader.py'),
                ('extract', 'Egg_TestModel/__init__.py'),
                ('extract', 'Egg_TestModel/sub/data2'),
                ('extract', 'Egg_TestModel/sub/data4'),
            ]

            # Add our file if we're not considered part of an egg.
            #if sys.modules[self.__module__].__file__.find('.egg') < 0:
            expected.append(('extract', 'Egg_TestModel/test_egg_save.py'))
            expected.append(('complete', None))

            self.assertEqual(len(OBSERVATIONS), len(expected))
            for i, observation in enumerate(OBSERVATIONS):
                state, string, file_fraction, byte_fraction = observation
                self.assertEqual(state,  expected[i][0])
                self.assertEqual(string, expected[i][1])
                self.assertEqual(file_fraction,
                                 float(i)/float(len(expected)-1))

            # Verify initial state.
            self.assertEqual(SOURCE_INIT, False)
            self.assertEqual(SINK_INIT, False)
            self.assertNotEqual(self.model.Sink.text_data,
                                self.model.Source.text_data)
            self.assertNotEqual(self.model.Sink.binary_data,
                                self.model.Source.sub.binary_data)

            for path in EXTERNAL_FILES:
                path = os.path.join(self.model.Source.get_abs_directory(), path)
                if not os.path.exists(path):
                    self.fail("after loading, path '%s' does not exist" % path)

            for i in range(3):
                self.assertEqual(self.model.Source.obj_list[i].data, i)

            self.assertEqual(self.model.Oddball.executions, 0)

            # Run and verify correct operation.
            self.model.run()
            self.assertEqual(self.model.Sink.text_data,
                             self.model.Source.text_data)
            self.assertEqual(all(self.model.Sink.binary_data ==
                             self.model.Source.sub.binary_data), True)
            self.assertEqual(
                self.model.Sink.binary_file.binary, True)

            self.assertEqual(self.model.Oddball.executions, 3)

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir, onerror=onerror)

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')
        self.save_load()

    def test_save_bad_name(self):
        logging.debug('')
        logging.debug('test_save_bad_name')
        code = "self.model.save_to_egg('#%^&', next_egg(), py_dir=PY_DIR)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Egg_TestModel: Egg name must be alphanumeric')

    def test_save_bad_version(self):
        logging.debug('')
        logging.debug('test_save_bad_version')
        code = "self.model.save_to_egg(self.model.name, '#%^&', py_dir=PY_DIR)"
        assert_raises(self, code, globals(), locals(), ValueError,
                      'Egg_TestModel: Egg version must be alphanumeric')

    def test_save_bad_directory(self):
        logging.debug('')
        logging.debug('test_save_bad_directory')

        # Set subcomponent directory outside model root.
        self.model.Oddball.directory = os.getcwd()
        code = 'self.model.save_to_egg(self.model.name, next_egg(), py_dir=PY_DIR)'
        msg = "Egg_TestModel: Can't save, Egg_TestModel.Oddball.oddcomp" \
              " directory"
        assert_raises(self, code, globals(), locals(), ValueError, msg)

    def test_save_bad_destination(self):
        logging.debug('')
        logging.debug('test_save_bad_destination')

# TODO: get make_protected_dir() to work on Windows.
        if sys.platform == 'win32':
            raise nose.SkipTest()

        directory = make_protected_dir()
        try:
            # Attempt to save to directory we aren't allowed to write to.
            self.model.save_to_egg(self.model.name, next_egg(), py_dir=PY_DIR,
                                   dst_dir=directory)
        except IOError, exc:
            self.assertTrue('no write permission' in str(exc) or
                            'Permission denied' in str(exc))
        else:
            self.fail('Expected IOError')
        finally:
            os.rmdir(directory)

    def test_save_bad_external(self):
        logging.debug('')
        logging.debug('test_save_bad_external')

        # Set external file path outside model root.
        path = os.path.join(os.getcwd(), 'bad-external')
        out = open(path, 'w')
        out.close()
        metadata = self.model.Source.external_files[0]
        metadata.path = path

        code = 'self.model.save_to_egg(self.model.name, next_egg(), py_dir=PY_DIR)'
        msg = "Egg_TestModel: Can't save, Egg_TestModel.Source file"
        try:
            assert_raises(self, code, globals(), locals(), ValueError, msg)
        finally:
            os.remove(path)

    def test_save_noforce(self):
        logging.debug('')
        logging.debug('test_save_noforce')

        # Set external file path outside model root.
        path = os.path.join(os.getcwd(), 'unforced-external')
        out = open(path, 'w')
        out.close()
        metadata = self.model.Source.external_files[0]
        metadata.path = path
        try:
            self.model.save_to_egg(self.model.name, next_egg(), py_dir=PY_DIR,
                                   require_relpaths=False)
        finally:
            os.remove(path)

    def test_save_bad_filevar(self):
        logging.debug('')
        logging.debug('test_save_bad_filevar')

        # Set file trait path outside model root.
        self.model.Source.text_file = FileRef('/illegal')
        code = 'self.model.save_to_egg(self.model.name, next_egg(), py_dir=PY_DIR)'
        msg = "Egg_TestModel: Can't save, Egg_TestModel.Source.text_file path"
        assert_raises(self, code, globals(), locals(), ValueError, msg)

    def test_save_function(self):
        logging.debug('')
        logging.debug('test_save_function')

        # Set reference to function defined in __main__.
        self.model.Oddball.function_socket = local_getpid
        self.save_load()

    def test_save_bad_method(self):
        logging.debug('')
        logging.debug('test_save_bad_method')

        # Set reference to unpickleable static method.
        self.model.Oddball.method_socket = self.model.Oddball.static_method
        code = 'self.model.save_to_egg(self.model.name, next_egg(), py_dir=PY_DIR)'
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      "Egg_TestModel: Can't save, 1 object cannot be pickled.")

    def test_save_bad_pickle(self):
        logging.debug('')
        logging.debug('test_save_bad_pickle')

        # Code objects don't pickle.
        self.model.code = compile('3 + 4', '<string>', 'eval')
        try:
            # This will fail due to code object.
            self.model.save_to_egg(self.model.name, next_egg(), py_dir=PY_DIR)
        except cPickle.PicklingError, exc:
            msg = "Egg_TestModel: Can't save to" \
                  " 'Egg_TestModel/Egg_TestModel.pickle': Can't pickle" \
                  " <type 'code'>: attribute lookup __builtin__.code failed"
            self.assertEqual(str(exc).replace('\\', '/'), msg)
        else:
            self.fail('Expected cPickle.PicklingError')

    def test_save_bad_child(self):
        logging.debug('')
        logging.debug('test_save_bad_child')

        # Create orphan component.
        orphan = Component()
        code = 'self.model.save_to_egg(self.model.name, next_egg(),' \
               ' py_dir=PY_DIR, child_objs=[orphan])'
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      'Entry point object has no parent!')

        # Create non-orphan component that is not part of model.
        badboy = orphan.add('badboy', Component())
        code = 'self.model.save_to_egg(self.model.name, next_egg(),' \
               ' py_dir=PY_DIR, child_objs=[badboy])'
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      'Egg_TestModel: badboy is not a child of Egg_TestModel.')

    def test_save_load_container(self):
        logging.debug('')
        logging.debug('test_save_load_container')

        # Save to egg.
        egg_info = self.model.Source.sub.save_to_egg(self.model.name,
                                                     next_egg(), py_dir=PY_DIR)
        self.egg_name = egg_info[0]

        # Restore in test directory.
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir, onerror=onerror)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            egg_path = os.path.join('..', self.egg_name)
            sub = Container.load_from_eggfile(egg_path)
            self.assertTrue(all(sub.binary_data == self.model.Source.sub.binary_data))
        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir, onerror=onerror)

    def test_load_badfile(self):
        logging.debug('')
        logging.debug('test_load_badfile')

        # Try to load from non-egg.
        assert_raises(self, "Component.load_from_eggfile('.')",
                      globals(), locals(), ValueError,
                      "'.' is not an egg/zipfile.")

    def test_load_nofile(self):
        logging.debug('')
        logging.debug('test_load_nofile')

        # Try to load from nonexistant egg file.
        assert_raises(self, "Component.load_from_eggfile('no-such-egg')",
                      globals(), locals(), ValueError,
                      "'no-such-egg' not found.")

    def test_load_nopackage(self):
        logging.debug('')
        logging.debug('test_load_nopackage')

        # Try to load from nonexistant egg package.
        assert_raises(self, "Component.load_from_eggpkg('no-such-egg')",
                      globals(), locals(), pkg_resources.DistributionNotFound,
                      'no-such-egg')

    def test_check_save_load(self):
        logging.debug('')
        logging.debug('test_check_save_load')

        # Exercise check_save_load().
        retcode = check_save_load(self.model, py_dir=PY_DIR)
        self.assertEqual(retcode, 0)

    def test_install_load(self):
        # Creates egg.
        # Installs in special directory.
        # Tries to load and run from installed egg in various ways.
        logging.debug('')
        logging.debug('test_install_load')

        # Find correct python.
        python = find_python()
        logging.debug('    Using python: %s' % python)

        # Write to egg.
        egg_info = self.model.save_to_egg(self.model.name, next_egg(),
                                          py_dir=PY_DIR,
                                          child_objs=self.child_objs)
        self.egg_name = egg_info[0]

        # Create directory for installation.
        install_dir = os.path.join(os.getcwd(), 'install_dir')
        if os.path.exists(install_dir):
            shutil.rmtree(install_dir, onerror=onerror)
        os.mkdir(install_dir)
        try:
            # Create special installer script.
            # This basically does an easy_install.
            installer = os.path.join(install_dir, 'installer.py')
            out = open(installer, 'w')
            out.write("""\
# EASY-INSTALL-ENTRY-SCRIPT: 'setuptools>=0.6c8','console_scripts','easy_install'
__requires__ = 'setuptools>=0.6c8'
import os
import sys
from pkg_resources import load_entry_point

#print
#print 'Installer Environment:'
#for name, val in sorted(os.environ.items(), key=lambda item: item[0]):
#    print '    %s = %r' % (name, val)
#print

sys.exit(
   load_entry_point('setuptools>=0.6c8', 'console_scripts', 'easy_install')()
)
""")
            out.close()

            # Install via subprocess with PYTHONPATH set (for easy_install).
            logging.debug('Installing via subprocess...')
            env = os.environ.copy()
            path = env.get('PYTHONPATH', '')
            if path:
                path += os.pathsep
            path += install_dir
            env['PYTHONPATH'] = path

#            logging.debug('Test Environment:')
#            for name, val in sorted(env.items(), key=lambda item: item[0]):
#                logging.debug('    %s = %r', name, val)

            cmdline = [python, installer, '-d', install_dir, self.egg_name]

            stdout = open(os.path.join(install_dir, 'installer.out'), 'w')
            retcode = subprocess.call(cmdline, env=env,
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
            code = "Component.load_from_eggpkg('no-such-pkg', 'no-such-entry')"
            assert_raises(self, code, globals(), locals(),
                          pkg_resources.DistributionNotFound, 'no-such-pkg')

            # Try a non-existent entry point.
            egg_path = os.path.join(install_dir, self.egg_name)
            sys.path.append(egg_path)
            orig_ws = pkg_resources.working_set
            pkg_resources.working_set = pkg_resources.WorkingSet()
            code = "Component.load_from_eggpkg(package_name, 'no-such-entry')"
            msg = "No 'openmdao.component' 'no-such-entry' entry point."
            try:
                assert_raises(self, code, globals(), locals(), RuntimeError, msg)
            finally:
                sys.path.pop()
                pkg_resources.working_set = orig_ws

        finally:
            shutil.rmtree(install_dir, onerror=onerror)

    def load_n_run(self, python, install_dir, package_name, entry_name):
        """ Load component from installed egg and run it. """
        # Create and move to test directory.
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir, onerror=onerror)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            # Create load-n-run script.
            out = open('load-n-run.py', 'w')
            out.write("""\
import sys
sys.path.append('%(egg)s')
from openmdao.main.api import Component
comp = Component.load_from_eggpkg('%(package)s', '%(entry)s')
comp.run()

""" % {'egg': os.path.join(install_dir, self.egg_name).replace('\\', '/'),
       'package': package_name, 'entry': entry_name})
            out.close()

            # Load & run in subprocess.
            logging.debug("Load and run '%s' in subprocess...", entry_name)
            logging.debug('    %s', os.path.join(install_dir, self.egg_name))
            cmdline = [python, 'load-n-run.py']
            stdout = open('load-n-run.out', 'w')
            retcode = subprocess.call(cmdline, stdout=stdout,
                                      stderr=subprocess.STDOUT)
            stdout.close()
            stdout = open('load-n-run.out', 'r')
            for line in stdout:
                logging.debug('    %s' % line.rstrip())
            stdout.close()
            return retcode

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir, onerror=onerror)

    def test_pkg_resources_factory(self):
        # NOTE: this test fails if run standalone:
        #       ImportError: No module named test_egg_save
        # Probably need Egg_TestModel.test_egg_save, or adjusted sys.path.
        if MODULE_NAME == '__main__':
            return

        logging.debug('')
        logging.debug('test_pkg_resources_factory')

        # Write to egg.
        egg_info = self.model.save_to_egg(self.model.name, next_egg(),
                                          py_dir=PY_DIR,
                                          child_objs=self.child_objs)
        self.egg_name = egg_info[0]

        # Create factory.
        factory = PkgResourcesFactory(['openmdao.component',
                                       'openmdao.container'],
                                      [os.getcwd()])

        # Create and move to test directory.
        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir, onerror=onerror)
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
            logging.debug('updated %s', path)
            self.create_and_check_model(factory, 'test_model_2', file_data)

            # Check observations.
            global OBSERVATIONS
            OBSERVATIONS = []
            model = factory.create('Egg_TestModel', name='observed',
                                   observer=observer)
            if model is None:
                self.fail("Create of 'observed' failed.")
            expected = [
                ('copy', 'Source/xyzzy'),
                ('copy', 'sub/data2'),
                ('copy', 'Source/hello'),
                ('copy', 'sub/data4'),
                ('complete', 'observed'),
            ]
            self.assertEqual(len(OBSERVATIONS), len(expected))
            for i, observation in enumerate(OBSERVATIONS):
                state, string, file_fraction, byte_fraction = observation
                self.assertEqual(state,  expected[i][0])
                self.assertEqual(string, expected[i][1])
                self.assertEqual(file_fraction, float(i)/float(len(expected)-1))

            # Create a component.
            comp = factory.create('Egg_TestModel.Oddball', name='test_comp',
                                  observer=observer)
            if comp is None:
                self.fail('Create of test_comp failed.')
            self.assertEqual(comp.executions, 0)
            comp.run()
            self.assertEqual(comp.executions, 3)
            self.assertEqual(comp.get_pathname(), 'test_comp')

            # Create a (sub)component.
            sub = factory.create('Egg_TestModel.Oddball.oddcomp',
                                 name='test_sub')
            if sub is None:
                self.fail('Create of test_sub failed.')
            self.assertEqual(sub.get_pathname(), 'test_sub')

            # Create a (sub)container.
            sub = factory.create('Egg_TestModel.Oddball.oddcont',
                                 name='test_sub')
            if sub is None:
                self.fail('Create of test_sub failed.')
            self.assertEqual(sub.get_pathname(), 'test_sub')

            # Try a non-existent entry point.
            obj = factory.create('no-such-entry', name='xyzzy')
            self.assertEqual(obj, None)

        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir, onerror=onerror)

    def create_and_check_model(self, factory, name, file_data):
        """ Create a complete model instance and check it's operation. """
        model = factory.create('Egg_TestModel', name=name)
        logging.debug('model.directory = %s' % model.directory)
        if model is None:
            self.fail("Create of '%s' failed." % name)
        self.assertEqual(model.get_abs_directory(),
                         os.path.join(os.getcwd(), name))
        self.assertEqual(model.Oddball.get_pathname(), name+'.Oddball')

        # Verify initial state.
        self.assertNotEqual(model.Sink.text_data,
                            model.Source.text_data)
        self.assertNotEqual(model.Sink.binary_data,
                            model.Source.sub.binary_data)

        orig_dir = os.getcwd()
        os.chdir(model.Source.get_abs_directory())
        try:
            for path in EXTERNAL_FILES:
                if not os.path.exists(path):
                    self.fail("path '%s' does not exist" % path)

            inp = open(EXTERNAL_FILES[2])
            data = inp.read()
            inp.close()
            self.assertEqual(data.strip(), file_data.strip())
        finally:
            os.chdir(orig_dir)

        for i in range(3):
            self.assertEqual(model.Source.obj_list[i].data, i)

        self.assertEqual(model.Oddball.executions, 0)

        # Run and verify correct operation.
        model.run()
        self.assertEqual(model.Sink.text_data,
                         model.Source.text_data)
        self.assertEqual(all(model.Sink.binary_data == model.Source.sub.binary_data),
                         True)

        self.assertEqual(model.Sink.binary_file.binary, True)

        self.assertEqual(model.Oddball.executions, 3)

    def test_main_module(self):
        if MODULE_NAME == '__main__':
            return

        # Ensure that __main__ translation is correctly handled.
        logging.debug('')
        logging.debug('test_main_module')

        # Find correct python.
        python = find_python()
        logging.debug('    Using python: %s' % python)

        orig_dir = os.getcwd()
        os.chdir(PY_DIR)
        try:
            cmdline = [python, 'test_egg_save.py']
            stdout = open('main_handling.out', 'w')
            retcode = subprocess.call(cmdline, stdout=stdout,
                                      stderr=subprocess.STDOUT)
            stdout.close()
            stdout = open('main_handling.out', 'r')
            for line in stdout:
                logging.debug('    %s' % line.rstrip())
            stdout.close()
            os.remove('main_handling.out')
        finally:
            os.chdir(orig_dir)

        self.assertEqual(retcode, 0)


if __name__ == '__main__':
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')

    # Clobber cache so we have a known state.
    path = os.path.expanduser(os.path.join('~', '.openmdao', 'eggsaver.dat'))
    if os.path.exists(path):
        os.remove(path)

    nose.runmodule()
