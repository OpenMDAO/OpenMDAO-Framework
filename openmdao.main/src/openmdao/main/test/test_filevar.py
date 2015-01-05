"""
Test of File traits.
"""

import cPickle
import logging
import os.path
import shutil
import sys
import unittest
import tempfile

from traits.api import TraitError

from openmdao.main.api import Assembly, Component, set_as_top, SimulationRoot
from openmdao.main.datatypes.api import Bool, File, FileRef, Str, List
from openmdao.util.fileutil import onerror
from openmdao.util.testutil import assert_raises

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """ Produces files. """

    write_files = Bool(True, iotype='in')
    text_data = Str(iotype='in')
    binary_data = List([1.0], iotype='in')
    text_file = File(FileRef('source.txt', content_type='txt'), iotype='out')
    binary_file = File(FileRef('source.bin', binary=True,
                               extra_stuff='Hello world!'), iotype='out')

    def execute(self):
        """ Write test data to files. """
        if self.write_files:
            out = open(self.text_file.path, 'w')
            out.write(self.text_data)
            out.close()

            out = open(self.binary_file.path, 'wb')
            cPickle.dump(list(self.binary_data), out, 2)
            out.close()


class Passthrough(Component):
    """ Copies input files (implicitly via local_path) to output. """
    text_in = File(iotype='in', local_path='tout', legal_types=['xyzzy', 'txt'])
    binary_in = File(iotype='in', local_path='bout')
    text_out = File(FileRef('tout'), iotype='out')
    binary_out = File(FileRef('bout', binary=True), iotype='out')

    def execute(self):
        """ File copies are performed implicitly. """
        # We have to manually propagate 'extra_stuff' because the output
        # FileRef isn't copied from the input FileRef.
        self.binary_out.extra_stuff = self.binary_in.extra_stuff


class Middle(Assembly):
    """ Intermediary which passes-on files. """

    def configure(self):

        comp = Passthrough()
        comp.directory = 'Passthrough'
        comp = self.add('passthrough', comp)
        self.driver.workflow.add('passthrough')

        self.create_passthrough('passthrough.text_in')
        self.create_passthrough('passthrough.binary_in')

        self.create_passthrough('passthrough.text_out')
        self.create_passthrough('passthrough.binary_out')


class Sink(Component):
    """ Consumes files. """

    bogus_path = Str('', iotype='in')
    text_data = Str(iotype='out')
    binary_data = List([1.0], iotype='out')
    text_file = File(iotype='in')
    binary_file = File(iotype='in')

    def execute(self):
        """ Read test data from files. """
        if self.bogus_path:
            self.text_file.path = self.bogus_path
        inp = self.text_file.open()
        self.text_data = inp.read()
        inp.close()

        inp = self.binary_file.open()
        self.binary_data = cPickle.load(inp)
        inp.close()


class Model(Assembly):
    """ Transfer files from producer to consumer. """

    def configure(self):

        comp = Source()
        comp.directory = 'Source'
        comp = self.add('source', comp)
        comp = Middle()
        comp.directory = 'Middle'
        comp = self.add('middle', comp)
        comp = Sink()
        comp.directory = 'Sink'
        comp = self.add('sink', comp)
        self.driver.workflow.add(['source', 'middle', 'sink'])

        self.connect('source.text_file', 'middle.text_in')
        self.connect('source.binary_file', 'middle.binary_in')

        self.connect('middle.text_out', 'sink.text_file')
        self.connect('middle.binary_out', 'sink.binary_file')

        self.source.text_data = 'Hello World!'
        self.source.binary_data = [3.14159, 2.781828, 42]

        # set passthrough paths to absolute to exercise code
        self.middle.passthrough.get_trait('text_in').trait_type._metadata['local_path'] = \
            os.path.join(self.middle.passthrough.get_abs_directory(),
                         self.middle.passthrough.get_trait('text_in').local_path)

        self.middle.passthrough.text_out.path = \
            os.path.join(self.middle.passthrough.get_abs_directory(),
                         self.middle.passthrough.text_out.path)


class TestCase(unittest.TestCase):
    """ Test of Files. """

    def setUp(self):
        """ Called before each test in this class. """
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_filevar-')
        os.chdir(self.tempdir)
        SimulationRoot.chroot(self.tempdir)
        self.model = set_as_top(Model())

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        self.model = None
        os.chdir(self.startdir)
        SimulationRoot.chroot(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

    def test_connectivity(self):
        logging.debug('')
        logging.debug('test_connectivity')

        # Verify expected initial state.
        self.assertNotEqual(self.model.sink.text_data,
                            self.model.source.text_data)
        self.assertNotEqual(self.model.sink.binary_data,
                            self.model.source.binary_data)

        self.model.run()

        # Verify data transferred.
        self.assertEqual(self.model.sink.text_data,
                         self.model.source.text_data)
        self.assertEqual(self.model.sink.binary_data,
                         self.model.source.binary_data)
        self.assertEqual(self.model.sink.binary_file.binary, True)
        self.assertEqual(self.model.sink.binary_file.extra_stuff,
                         self.model.source.binary_file.extra_stuff)

    def test_src_failure(self):
        logging.debug('')
        logging.debug('test_src_failure')

        # Turn off source write, verify error message.
        self.model.source.write_files = False
        try:
            self.model.run()
        except RuntimeError as exc:
            if 'source.txt' not in str(exc) and 'source.bin' not in str(exc):
                self.fail("Wrong message '%s'" % exc)
        else:
            self.fail('IOError expected')

    def test_illegal_src(self):
        logging.debug('')
        logging.debug('test_illegal_src')

        # Set illegal path (during execution of sink), verify error message.
        self.model.sink.bogus_path = '/illegal'
        msg = "middle.passthrough (1-middle.1-passthrough): Illegal path '/illegal'," \
              " not a descendant of"
        try:
            self.model.run()
        except ValueError as exc:
            print exc
            self.assertTrue(msg in str(exc))
        else:
            self.fail('ValueError expected')

    def test_legal_types(self):
        logging.debug('')
        logging.debug('test_legal_types')

        # Set mismatched type and verify error message.
        self.model.source.text_file.content_type = 'invalid'
        msg = ": cannot set 'middle.text_in' from 'source.text_file':" \
              " : Content type 'invalid' not one of ['xyzzy', 'txt']"
        try:
            self.model.run()
        except ValueError as exc:
            print exc
            self.assertTrue(msg in str(exc))
        else:
            self.fail('ValueError expected')

        # Set null type and verify error message.
        self.model.source.text_file.content_type = ''
        msg = ": cannot set 'middle.text_in' from 'source.text_file':" \
              " : Content type '' not one of ['xyzzy', 'txt']"
        try:
            self.model.run()
        except ValueError as exc:
            self.assertTrue(msg in str(exc))
        else:
            self.fail('ValueError expected')

    def test_formatting(self):
        logging.debug('')
        logging.debug('test_formatting')
        msg = "{'big_endian': False, 'binary': True, 'content_type': ''," \
              " 'desc': '', 'extra_stuff': 'Hello world!'," \
              " 'integer_8': False, 'path': 'source.bin', 'platform': %r," \
              " 'recordmark_8': False, 'single_precision': False," \
              " 'unformatted': False}" % sys.platform
        self.assertEqual(str(self.model.source.binary_file), msg)

    def test_no_owner(self):
        logging.debug('')
        logging.debug('test_no_owner')

        # No owner.
        path = os.path.join(os.sep, 'xyzzy')
        ref = FileRef(path)
        msg = 'abspath() failed: no owner specified for FileRef'
        assert_raises(self, 'ref.open()', globals(), locals(), ValueError, msg)

        # Absolute FileRef.
        path = os.path.join(os.sep, 'xyzzy')
        ref = FileRef(path, self)
        code = 'ref.open()'
        msg = "Path '%s' is absolute and no path checker is available." % path
        assert_raises(self, code, globals(), locals(), ValueError, msg)

        # Relative FileRef.
        path = 'xyzzy'
        ref = FileRef(path, self)
        msg = "Path '%s' is relative and no absolute directory is available." % path
        assert_raises(self, code, globals(), locals(), ValueError, msg)

    def test_bad_trait(self):
        logging.debug('')
        logging.debug('test_bad_trait')

        code = 'File(42)'
        msg = 'File default value must be a FileRef.'
        assert_raises(self, code, globals(), locals(), TypeError, msg)

        code = "File(iotype='out', path='xyzzy', legal_types=42)"
        msg = "'legal_types' invalid for output File."
        assert_raises(self, code, globals(), locals(), ValueError, msg)

        code = "File(iotype='out', path='xyzzy', local_path=42)"
        msg = "'local_path' invalid for output File."
        assert_raises(self, code, globals(), locals(), ValueError, msg)

    def test_bad_value(self):
        logging.debug('')
        logging.debug('test_bad_value')

        code = 'self.model.source.text_file = 42'
        msg = "The 'text_file' trait of a Source instance must be" \
              " a legal value, but a value of 42 <type 'int'> was" \
              " specified."
        assert_raises(self, code, globals(), locals(), TraitError, msg,
                      use_exec=True)


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao.main')
    nose.runmodule()
