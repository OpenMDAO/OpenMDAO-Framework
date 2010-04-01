"""
Test of File traits.
"""

import cPickle
import logging
import os.path
import shutil
import unittest

from numpy.testing import assert_equal

from enthought.traits.api import Bool, Array, Str, TraitError

from openmdao.main.api import Assembly, Component, set_as_top, FileRef
from openmdao.lib.api import File

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """ Produces files. """

    write_files = Bool(True, iotype='in')
    text_data = Str(iotype='in')
    binary_data = Array('d', iotype='in')
    text_file = File(path='source.txt', iotype='out', content_type='txt')
    binary_file = File(path='source.bin', iotype='out', binary=True,
                            extra_stuff='Hello world!')

    def execute(self):
        """ Write test data to files. """
        if self.write_files:
            out = open(self.text_file.path, 'w')
            out.write(self.text_data)
            out.close()

            out = open(self.binary_file.path, 'wb')
            cPickle.dump(self.binary_data, out, 2)
            out.close()


class Passthrough(Component):
    """ Copies input files (implicitly via local_path) to output. """
    text_in = File(iotype='in', local_path='tout',
                        legal_types=['xyzzy', 'txt'])
    binary_in = File(iotype='in', local_path='bout')
    text_out = File(path='tout', iotype='out')
    binary_out = File(path='bout', iotype='out', binary=True)

    def execute(self):
        """ File copies are performed implicitly. """
        # We have to manually propagate 'extra_stuff' because the output
        # FileRef isn't copied from the input FileRef.
        self.binary_out.extra_stuff = self.binary_in.extra_stuff


class Middle(Assembly):
    """ Intermediary which passes-on files. """

    def __init__(self, *args, **kwargs):
        super(Middle, self).__init__(*args, **kwargs)

        self.add_container('passthrough', Passthrough(directory='Passthrough'))

        self.create_passthrough('passthrough.text_in')
        self.create_passthrough('passthrough.binary_in')

        self.create_passthrough('passthrough.text_out')
        self.create_passthrough('passthrough.binary_out')


class Sink(Component):
    """ Consumes files. """

    bogus_path = Str('', iotype='in')
    text_data = Str(iotype='out')
    binary_data = Array('d', iotype='out')
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

    def __init__(self, *args, **kwargs):
        super(Model, self).__init__(*args, **kwargs)

        self.add_container('source', Source(directory='Source'))
        self.add_container('middle', Middle(directory='Middle'))
        self.add_container('sink', Sink(directory='Sink'))

        self.connect('source.text_file', 'middle.text_in')
        self.connect('source.binary_file', 'middle.binary_in')

        self.connect('middle.text_out', 'sink.text_file')
        self.connect('middle.binary_out', 'sink.binary_file')

        self.source.text_data = 'Hello World!'
        self.source.binary_data = [3.14159, 2.781828, 42]

    def tree_rooted(self):
        """ Sets passthrough paths to absolute to exercise code. """
        super(Model, self).tree_rooted()

        self.middle.passthrough.trait('text_in').trait_type._metadata['local_path'] = \
            os.path.join(self.middle.passthrough.get_abs_directory(),
                         self.middle.passthrough.trait('text_in').local_path)

        self.middle.passthrough.trait('text_out').trait_type._metadata['path'] = \
            os.path.join(self.middle.passthrough.get_abs_directory(),
                         self.middle.passthrough.trait('text_out').path)


class TestCase(unittest.TestCase):
    """ Test of Files. """

    def setUp(self):
        """ Called before each test in this class. """
        self.model = set_as_top(Model())

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        for directory in ('Source', 'Middle', 'Sink'):
            try:
                shutil.rmtree(directory)
            except OSError:
                pass
        self.model = None

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
        assert_equal(self.model.sink.binary_data,
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
        except IOError, exc:
            if 'source.txt' not in str(exc) and 'source.bin' not in str(exc):
                self.fail("Wrong message '%s'" % exc)
        else:
            self.fail('IOError expected')

    def test_illegal_src(self):
        logging.debug('')
        logging.debug('test_illegal_src')

        # Set illegal path (during execution of sink), verify error message.
        self.model.sink.bogus_path = '/illegal'
        try:
            self.model.run()
        except ValueError, exc:
            msg = "middle.passthrough: Illegal path '/illegal'," \
                  " not a descendant of"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')

    def test_legal_types(self):
        logging.debug('')
        logging.debug('test_legal_types')

        # Set mismatched type and verify error message.
        self.model.source.text_file.content_type = 'invalid'
        try:
            self.model.run()
        except TraitError, exc:
            msg = ": cannot set 'middle.text_in' from 'source.text_file':" \
                  " Content type 'invalid' not one of ['xyzzy', 'txt']"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected TraitError')

        # Set null type and verify error message.
        self.model.source.text_file.content_type = ''
        try:
            self.model.run()
        except TraitError, exc:
            msg = ": cannot set 'middle.text_in' from 'source.text_file':" \
                  " Content type '' not one of ['xyzzy', 'txt']"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected TraitError')

    def test_formatting(self):
        logging.debug('')
        logging.debug('test_formatting')
        msg = "{'big_endian': False, 'binary': True, 'content_type': ''," \
              " 'desc': '', 'extra_stuff': 'Hello world!'," \
              " 'path': 'source.bin', 'recordmark_8': False," \
              " 'single_precision': False, 'unformatted': False}"
        self.assertEqual(str(self.model.source.binary_file), msg)

    def test_no_owner(self):
        logging.debug('')
        logging.debug('test_no_owner')

        # Absolute FileRef.
        path = os.path.join(os.sep, 'xyzzy')
        ref = FileRef(path)
        try:
            ref.open()
        except ValueError, exc:
            msg = "Path '%s' is absolute and no path checker is available." \
                  % path
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected ValueError')

        # Relative FileRef.
        path = 'xyzzy'
        ref = FileRef(path)
        try:
            ref.open()
        except ValueError, exc:
            msg = "Path '%s' is relative and no absolute directory is available." \
                  % path
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected ValueError')

    def test_bad_trait(self):
        logging.debug('')
        logging.debug('test_bad_trait')

        try:
            File(42)
        except TraitError, exc:
            self.assertEqual(str(exc),
                             'File default value must be a FileRef.')
        else:
            self.fail('Expected TraitError')

        try:
            File()
        except TraitError, exc:
            self.assertEqual(str(exc),
                             "File must have 'iotype' defined.")
        else:
            self.fail('Expected TraitError')

        try:
            File(iotype='out')
        except TraitError, exc:
            self.assertEqual(str(exc),
                             "Output File must have 'path' defined.")
        else:
            self.fail('Expected TraitError')

        try:
            File(iotype='out', path='xyzzy', legal_types=42)
        except TraitError, exc:
            self.assertEqual(str(exc),
                             "'legal_types' invalid for output File.")
        else:
            self.fail('Expected TraitError')

        try:
            File(iotype='out', path='xyzzy', local_path=42)
        except TraitError, exc:
            self.assertEqual(str(exc),
                             "'local_path' invalid for output File.")
        else:
            self.fail('Expected TraitError')

        try:
            File(iotype='in', path='xyzzy')
        except TraitError, exc:
            self.assertEqual(str(exc),
                             "'path' invalid for input File.")
        else:
            self.fail('Expected TraitError')

    def test_bad_value(self):
        logging.debug('')
        logging.debug('test_bad_value')
        try:
            self.model.source.text_file = 42
        except TraitError, exc:
            msg = "The 'text_file' trait of a Source instance must be" \
                  " a legal value, but a value of 42 <type 'int'> was" \
                  " specified."
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected TraitError')


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.main')
    nose.runmodule()

