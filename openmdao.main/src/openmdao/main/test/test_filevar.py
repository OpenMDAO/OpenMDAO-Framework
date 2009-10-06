"""
Test of FileTraits.
"""

import cPickle
import logging
import shutil
import unittest

from numpy.testing import assert_equal

from enthought.traits.api import Bool, Array, Str, TraitError

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.filevar import FileTrait

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """ Produces files. """

    write_files = Bool(True, iostatus='in')
    text_data = Str(iostatus='in')
    binary_data = Array('d', iostatus='in')
    text_file = FileTrait(path='source.txt', iostatus='out', content_type='txt')
    binary_file = FileTrait(path='source.bin', iostatus='out', binary=True)

    def execute(self):
        """ Write test data to files. """
        if self.write_files:
            out = open(self.text_file.path, 'w')
            out.write(self.text_data)
            out.close()

            out = open(self.binary_file.path, 'wb')
            cPickle.dump(self.binary_data, out, 2)
            out.close()


class Passthru(Component):
    """ Copies input files to output. """

    text_in = FileTrait(iostatus='in', legal_types=['xyzzy', 'txt'])
    binary_in = FileTrait(iostatus='in')
    text_out = FileTrait(path='tout', iostatus='out')
    binary_out = FileTrait(path='bout', iostatus='out', binary=True)

    def execute(self):
        """ Copy input files to local directory. """
        self.copy(self.text_in, self.text_out.path)
        self.copy(self.binary_in, self.binary_out.path)

    def copy(self, src_ref, dst_path):
        """ Copy file to `dst_path`. """
        src = src_ref.open()
        mode = 'wb' if src_ref.binary else 'w'
        dst = open(dst_path, mode)
        dst.write(src.read())
        dst.close()
        src.close()


class Middle(Assembly):
    """ Intermediary which passes-on files. """

    def __init__(self, *args, **kwargs):
        super(Middle, self).__init__(*args, **kwargs)

        self.add_container('passthru', Passthru(directory='Passthru'))

        self.create_passthru('passthru.text_in')
        self.create_passthru('passthru.binary_in')

        self.create_passthru('passthru.text_out')
        self.create_passthru('passthru.binary_out')


class Sink(Component):
    """ Consumes files. """

    bogus_path = Str('', iostatus='in')
    text_data = Str(iostatus='out')
    binary_data = Array('d', iostatus='out')
    text_file = FileTrait(iostatus='in')
    binary_file = FileTrait(iostatus='in')

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


class TestCase(unittest.TestCase):
    """ Test of FileTraits. """

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
            msg = "middle.passthru: Illegal path '/illegal'," \
                  " not a descendant of"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')

    def test_legal_types(self):
        logging.debug('')
        logging.debug('test_legal_types')

        saved_type = self.model.source.text_file.content_type
        try:
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
        finally:
            self.model.source.text_file.content_type = saved_type


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.main')
    nose.runmodule()

