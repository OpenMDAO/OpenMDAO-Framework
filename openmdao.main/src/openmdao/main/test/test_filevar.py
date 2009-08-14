"""
Test of FileTraits.
"""

import cPickle
import logging
import os
import shutil
import unittest

from enthought.traits.api import Bool, Array, Str

from openmdao.main.api import Assembly, Component
from openmdao.main.filevar import FileTrait

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """ Produces files. """

    write_files = Bool(True, iostatus='in')
    text_data = Str(iostatus='in')
    binary_data = Array('d', iostatus='in')
    text_file = FileTrait(iostatus='out')
    binary_file = FileTrait(iostatus='out', binary=True)
        
    def __init__(self, *args, **kwargs):
        super(Source, self).__init__(*args, **kwargs)
        self.text_file.filename = 'source.txt'
        self.binary_file.filename = 'source.bin'

    def execute(self):
        """ Write test data to files. """
        if self.write_files:
            out = open(self.text_file.filename, 'w')
            out.write(self.text_data)
            out.close()

            out = open(self.binary_file.filename, 'wb')
            cPickle.dump(self.binary_data, out, 2)
            out.close()


class Sink(Component):
    """ Consumes files. """

    text_data = Str(iostatus='out')
    binary_data = Array('d', iostatus='out')
    text_file = FileTrait(iostatus='in')
    binary_file = FileTrait(iostatus='in')
        
    def __init__(self, *args, **kwargs):
        super(Sink, self).__init__(*args, **kwargs)
        self.text_file.filename = 'sink.txt'
        self.binary_file.filename = 'sink.bin'

    def execute(self):
        """ Read test data from files. """
        inp = open(self.text_file.filename, 'r')
        self.text_data = inp.read()
        inp.close()

        inp = open(self.binary_file.filename, 'rb')
        self.binary_data = cPickle.load(inp)
        inp.close()


class MyModel(Assembly):
    """ Transfer files from producer to consumer. """

    #name='FileVar_TestModel', 
    def __init__(self, *args, **kwargs):
        super(MyModel, self).__init__(*args, **kwargs)

        self.add_container('Source', Source(directory='Source'))
        self.add_container('Sink', Sink(directory='Sink'))

        self.connect('Source.text_file', 'Sink.text_file')
        self.connect('Source.binary_file', 'Sink.binary_file')

        self.Source.text_data = 'Hello World!'
        self.Source.binary_data = [3.14159, 2.781828, 42]


class FileTestCase(unittest.TestCase):
    """ Test of FileTraits. """

    def setUp(self):
        """ Called before each test in this class. """
        self.model = MyModel()

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        try:
            shutil.rmtree('Source')
        except OSError:
            pass
        try:
            shutil.rmtree('Sink')
        except OSError:
            pass
        self.model = None

    def test_connectivity(self):
        logging.debug('')
        logging.debug('test_connectivity')

        self.assertNotEqual(self.model.Sink.text_data,
                            self.model.Source.text_data)
        self.assertNotEqual(self.model.Sink.binary_data,
                            self.model.Source.binary_data)
        self.assertNotEqual(
            self.model.Sink.binary_file.binary, True)

        self.model.run()

        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(all(self.model.Sink.binary_data==self.model.Source.binary_data),
                         True)
        self.assertEqual(
            self.model.Sink.binary_file.binary, True)

    def test_src_failure(self):
        logging.debug('')
        logging.debug('test_src_failure')

        self.model.Source.write_files = False
        try:
            self.model.run()
        except IOError, exc:
            if 'source.txt' not in str(exc) and 'source.bin' not in str(exc):
                self.fail("Wrong message '%s'" % exc)
        else:
            self.fail('IOError expected')

    def test_bad_directory(self):
        logging.debug('')
        logging.debug('test_bad_directory')

        try:
            src = Source(directory='/illegal')
            src.hierarchy_defined()
        except ValueError, exc:
            msg = ": Illegal execution directory '/illegal'," \
                  " not a decendant of"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')

        directory = 'protected'
        if os.path.exists(directory):
            os.rmdir(directory)
        os.mkdir(directory)
        os.chmod(directory, 0)
        exe_dir = os.path.join(directory, 'xyzzy')
        try:
            src = Source(directory=exe_dir)
            src.hierarchy_defined()
        except OSError, exc:
            msg = ": Can't create execution directory"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected OSError')
        finally:
            os.rmdir(directory)

        directory = 'plain_file'
        if os.path.exists(directory):
            os.remove(directory)
        out = open(directory, 'w')
        out.write('Hello world!\n')
        out.close()
        try:
            self.source = Source(directory=directory)
            self.source.hierarchy_defined()
        except ValueError, exc:
            path = os.path.join(os.getcwd(), directory)
            self.assertEqual(str(exc),
                ": Execution directory path '%s' is not a directory."
                % path)
        else:
            self.fail('Expected ValueError')
        finally:
            os.remove(directory)

    def test_bad_new_directory(self):
        logging.debug('')
        logging.debug('test_bad_new_directory')

        self.model.Source.directory = '/illegal'
        try:
            self.model.run()
        except ValueError, exc:
            msg = "Source: Illegal execution directory '/illegal'," \
                  " not a decendant of"
            self.assertEqual(str(exc)[:len(msg)], msg)
        else:
            self.fail('Expected ValueError')

        ## this test no longer fails because no-such-dir gets
        ## created on-the-fly
        #self.model.Source.directory = 'no-such-dir'
        #try:
            #self.model.run()
        #except RuntimeError, exc:
            #msg = "Source: Could not move to execution" \
                  #" directory"
            #self.assertEqual(str(exc)[:len(msg)], msg)
        #else:
            #self.fail('Expected RuntimeError')


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.main')
    nose.runmodule()

