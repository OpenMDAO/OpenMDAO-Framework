"""
Test of File traits.
"""

import cPickle
import logging
import os.path
import shutil
import unittest

from numpy.testing import assert_equal

from enthought.traits.api import Bool, Str

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.file import File
from openmdao.main.datatypes.array import Array

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"



class Source(Component):
    """ Produces files. """

    write_files = Bool(True, iotype='in')
    text_data = Str(iotype='in')
    binary_data = Array(dtype='d', iotype='in')
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

        self.add('passthrough', Passthrough(directory='Passthrough'))
        self.driver.workflow.add('passthrough')

        self.create_passthrough('passthrough.text_in')
        self.create_passthrough('passthrough.binary_in')

        self.create_passthrough('passthrough.text_out')
        self.create_passthrough('passthrough.binary_out')



class Sink(Component):
    """ Consumes files. """

    bogus_path = Str('', iotype='in')
    text_data = Str(iotype='out')
    binary_data = Array(dtype='d', iotype='out')
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

        self.add('source', Source(directory='Source'))
        self.add('middle', Middle(directory='Middle'))
        self.add('sink', Sink(directory='Sink'))
        self.driver.workflow.add(['source','middle','sink'])

        self.connect('source.text_file', 'middle.text_in')
        self.connect('source.binary_file', 'middle.binary_in')

        self.connect('middle.text_out', 'sink.text_file')
        self.connect('middle.binary_out', 'sink.binary_file')

        self.source.text_data = 'Hello World!'
        self.source.binary_data = [3.14159, 2.781828, 42]

    def tree_rooted(self):
        """ Sets passthrough paths to absolute to exercise code. """
        super(Model, self).tree_rooted()

        self.middle.passthrough.get_trait('text_in').trait_type._metadata['local_path'] = \
            os.path.join(self.middle.passthrough.get_abs_directory(),
                         self.middle.passthrough.get_trait('text_in').local_path)

        self.middle.passthrough.get_trait('text_out').trait_type._metadata['path'] = \
            os.path.join(self.middle.passthrough.get_abs_directory(),
                         self.middle.passthrough.get_trait('text_out').path)


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

    def test_items(self):
        # This verifies a fix to a problem in Container.items() detected by
        # an unassigned input file variable. It's here to avoid a datatypes
        # dependency in main.
        for name, obj in self.model.items(recurse=True, iotype='in'):
            if name == 'sink.text_file':
                return
        self.fail('sink.text_file not found')


    # Need to come up with some good tests to cover what's not covered
    # in the more general fileref tests (test_filevar). Some of the stuff
    # in there tests creation of the File trait, but it's difficult to
    # separate out what is really testing FileRef functionality (for
    # coverage purposes.)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.lib')
    nose.runmodule()

