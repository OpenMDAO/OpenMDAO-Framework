"""
Test of File traits.
"""

import os
import shutil
import tempfile
import unittest

from openmdao.main.api import Assembly, Component, set_as_top, SimulationRoot
from openmdao.main.datatypes.api import Str, File
from openmdao.util.fileutil import onerror

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """ Produces files. """

    text_data = Str('Hello world', iotype='in')
    text_file = File('source.txt', iotype='out')

    def execute(self):
        """ Write test data to files. """
        with open(self.text_file.path, 'w') as out:
            out.write(self.text_data)


class Passthrough(Component):
    """ Copies input files (implicitly via local_path) to output. """

    text_in = File(iotype='in', local_path='tout')
    text_out = File(iotype='out')

    def execute(self):
        """ Just set name of output file. """
        self.text_out = 'tout'


class Middle(Assembly):
    """ Intermediary which passes-on files. """

    def configure(self):
        comp = Passthrough()
        comp.directory = 'Passthrough'
        comp = self.add('passthrough', comp)
        self.driver.workflow.add('passthrough')
        self.create_passthrough('passthrough.text_in')
        self.create_passthrough('passthrough.text_out')


class Sink(Component):
    """ Consumes files. """

    text_data = Str(iotype='out')
    text_file = File(iotype='in')

    def execute(self):
        """ Read test data from files. """
        with self.text_file.open() as inp:
            self.text_data = inp.read()


class Model(Assembly):
    """ Transfer files from producer to consumer. """

    def configure(self):
        """ Sets passthrough paths to absolute to exercise code. """
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
        self.connect('middle.text_out', 'sink.text_file')


class TestCase(unittest.TestCase):
    """ Test of Files. """

    def setUp(self):
        """ Called before each test in this class. """
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_file-')
        os.chdir(self.tempdir)
        SimulationRoot.chroot(self.tempdir)
        self.model = set_as_top(Model())

    def tearDown(self):
        """ Called after each test in this class. """
        os.chdir(self.startdir)
        SimulationRoot.chroot(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

    def test_items(self):
        # This verifies a fix to a problem in Container.items() detected by
        # an unassigned input file variable. It's here to avoid a datatypes
        # dependency in main.
        for name, obj in self.model.items(recurse=True, iotype='in'):
            if name == 'sink.text_file':
                return
        self.fail('sink.text_file not found')

    def test_basic(self):
        # Just run the model. This model uses strings rather than FileRefs like
        # test_filevar, so the coverage is somewhat different.
        self.model.run()
        self.assertEqual(self.model.sink.text_data, self.model.source.text_data)


    # Need to come up with some good tests to cover what's not covered
    # in the more general fileref tests (test_filevar). Some of the stuff
    # in there tests creation of the File trait, but it's difficult to
    # separate out what is really testing FileRef functionality (for
    # coverage purposes.)


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=openmdao.main.datatypes')
    nose.runmodule()
