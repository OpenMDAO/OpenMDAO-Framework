"""
Test saving and loading of simulations as eggs.
"""

import cPickle
import logging
import os
import shutil
import unittest

from openmdao.main import Assembly, Component, \
                          ArrayVariable, FileVariable, StringList, Bool
from openmdao.main.variable import INPUT, OUTPUT

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

EXTERNAL_FILENAME = 'xyzzy'

source_init = False
sink_init = False


class Source(Component):
    """ Produces files. """

    def __init__(self, name='Source', *args, **kwargs):
        super(Source, self).__init__(name, *args, **kwargs)

        global source_init
        source_init = True

        Bool('write_files', self, INPUT, default=True)
        StringList('text_data', self, INPUT, default=[])
        ArrayVariable('binary_data', self, INPUT, float, default=[])
        FileVariable('text_file', self, OUTPUT, default='source.txt')
        FileVariable('binary_file', self, OUTPUT, default='source.bin',
                     metadata={'binary':True})

        # Example of external file that isn't a variable.
        self.push_dir(self.get_directory())
        out = open(EXTERNAL_FILENAME, 'w')
        out.write('Twisty narrow passages.\n')
        out.close()
        self.pop_dir()
        self.external_files.append({'path':EXTERNAL_FILENAME})

    def execute(self):
        """ Write test data to files. """
        if self.write_files:
            out = open(self.text_file, 'w')
            out.write(self.text_data)
            out.close()

            out = open(self.binary_file, 'wb')
            cPickle.dump(self.binary_data, out, 2)
            out.close()


class Sink(Component):
    """ Consumes files. """

    def __init__(self, name='Sink', *args, **kwargs):
        super(Sink, self).__init__(name, *args, **kwargs)

        global sink_init
        sink_init = True

        StringList('text_data', self, OUTPUT, default=[])
        ArrayVariable('binary_data', self, OUTPUT, float, default=[])
        FileVariable('text_file', self, INPUT, default='sink.txt')
        FileVariable('binary_file', self, INPUT, default='sink.bin')

    def execute(self):
        """ Read test data from files. """
        inp = open(self.text_file, 'r')
        self.text_data = inp.read()
        inp.close()

        inp = open(self.binary_file, 'rb')
        self.binary_data = cPickle.load(inp)
        inp.close()


class Model(Assembly):
    """ Transfer files from producer to consumer. """

    def __init__(self, name='Egg_TestModel', *args, **kwargs):
        super(Model, self).__init__(name, *args, **kwargs)

        self.workflow.add_node(Source(parent=self, directory='Source'))
        self.workflow.add_node(Sink(parent=self, directory='Sink'))

        self.connect('Source.text_file', 'Sink.text_file')
        self.connect('Source.binary_file', 'Sink.binary_file')

        self.Source.text_data = 'Hello World!'
        self.Source.binary_data = [3.14159, 2.781828, 42]


class EggTestCase(unittest.TestCase):

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
        if os.path.exists('EggTest'):
            shutil.rmtree('EggTest')

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        global source_init, sink_init

        # Verify initial state.
        self.assertEqual(source_init, True)
        self.assertEqual(sink_init, True)
        self.assertNotEqual(self.model.Sink.text_data,
                            self.model.Source.text_data)
        self.assertNotEqual(self.model.Sink.binary_data,
                            self.model.Source.binary_data)
        self.assertNotEqual(
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

        external = os.path.join(self.model.get_directory(),
                                'Source', EXTERNAL_FILENAME)
        self.assertEqual(os.path.exists(external), True)

        # Save to egg.
        self.egg_name = self.model.save_to_egg()

        # Run and verify correct operation.
        self.model.run()
        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(self.model.Sink.binary_data,
                         self.model.Source.binary_data)
        self.assertEqual(
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

        # Restore in test directory.
        orig_dir = os.getcwd()
        os.mkdir('EggTest')
        os.chdir('EggTest')
        try:
            # Clear flags to detect if loading calls __init__.
            source_init = False
            sink_init = False

            # Load from saved initial state in egg.
            self.model.pre_delete()
            self.model = Component.load_from_egg(os.path.join('..',
                                                              self.egg_name))
            self.model.post_load()

            # Verify initial state.
            self.assertEqual(source_init, False)
            self.assertEqual(sink_init, False)
            self.assertNotEqual(self.model.Sink.text_data,
                                self.model.Source.text_data)
            self.assertNotEqual(self.model.Sink.binary_data,
                                self.model.Source.binary_data)
            self.assertNotEqual(
                self.model.Sink.getvar('binary_file').metadata['binary'], True)

            external = os.path.join(self.model.get_directory(),
                                    'Source', EXTERNAL_FILENAME)
            self.assertEqual(os.path.exists(external), True)

            # Run and verify correct operation.
            self.model.run()
            self.assertEqual(self.model.Sink.text_data,
                             self.model.Source.text_data)
            self.assertEqual(self.model.Sink.binary_data,
                             self.model.Source.binary_data)
            self.assertEqual(
                self.model.Sink.getvar('binary_file').metadata['binary'], True)

        finally:
            os.chdir(orig_dir)


if __name__ == '__main__':
    unittest.main()

