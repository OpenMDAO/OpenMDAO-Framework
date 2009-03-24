"""
Test of FileVariables.
"""

import cPickle
import unittest

from openmdao.main import Assembly, Component, \
                          ArrayVariable, FileVariable, StringList
from openmdao.main.component import RUN_OK
from openmdao.main.variable import INPUT, OUTPUT

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """ Produces files. """

    def __init__(self, name='Source', parent=None, directory=''):
        super(Source, self).__init__(name, parent, directory=directory)
        StringList('text_data', self, INPUT, default=[])
        ArrayVariable('binary_data', self, INPUT, float, default=[])
        FileVariable('text_file', self, OUTPUT, default='source.txt')
        FileVariable('binary_file', self, OUTPUT, default='source.bin',
                     metadata={'binary':True})

    def execute(self):
        """ Write test data to files. """
        out = open(self.text_file, 'w')
        out.write(self.text_data)
        out.close()

        out = open(self.binary_file, 'wb')
        cPickle.dump(self.binary_data, out, 2)
        out.close()

        return RUN_OK


class Sink(Component):
    """ Consumes files. """

    def __init__(self, name='Sink', parent=None, directory=''):
        super(Sink, self).__init__(name, parent, directory=directory)
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

        return RUN_OK


class FileTestCase(unittest.TestCase):

    def setUp(self):
        """ Called before each test in this class. """
        self.tla = Assembly('TLA')
        self.tla.workflow.add_node(Source(parent=self.tla, directory='Source'))
        self.tla.workflow.add_node(Sink(parent=self.tla, directory='Sink'))

        self.tla.connect('Source.text_file', 'Sink.text_file')
        self.tla.connect('Source.binary_file', 'Sink.binary_file')

        self.tla.Source.text_data = 'Hello World!'
        self.tla.Source.binary_data = [3.14159, 2.781828, 42]

    def tearDown(self):
        """ Called after each test in this class. """
        pass

    def test_connectivity(self):
        self.assertNotEqual(self.tla.Sink.text_data, self.tla.Source.text_data)
        self.assertNotEqual(self.tla.Sink.binary_data, self.tla.Source.binary_data)
        self.assertNotEqual(
            self.tla.Sink.getvar('binary_file').metadata['binary'], True)

        self.tla.run()

        self.assertEqual(self.tla.Sink.text_data, self.tla.Source.text_data)
        self.assertEqual(self.tla.Sink.binary_data, self.tla.Source.binary_data)
        self.assertEqual(
            self.tla.Sink.getvar('binary_file').metadata['binary'], True)


if __name__ == '__main__':
    unittest.main()

