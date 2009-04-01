"""
Test of FileVariables.
"""

import cPickle
import shutil
import unittest

from openmdao.main import Assembly, Component, \
                          ArrayVariable, FileVariable, StringList, Bool
from openmdao.main.variable import INPUT, OUTPUT

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """ Produces files. """

    def __init__(self, name='Source', *args, **kwargs):
        super(Source, self).__init__(name, *args, **kwargs)
        Bool('write_files', self, INPUT, default=True)
        StringList('text_data', self, INPUT, default=[])
        ArrayVariable('binary_data', self, INPUT, float, default=[])
        FileVariable('text_file', self, OUTPUT, default='source.txt')
        FileVariable('binary_file', self, OUTPUT, default='source.bin',
                     metadata={'binary':True})

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

    def __init__(self, name='FileVar_TestModel', *args, **kwargs):
        super(Model, self).__init__(name, *args, **kwargs)

        self.workflow.add_node(Source(parent=self, directory='Source'))
        self.workflow.add_node(Sink(parent=self, directory='Sink'))

        self.connect('Source.text_file', 'Sink.text_file')
        self.connect('Source.binary_file', 'Sink.binary_file')

        self.Source.text_data = 'Hello World!'
        self.Source.binary_data = [3.14159, 2.781828, 42]


class FileTestCase(unittest.TestCase):

    def setUp(self):
        """ Called before each test in this class. """
        self.model = Model()

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        shutil.rmtree('Source')
        shutil.rmtree('Sink')
        self.model = None

    def test_connectivity(self):
        self.assertNotEqual(self.model.Sink.text_data,
                            self.model.Source.text_data)
        self.assertNotEqual(self.model.Sink.binary_data,
                            self.model.Source.binary_data)
        self.assertNotEqual(
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

        self.model.run()

        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(self.model.Sink.binary_data,
                         self.model.Source.binary_data)
        self.assertEqual(
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

    def test_src_failure(self):
        self.model.Source.write_files = False
        try:
            self.model.run()
        except IOError, exc:
            if str(exc).find('source.txt') < 0:
                self.fail("Wrong message '%s'" % str(exc))
        else:
            self.fail('IOError expected')


if __name__ == '__main__':
    unittest.main()

