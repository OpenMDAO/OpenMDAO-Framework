"""
Test of multiple NPSS instances.
"""

import cPickle
import logging
import os
import os.path
import pkg_resources
import shutil
import unittest

from numpy.testing import assert_equal

from openmdao.main import Assembly, Component, ArrayVariable, Bool, \
                          FileVariable, Float, Int, String, StringList
from openmdao.main.variable import INPUT, OUTPUT

from npsscomponent import NPSScomponent

ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

class Source(Component):
    """ Just something to connect NPSS inputs to. """

    def __init__(self, name='Source', *args, **kwargs):
        super(Source, self).__init__(name, *args, **kwargs)

        Bool('b', self, OUTPUT, default=False)
        Float('f', self, OUTPUT, default=0.)
        ArrayVariable('f1d', self, OUTPUT, entry_type=float, num_dims=1,
                      default=[])
        ArrayVariable('f2d', self, OUTPUT, entry_type=float, num_dims=2,
                      default=[[]])
        ArrayVariable('f3d', self, OUTPUT, entry_type=float, num_dims=3,
                      default=[[[]]])
        Int('i', self, OUTPUT, default=0)
        ArrayVariable('i1d', self, OUTPUT, entry_type=int, num_dims=1,
                      default=[])
        ArrayVariable('i2d', self, OUTPUT, entry_type=int, num_dims=2,
                      default=[[]])
        String('s', self, OUTPUT, default='')
        StringList('s1d', self, OUTPUT, default=[])

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


class Passthrough(NPSScomponent):
    """ An NPSS component that passes-through various types of variable. """

    def __init__(self, name, parent=None, doc=None, directory=''):
        arglist = ['-D', 'XYZZY=twisty narrow passages', '-D', 'FLAG',
                   '-I', '.', '-trace', os.path.join('..', 'passthrough.mdl')]
        super(Passthrough, self).__init__(name, parent, doc, directory,
                                          arglist, 'passthrough.out')

        # Manual interface variable creation.
        Bool('b_in', self, INPUT)
        Float('f_in', self, INPUT)
        ArrayVariable('f1d_in', self, INPUT, entry_type=float, num_dims=1)
        ArrayVariable('f2d_in', self, INPUT, entry_type=float, num_dims=2)
        ArrayVariable('f3d_in', self, INPUT, entry_type=float, num_dims=3)
        Int('i_in', self, INPUT, default=0)
        ArrayVariable('i1d_in', self, INPUT, entry_type=int, num_dims=1)
        ArrayVariable('i2d_in', self, INPUT, entry_type=int, num_dims=2)
        String('s_in', self, INPUT)
        StringList('s1d_in', self, INPUT)
        FileVariable('text_in', self, INPUT, ref_name='text_in.filename')
        FileVariable('binary_in', self, INPUT, ref_name='binary_in.filename')

        # Automagic interface variable creation (not for Bool though).
        Bool('b_out', self, OUTPUT)
        self.make_public([
            ('f_out',      '', OUTPUT),
            ('f1d_out',    '', OUTPUT),
            ('f2d_out',    '', OUTPUT),
            ('f3d_out',    '', OUTPUT),
            ('i_out',      '', OUTPUT),
            ('i1d_out',    '', OUTPUT),
            ('i2d_out',    '', OUTPUT),
            ('s_out',      '', OUTPUT),
            ('s1d_out',    '', OUTPUT),
            ('text_out',   '', OUTPUT),
            ('binary_out', '', OUTPUT)])


class Sink(Component):
    """ Just something to connect NPSS outputs to. """

    def __init__(self, name='Sink', *args, **kwargs):
        super(Sink, self).__init__(name, *args, **kwargs)

        Bool('b', self, INPUT, default=False)
        Float('f', self, INPUT, default=0.)
        ArrayVariable('f1d', self, INPUT, entry_type=float, num_dims=1,
                      default=[])
        ArrayVariable('f2d', self, INPUT, entry_type=float, num_dims=2,
                      default=[[]])
        ArrayVariable('f3d', self, INPUT, entry_type=float, num_dims=3,
                      default=[[[]]])
        Int('i', self, INPUT, default=0)
        ArrayVariable('i1d', self, INPUT, entry_type=int, num_dims=1,
                      default=[])
        ArrayVariable('i2d', self, INPUT, entry_type=int, num_dims=2,
                      default=[[]])
        String('s', self, INPUT, default='')
        StringList('s1d', self, INPUT, default=[])

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
    """ Sends data through Source -> NPSS_A -> NPSS_B -> Sink. """

    def __init__(self, name='TestModel', *args, **kwargs):
        super(Model, self).__init__(name, *args, **kwargs)

        self.workflow.add_node(Source(parent=self))
        self.Source.b = True
        self.Source.f = 3.14159
        self.Source.f1d = [3.14159, 2.781828]
        self.Source.f2d = [[3.14159, 2.781828], [1., 2.]]
        self.Source.f3d = [[[3.14159, 2.781828], [1., 2.]],
                               [[0.1, 1.2], [2.3, 3.4]]]
        self.Source.i = 42
        self.Source.i1d = [42, 666]
        self.Source.i2d = [[42, 666], [9, 8]]
        self.Source.s = 'Hello World!'
        self.Source.s1d = ['the', 'quick', 'brown', 'fox']

        self.Source.text_data = 'Hello World!'
        self.Source.binary_data = [3.14159, 2.781828, 42]

        name = 'NPSS_A'
        directory = \
            os.path.join(pkg_resources.resource_filename('npsscomponent',
                                                         'test'), name)
        Passthrough(name, self, directory=directory)
        self.workflow.add_node(self.NPSS_A)

        name = 'NPSS_B'
        directory = \
            os.path.join(pkg_resources.resource_filename('npsscomponent',
                                                         'test'), name)
        Passthrough(name, self, directory=directory)
        self.workflow.add_node(self.NPSS_B)

        self.workflow.add_node(Sink(parent=self))

        self.connect('Source.b',   'NPSS_A.b_in')
        self.connect('Source.f',   'NPSS_A.f_in')
        self.connect('Source.f1d', 'NPSS_A.f1d_in')
        self.connect('Source.f2d', 'NPSS_A.f2d_in')
        self.connect('Source.f3d', 'NPSS_A.f3d_in')
        self.connect('Source.i',   'NPSS_A.i_in')
        self.connect('Source.i1d', 'NPSS_A.i1d_in')
        self.connect('Source.i2d', 'NPSS_A.i2d_in')
        self.connect('Source.s',   'NPSS_A.s_in')
        self.connect('Source.s1d', 'NPSS_A.s1d_in')
        self.connect('Source.text_file', 'NPSS_A.text_in')
        self.connect('Source.binary_file', 'NPSS_A.binary_in')

        self.connect('NPSS_A.b_out',   'NPSS_B.b_in')
        self.connect('NPSS_A.f_out',   'NPSS_B.f_in')
        self.connect('NPSS_A.f1d_out', 'NPSS_B.f1d_in')
        self.connect('NPSS_A.f2d_out', 'NPSS_B.f2d_in')
        self.connect('NPSS_A.f3d_out', 'NPSS_B.f3d_in')
        self.connect('NPSS_A.i_out',   'NPSS_B.i_in')
        self.connect('NPSS_A.i1d_out', 'NPSS_B.i1d_in')
        self.connect('NPSS_A.i2d_out', 'NPSS_B.i2d_in')
        self.connect('NPSS_A.s_out',   'NPSS_B.s_in')
        self.connect('NPSS_A.s1d_out', 'NPSS_B.s1d_in')
        self.connect('NPSS_A.text_out', 'NPSS_B.text_in')
        self.connect('NPSS_A.binary_out', 'NPSS_B.binary_in')

        self.connect('NPSS_B.b_out',   'Sink.b')
        self.connect('NPSS_B.f_out',   'Sink.f')
        self.connect('NPSS_B.f1d_out', 'Sink.f1d')
        self.connect('NPSS_B.f2d_out', 'Sink.f2d')
        self.connect('NPSS_B.f3d_out', 'Sink.f3d')
        self.connect('NPSS_B.i_out',   'Sink.i')
        self.connect('NPSS_B.i1d_out', 'Sink.i1d')
        self.connect('NPSS_B.i2d_out', 'Sink.i2d')
        self.connect('NPSS_B.s_out',   'Sink.s')
        self.connect('NPSS_B.s1d_out', 'Sink.s1d')
        self.connect('NPSS_B.text_out', 'Sink.text_file')
        self.connect('NPSS_B.binary_out', 'Sink.binary_file')


class NPSSTestCase(unittest.TestCase):

    def setUp(self):
        """ Called before each test in this class. """
        self.model = Model()

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        shutil.rmtree(self.model.NPSS_A.directory)
        shutil.rmtree(self.model.NPSS_B.directory)
        self.model = None
        if os.getcwd() != ORIG_DIR:
            bad_dir = os.getcwd()
            os.chdir(ORIG_DIR)
            self.fail('Ended in %s, expected %s' % (bad_dir, ORIG_DIR))

    def test_connectivity(self):
        logging.debug('')
        logging.debug('test_connectivity')

        self.assertNotEqual(self.model.Sink.b,   self.model.Source.b)
        self.assertNotEqual(self.model.Sink.f,   self.model.Source.f)
        self.assertNotEqual(self.model.Sink.f1d, self.model.Source.f1d)
        self.assertNotEqual(self.model.Sink.f2d, self.model.Source.f2d)
        self.assertNotEqual(self.model.Sink.f3d, self.model.Source.f3d)
        self.assertNotEqual(self.model.Sink.i,   self.model.Source.i)
        self.assertNotEqual(self.model.Sink.i1d, self.model.Source.i1d)
        self.assertNotEqual(self.model.Sink.i2d, self.model.Source.i2d)
        self.assertNotEqual(self.model.Sink.s,   self.model.Source.s)
        self.assertNotEqual(self.model.Sink.s1d, self.model.Source.s1d)
        self.assertNotEqual(self.model.Sink.text_data,
                            self.model.Source.text_data)
        self.assertNotEqual(self.model.Sink.binary_data,
                            self.model.Source.binary_data)
        self.assertNotEqual(
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

        self.model.run()

        self.assertEqual(self.model.Sink.b, self.model.Source.b)
        self.assertEqual(self.model.Sink.f, self.model.Source.f)
        assert_equal(self.model.Sink.f1d, self.model.Source.f1d)
        assert_equal(self.model.Sink.f2d, self.model.Source.f2d)
        assert_equal(self.model.Sink.f3d, self.model.Source.f3d)
        self.assertEqual(self.model.Sink.i, self.model.Source.i)
        assert_equal(self.model.Sink.i1d, self.model.Source.i1d)
        assert_equal(self.model.Sink.i2d, self.model.Source.i2d)
        self.assertEqual(self.model.Sink.s, self.model.Source.s)
        self.assertEqual(self.model.Sink.s1d, self.model.Source.s1d)
        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(self.model.Sink.binary_data,
                         self.model.Source.binary_data)
        self.assertEqual(
            self.model.Sink.getvar('binary_file').metadata['binary'], True)

        for path in ('source.txt', 'source.bin', 'sink.txt', 'sink.bin'):
            os.remove(path)  # Will raise exception if any files don't exist.

    def test_preprocessor(self):
        logging.debug('')
        logging.debug('test_preprocessor')

        self.assertEqual(self.model.NPSS_A.xyzzy_val, 'twisty narrow passages')
        self.assertEqual(self.model.NPSS_A.flag_val, 1)


if __name__ == '__main__':
    unittest.main()

