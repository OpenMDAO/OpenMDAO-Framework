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

import numpy
from numpy.testing import assert_equal

from enthought.traits.api import Float, Int, Str, List, Bool, Array

from openmdao.main.api import Assembly, Component, Container, FileTrait
from openmdao.main.component import SimulationRoot

from npsscomponent import NPSScomponent

ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """ Just something to connect NPSS inputs to. """

    b = Bool(False, iostatus='out')
    f = Float(0., iostatus='out')
    f1d = Array(dtype=numpy.float, shape=(None,), 
                iostatus='out')
    f2d = Array(dtype=numpy.float, shape=(None,None),
                iostatus='out')
    f3d = Array(dtype=numpy.float, shape=(None,None,None),
                iostatus='out')
    i = Int(0, iostatus='out')
    i1d = Array(dtype=numpy.int, shape=(None,), 
                iostatus='out')
    i2d = Array(dtype=numpy.int, shape=(None,None), 
                iostatus='out')
    s = Str('', iostatus='out')
    s1d = List(str, iostatus='out')

    text_data = Str(iostatus='in')
    binary_data = Array(dtype=numpy.float, shape=(None,), iostatus='in')
    text_file = FileTrait(iostatus='out')
    binary_file = FileTrait(iostatus='out', binary=True)
        
    def __init__(self, name='Source', *args, **kwargs):
        super(Source, self).__init__(name, *args, **kwargs)
        self.text_file.filename = 'sink.txt'
        self.binary_file.filename = 'sink.bin'

        SourceData(name='sub', parent=self)

    def execute(self):
        """ Write test data to files. """
        out = open(self.text_file, 'w')
        out.write(self.text_data)
        out.close()

        out = open(self.binary_file, 'wb')
        cPickle.dump(self.binary_data, out, 2)
        out.close()


class SourceData(Container):
    """ Sub-container data. """

    b = Bool(False, iostatus='out')
    f = Float(0., iostatus='out')
    f1d = Array(dtype=numpy.float, shape=(None,), 
                iostatus='out')
    f2d = Array(dtype=numpy.float, shape=(None,None),
                iostatus='out')
    f3d = Array(dtype=numpy.float, shape=(None,None,None),
                iostatus='out')
    i = Int(0, iostatus='out')
    i1d = Array(dtype=numpy.int, shape=(None,), 
                iostatus='out')
    i2d = Array(dtype=numpy.int, shape=(None,None), 
                iostatus='out')
    s = Str('', iostatus='out')
    s1d = List(str, iostatus='out')
        
    def __init__(self, name='SourceData', *args, **kwargs):
        super(SourceData, self).__init__(name, *args, **kwargs)


class Passthrough(NPSScomponent):
    """ An NPSS component that passes-through various types of variable. """

    def __init__(self, name, parent=None, doc=None, directory=''):
        arglist = ['-D', 'XYZZY=twisty narrow passages', '-D', 'FLAG',
                   '-I', '.', '-trace', os.path.join('..', 'passthrough.mdl')]
        super(Passthrough, self).__init__(name, parent, doc, directory,
                                          arglist, 'passthrough.out')

        # Manual interface variable creation.
        # (skip 'f_in' to exercise connect().)
        self.add_trait('b_in', Bool(iostatus='in'))
        self.add_trait('f1d_in', Array(dtype=numpy.float, shape=(None,), iostatus='in'))
        self.add_trait('f2d_in', Array(dtype=numpy.float, shape=(None,None), iostatus='in'))
        self.add_trait('f3d_in', Array(dtype=numpy.float, shape=(None,None,None), 
                                       iostatus='in'))
        self.add_trait('i_in', Int(0, iostatus='in'))
        self.add_trait('i1d_in', Array(dtype=numpy.int, shape=(None,), iostatus='in'))
        self.add_trait('i2d_in', Array(dtype=numpy.int, shape=(None,None), iostatus='in'))
        self.add_trait('s_in', Str(iostatus='in'))
        self.add_trait('s1d_in', List(str, iostatus='in'))
        
        self.add_trait('text_in', 
                       FileTrait(iostatus='in', ref_name='text_in.filename'))
        self.add_trait('binary_in',
                       FileTrait(iostatus='in', ref_name='binary_in.filename'))
        
        # Automagic interface variable creation (not for Bool though).
        # (skip 'f_out' to exercise connect().)
        self.add_trait('b_out', Bool(iostatus='out'))
        self.make_public([
            ('f1d_out',    '', 'out'),
            ('f2d_out',    '', 'out'),
            ('f3d_out',    '', 'out'),
            ('i_out',      '', 'out'),
            ('i1d_out',    '', 'out'),
            ('i2d_out',    '', 'out'),
            ('s_out',      '', 'out'),
            ('s1d_out',    '', 'out'),
            ('text_out',   '', 'out'),
            ('binary_out', '', 'out')])

        ## Sub-container needs Bools explicitly declared.
        #Bool('sub.b_in', self, iostatus='in')
        #Bool('sub.b_out', self, iostatus='out')


class Sink(Component):
    """ Just something to connect NPSS outputs to. """

    b = Bool(False, iostatus='in')
    f = Float(0., iostatus='in')
    f1d = Array(dtype=numpy.float, shape=(None,), 
                iostatus='in')
    f2d = Array(dtype=numpy.float, shape=(None,None),
                iostatus='in')
    f3d = Array(dtype=numpy.float, shape=(None,None,None),
                iostatus='in')
    i = Int(0, iostatus='in')
    i1d = Array(dtype=numpy.int, shape=(None,), 
                iostatus='in')
    i2d = Array(dtype=numpy.int, shape=(None,None), 
                iostatus='in')
    s = Str('', iostatus='in')
    s1d = List(str, iostatus='in')
    
    text_data = List(str, iostatus='out')
    binary_data = Array(dtype=numpy.float, shape=(None,),
                        iostatus='out')
    text_file = FileTrait(iostatus='in')
    binary_file = FileTrait(iostatus='in')
        
    def __init__(self, name='Sink', *args, **kwargs):
        super(Sink, self).__init__(name, *args, **kwargs)
        self.text_file.filename = 'sink.txt'
        self.binary_file.filename = 'sink.bin'

        SinkData(name='sub', parent=self)

    def execute(self):
        """ Read test data from files. """
        inp = open(self.text_file, 'r')
        self.text_data = inp.read()
        inp.close()

        inp = open(self.binary_file, 'rb')
        self.binary_data = cPickle.load(inp)
        inp.close()


class SinkData(Container):
    """ Sub-container data. """

    b = Bool(False, iostatus='in')
    f = Float(0., iostatus='in')
    f1d = Array(dtype=numpy.float, shape=(None,), 
                iostatus='in')
    f2d = Array(dtype=numpy.float, shape=(None,None),
                iostatus='in')
    f3d = Array(dtype=numpy.float, shape=(None,None,None),
                iostatus='in')
    i = Int(0, iostatus='in')
    i1d = Array(dtype=numpy.int, shape=(None,), 
                iostatus='in')
    i2d = Array(dtype=numpy.int, shape=(None,None), 
                iostatus='in')
    s = Str('', iostatus='in')
    s1d = List(str, iostatus='in')
    
    def __init__(self, name='SinkData', *args, **kwargs):
        super(SinkData, self).__init__(name, *args, **kwargs)

class Model(Assembly):
    """ Sends data through Source -> NPSS_A -> NPSS_B -> Sink. """

    def connect(self, src_path, dst_path):
        """ Overriding default to dynamically publicise/hoist variables. """
        comp, rest = src_path.split('.', 1)
        src_comp = getattr(self, comp)
        if rest.find('.') > 0:
            src_path = self.hoist(src_comp, rest, 'out')
        else:
            if src_comp.trait(rest) is None:
                src_comp.make_public((rest, '', 'out'))
                self._var_graph.add_node(src_path)

        comp, rest = dst_path.split('.', 1)
        dst_comp = getattr(self, comp)
        if rest.find('.') > 0:
            dst_path = self.hoist(dst_comp, rest, iostatus='in')
        else:
            if dst_comp.trait(rest) is None:
                dst_comp.make_public(rest)
                self._var_graph.add_node(dst_path)

        super(Model, self).connect(src_path, dst_path)

    def hoist(self, comp, path, io_status):
        """ Hoist a variable so that it may be connected. """
        name = '_'+path.replace('.', '_')
        trait = comp.trait(path)
        if trait is None:
            comp.make_public((name, path, io_status))
            trait = comp.trait(name)

        newpath = '.'.join([comp.name,name])
        if newpath not in self._var_graph:
            self.create_passthru(newpath)
        return newpath
    
    
    def __init__(self, name='TestModel', *args, **kwargs):
        super(Model, self).__init__(name, *args, **kwargs)

        Source(parent=self)
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

        self.Source.sub.b = True
        self.Source.sub.f = 1.2345
        self.Source.sub.f1d = [1.2345, 3.14159, 2.781828]
        self.Source.sub.f2d = [[1.2345, 3.14159, 2.781828], [1., 2., 3.]]
        self.Source.sub.f3d = [[[1.2345, 3.14159, 2.781828], [1., 2., 3.]],
                               [[0.1, 1.2, 2.3], [2.3, 3.4, 4.5]]]
        self.Source.sub.i = 24
        self.Source.sub.i1d = [24, 42, 666]
        self.Source.sub.i2d = [[24, 42, 666], [9, 8, 7]]
        self.Source.sub.s = 'xyzzy'
        self.Source.sub.s1d = ['maze', 'of', 'twisty', 'passages']

        name = 'NPSS_A'
        Passthrough(name, self, directory=name)

        name = 'NPSS_B'
        Passthrough(name, self, directory=name)

        Sink(parent=self)

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

        self.connect('Source.sub.b',   'NPSS_A.sub.b_in')
        self.connect('Source.sub.f',   'NPSS_A.sub.f_in')
        self.connect('Source.sub.f1d', 'NPSS_A.sub.f1d_in')
        self.connect('Source.sub.f2d', 'NPSS_A.sub.f2d_in')
        self.connect('Source.sub.f3d', 'NPSS_A.sub.f3d_in')
        self.connect('Source.sub.i',   'NPSS_A.sub.i_in')
        self.connect('Source.sub.i1d', 'NPSS_A.sub.i1d_in')
        self.connect('Source.sub.i2d', 'NPSS_A.sub.i2d_in')
        self.connect('Source.sub.s',   'NPSS_A.sub.s_in')
        self.connect('Source.sub.s1d', 'NPSS_A.sub.s1d_in')


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

        self.connect('NPSS_A.sub.b_out',   'NPSS_B.sub.b_in')
        self.connect('NPSS_A.sub.f_out',   'NPSS_B.sub.f_in')
        self.connect('NPSS_A.sub.f1d_out', 'NPSS_B.sub.f1d_in')
        self.connect('NPSS_A.sub.f2d_out', 'NPSS_B.sub.f2d_in')
        self.connect('NPSS_A.sub.f3d_out', 'NPSS_B.sub.f3d_in')
        self.connect('NPSS_A.sub.i_out',   'NPSS_B.sub.i_in')
        self.connect('NPSS_A.sub.i1d_out', 'NPSS_B.sub.i1d_in')
        self.connect('NPSS_A.sub.i2d_out', 'NPSS_B.sub.i2d_in')
        self.connect('NPSS_A.sub.s_out',   'NPSS_B.sub.s_in')
        self.connect('NPSS_A.sub.s1d_out', 'NPSS_B.sub.s1d_in')


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

        self.connect('NPSS_B.sub.b_out',   'Sink.sub.b')
        self.connect('NPSS_B.sub.f_out',   'Sink.sub.f')
        self.connect('NPSS_B.sub.f1d_out', 'Sink.sub.f1d')
        self.connect('NPSS_B.sub.f2d_out', 'Sink.sub.f2d')
        self.connect('NPSS_B.sub.f3d_out', 'Sink.sub.f3d')
        self.connect('NPSS_B.sub.i_out',   'Sink.sub.i')
        self.connect('NPSS_B.sub.i1d_out', 'Sink.sub.i1d')
        self.connect('NPSS_B.sub.i2d_out', 'Sink.sub.i2d')
        self.connect('NPSS_B.sub.s_out',   'Sink.sub.s')
        self.connect('NPSS_B.sub.s1d_out', 'Sink.sub.s1d')


class NPSSTestCase(unittest.TestCase):

    directory = pkg_resources.resource_filename('npsscomponent', 'test')

    def setUp(self):
        """ Called before each test in this class. """
        # Set new simulation root so we can legally access files.
        SimulationRoot.chdir(NPSSTestCase.directory)
        self.model = Model()

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        shutil.rmtree(self.model.NPSS_A.directory)
        shutil.rmtree(self.model.NPSS_B.directory)
        self.model = None
        end_dir = os.getcwd()
        SimulationRoot.chdir(ORIG_DIR)
        if end_dir != NPSSTestCase.directory:
            self.fail('Ended in %s, expected %s' \
                      % (end_dir, NPSSTestCase.directory))

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
            self.model.Sink.binary_file.metadata['binary'], True)

        self.assertNotEqual(self.model.Sink.sub.b,   self.model.Source.sub.b)
        self.assertNotEqual(self.model.Sink.sub.f,   self.model.Source.sub.f)
        self.assertNotEqual(self.model.Sink.sub.f1d, self.model.Source.sub.f1d)
        self.assertNotEqual(self.model.Sink.sub.f2d, self.model.Source.sub.f2d)
        self.assertNotEqual(self.model.Sink.sub.f3d, self.model.Source.sub.f3d)
        self.assertNotEqual(self.model.Sink.sub.i,   self.model.Source.sub.i)
        self.assertNotEqual(self.model.Sink.sub.i1d, self.model.Source.sub.i1d)
        self.assertNotEqual(self.model.Sink.sub.i2d, self.model.Source.sub.i2d)
        self.assertNotEqual(self.model.Sink.sub.s,   self.model.Source.sub.s)
        self.assertNotEqual(self.model.Sink.sub.s1d, self.model.Source.sub.s1d)

        self.model.run()

        self.assertEqual(self.model.Sink.b,   self.model.Source.b)
        self.assertEqual(self.model.Sink.f,   self.model.Source.f)
        assert_equal(self.model.Sink.f1d,     self.model.Source.f1d)
        assert_equal(self.model.Sink.f2d,     self.model.Source.f2d)
        assert_equal(self.model.Sink.f3d,     self.model.Source.f3d)
        self.assertEqual(self.model.Sink.i,   self.model.Source.i)
        assert_equal(self.model.Sink.i1d,     self.model.Source.i1d)
        assert_equal(self.model.Sink.i2d,     self.model.Source.i2d)
        self.assertEqual(self.model.Sink.s,   self.model.Source.s)
        self.assertEqual(self.model.Sink.s1d, self.model.Source.s1d)

        self.assertEqual(self.model.Sink.text_data,
                         self.model.Source.text_data)
        self.assertEqual(self.model.Sink.binary_data,
                         self.model.Source.binary_data)
        self.assertEqual(
            self.model.Sink.binary_file.metadata['binary'], True)

        self.assertEqual(self.model.Sink.sub.b,   self.model.Source.sub.b)
        self.assertEqual(self.model.Sink.sub.f,   self.model.Source.sub.f)
        assert_equal(self.model.Sink.sub.f1d,     self.model.Source.sub.f1d)
        assert_equal(self.model.Sink.sub.f2d,     self.model.Source.sub.f2d)
        assert_equal(self.model.Sink.sub.f3d,     self.model.Source.sub.f3d)
        self.assertEqual(self.model.Sink.sub.i,   self.model.Source.sub.i)
        assert_equal(self.model.Sink.sub.i1d,     self.model.Source.sub.i1d)
        assert_equal(self.model.Sink.sub.i2d,     self.model.Source.sub.i2d)
        self.assertEqual(self.model.Sink.sub.s,   self.model.Source.sub.s)
        self.assertEqual(self.model.Sink.sub.s1d, self.model.Source.sub.s1d)

        for path in ('source.txt', 'source.bin', 'sink.txt', 'sink.bin'):
            os.remove(path)  # Will raise exception if any files don't exist.

    def test_preprocessor(self):
        logging.debug('')
        logging.debug('test_preprocessor')

        self.assertEqual(self.model.NPSS_A.xyzzy_val, 'twisty narrow passages')
        self.assertEqual(self.model.NPSS_A.flag_val, 1)


if __name__ == '__main__':
    unittest.main()

