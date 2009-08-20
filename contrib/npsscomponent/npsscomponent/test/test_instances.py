"""
Test of multiple NPSS instances.
Various data types are sent from 'Source' to 'NPSS_A' to 'NPSS_B' and finally
on to 'Sink'.  Both instances of NPSS are running passthrough.mdl.

Two types of model are constructed: 'direct' as described above, and 'indirect'
which puts 'NPSS_A' and 'NPSS_B' inside an Assembly.  This is to exercise the
interaction of NPSS Property traits with passthru connections.
"""

import cPickle
import logging
import os.path
import pkg_resources
import shutil
import unittest

import numpy
from numpy.testing import assert_equal

from enthought.traits.api import Float, Int, Str, List, CBool, Array

from openmdao.main.api import Assembly, Component, Container, FileTrait, set_as_top
from openmdao.main.component import SimulationRoot

from npsscomponent import NPSScomponent, NPSSProperty

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()

# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"


class Source(Component):
    """
    Just something to connect NPSS inputs to.
    Has an instance of each supported NPSS data type as an output.
    """
    b = CBool(False, iostatus='out')
    f = Float(0., iostatus='out')
    f1d = Array(dtype=numpy.float, shape=(None,), iostatus='out')
    f2d = Array(dtype=numpy.float, shape=(None, None), iostatus='out')
    f3d = Array(dtype=numpy.float, shape=(None, None, None), iostatus='out')
    i = Int(0, iostatus='out')
    i1d = Array(dtype=numpy.int, shape=(None,), iostatus='out')
    i2d = Array(dtype=numpy.int, shape=(None, None), iostatus='out')
    s = Str('', iostatus='out')
    s1d = List(str, iostatus='out')

    text_data = Str(iostatus='in')
    binary_data = Array(dtype=numpy.float, shape=(None,), iostatus='in')
    text_file = FileTrait(iostatus='out')
    binary_file = FileTrait(iostatus='out', binary=True)
        
    def hierarchy_defined(self):
        super(Source, self).hierarchy_defined()
        
        self.text_file.filename = 'source.txt'
        self.binary_file.filename = 'source.bin'

        self.add_container('sub', SourceData())
        self.add_container('sub_sub', SourceSubData())

    def execute(self):
        """ Write test data to files. """
        out = open(self.text_file.filename, 'w')
        out.write(self.text_data)
        out.close()

        out = open(self.binary_file.filename, 'wb')
        cPickle.dump(self.binary_data, out, 2)
        out.close()


class SourceData(Container):
    """ Sub-container data. """

    b = CBool(False, iostatus='out')
    f = Float(0., iostatus='out')
    f1d = Array(dtype=numpy.float, shape=(None,), iostatus='out')
    f2d = Array(dtype=numpy.float, shape=(None, None), iostatus='out')
    f3d = Array(dtype=numpy.float, shape=(None, None, None), iostatus='out')
    i = Int(0, iostatus='out')
    i1d = Array(dtype=numpy.int, shape=(None,), iostatus='out')
    i2d = Array(dtype=numpy.int, shape=(None, None), iostatus='out')
    s = Str('', iostatus='out')
    s1d = List(str, iostatus='out')
        

class SourceSubData(Container):
    """
    Sub-sub-container data.
    To exercise connections multiple levels deep.
    """
    ii = Int(0, iostatus='out')


class Passthrough(NPSScomponent):
    """ An NPSS component that passes-through various types of variable. """

    def __init__(self, indirect=False, doc=None, directory=''):
        # Probably only need '-I .' here, others are just for testing.
        arglist = ['-D', 'XYZZY=twisty narrow passages', '-D', 'FLAG',
                   '-I', '.', '-trace']
        mdl = os.path.join('..', 'passthrough.mdl')
        if indirect:
            # When inside an assembly we run in a subdirectory.
            mdl = os.path.join('..', mdl)
        arglist.append(mdl)
        super(Passthrough, self).__init__(doc, directory,
                                          arglist, 'passthrough.out')
        
    def hierarchy_defined(self):
        super(Passthrough, self).hierarchy_defined()

        # Manual interface variable creation.
        # BAN - manual interface variable creation is error prone, because
        #       you can easily create traits that don't talk to the underlying
        #       NPSS model if you're not careful.  All traits talking to NPSS
        #       need to be wrapped in a NPSSProperty trait, which _build_trait()
        #       and make_public() do for you automatically.
        
        #  the following add_trait call adds a trait that will not talk to NPSS
        #      self.add_trait('b_out', CBool(iostatus='out'))
        
        #  this call to add_trait will have the 'b_out' attribute talk to NPSS
        self.add_trait('b_out', NPSSProperty(trait=CBool(iostatus='out')))
                
        self.make_public(['b_in', 'f1d_in', 'f2d_in', 'f3d_in',
                          'i_in', 'i1d_in', 'i2d_in', 's_in', 's1d_in',
                          'text_in', 'binary_in'], iostatus='in')
        
        # Automagic interface variable creation (not for Bool though).
        self.make_public(['f1d_out', 'f2d_out', 'f3d_out',
                          'i_out', 'i1d_out', 'i2d_out', 's_out', 's1d_out',
                          'text_out', 'binary_out'], iostatus='out')

        # (skip 'f_in' to test dynamic trait creation during connect().)
        # (skip 'f_out' to test dynamic trait creation during connect().)
        
        # Sub-container needs Bools explicitly declared.
        self.hoist('sub.b_in', 'in', trait=CBool(iostatus='in'))
        self.hoist('sub.b_out', 'out', trait=CBool(iostatus='out'))


class Sink(Component):
    """
    Just something to connect NPSS outputs to.
    Has an instance of each supported NPSS data type as an input.
    """

    b = CBool(False, iostatus='in')
    f = Float(0., iostatus='in')
    f1d = Array(dtype=numpy.float, shape=(None,), iostatus='in')
    f2d = Array(dtype=numpy.float, shape=(None, None), iostatus='in')
    f3d = Array(dtype=numpy.float, shape=(None, None, None), iostatus='in')
    i = Int(0, iostatus='in')
    i1d = Array(dtype=numpy.int, shape=(None,), iostatus='in')
    i2d = Array(dtype=numpy.int, shape=(None, None), iostatus='in')
    s = Str('', iostatus='in')
    s1d = List(str, iostatus='in')
    
    text_data = Str(iostatus='out')
    binary_data = Array(dtype=numpy.float, shape=(None,), iostatus='out')
    text_file = FileTrait(iostatus='in')
    binary_file = FileTrait(iostatus='in')
        
    def hierarchy_defined(self):
        super(Sink, self).hierarchy_defined()
        
        self.text_file.filename = 'sink.txt'
        self.binary_file.filename = 'sink.bin'

        self.add_container('sub', SinkData())
        self.add_container('sub_sub', SinkSubData())

    def execute(self):
        """ Read test data from files. """
        inp = open(self.text_file.filename, 'r')
        self.text_data = inp.read()
        inp.close()

        inp = open(self.binary_file.filename, 'rb')
        self.binary_data = cPickle.load(inp)
        inp.close()


class SinkData(Container):
    """ Sub-container data. """

    b = CBool(False, iostatus='in')
    f = Float(0., iostatus='in')
    f1d = Array(dtype=numpy.float, shape=(None,), iostatus='in')
    f2d = Array(dtype=numpy.float, shape=(None, None), iostatus='in')
    f3d = Array(dtype=numpy.float, shape=(None, None, None), iostatus='in')
    i = Int(0, iostatus='in')
    i1d = Array(dtype=numpy.int, shape=(None,), iostatus='in')
    i2d = Array(dtype=numpy.int, shape=(None, None), iostatus='in')
    s = Str('', iostatus='in')
    s1d = List(str, iostatus='in')
    

class SinkSubData(Container):
    """ Sub-sub-container data. """

    ii = Int(0, iostatus='in')


class Model(Assembly):
    """
    Sends data through Source -> NPSS_A -> NPSS_B -> Sink.
    If 'indirect', then NPSS_A and NPSS_B are placed inside a subassembly.
    """
    
    #name='TestModel', 
    def __init__(self, indirect=False, *args, **kwargs):
        super(Model, self).__init__(*args, **kwargs)        
        self._indirect = indirect
        
    def hierarchy_defined(self):
        super(Model, self).hierarchy_defined()
        
        self.add_container('Source', Source())
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

        self.Source.sub_sub.ii = 12345678

        self.add_container('Sink', Sink())

        vnames = ('b', 'f', 'f1d', 'f2d', 'f3d', 'i', 'i1d', 'i2d', 's', 's1d')

        if self._indirect:
            # Exercise passthru capability by placing NPSS instances
            # in a subassembly.
            assembly = self.add_container('Assembly',
                                          Assembly(directory='Assembly'))
            for name in ['NPSS_A', 'NPSS_B']:
                assembly.add_container(name, 
                                       Passthrough(self._indirect, directory=name))
            for var in vnames:
                assembly.create_passthru('NPSS_A.'+var+'_in')
                assembly.create_passthru('NPSS_A.sub.'+var+'_in',
                                         'sub_'+var+'_in')
                assembly.connect('NPSS_A.'+var+'_out', 'NPSS_B.'+var+'_in')
                assembly.connect('NPSS_A.sub.'+var+'_out',
                                 'NPSS_B.sub.'+var+'_in')
                assembly.create_passthru('NPSS_B.'+var+'_out')
                assembly.create_passthru('NPSS_B.sub.'+var+'_out',
                                         'sub_'+var+'_out')

            assembly.create_passthru('NPSS_A.text_in')
            assembly.create_passthru('NPSS_A.binary_in')
            assembly.create_passthru('NPSS_A.sub.sub.i_in', 'sub_sub_i_in')

            assembly.connect('NPSS_A.text_out', 'NPSS_B.text_in')
            assembly.connect('NPSS_A.binary_out', 'NPSS_B.binary_in')
            assembly.connect('NPSS_A.sub.sub.i_out', 'NPSS_B.sub.sub.i_in')

            assembly.create_passthru('NPSS_B.text_out')
            assembly.create_passthru('NPSS_B.binary_out')
            assembly.create_passthru('NPSS_B.sub.sub.i_out', 'sub_sub_i_out')

            to_comp = 'Assembly.'
            from_comp = 'Assembly.'

        else:
            # Don't communicate indirectly through a subassembly.
            for name in ['NPSS_A', 'NPSS_B']:
                self.add_container(name, 
                                   Passthrough(self._indirect, directory=name))
            for var in vnames:
                self.connect('NPSS_A.'+var+'_out', 'NPSS_B.'+var+'_in')
                self.connect('NPSS_A.sub.'+var+'_out', 'NPSS_B.sub.'+var+'_in')
            self.connect('NPSS_A.text_out', 'NPSS_B.text_in')
            self.connect('NPSS_A.binary_out', 'NPSS_B.binary_in')
            self.connect('NPSS_A.sub.sub.i_out', 'NPSS_B.sub.sub.i_in')

            to_comp = 'NPSS_A.'
            from_comp = 'NPSS_B.'

        for var in vnames:
            self.connect('Source.'+var, to_comp+var+'_in')
            self.connect(from_comp+var+'_out', 'Sink.'+var)
            if self._indirect:
                self.connect('Source.sub.'+var, to_comp+'sub_'+var+'_in')
                self.connect(from_comp+'sub_'+var+'_out', 'Sink.sub.'+var)
            else:
                self.connect('Source.sub.'+var, to_comp+'sub.'+var+'_in')
                self.connect(from_comp+'sub.'+var+'_out', 'Sink.sub.'+var)

        self.connect('Source.text_file',   to_comp+'text_in')
        self.connect('Source.binary_file', to_comp+'binary_in')

        self.connect(from_comp+'text_out', 'Sink.text_file')
        self.connect(from_comp+'binary_out', 'Sink.binary_file')

        if self._indirect:
            self.connect('Source.sub_sub.ii', to_comp+'sub_sub_i_in')
            self.connect(from_comp+'sub_sub_i_out', 'Sink.sub_sub.ii')
        else:
            self.connect('Source.sub_sub.ii', to_comp+'sub.sub.i_in')
            self.connect(from_comp+'sub.sub.i_out', 'Sink.sub_sub.ii')


class NPSSTestCase(unittest.TestCase):

    # Directory where we can find NPSS model.
    directory = pkg_resources.resource_filename('npsscomponent', 'test')

    def setUp(self):
        """ Called before each test in this class. """
        # Set new simulation root so we can legally access files.
        SimulationRoot.chdir(NPSSTestCase.directory)
        self.model = None

    def tearDown(self):
        """ Called after each test in this class. """
        if self.model:
            self.model.pre_delete()
            # Cleanup directory dependent upon direct/indirect model.
            try:
                shutil.rmtree(self.model.NPSS_A.directory)
                shutil.rmtree(self.model.NPSS_B.directory)
            except AttributeError:
                shutil.rmtree(self.model.Assembly.directory)
        self.model = None

        # Verify NPSScomponent didn't mess-up working directory.
        end_dir = os.getcwd()
        SimulationRoot.chdir(ORIG_DIR)
        if end_dir != NPSSTestCase.directory:
            self.fail('Ended in %s, expected %s' \
                      % (end_dir, NPSSTestCase.directory))

    def test_direct(self):
        # Source -> NPSS_A -> NPSS_B -> Sink.
        logging.debug('')
        logging.debug('test_direct')
        self.model = set_as_top(Model())
        self.check_connectivity()

    def test_indirect(self):
        # Source -> Assembly                        Assembly-> Sink.
        #                    -> NPSS_A -> NPSS_B ->
        logging.debug('')
        logging.debug('test_indirect')
        self.model = set_as_top(Model(indirect=True))
        self.check_connectivity(indirect=True)

    def check_connectivity(self, indirect=False):
        """ Verify Sink != Source, run, verify Sink == Source. """
        self.assertNotEqual(self.model.Sink.b, self.model.Source.b)
        self.assertNotEqual(self.model.Sink.f, self.model.Source.f)
        self.assertNotEqual(numpy.all(self.model.Sink.f1d==self.model.Source.f1d), True)
        self.assertNotEqual(numpy.all(self.model.Sink.f2d==self.model.Source.f2d), True)
        self.assertNotEqual(numpy.all(self.model.Sink.f3d==self.model.Source.f3d), True)
        self.assertNotEqual(self.model.Sink.i, self.model.Source.i)
        self.assertNotEqual(numpy.all(self.model.Sink.i1d==self.model.Source.i1d), True)
        self.assertNotEqual(numpy.all(self.model.Sink.i2d==self.model.Source.i2d), True)
        self.assertNotEqual(self.model.Sink.s, self.model.Source.s)
        self.assertNotEqual(numpy.all(self.model.Sink.s1d==self.model.Source.s1d), True)

        self.assertNotEqual(self.model.Sink.text_data,
                            self.model.Source.text_data)
        self.assertNotEqual(numpy.all(self.model.Sink.binary_data==self.model.Source.binary_data),
                            True)
        self.assertNotEqual(self.model.Sink.binary_file.binary, True)

        self.assertNotEqual(self.model.Sink.sub.b, self.model.Source.sub.b)
        self.assertNotEqual(self.model.Sink.sub.f, self.model.Source.sub.f)
        self.assertNotEqual(numpy.all(self.model.Sink.sub.f1d==self.model.Source.sub.f1d), True)
        self.assertNotEqual(numpy.all(self.model.Sink.sub.f2d==self.model.Source.sub.f2d), True)
        self.assertNotEqual(numpy.all(self.model.Sink.sub.f3d==self.model.Source.sub.f3d), True)
        self.assertNotEqual(self.model.Sink.sub.i, self.model.Source.sub.i)
        self.assertNotEqual(numpy.all(self.model.Sink.sub.i1d==self.model.Source.sub.i1d), True)
        self.assertNotEqual(numpy.all(self.model.Sink.sub.i2d==self.model.Source.sub.i2d), True)
        self.assertNotEqual(self.model.Sink.sub.s, self.model.Source.sub.s)
        self.assertNotEqual(numpy.all(self.model.Sink.sub.s1d==self.model.Source.sub.s1d), True)

        self.assertNotEqual(self.model.Sink.sub_sub.ii,
                            self.model.Source.sub_sub.ii)

        self.model.run()

        if indirect:
            self.assertEqual(self.model.Source.b,
                             self.model.Assembly.NPSS_A.b_in)
            self.assertEqual(self.model.Assembly.NPSS_A.b_in,
                             self.model.Assembly.NPSS_A.b_out)
        else:
            self.assertEqual(self.model.Source.b,    self.model.NPSS_A.b_in)
            self.assertEqual(self.model.NPSS_A.b_in, self.model.NPSS_A.b_out)

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
        self.assertEqual(numpy.all(self.model.Sink.binary_data==self.model.Source.binary_data),
                         True)
        self.assertEqual(self.model.Sink.binary_file.binary, True)

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

        self.assertEqual(self.model.Sink.sub_sub.ii,
                         self.model.Source.sub_sub.ii)

        for path in ('source.txt', 'source.bin', 'sink.txt', 'sink.bin'):
            os.remove(path)  # Will raise exception if any files don't exist.

    def test_preprocessor(self):
        # Just verifying that some '-D' settings got propagated.
        logging.debug('')
        logging.debug('test_preprocessor')
        self.model = set_as_top(Model())
        self.assertEqual(self.model.NPSS_A.xyzzy_val, 'twisty narrow passages')
        self.assertEqual(self.model.NPSS_A.flag_val, 1)

if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=npsscomponent')
    sys.argv.append('--cover-erase')
    nose.runmodule()

