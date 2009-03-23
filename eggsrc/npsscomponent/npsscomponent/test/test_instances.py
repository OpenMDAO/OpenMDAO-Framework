"""
Test of multiple NPSS instances.
"""

import os.path
import pkg_resources
import unittest

from openmdao.main import Assembly, Component, \
                          ArrayVariable, Bool, Float, Int, String, StringList
from openmdao.main.variable import INPUT, OUTPUT

from npsscomponent import NPSScomponent


class Source(Component):
    """ Just something to connect NPSS inputs to. """

    def __init__(self, name='Source', parent=None):
        super(Source, self).__init__(name, parent)
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


class Passthrough(NPSScomponent):
    """ NPSS component that passes-through various types of variable. """

    def __init__(self, name='Passthrough', parent=None, directory=''):
        arglist = ['-D', 'XYZZY=twisty narrow passages',
                   '-trace', os.path.join('..', 'passthrough.mdl')]
        super(Passthrough, self).__init__(name, parent, '', directory,
                                          arglist, 'passthrough.out')

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

        Bool('b_out', self, OUTPUT)
        Float('f_out', self, OUTPUT)
        ArrayVariable('f1d_out', self, OUTPUT, entry_type=float, num_dims=1)
        ArrayVariable('f2d_out', self, OUTPUT, entry_type=float, num_dims=2)
        ArrayVariable('f3d_out', self, OUTPUT, entry_type=float, num_dims=3)
        Int('i_out', self, OUTPUT)
        ArrayVariable('i1d_out', self, OUTPUT, entry_type=int, num_dims=1)
        ArrayVariable('i2d_out', self, OUTPUT, entry_type=int, num_dims=2)
        String('s_out', self, OUTPUT)
        StringList('s1d_out', self, OUTPUT)


class Sink(Component):
    """ Just something to connect NPSS outputs to. """

    def __init__(self, name='Sink', parent=None):
        super(Sink, self).__init__(name, parent)
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


# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

class NPSSTestCase(unittest.TestCase):

    def setUp(self):
        """ Called before each test in this class. """
        self.tla = Assembly('TLA')

        self.tla.workflow.add_node(Source(parent=self.tla))
        self.tla.Source.b = True
        self.tla.Source.f = 3.14159
        self.tla.Source.f1d = [3.14159, 2.781828]
        self.tla.Source.f2d = [[3.14159, 2.781828], [1., 2.]]
        self.tla.Source.f3d = [[[3.14159, 2.781828], [1., 2.]],
                               [[0.1, 1.2], [2.3, 3.4]]]
        self.tla.Source.i = 42
        self.tla.Source.i1d = [42, 666]
        self.tla.Source.i2d = [[42, 666], [9, 8]]
        self.tla.Source.s = 'Hello World!'
        self.tla.Source.s1d = ['the', 'quick', 'brown', 'fox']

        name = 'NPSS_A'
        directory = \
            os.path.join(pkg_resources.resource_filename('npsscomponent',
                                                         'test'), name)
        Passthrough(name=name, parent=self.tla, directory=directory)
        self.tla.workflow.add_node(self.tla.NPSS_A)

        name = 'NPSS_B'
        directory = \
            os.path.join(pkg_resources.resource_filename('npsscomponent',
                                                         'test'), name)
        Passthrough(name=name, parent=self.tla, directory=directory)
        self.tla.workflow.add_node(self.tla.NPSS_B)

        self.tla.workflow.add_node(Sink(parent=self.tla))

        self.tla.connect('Source.b',   'NPSS_A.b_in')
        self.tla.connect('Source.f',   'NPSS_A.f_in')
        self.tla.connect('Source.f1d', 'NPSS_A.f1d_in')
        self.tla.connect('Source.f2d', 'NPSS_A.f2d_in')
        self.tla.connect('Source.f3d', 'NPSS_A.f3d_in')
        self.tla.connect('Source.i',   'NPSS_A.i_in')
        self.tla.connect('Source.i1d', 'NPSS_A.i1d_in')
        self.tla.connect('Source.i2d', 'NPSS_A.i2d_in')
        self.tla.connect('Source.s',   'NPSS_A.s_in')
        self.tla.connect('Source.s1d', 'NPSS_A.s1d_in')

        self.tla.connect('NPSS_A.b_out',   'NPSS_B.b_in')
        self.tla.connect('NPSS_A.f_out',   'NPSS_B.f_in')
        self.tla.connect('NPSS_A.f1d_out', 'NPSS_B.f1d_in')
        self.tla.connect('NPSS_A.f2d_out', 'NPSS_B.f2d_in')
        self.tla.connect('NPSS_A.f3d_out', 'NPSS_B.f3d_in')
        self.tla.connect('NPSS_A.i_out',   'NPSS_B.i_in')
        self.tla.connect('NPSS_A.i1d_out', 'NPSS_B.i1d_in')
        self.tla.connect('NPSS_A.i2d_out', 'NPSS_B.i2d_in')
        self.tla.connect('NPSS_A.s_out',   'NPSS_B.s_in')
        self.tla.connect('NPSS_A.s1d_out', 'NPSS_B.s1d_in')

        self.tla.connect('NPSS_B.b_out',   'Sink.b')
        self.tla.connect('NPSS_B.f_out',   'Sink.f')
        self.tla.connect('NPSS_B.f1d_out', 'Sink.f1d')
        self.tla.connect('NPSS_B.f2d_out', 'Sink.f2d')
        self.tla.connect('NPSS_B.f3d_out', 'Sink.f3d')
        self.tla.connect('NPSS_B.i_out',   'Sink.i')
        self.tla.connect('NPSS_B.i1d_out', 'Sink.i1d')
        self.tla.connect('NPSS_B.i2d_out', 'Sink.i2d')
        self.tla.connect('NPSS_B.s_out',   'Sink.s')
        self.tla.connect('NPSS_B.s1d_out', 'Sink.s1d')

    def tearDown(self):
        """ Called after each test in this class. """
        self.tla.NPSS_A.pre_delete()
        self.tla.NPSS_B.pre_delete()
        self.tla = None

    def test_connectivity(self):
        self.assertNotEqual(self.tla.Sink.b,   self.tla.Source.b)
        self.assertNotEqual(self.tla.Sink.f,   self.tla.Source.f)
        self.assertNotEqual(self.tla.Sink.f1d, self.tla.Source.f1d)
        self.assertNotEqual(self.tla.Sink.f2d, self.tla.Source.f2d)
        self.assertNotEqual(self.tla.Sink.f3d, self.tla.Source.f3d)
        self.assertNotEqual(self.tla.Sink.i,   self.tla.Source.i)
        self.assertNotEqual(self.tla.Sink.i1d, self.tla.Source.i1d)
        self.assertNotEqual(self.tla.Sink.i2d, self.tla.Source.i2d)
        self.assertNotEqual(self.tla.Sink.s,   self.tla.Source.s)
        self.assertNotEqual(self.tla.Sink.s1d, self.tla.Source.s1d)

        self.tla.run()

        self.assertEqual(self.tla.Sink.b, self.tla.Source.b)

        self.assertEqual(self.tla.Sink.f, self.tla.Source.f)
        for i in xrange(len(self.tla.Source.f1d)):
            self.assertEqual(self.tla.Sink.f1d[i], self.tla.Source.f1d[i])
        for i in xrange(len(self.tla.Source.f2d)):
            for j in xrange(len(self.tla.Source.f2d[i])):
                self.assertEqual(self.tla.Sink.f2d[i][j],
                                 self.tla.Source.f2d[i][j])
        for i in xrange(len(self.tla.Source.f3d)):
            for j in xrange(len(self.tla.Source.f3d[i])):
                for k in xrange(len(self.tla.Source.f3d[i][j])):
                    self.assertEqual(self.tla.Sink.f3d[i][j][k],
                                     self.tla.Source.f3d[i][j][k])

        self.assertEqual(self.tla.Sink.i, self.tla.Source.i)
        for i in xrange(len(self.tla.Source.i1d)):
            self.assertEqual(self.tla.Sink.i1d[i], self.tla.Source.i1d[i])
        for i in xrange(len(self.tla.Source.i2d)):
            for j in xrange(len(self.tla.Source.i2d[i])):
                self.assertEqual(self.tla.Sink.i2d[i][j],
                                 self.tla.Source.i2d[i][j])

        self.assertEqual(self.tla.Sink.s, self.tla.Source.s)
        for i in xrange(len(self.tla.Source.s1d)):
            self.assertEqual(self.tla.Sink.s1d[i], self.tla.Source.s1d[i])


if __name__ == '__main__':
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(NPSSTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)

