"""
Test of NPSS auto-reload capability.
"""

import os.path
import pkg_resources
import unittest

from openmdao.main import Assembly, Component, Bool, Float, String
from openmdao.main.variable import INPUT, OUTPUT

from npsscomponent import NPSScomponent


class Source(Component):
    """ Just something to connect NPSS inputs to. """

    def __init__(self, name='Source', parent=None):
        super(Source, self).__init__(name, parent)
        Bool('npss_reload', self, desc='Test input to NPSS',
              default=False, iostatus=OUTPUT)
        Float('npss_in', self, desc='Test input to NPSS',
              default=0., iostatus=OUTPUT)


class Sink(Component):
    """ Just something to connect NPSS outputs to. """

    def __init__(self, name='Sink', parent=None):
        super(Sink, self).__init__(name, parent)
        Float('npss_out', self, desc='Test output from NPSS',
              default=0., iostatus=INPUT)


# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

class NPSSTestCase(unittest.TestCase):

    def setUp(self):
        """called before each test in this class"""
        self.tla = Assembly('TLA')
        self.tla.workflow.add_node(Source(parent=self.tla))
        self.tla.Source.npss_in = 9

        directory = \
            os.path.join(pkg_resources.resource_filename('npsscomponent',
                                                         'test'))
        arglist = ['-trace', 'reload.mdl']
        self.npss = NPSScomponent(parent=self.tla, directory=directory,
                                  arglist=arglist, output_filename='reload.out')
        self.npss.reload_flag = 'reload_requested'
        Float('xyzzy_in',  self.npss, desc='Test input',  iostatus=INPUT)
        Float('xyzzy_out', self.npss, desc='Test output', iostatus=OUTPUT)
        String('s', self.npss, desc='Unconnected input', iostatus=INPUT)

        self.tla.workflow.add_node(self.npss)

        self.tla.workflow.add_node(Sink(parent=self.tla))

        self.tla.connect('Source.npss_reload', 'NPSS.reload_model')
        self.tla.connect('Source.npss_in', 'NPSS.xyzzy_in')
        self.tla.connect('NPSS.xyzzy_out', 'Sink.npss_out')

    def tearDown(self):
        """called after each test in this class"""
        self.npss.pre_delete()
        self.npss = None
        self.tla = None

    def test_internal_reload(self):
        self.assertEqual(self.npss.run_count, 0)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.tla.Sink.npss_out, 0)
        self.assertEqual(self.tla.Source.npss_in, 9)

        self.tla.run()
        self.assertEqual(self.npss.run_count, 1)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.npss.xyzzy_out, 9)
        self.assertEqual(self.tla.Sink.npss_out, 9)

        self.tla.run()
        self.assertEqual(self.npss.run_count, 2)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.tla.Sink.npss_out, 18)

        path = self.npss.get('reload_flag')
        self.npss.set(path, True)
        self.tla.debug('reload_flag = %d', self.npss.get(path))
        self.tla.run()
        self.assertEqual(self.npss.run_count, 1)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.tla.Sink.npss_out, 9)
        self.assertEqual(self.npss.s, 'unconnected')

    def test_external_reload(self):
        self.assertEqual(self.npss.run_count, 0)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.tla.Sink.npss_out, 0)
        self.assertEqual(self.tla.Source.npss_in, 9)

        self.tla.run()
        self.assertEqual(self.npss.run_count, 1)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.npss.xyzzy_out, 9)
        self.assertEqual(self.tla.Sink.npss_out, 9)

        self.tla.run()
        self.assertEqual(self.npss.run_count, 2)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.tla.Sink.npss_out, 18)

        self.tla.Source.npss_reload = True
        self.tla.debug('Source.npss_reload = %d', self.tla.Source.npss_reload)
        self.tla.run()
        self.assertEqual(self.npss.run_count, 1)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.tla.Sink.npss_out, 9)
        self.assertEqual(self.npss.s, 'unconnected')
 
    def test_custom_run(self):
        self.assertEqual(self.npss.run_count, 0)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.tla.Sink.npss_out, 0)
        self.assertEqual(self.tla.Source.npss_in, 9)

        self.tla.run()
        self.assertEqual(self.npss.run_count, 1)
        self.assertEqual(self.npss.mcRun_count, 0)
        self.assertEqual(self.npss.xyzzy_out, 9)
        self.assertEqual(self.tla.Sink.npss_out, 9)

        self.npss.run_command = 'mcRun()'
        self.tla.run()
        self.assertEqual(self.npss.run_count, 1)
        self.assertEqual(self.npss.mcRun_count, 1)
        self.assertEqual(self.tla.Sink.npss_out, 90)
        self.assertEqual(self.npss.s, 'unconnected')


if __name__ == '__main__':
    unittest.main()
    #suite = unittest.TestLoader().loadTestsFromTestCase(NPSSTestCase)
    #unittest.TextTestRunner(verbosity=2).run(suite)

