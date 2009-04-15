"""
Test of NPSS auto-reload capability.
"""

import logging
import os
import os.path
import pkg_resources
import unittest

from openmdao.main import Assembly, Component, Bool, Float, String
from openmdao.main.variable import INPUT, OUTPUT

from npsscomponent import NPSScomponent

ORIG_DIR = os.getcwd()


class Source(Component):
    """ Just something to connect NPSS inputs to. """

    def __init__(self, name='Source', *args, **kwargs):
        super(Source, self).__init__(name, *args, **kwargs)
        Bool('npss_reload', self, OUTPUT, default=False,
             doc='Test input to NPSS')
        Float('npss_in', self, OUTPUT, default=0.,
              doc='Test input to NPSS')


class Sink(Component):
    """ Just something to connect NPSS outputs to. """

    def __init__(self, name='Sink', *args, **kwargs):
        super(Sink, self).__init__(name, *args, **kwargs)
        Float('npss_out', self, INPUT, default=0.,
              doc='Test output from NPSS')


# pylint: disable-msg=E1101
# "Instance of <class> has no <attr> member"

class Model(Assembly):
    """ Exercises NPSS auto-reload capability. """ 

    def __init__(self, *args, **kwargs):
        super(Model, self).__init__(*args, **kwargs)

        self.workflow.add_node(Source(parent=self))
        self.Source.npss_in = 9

        directory = \
            os.path.join(pkg_resources.resource_filename('npsscomponent',
                                                         'test'))
        arglist = '-trace reload.mdl'
        NPSScomponent(parent=self, directory=directory,
                      arglist=arglist, output_filename='reload.out')
        self.NPSS.reload_flag = 'reload_requested'
        Float('xyzzy_in',  self.NPSS, INPUT, doc='Test input')
        Float('xyzzy_out', self.NPSS, OUTPUT, doc='Test output')
        String('s', self.NPSS, INPUT, doc='Unconnected input')

        self.workflow.add_node(self.NPSS)

        self.workflow.add_node(Sink(parent=self))

        self.connect('Source.npss_reload', 'NPSS.reload_model')
        self.connect('Source.npss_in', 'NPSS.xyzzy_in')
        self.connect('NPSS.xyzzy_out', 'Sink.npss_out')


class NPSSTestCase(unittest.TestCase):

    def setUp(self):
        """ Called before each test in this class. """
        self.model = Model('TestModel')

    def tearDown(self):
        """ Called after each test in this class. """
        self.model.pre_delete()
        os.remove(os.path.join(self.model.NPSS.directory, 'reload.out'))
        self.model = None
        if os.getcwd() != ORIG_DIR:
            bad_dir = os.getcwd()
            os.chdir(ORIG_DIR)
            self.fail('Ended in %s, expected %s' % (bad_dir, ORIG_DIR))

    def test_internal_reload(self):
        logging.debug('')
        logging.debug('test_internal_reload')

        self.assertEqual(self.model.NPSS.run_count, 0)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.Sink.npss_out, 0)
        self.assertEqual(self.model.Source.npss_in, 9)

        self.model.run()

        self.assertEqual(self.model.NPSS.run_count, 1)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.NPSS.xyzzy_out, 9)
        self.assertEqual(self.model.Sink.npss_out, 9)

        self.model.run()

        self.assertEqual(self.model.NPSS.run_count, 2)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.Sink.npss_out, 18)

        path = self.model.NPSS.reload_flag
        self.model.NPSS.set(path, True)
        self.model.debug('reload_flag = %d', self.model.NPSS.get(path))

        self.model.run()

        self.assertEqual(self.model.NPSS.run_count, 1)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.Sink.npss_out, 9)
        self.assertEqual(self.model.NPSS.s, 'unconnected')

        self.model.NPSS.set(path, True)
        self.model.debug('reload_flag = %d', self.model.NPSS.get(path))
        self.model.NPSS.model_filename = 'no_such_model'
        try:
            self.model.run()
        except RuntimeError, exc:
            self.assertEqual(str(exc).startswith(
                "TestModel.NPSS: Exception during reload: Model file 'no_such_model' not found while reloading in"),
                True)
        else:
            self.fail('Expected RuntimeError')

        self.model.NPSS.reload_flag = 'no_such_variable'
        try:
            self.model.run()
        except RuntimeError, exc:
            self.assertEqual(str(exc), "TestModel.NPSS: Exception getting 'no_such_variable': no_such_variable not found")
        else:
            self.fail('Expected RuntimeError')

    def test_external_reload(self):
        logging.debug('')
        logging.debug('test_external_reload')

        self.assertEqual(self.model.NPSS.run_count, 0)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.Sink.npss_out, 0)
        self.assertEqual(self.model.Source.npss_in, 9)

        self.model.run()

        self.assertEqual(self.model.NPSS.run_count, 1)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.NPSS.xyzzy_out, 9)
        self.assertEqual(self.model.Sink.npss_out, 9)

        self.model.run()

        self.assertEqual(self.model.NPSS.run_count, 2)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.Sink.npss_out, 18)

        self.model.Source.npss_reload = True
        self.model.debug('Source.npss_reload = %d',
                         self.model.Source.npss_reload)

        self.model.run()

        self.assertEqual(self.model.NPSS.run_count, 1)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.Sink.npss_out, 9)
        self.assertEqual(self.model.NPSS.s, 'unconnected')
 
        self.model.NPSS.model_filename = 'no_such_model'
        try:
            self.model.run()
        except RuntimeError, exc:
            self.assertEqual(str(exc).startswith(
                "TestModel.NPSS: Exception during reload: Model file 'no_such_model' not found while reloading in"),
                True)
        else:
            self.fail('Expected RuntimeError')

    def test_custom_run(self):
        logging.debug('')
        logging.debug('test_custom_run')

        self.assertEqual(self.model.NPSS.run_count, 0)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.Sink.npss_out, 0)
        self.assertEqual(self.model.Source.npss_in, 9)

        self.model.run()

        self.assertEqual(self.model.NPSS.run_count, 1)
        self.assertEqual(self.model.NPSS.mcRun_count, 0)
        self.assertEqual(self.model.NPSS.xyzzy_out, 9)
        self.assertEqual(self.model.Sink.npss_out, 9)

        self.model.NPSS.run_command = 'mcRun()'

        self.model.run()

        self.assertEqual(self.model.NPSS.run_count, 1)
        self.assertEqual(self.model.NPSS.mcRun_count, 1)
        self.assertEqual(self.model.Sink.npss_out, 90)
        self.assertEqual(self.model.NPSS.s, 'unconnected')


if __name__ == '__main__':
    unittest.main()

