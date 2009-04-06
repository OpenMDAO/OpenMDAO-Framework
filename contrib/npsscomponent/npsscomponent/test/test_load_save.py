"""
Test load/save of NPSS component Python configuration.

TODO: load/save of NPSS model state.
"""

import cPickle
import logging
import os
import os.path
import pkg_resources
import unittest

from numpy import ndarray
from numpy.testing import assert_equal

from openmdao.main import Bool, FileVariable
from openmdao.main.constants import SAVE_LIBYAML
from openmdao.main.variable import OUTPUT

from npsscomponent import NPSScomponent

ORIG_DIR = os.getcwd()

class Passthrough(NPSScomponent):
    """ An NPSS component that passes-through various types of variable. """

    def __init__(self):
        directory = pkg_resources.resource_filename('npsscomponent', 'test')
        arglist = 'passthrough.mdl'
        super(Passthrough, self).__init__('NPSS', directory=directory,
                                          arglist=arglist)

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


class NPSSTestCase(unittest.TestCase):

    def setUp(self):
        """ Called before each test in this class. """
        self.npss = Passthrough()

    def tearDown(self):
        """ Called after each test in this class. """
        if self.npss is not None:
            self.npss.pre_delete()
            self.npss = None

        if os.getcwd() != ORIG_DIR:
            bad_dir = os.getcwd()
            os.chdir(ORIG_DIR)
            self.fail('Ended in %s, expected %s' % (bad_dir, ORIG_DIR))

        try:
            os.remove('npss.pickle')
        except OSError:
            pass

    def test_load_save(self):
        logging.debug('')
        logging.debug('test_load_save')

        saved_values = {}
        for name, var in self.npss._pub.items():
            saved_values[name] = var.get('value')

        self.npss.save('npss.pickle')
        self.npss.pre_delete()
        self.npss = None

        self.npss = NPSScomponent.load('npss.pickle')
        self.npss.post_load()

        for name, val in saved_values.items():
            if isinstance(val, ndarray):
                assert_equal(getattr(self.npss, name), val)
            else:
                if isinstance(self.npss._pub[name], FileVariable):
                    obj = getattr(self.npss, name)
                    self.assertEqual(getattr(obj, 'filename'), val)
                else:
                    self.assertEqual(getattr(self.npss, name), val)

    def test_nofile(self):
        logging.debug('')
        logging.debug('test_nofile')

        self.npss.pre_delete()
        self.npss = None
        try:
            self.npss = NPSScomponent.load('npss.pickle')
        except IOError, exc:
            self.assertEqual(str(exc), "[Errno 2] No such file or directory: 'npss.pickle'")
        else:
            self.fail('Expected IOError')

    def test_badfile(self):
        logging.debug('')
        logging.debug('test_badfile')

        self.npss.pre_delete()
        self.npss = None
        directory = pkg_resources.resource_filename('npsscomponent', 'test')
        badfile = os.path.join(directory, 'test_load_save.py')
        try:
            self.npss = NPSScomponent.load(badfile)
        except cPickle.UnpicklingError, exc:
            self.assertEqual(str(exc), "invalid load key, '\"'.")
        else:
            self.fail('Expected UnpicklingError')

    def test_nomodel(self):
        logging.debug('')
        logging.debug('test_nomodel')

        self.npss.model_filename = 'xyzzy.mdl'
        self.npss.save('npss.pickle')
        self.npss.pre_delete()
        self.npss = None

        self.npss = NPSScomponent.load('npss.pickle')
        try:
            self.npss.post_load()
        except RuntimeError, exc:
            self.assertEqual(str(exc), "NPSS: Reload caught exception: parseFile() failed: ERROR(991031001) in MemberFunction 'parseFile': Couldn't find file 'xyzzy.mdl'")
        else:
            self.fail('Expected RuntimeError')

    def test_badsave(self):
        logging.debug('')
        logging.debug('test_badsave')

        try:
            self.npss.save('/no-permission')
        except IOError, exc:
            self.assertEqual(str(exc), "NPSS: Can't save to '/no-permission': Permission denied")
        else:
            self.fail('Expected IOError')

    def test_save_yaml(self):
        logging.debug('')
        logging.debug('test_save_yaml')

        # This currently fails, not sure why.
        try:
            self.npss.save('npss.pickle', SAVE_LIBYAML)
        except TypeError, exc:
            self.assertEqual(str(exc), 'data type not understood')
        else:
            self.fail('Expected TypeError')


if __name__ == '__main__':
    unittest.main()

