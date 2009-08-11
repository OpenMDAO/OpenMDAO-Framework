"""
Test load/save of NPSS component Python configuration.

TODO: load/save of NPSS model state.
"""

import logging
import os.path
import pkg_resources
import shutil
import unittest

from numpy import ndarray
from numpy.testing import assert_equal

from enthought.traits.api import Bool

from openmdao.main.api import FileValue, SAVE_LIBYAML
from openmdao.main.component import SimulationRoot

from npsscomponent import NPSScomponent

ORIG_DIR = os.getcwd()


class Passthrough(NPSScomponent):
    """ An NPSS component that passes-through various types of variable. """

    def __init__(self):
        super(Passthrough, self).__init__('NPSS', arglist='passthrough.mdl')

        # Automagic interface variable creation
        self.make_public([
            'f_out',
            'f1d_out',
            'f2d_out',
            'f3d_out',
            'i_out',
            'i1d_out',
            'i2d_out',
            's_out',
            's1d_out', 
            'text_out',
            'binary_out',
            ('b_out','','out',Bool(iostatus='out')), # for bools, we need to supply a trait
        ], iostatus='out')
        

class NPSSTestCase(unittest.TestCase):

    directory = pkg_resources.resource_filename('npsscomponent', 'test')

    def setUp(self):
        """ Called before each test in this class. """
        # Reset simulation root so we can legally access files.
        SimulationRoot.chdir(NPSSTestCase.directory)
        self.npss = Passthrough()
        self.egg_name = None

    def tearDown(self):
        """ Called after each test in this class. """
        if self.npss is not None:
            self.npss.pre_delete()
            self.npss = None

        if self.egg_name and os.path.exists(self.egg_name):
            os.remove(self.egg_name)

        SimulationRoot.chdir(ORIG_DIR)

    def test_load_save(self):
        logging.debug('')
        logging.debug('test_load_save')

        saved_values = {}
        for name, var in self.npss.items():
            saved_values[name] = var

        egg_info = self.npss.save_to_egg()
        self.egg_name = egg_info[0]
        self.npss.pre_delete()
        self.npss = None

        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            egg_path = os.path.join('..', self.egg_name)
            self.npss = NPSScomponent.load_from_eggfile(egg_path, install=False)

            for name, val in saved_values.items():
                if name == 'directory':
                    continue  # This gets reset on purpose.
                if isinstance(val, ndarray):
                    assert_equal(getattr(self.npss, name), val)
                else:
                    if isinstance(val, FileValue):
                        obj = getattr(self.npss, name)
                        self.assertEqual(getattr(obj, 'filename'), val.filename)
                    else:
                        self.assertEqual(getattr(self.npss, name), val)
        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)

    def test_nomodel(self):
        logging.debug('')
        logging.debug('test_nomodel')

        self.npss.model_filename = 'xyzzy.mdl'
        egg_info = self.npss.save_to_egg(version='0.0')
        self.egg_name = egg_info[0]
        self.npss.pre_delete()
        self.npss = None

        orig_dir = os.getcwd()
        test_dir = 'EggTest'
        if os.path.exists(test_dir):
            shutil.rmtree(test_dir)
        os.mkdir(test_dir)
        os.chdir(test_dir)
        try:
            try:
                egg_path = os.path.join('..', self.egg_name)
                self.npss = NPSScomponent.load_from_eggfile(egg_path,
                                                            install=False)
            except RuntimeError, exc:
                msg = "NPSS: Reload caught exception: Model file 'xyzzy.mdl'" \
                      " not found while reloading in"
                self.assertEqual(str(exc)[:len(msg)], msg)
            else:
                self.fail('Expected RuntimeError')
        finally:
            os.chdir(orig_dir)
            shutil.rmtree(test_dir)

    def test_badsave(self):
        logging.debug('')
        logging.debug('test_badsave')

        try:
            self.npss.save_to_egg(dst_dir='/no-permission')
        except IOError, exc:
            self.assertEqual(str(exc),
                "NPSS: Can't save to '/no-permission', no write permission")
        else:
            self.fail('Expected IOError')

    def test_save_yaml(self):
        logging.debug('')
        logging.debug('test_save_yaml')

        # This currently fails, not sure why.
        try:
            egg_info = self.npss.save_to_egg(format=SAVE_LIBYAML)
            self.egg_name = egg_info[0]
        except Exception, exc:
            msg = "NPSS: Can't save to 'NPSS/NPSS.yaml': data type not" \
                  " understood"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected TypeError')
        finally:
            if os.path.exists('NPSS.yaml'):
                os.remove('NPSS.yaml')


if __name__ == '__main__':
    import nose
    import sys
    sys.argv.append('--cover-package=npsscomponent')
    sys.argv.append('--cover-erase')
    nose.runmodule()

