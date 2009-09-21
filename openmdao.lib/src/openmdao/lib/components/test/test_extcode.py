"""
Test the ExternalCode component.
"""

import logging
import os
import pkg_resources
import sys
import unittest

from openmdao.main.api import SimulationRoot, set_as_top
from openmdao.main.exceptions import RunInterrupted
from openmdao.lib.components.external_code import ExternalCode
from openmdao.util.eggchecker import check_save_load

# Capture original working directory so we can restore in tearDown().
ORIG_DIR = os.getcwd()
# Directory where we can find sleep.py.
DIRECTORY = pkg_resources.resource_filename('openmdao.lib.components', 'test')


class TestCase(unittest.TestCase):
    """ Test the ExternalCode component. """

    def setUp(self):
        SimulationRoot.chdir(DIRECTORY)
        
    def tearDown(self):
        SimulationRoot.chdir(ORIG_DIR)
        
    def test_normal(self):
        logging.debug('')
        logging.debug('test_normal')

        # Normal run should have no issues.
        externp = set_as_top(ExternalCode())
        externp.timeout = 5
        externp.command = 'python sleep.py 1'
        externp.run()
        self.assertEqual(externp.return_code, 0)
        self.assertEqual(externp.timed_out, False)

    def test_save_load(self):
        logging.debug('')
        logging.debug('test_save_load')

        externp = set_as_top(ExternalCode())
        externp.name = 'ExternalCode'
        externp.timeout = 5
        externp.command = 'python sleep.py 1'

        # Exercise check_save_load().
        retcode = check_save_load(externp)
        self.assertEqual(retcode, 0)

    def test_timeout(self):
        logging.debug('')
        logging.debug('test_timeout')

        # Set timeout to less than execution time.
        externp = set_as_top(ExternalCode())
        externp.timeout = 1
        externp.command = 'python sleep.py 5'
        try:
            externp.run()
        except RunInterrupted, exc:
            self.assertEqual(str(exc), ': Timed out')
            self.assertEqual(externp.timed_out, True)
        else:
            self.fail('Expected RunInterrupted')

    def test_badcmd(self):
        logging.debug('')
        logging.debug('test_badcmd')

        # Set command to nonexistant path.
        externp = set_as_top(ExternalCode())
        externp.command = 'xyzzy'
        externp.stdout = 'badcmd.out'
        externp.stderr = ExternalCode.STDOUT
        try:
            externp.run()
        except RuntimeError, exc:
            msg = ': return_code = 127'
            self.assertEqual(str(exc).startswith(msg), True)
            self.assertEqual(externp.return_code, 127)
            self.assertEqual(os.path.exists(externp.stdout), True)
        else:
            self.fail('Expected RuntimeError')
        finally:
            if os.path.exists(externp.stdout):
                os.remove(externp.stdout)

    def test_nullcmd(self):
        logging.debug('')
        logging.debug('test_nullcmd')

        # Check response to no command set.
        externp = set_as_top(ExternalCode())
        externp.stdout = 'nullcmd.out'
        externp.stderr = ExternalCode.STDOUT
        try:
            externp.run()
        except ValueError, exc:
            self.assertEqual(str(exc), ': Null command line')
        else:
            self.fail('Expected ValueError')
        finally:
            if os.path.exists(externp.stdout):
                os.remove(externp.stdout)
    

if __name__ == "__main__":
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

