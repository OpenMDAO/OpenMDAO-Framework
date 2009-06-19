"""
Test the ExternalCode component.
"""

import os
import pkg_resources
import unittest

from openmdao.main.component import SimulationRoot
from openmdao.main.exceptions import RunInterrupted
from openmdao.lib.components.external_code import ExternalCode

ORIG_DIR = os.getcwd()
directory = pkg_resources.resource_filename('openmdao.lib.components', 'test')


class TestCase(unittest.TestCase):
    """ Test the ExternalCode component. """

    def setUp(self):
        SimulationRoot.chdir(directory)
        
    def tearDown(self):
        SimulationRoot.chdir(ORIG_DIR)
        
    def test_normal(self):
        externp = ExternalCode()
        externp.timeout = 5
        externp.command = 'python sleep.py 1'
        externp.run()
        self.assertEqual(externp.return_code, 0)
        self.assertEqual(externp.timed_out, False)

    def test_timeout(self):
        externp = ExternalCode()
        externp.timeout = 1
        externp.command = 'python sleep.py 5'
        try:
            externp.run()
        except RunInterrupted, exc:
            self.assertEqual(str(exc), 'ExternalCode: Timed out')
            self.assertEqual(externp.timed_out, True)
        else:
            self.fail('Expected RunInterrupted')

    def test_badcmd(self):
        externp = ExternalCode()
        externp.command = 'xyzzy'
        externp.stdout = 'badcmd.out'
        externp.stderr = ExternalCode.STDOUT
        try:
            externp.run()
        except RuntimeError, exc:
            msg = 'ExternalCode: return_code = 127'
            self.assertEqual(str(exc).startswith(msg), True)
            self.assertEqual(externp.return_code, 127)
            self.assertEqual(os.path.exists(externp.stdout), True)
        else:
            self.fail('Expected RuntimeError')
        finally:
            if os.path.exists(externp.stdout):
                os.remove(externp.stdout)

    def test_nullcmd(self):
        externp = ExternalCode()
        externp.stdout = 'nullcmd.out'
        externp.stderr = ExternalCode.STDOUT
        try:
            externp.run()
        except ValueError, exc:
            msg = 'ExternalCode: return_code = 127'
            self.assertEqual(str(exc), 'ExternalCode: Null command line')
        else:
            self.fail('Expected ValueError')
        finally:
            if os.path.exists(externp.stdout):
                os.remove(externp.stdout)
    

if __name__ == "__main__":
    unittest.main()

