"""
Test ShellProc functions.
"""

import logging
import os.path
import unittest

from openmdao.util.shellproc import call, check_call


class TestCase(unittest.TestCase):
    """ Test ShellProc functions. """

    def test_call(self):
        logging.debug('')
        logging.debug('test_call')

        try:
            return_code, error_msg = call('dir', stdout='stdout',
                                                 stderr='stderr')
            self.assertEqual(os.path.exists('stdout'), True)
            self.assertEqual(os.path.exists('stderr'), True)
        finally:
            if os.path.exists('stdout'):
                os.remove('stdout')
            if os.path.exists('stderr'):
                os.remove('stderr')
 
    def test_check_call(self):
        logging.debug('')
        logging.debug('test_check_call')

        try:
            check_call('dir', stdout='stdout', stderr='stderr')
            self.assertEqual(os.path.exists('stdout'), True)
            self.assertEqual(os.path.exists('stderr'), True)
        finally:
            if os.path.exists('stdout'):
                os.remove('stdout')
            if os.path.exists('stderr'):
                os.remove('stderr')


if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()

