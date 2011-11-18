"""
Who is Testing the Testers?
"""

import logging
import os.path
import sys
import unittest

from openmdao.util.testutil import assert_rel_error

class TestCase(unittest.TestCase):
    """ Test Test functions. """

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_rel_error_inf_nan(self):

        try:
            assert_rel_error(self, float('nan'), 6.5, 0.0001)
        except AssertionError, exc:
            msg = "actual nan, desired 6.5, error nan, tolerance 0.0001"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected AssertionError')
        
        try:
            assert_rel_error(self, float('inf'), 6.5, 0.0001)
        except AssertionError, exc:
            msg = "actual inf, desired 6.5, error inf, tolerance 0.0001"
            self.assertEqual(str(exc), msg)
        else:
            self.fail('Expected AssertionError')
        
        # We may actually want this to work for some tests.
        assert_rel_error(self, float('nan'), float('nan'), 0.0001)

if __name__ == '__main__':
    import nose
    sys.argv.append('--cover-package=openmdao')
    sys.argv.append('--cover-erase')
    nose.runmodule()
  
