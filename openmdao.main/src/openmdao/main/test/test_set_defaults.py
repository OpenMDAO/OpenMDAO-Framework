"""
Test for setting variables back to their default values.
"""

import unittest

from enthought.traits.api import Float, Array, Int, List

from openmdao.main.api import Component

class MyComp(Component):
    pass


class SetDefaultsTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        pass

    def tearDown(self):
        """this teardown function will be called after each test"""
        pass

    def test_set_to_unset_default(self):
        pass
    
    def test_set_to_default(self):
        pass
    
if __name__ == '__main__':
    unittest.main()

