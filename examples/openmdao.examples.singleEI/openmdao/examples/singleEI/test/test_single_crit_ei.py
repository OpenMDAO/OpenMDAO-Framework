"""
Test for single criteria EI example.
"""

import unittest

from openmdao.examples.singleEI.branin_surrogate import Analysis, Iterator


class EITest(unittest.TestCase):
    """Test to make sure the EI sample problem works as it should"""
    
    def setUp(self):
        pass

    def tearDown(self):
        pass
    
    def test_EI(self): 
        analysis = Analysis()
       
        set_as_top(analysis)
        analysis.run()
        
        