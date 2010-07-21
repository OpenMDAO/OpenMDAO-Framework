"""
Test FullFactorial.
"""

import unittest

from openmdao.lib.doegenerators.full_factorial import FullFactorial

class TestCase(unittest.TestCase): 
    
    def test_full_factorial(self): 
        
        ff = FullFactorial(2,2)
        
        cases = [case for case in ff]
        
        self.assertEqual([(0,0),(0,1),(1,0),(1,1)],cases)
