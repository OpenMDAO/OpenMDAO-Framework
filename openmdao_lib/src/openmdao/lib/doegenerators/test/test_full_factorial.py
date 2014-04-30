"""
Test FullFactorial.
"""

# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.lib.doegenerators.full_factorial import FullFactorial

class TestCase(unittest.TestCase):
    
    def setup(self):
        pass
    
    def teardown(self):
        pass
    
    def test_full_factorial(self): 
        
        ff = FullFactorial(num_levels=2)
        ff.num_parameters = 2
        
        cases = [case for case in ff]
        
        self.assertEqual([(0,0),(0,1),(1,0),(1,1)],cases)

        
if __name__ == "__main__":
    unittest.main()
