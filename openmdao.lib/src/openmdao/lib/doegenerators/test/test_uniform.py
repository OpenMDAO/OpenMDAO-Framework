"""
Test Uniform.
"""

import sys
import unittest
import random

from openmdao.lib.doegenerators.uniform import Uniform


class TestCase(unittest.TestCase):
    def setUp(self):
        random.seed(10)

    def test_num_cases(self):
        uni = Uniform(10)
        uni.num_parameters = 3
        cases = [case for case in uni]
        expected = 10*[[1.0,1.0,1.0]]
        self.assertEqual(len(expected),len(cases))
        self.assertEqual(len(expected[0]),len(cases[0]))   
        
        # See if we can get cases again
        cases = [case for case in uni]
        self.assertEqual(len(expected),len(cases))
        self.assertEqual(len(expected[0]),len(cases[0]))

    def test_nested_loop(self):
        # test to make sure the generator can handle nested loops
        uni = Uniform(5)
        uni.num_parameters = 2

        inner_count = 0
        outer_count = 0
        for case_outer in uni:
            outer_count += 1
            for case_inner in uni:
                inner_count += 1

        self.assertEqual(5,outer_count)
        self.assertEqual(25,inner_count)
        
    def test_low_sample_count(self): 
        uni = Uniform()
        uni.num_samples = 1
        
        try: 
            for case in uni: 
                pass
        except ValueError as err: 
            self.assertEqual(str(err),"Uniform distributions "
                             "must have at least 2 samples. "
                             "num_samples is set to less than 2.")

if __name__ == "__main__":
    unittest.main()
