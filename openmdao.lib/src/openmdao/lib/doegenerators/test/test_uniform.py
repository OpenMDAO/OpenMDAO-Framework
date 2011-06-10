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

if __name__ == "__main__":
    unittest.main()