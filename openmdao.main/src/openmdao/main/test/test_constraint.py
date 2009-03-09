# pylint: disable-msg=C0111,C0103

import unittest

from openmdao.main.constraint import MinConstraint, MaxConstraint
from openmdao.main.constraint import MinLengthConstraint, MaxLengthConstraint
from openmdao.main.exceptions import ConstraintError

class ConstraintTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test"""
        pass
    
    def tearDown(self):
        """this teardown function will be called after each test"""
        pass
    
    def test_min(self):
        mincon = MinConstraint(-5)
        mincon.test(-5)
        mincon.test(0)
        try:
            mincon.test(-5.00001)
        except ConstraintError, err:
            self.assertEqual(str(err), "constraint '-5.00001 >= -5' has been violated")
            
    def test_max(self):
        maxcon = MaxConstraint(99)
        maxcon.test(-5)
        maxcon.test(0)
        try:
            maxcon.test(100)
        except ConstraintError, err:
            self.assertEqual(str(err), "constraint '100 <= 99' has been violated")
            
    def test_min_length(self):
        mincon = MinLengthConstraint(1)
        mincon.test([1, 2])
        mincon.test((3, 4, 5))
        try:
            mincon.test([])
        except ConstraintError, err:
            self.assertEqual(str(err), "min length constraint '0 >= 1' has been violated")
        else:
            self.fail("ConstraintError expected")
        try:
            mincon.test(5)
        except ConstraintError, err:
            self.assertEqual(str(err), "length constraint violated. value (5) has no length")
        else:
            self.fail("ConstraintError expected")
            
    def test_max_length(self):
        maxcon = MaxLengthConstraint(3)
        maxcon.test([1, 2, 3])
        maxcon.test((3, 4))
        try:
            maxcon.test([1, 2, 3, 4])
        except ConstraintError, err:
            self.assertEqual(str(err), "max length constraint '4 <= 3' has been violated")
        else:
            self.fail("ConstraintError expected")
        try:
            maxcon.test(5)
        except ConstraintError, err:
            self.assertEqual(str(err), "length constraint violated. value (5) has no length")
        else:
            self.fail("ConstraintError expected")
            
