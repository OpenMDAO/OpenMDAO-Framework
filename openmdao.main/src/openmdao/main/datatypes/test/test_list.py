# pylint: disable-msg=C0111,C0103

import unittest

import numpy as np

from openmdao.main.api import Container
from openmdao.main.datatypes.list import List

class ListTestCase(unittest.TestCase):

    def setUp(self):
        """this setup function will be called before each test in this class"""
        self.hobj = Container()
        self.hobj.add('list1', List(iotype='in'))

    def tearDown(self):
        """this teardown function will be called after each test"""
        self.hobj = None

    def test_assignment(self):
        #check assignment with generator
        self.hobj.list1 = range(5)
        self.hobj.list1 = xrange(5)
        self.hobj.list1 = (x for x in xrange(5))
        self.hobj.list1 = np.zeros(2)

    def test_get(self):
        self.hobj.list1 = range(5)
        self.assertEqual([0, 1, 2, 3, 4], self.hobj.list1)

        self.hobj.list1 = xrange(5)
        self.assertEqual([0, 1, 2, 3, 4], self.hobj.list1)

        self.hobj.list1 = (x for x in range(5))
        self.assertEqual([0, 1, 2, 3, 4], self.hobj.list1)

        self.hobj.list1 = np.zeros(1)
        self.assertEqual([0.0], self.hobj.list1)

        self.hobj.list1 = np.zeros((2,2))
        self.assertEqual([[0.0, 0.0], [0.0, 0.0]], self.hobj.list1)

if __name__ == "__main__":
    unittest.main()
