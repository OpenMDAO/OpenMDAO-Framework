"""
Test array sizing with a sub-driver.
"""

import unittest

import numpy as np

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Array
from openmdao.main.test.simpledriver import SimpleDriver


class SrcComp(Component):
    """ Source Component
    """

    x = Array(iotype='in')
    y = Array(iotype='out')

    def __init__(self):
        super(SrcComp, self).__init__()

        self.x = np.zeros((1,2))
        self.x[0, 0] = 3.3
        self.y = np.zeros((1,2))

    def execute(self):
        self.y = 2.0*self.x


class TargComp(Component):
    """ Target Component
    """

    x = Array(iotype='in')
    y = Array(iotype='out')

    def __init__(self):
        super(TargComp, self).__init__()

        self.x = np.zeros((4,4))
        self.y = np.zeros((4,4))

    def execute(self):
        self.y = 2.0*self.x


class Top(Assembly):
    """ Assembly that passes an array to a sub-driver
    """

    def configure(self):
        self.add('src', SrcComp())
        self.add('tgt', TargComp())
        self.add('sub', SimpleDriver())

        self.connect('src.y', 'tgt.x')

        self.sub.workflow.add('src')
        self.driver.workflow.add(['sub', 'tgt'])


class TestCaseArraySizing(unittest.TestCase):
    def test_array_sizing(self):
        top = set_as_top(Top())
        top.run()

        # make sure it ran without error
        self.assertEqual(top.src.y[0][0], 6.6)
        self.assertEqual(top.tgt.y[0][0], 13.2)


if __name__ == "__main__":
    unittest.main()
