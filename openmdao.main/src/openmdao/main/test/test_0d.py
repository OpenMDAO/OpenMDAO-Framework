import unittest

import numpy as np

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Array, Float

class AComp(Component):
    arr1 = Array(np.array(2.0), iotype='out', units='ft')

    def execute(self):
        pass

class FComp(Component):
    f = Float(1.11, iotype='in', units='inch')

    def execute(self):
        pass

class ZeroDTestCase(unittest.TestCase):
    def test_0d(self):
        a = set_as_top(Assembly())
        a.add('c', AComp())
        a.add('c2', FComp())
        a.driver.workflow.add(['c','c2'])
        a.connect('c.arr1[()]', 'c2.f')

        a.run()

        self.assertAlmostEqual(a.c2.f, 24.0)

if __name__ == '__main__':
    unittest.main()
    