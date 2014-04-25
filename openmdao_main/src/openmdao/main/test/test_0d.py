import unittest

import numpy as np

from openmdao.main.api import Assembly, Component, set_as_top
from openmdao.main.datatypes.api import Array, Float


class AComp(Component):
    arr_in = Array(np.array(2.0), iotype='in', units='ft')
    arr_out = Array(np.array(2.0), iotype='out', units='ft')
    arr_nounit_in = Array(np.array(2.0), iotype='in')
    arr_nounit_out = Array(np.array(2.0), iotype='out')

    def execute(self):
        pass


class FComp(Component):
    f_in = Float(1.11, iotype='in', units='inch')
    f_out = Float(12.0, iotype='out', units='inch')
    f_nounit_in = Float(12.0, iotype='in')
    f_nounit_out = Float(12.0, iotype='out')

    def execute(self):
        pass


class ZeroDTestCase(unittest.TestCase):
    def test_0d(self):
        a = set_as_top(Assembly())
        a.add('c', AComp())
        a.add('c2', FComp())

        a.add('c3', FComp())
        a.add('c4', AComp())

        a.add('c5', AComp())
        a.add('c6', FComp())

        a.add('c7', FComp())
        a.add('c8', AComp())
        a.driver.workflow.add(['c', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8'])
        a.connect('c.arr_out[()]', 'c2.f_in')
        a.connect('c3.f_out', 'c4.arr_in[()]')
        a.connect('c5.arr_nounit_out[()]', 'c6.f_in')
        a.connect('c7.f_out', 'c8.arr_nounit_in[()]')
        a.run()

        self.assertAlmostEqual(a.c2.f_in, 24.0)
        self.assertAlmostEqual(a.c4.arr_in, 1.0)
        self.assertEqual(a.c6.f_in, 2.0)
        self.assertEqual(a.c8.arr_nounit_in, 12.0)


if __name__ == '__main__':
    unittest.main()
