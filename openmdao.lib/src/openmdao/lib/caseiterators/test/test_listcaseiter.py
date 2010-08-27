"""
Test for Case iterators.
"""

import unittest

import cStringIO

from openmdao.main.api import Case
from openmdao.lib.api import ListCaseIterator

class ListCaseIterTestCase(unittest.TestCase):

    def test_list(self):
        outputs = [('z1', None, None), ('z2', None, None)]
        cases = []
        for i in range(5):
            inputs = [('x', None, i), ('y', None, i*2)]
            cases.append(Case(inputs, outputs))
        iterator = ListCaseIterator(cases)

        for i, case in enumerate(iterator):
            self.assertEqual(len(case.inputs), 2)
            self.assertEqual(len(case.outputs), 2)

            self.assertEqual(case.inputs[0][0], 'x')
            self.assertEqual(case.inputs[0][1], None)
            self.assertEqual(case.inputs[0][2], i)
            self.assertEqual(len(case.inputs[0]), 3)

            self.assertEqual(case.inputs[1][0], 'y')
            self.assertEqual(case.inputs[1][1], None)
            self.assertEqual(case.inputs[1][2], i*2)
            self.assertEqual(len(case.inputs[1]), 3)

            self.assertEqual(case.outputs[0][0], 'z1')
            self.assertEqual(case.outputs[0][1], None)
            self.assertEqual(case.outputs[0][2], None)
            self.assertEqual(len(case.outputs[0]), 3)

            self.assertEqual(case.outputs[1][0], 'z2')
            self.assertEqual(case.outputs[1][1], None)
            self.assertEqual(case.outputs[1][2], None)
            self.assertEqual(len(case.outputs[1]), 3)

        self.assertEqual(i, 4)


if __name__ == '__main__':
    unittest.main()

