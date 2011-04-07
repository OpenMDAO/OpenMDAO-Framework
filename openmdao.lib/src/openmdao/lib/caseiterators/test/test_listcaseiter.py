"""
Test for Case iterators.
"""

import unittest

import cStringIO

from openmdao.main.api import Case
from openmdao.lib.caseiterators.api import ListCaseIterator
from openmdao.main.case import _Missing

class ListCaseIterTestCase(unittest.TestCase):

    def test_list(self):
        outputs = ['z1', 'z2']
        cases = []
        for i in range(5):
            inputs = [('x', i), ('y', i*2)]
            cases.append(Case(inputs, outputs))
        iterator = ListCaseIterator(cases)

        for i, case in enumerate(iterator):
            self.assertEqual(len(case.items(iotype='in')), 2)
            self.assertEqual(len(case.items(iotype='out')), 2)

            self.assertTrue('x' in case)
            self.assertEqual(case['x'], i)
            
            self.assertTrue('y' in case)
            self.assertEqual(case['y'], i*2)
            
            self.assertTrue('z1' in case)
            self.assertEqual(case['z1'], _Missing)
            self.assertTrue('z2' in case)
            self.assertEqual(case['z2'], _Missing)

        self.assertEqual(i, 4)


if __name__ == '__main__':
    unittest.main()

