"""
Tests for Case filters.
"""

import unittest

from openmdao.main.api import Case
from openmdao.lib.casehandlers.api import SequenceCaseFilter, SliceCaseFilter, \
                                          ExprCaseFilter
from openmdao.util.testutil import assert_raises


class TestCase(unittest.TestCase):
    """ Tests for Case filters. """

    def test_sequence(self):
        seq_numbers = range(10)
        ok_numbers = [1, 3, 5]
        accepted = []
        filter = SequenceCaseFilter(ok_numbers)
        for seqno in seq_numbers:
            if filter.select(seqno, Case()):
                accepted.append(seqno)
        self.assertEqual(accepted, ok_numbers)

    def test_slice(self):
        seq_numbers = range(10)
        ok_numbers = range(2, 10, 3)
        accepted = []
        filter = SliceCaseFilter(start=2, stop=10, step=3)
        for seqno in seq_numbers:
            if filter.select(seqno, Case()):
                accepted.append(seqno)
        self.assertEqual(accepted, ok_numbers)

    def test_expr(self):
        cases = (
            Case(inputs=(('comp1.a', 4),),
                 outputs=(('comp2.b', 3),)),
            Case(inputs=(('comp1.a', 6),),
                 outputs=(('comp2.b', 3),)),
            Case(inputs=(('comp1.a', 4),),
                 outputs=(('comp2.b', 2),)),
        )

        # By sequence number.
        filter = ExprCaseFilter('seqno > 1')
        accepted = []
        for seqno, case in enumerate(cases):
            if filter.select(seqno, case):
                accepted.append(seqno)
        self.assertEqual(accepted, [2])

        # Simple variable test.
        filter = ExprCaseFilter("case['comp1.a'] < 5")
        accepted = []
        for seqno, case in enumerate(cases):
            if filter.select(seqno, case):
                accepted.append(seqno)
        self.assertEqual(accepted, [0, 2])

        # Two variable test.
        filter = ExprCaseFilter("case['comp1.a'] < 5 and case['comp2.b'] > 2")
        accepted = []
        for seqno, case in enumerate(cases):
            if filter.select(seqno, case):
                accepted.append(seqno)
        self.assertEqual(accepted, [0])

        # Missing variable.
        filter = ExprCaseFilter("case['comp1.z'] < 5")
        assert_raises(self, 'filter.select(0, cases[0])',
                      globals(), locals(), KeyError,
                      '\'Can\\\'t evaluate expression "case[\\\'comp1.z\\\'] < 5":'
                      ' \\\'comp1.z\\\'')

        # Bad expression.
        assert_raises(self, """ExprCaseFilter("case['comp1.z] < 5")""",
                      globals(), locals(), SyntaxError,
                      'Can\'t compile expression "case[\'comp1.z] < 5":'
                      ' EOL while scanning ')


if __name__ == '__main__':
    unittest.main()

