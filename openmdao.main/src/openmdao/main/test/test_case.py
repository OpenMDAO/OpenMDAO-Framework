import unittest
import copy
import array

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.main.datatypes.api import Int, List
from numpy import array as nparray

from openmdao.main.test.test_vartree import DumbVT


class Simple(Component):
    a = Int(iotype='in')
    b = Int(iotype='in')
    c = Int(iotype='out')
    d = Int(iotype='out')
    a_lst = List(Int, iotype='in')
    c_lst = List(Int, iotype='out')

    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1
        self.a_lst = [1, 2, 3]

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b
        self.c_lst = [x*2 for x in self.a_lst]


class CaseTestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('comp1', Simple())
        self.top.add('comp2', Simple())
        self.top.connect('comp1.c', 'comp2.a')
        self.top.connect('comp1.d', 'comp2.b')
        self.top.connect('comp1.c_lst', 'comp2.a_lst')
        self.top.driver.workflow.add(['comp1', 'comp2'])

        self.inputs = [('comp1.a', 2), ('comp1.b', 4),
                       ('comp1.a_lst', [4, 5, 6])]
        self.outputs = ['comp2.c+comp2.d', 'comp2.c_lst[2]', 'comp2.d']
        self.case = case = Case(inputs=self.inputs, outputs=self.outputs)
        case.apply_inputs(self.top)
        self.top.run()
        case.update_outputs(self.top)

    def test_subcase(self):
        subcase = self.case.subcase(['comp1.b', 'comp2.d'])
        self.assertEqual(self.case.timestamp, subcase.timestamp)
        self.assertEqual(len(subcase.get_inputs()), 1)
        self.assertEqual(subcase['comp1.b'], 4)
        self.assertEqual(len(subcase.get_outputs()), 1)
        self.assertEqual(subcase.get_outputs()[0][0], 'comp2.d')

    def test_case_access(self):
        self.assertEqual(self.case['comp1.a'], 2)
        self.assertEqual(self.case['comp2.c_lst[2]'], 24)

        self.case['comp1.a'] = 5
        self.assertEqual(self.case['comp1.a'], 5)

    def test_case_containment(self):
        self.assertTrue('comp1.a' in self.case)
        self.assertFalse('comp1.z' in self.case)
        self.assertTrue('comp2.c+comp2.d' in self.case)

    def test_len(self):
        self.assertEqual(len(self.case), 6)

    def test_str(self):
        expected = ["Case:",
                    "   uuid: sdfsfdasfdasdf",
                    '   timestamp: 1383239074.309192',
                    "   inputs:",
                    "      comp1.a: 2",
                    "      comp1.a_lst: [4, 5, 6]",
                    "      comp1.b: 4",
                    "   outputs:",
                    "      comp2.c+comp2.d: 12",
                    "      comp2.c_lst[2]: 24",
                    "      comp2.d: 8",
                    "",
               ]
        for i, line in enumerate(str(self.case).split('\n')):
            if expected[i].startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            elif expected[i].startswith('   timestamp:'):
                self.assertTrue(line.startswith('   timestamp:'))
            else:
                self.assertEqual(line, expected[i])

        case = Case(inputs=[('comp1.a', 4), ('comp1.b', 8)],
                    case_uuid='abcd-efg', parent_uuid='abc-xyz-pdq')
        expected = ["Case:",
                    "   uuid: abcd-efg",
                    '   timestamp: 1383239074.309192',
                    "   parent_uuid: abc-xyz-pdq",
                    "   inputs:",
                    "      comp1.a: 4",
                    "      comp1.b: 8",
                    "",
               ]
        for i, line in enumerate(str(case).split('\n')):
            if expected[i].startswith('   timestamp:'):
                self.assertTrue(line.startswith('   timestamp:'))
            else:
                self.assertEqual(line, expected[i])

        self.assertFalse(case == self.case)
        self.assertTrue(case == case)
        self.assertTrue(case == copy.deepcopy(case))

    def test_items(self):
        inputs = dict(self.case.items(iotype='in'))
        self.assertEqual(len(self.inputs), len(inputs))
        for name, val in self.inputs:
            self.assertTrue(name in inputs)
            self.assertEqual(val, inputs[name])

        outputs = dict(self.case.items(iotype='out'))
        expected_outs = dict([(k,v) for k,v in zip(self.outputs, [12, 24, 8])])
        self.assertEqual(len(expected_outs), len(outputs))
        for name, val in expected_outs.items():
            self.assertTrue(name in outputs)
            self.assertEqual(val, outputs[name])

        both = dict(self.case.items())
        expected = expected_outs
        expected.update(self.inputs)
        self.assertEqual(len(both), len(self.inputs)+len(self.outputs))
        for name, val in expected.items():
            self.assertTrue(name in both)
            self.assertEqual(val, both[name])

    def test_flatten(self):
        dvt = DumbVT()
        inputs = [('comp1.a_lst', [1, 2, 3, [7, 8, 9]]),
                  ('comp1.a_arr', array.array('d', [4, 5, 6])),
                  ('comp1.np_arr', nparray([[1, 2], [3, 4], [5, 6]])),
                  ('comp1.vt', dvt),
                  ]
        case = Case(inputs=inputs)
        self.assertEqual(set(case.items(flatten=True)),
                         set([('comp1.a_lst[0]', 1),
                              ('comp1.a_lst[1]', 2),
                              ('comp1.a_lst[2]', 3),
                              ('comp1.a_lst[3][0]', 7),
                              ('comp1.a_lst[3][1]', 8),
                              ('comp1.a_lst[3][2]', 9),
                              ('comp1.a_arr[0]', 4.0),
                              ('comp1.a_arr[1]', 5.0),
                              ('comp1.a_arr[2]', 6.0),
                              ('comp1.np_arr[0][0]', 1),
                              ('comp1.np_arr[0][1]', 2),
                              ('comp1.np_arr[1][0]', 3),
                              ('comp1.np_arr[1][1]', 4),
                              ('comp1.np_arr[2][0]', 5),
                              ('comp1.np_arr[2][1]', 6),
                              ('comp1.vt.vt2.vt3.a', 1.),
                              ('comp1.vt.vt2.vt3.b', 12.),
                              ('comp1.vt.vt2.x', -1.),
                              ('comp1.vt.vt2.y', -2.),
                              ('comp1.vt.v1', 1.),
                              ('comp1.vt.v2', 2.),
                              ('comp1.vt.data', ''),
                              ('comp1.vt.vt2.data', ''),
                              ('comp1.vt.vt2.vt3.data', '')]))

if __name__ == "__main__":
    unittest.main()

