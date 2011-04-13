import unittest

from nose import SkipTest

from openmdao.main.api import Component, Assembly, Case, CaseSet, set_as_top
from openmdao.lib.datatypes.api import Int, List

class Simple(Component):
    a = Int(iotype='in')
    b = Int(iotype='in')
    c = Int(iotype='out')
    d = Int(iotype='out')
    
    def __init__(self):
        super(Simple, self).__init__()
        self.a = 1
        self.b = 2
        self.c = 3
        self.d = -1

    def execute(self):
        self.c = self.a + self.b
        self.d = self.a - self.b

class CaseSetTestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('comp1', Simple())
        self.top.add('comp2', Simple())
        self.top.connect('comp1.c', 'comp2.a')
        self.top.driver.workflow.add(['comp1','comp2'])
        inputs=[('comp1.a',4),('comp1.b',8),('comp2.b',2)]
        outputs=['comp2.c+comp2.d','max(comp1.d,comp2.d)']
        self.case1 = Case(inputs=inputs, outputs=outputs)
        self.case1_dup = Case(inputs=inputs, outputs=outputs)
        inputs[1] = ('comp1.b',9)
        self.case2 = Case(inputs=inputs, outputs=outputs)
    
    def test_caseset_from_dict(self):
        dct = { 'comp1.a': [2,4,6],
                'comp1.b': [4,8,12],
                'comp2.b': [1,2,3],
            }
        cs = CaseSet(dct)
        self.assertEqual(3, len(cs))
        self.assertEqual(set(['comp1.a','comp1.b','comp2.b']),
                         set(cs._names))
        self.assertEqual(cs['comp1.a'], [2,4,6])
        case = cs[1]
        expected = Case(inputs=[('comp1.a',4),('comp1.b',8),('comp2.b',2)])
        self.assertEqual(case._inputs, expected._inputs)
        
    def test_caseset_from_case(self):
        cs = CaseSet(self.case1)
        self.assertEqual(1, len(cs))
        self.assertEqual(cs[0]._inputs, self.case1_dup._inputs)
        self.assertEqual(cs[0]._outputs, self.case1_dup._outputs)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        self.assertEqual(3, len(cs))  # not unique, so dups allowed
        
    def test_caseset_start_empty(self):
        cs = CaseSet()
        self.assertEqual(0, len(cs))
        cs.record(self.case1)
        self.assertEqual(1, len(cs))
        self.assertEqual(cs[0]._inputs, self.case1_dup._inputs)
        self.assertEqual(cs[0]._outputs, self.case1_dup._outputs)
        
    def test_caseset_unique(self):
        cs = CaseSet(unique=True)
        cs.record(self.case1)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        self.assertEqual(2, len(cs))
        self.assertEqual(cs[0]._inputs, self.case1_dup._inputs)
        self.assertEqual(cs[0]._outputs, self.case1_dup._outputs)
        self.assertEqual(cs[1]._inputs, self.case2._inputs)
        self.assertEqual(cs[1]._outputs, self.case2._outputs)
        
    def test_caseset_iteration(self):
        cs = CaseSet()
        cs.record(self.case1)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        expected = [self.case1, self.case2, self.case1_dup]
        for i,case in enumerate(cs.get_iter()):
            self.assertEqual(case._inputs, expected[i]._inputs)
            self.assertEqual(case._outputs, expected[i]._outputs)

if __name__ == "__main__":
    unittest.main()


