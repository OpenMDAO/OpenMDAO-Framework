import unittest
import copy

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.lib.datatypes.api import Int, List

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
        self.a_lst = [1,2,3]

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
        self.top.driver.workflow.add(['comp1','comp2'])
        
        self.inputs = [('comp1.a',2),('comp1.b',4),('comp1.a_lst', [4,5,6])]
        self.outputs = ['comp2.c+comp2.d', 'comp2.c_lst[2]', 'comp2.d']
        self.case = case = Case(inputs=self.inputs, outputs=self.outputs)
        case.apply_inputs(self.top)
        self.top.run()
        case.update_outputs(self.top)
    
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
        expected = ["Case blah-blah-blah:",
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
        for i,line in enumerate(str(self.case).split('\n')):
            if i==0: # case id will always change
                self.assertTrue(line.startswith('Case '))
            else:
                self.assertEqual(line, expected[i])
                
        case = Case(inputs=[('comp1.a',4), ('comp1.b',8)],desc='foo',
                    parent_id='abc-xyz-pdq', max_retries=5, retries=4,
                    msg='failed')
        expected = ["Case blah-blah-blah: (parent_id: abc-xyz-pdq)",
                    "   description: foo",
                    "   inputs:",
                    "      comp1.a: 4",
                    "      comp1.b: 8",
                    "   max_retries: 5",
                    "   retries: 4",
                    "   msg: failed",
                    "",
               ]
        for i,line in enumerate(str(case).split('\n')):
            if i==0: # case id will always change
                self.assertTrue(line.startswith('Case '))
                self.assertTrue(line.endswith(' (parent_id abc-xyz-pdq)'))
            else:
                self.assertEqual(line, expected[i])
                
        self.assertFalse(case == self.case)
        self.assertTrue(case == case)
        self.assertTrue(case == copy.deepcopy(case))
        
        
    def test_items(self):
        inputs = dict(self.case.items(iotype='in'))
        self.assertEqual(len(self.inputs), len(inputs))
        for name,val in self.inputs:
            self.assertTrue(name in inputs)
            self.assertEqual(val, inputs[name])
            
        outputs = dict(self.case.items(iotype='out'))
        expected_outs = dict([(k,v) for k,v in zip(self.outputs, [12,24,8])])
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
        

if __name__ == "__main__":
    unittest.main()


