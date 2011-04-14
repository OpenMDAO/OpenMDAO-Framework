import unittest


from openmdao.main.api import Case, CaseSet, CaseArray

class CaseArrayTestCase(unittest.TestCase):

    def setUp(self):
        inputs=[('comp1.a',4),('comp1.b',8),('comp2.b',2)]
        outputs=['comp2.c+comp2.d','max(comp1.d,comp2.d)']
        self.case1 = Case(inputs=inputs, outputs=outputs)
        self.case1_dup = Case(inputs=inputs, outputs=outputs)
        inputs[1] = ('comp1.b',9)
        self.case2 = Case(inputs=inputs, outputs=outputs)
    
    def test_from_dict(self):
        dct = { 'comp1.a': [2,4,6],
                'comp1.b': [4,8,12],
                'comp2.b': [1,2,3],
            }
        cs = CaseArray(dct)
        self.assertEqual(3, len(cs))
        self.assertEqual(set(['comp1.a','comp1.b','comp2.b']),
                         set(cs._names))
        self.assertEqual(cs['comp1.a'], [2,4,6])
        case = cs[1]
        expected = Case(inputs=[('comp1.a',4),('comp1.b',8),('comp2.b',2)])
        self.assertEqual(case._inputs, expected._inputs)
        
    def test_from_case(self):
        cs = CaseArray(self.case1)
        self.assertEqual(1, len(cs))
        self.assertEqual(cs[0]._inputs, self.case1_dup._inputs)
        self.assertEqual(cs[0]._outputs, self.case1_dup._outputs)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        self.assertEqual(3, len(cs))
        
    def test_start_empty(self):
        cs = CaseArray()
        cs.record(self.case1)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        self.assertEqual(3, len(cs))
        self.assertEqual(cs[0]._inputs, self.case1_dup._inputs)
        self.assertEqual(cs[0]._outputs, self.case1_dup._outputs)
        self.assertEqual(cs[1]._inputs, self.case2._inputs)
        self.assertEqual(cs[1]._outputs, self.case2._outputs)
        self.assertEqual(cs[2]._inputs, self.case1_dup._inputs)
        self.assertEqual(cs[2]._outputs, self.case1_dup._outputs)
        
    def test_iteration(self):
        cs = CaseArray()
        cs.record(self.case1)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        expected = [self.case1, self.case2, self.case1_dup]
        for i,case in enumerate(cs.get_iter()):
            self.assertEqual(case._inputs, expected[i]._inputs)
            self.assertEqual(case._outputs, expected[i]._outputs)

class CaseSetTestCase(unittest.TestCase):

    def setUp(self):
        inputs=[('comp1.a',4),('comp1.b',8),('comp2.b',2)]
        outputs=['comp2.c+comp2.d','max(comp1.d,comp2.d)']
        self.case1 = Case(inputs=inputs, outputs=outputs)
        self.case1_dup = Case(inputs=inputs, outputs=outputs)
        inputs[1] = ('comp1.b',9)
        self.case2 = Case(inputs=inputs, outputs=outputs)
        self.caselist = [self.case1, self.case2, self.case1_dup]
        for i in range(5):
            inputs[1] = ('comp1.b', 10+i)
            self.caselist.append(Case(inputs=inputs, outputs=outputs))
        self.caselist.append(Case(inputs=inputs, outputs=outputs)) # another dup
    
    def test_from_dict(self):
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
        
    def test_from_case(self):
        cs = CaseSet(self.case1)
        self.assertEqual(1, len(cs))
        self.assertEqual(cs[0]._inputs, self.case1_dup._inputs)
        self.assertEqual(cs[0]._outputs, self.case1_dup._outputs)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        self.assertEqual(2, len(cs))
        
    def test_start_empty(self):
        cs = CaseSet()
        cs.record(self.case1)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        self.assertEqual(2, len(cs))
        self.assertEqual(cs[0]._inputs, self.case1_dup._inputs)
        self.assertEqual(cs[0]._outputs, self.case1_dup._outputs)
        self.assertEqual(cs[1]._inputs, self.case2._inputs)
        self.assertEqual(cs[1]._outputs, self.case2._outputs)
        
    def test_iteration(self):
        cs = CaseSet()
        cs.record(self.case1)
        cs.record(self.case2)
        cs.record(self.case1_dup)
        expected = [self.case1, self.case2, self.case1_dup]
        for i,case in enumerate(cs.get_iter()):
            self.assertEqual(case._inputs, expected[i]._inputs)
            self.assertEqual(case._outputs, expected[i]._outputs)
            
    def test_set_ops(self):
        cs = CaseSet()
        cs2 = CaseSet()
        for case in self.caselist:
            cs.record(case)
            cs2.record(case)
        self.assertEqual(len(cs), len(self.caselist)-2)
        self.assertTrue(cs == cs2)
        case = self.caselist[1]
        vals = cs2.pop(1)
        self.assertFalse(cs == cs2)
        self.assertTrue(cs2 < cs)
        self.assertFalse(cs < cs)
        self.assertTrue(cs <= cs)
        self.assertTrue(cs >= cs)
        self.assertFalse(cs2 > cs)
        self.assertFalse(cs2 > cs2)
        self.assertTrue(case in cs)
        self.assertFalse(case in cs2)
        self.assertTrue(case not in cs2)
        self.assertFalse(case not in cs)
        
        cs3 = CaseSet()
        cs4 = CaseSet()
        cs3.record(self.case1)
        cs3.record(self.case2)
        try:
            self.assertTrue(cs3.isdisjoint(cs4))
        except ValueError, err:
            self.assertEqual(str(err), "case containers have different sets of variables")
        else:
            self.fail("expected ValueError")
        cs4.record(self.caselist[6])
        self.assertTrue(cs3.isdisjoint(cs4))
        cs4.record(self.case1)
        self.assertFalse(cs3.isdisjoint(cs4))
        
        cs_union = cs3 | cs4
        self.assertEqual(len(cs_union), len(cs3)+len(cs4)-1)
        
        cs_intersect = cs3 & cs4
        self.assertEqual(len(cs_intersect), 1)
        self.assertEqual(cs_intersect[0], self.case1)
        

if __name__ == "__main__":
    unittest.main()


