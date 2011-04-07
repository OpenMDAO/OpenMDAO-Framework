import unittest

from openmdao.lib.caseiterators.listcaseiter import ListCaseIterator
from openmdao.main.api import Case
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.main.caseiter import caseiter_to_dict
from openmdao.main.container import _get_entry_group

class CaseIterTestCase(unittest.TestCase):

    def setUp(self):
        cases = []
        for i in range(20):
            inputs = [('comp1.x', float(i)), ('comp1.y', i*2.)]
            outputs = [('comp1.z', i*1.5), ('comp2.normal', NormalDistribution(float(i),0.5))]
            if i < 10:
                msg = ''
            else:
                msg = 'had an error'
            case = Case(inputs=inputs, msg=msg)
            case._outputs = dict(outputs)
            cases.append(case)
        self.caseiter = ListCaseIterator(cases)
        self.varnames = ['comp2.normal', 'comp1.x', 'comp1.z']
        
    def test_caseiter_to_dict_without_errors(self):
        dct = caseiter_to_dict(self.caseiter, self.varnames, include_errors=False)
        
        self.assertEqual(len(dct), 3)
        
        for name,value in dct.items():
            self.assertEqual(len(value), 10)
            if name == 'comp2.normal':
                self.assertTrue(isinstance(value[0], NormalDistribution))
            else:
                self.assertTrue(isinstance(value[0], float))

    def test_caseiter_to_dict_with_errors(self):
        dct = caseiter_to_dict(self.caseiter, self.varnames, include_errors=True)
        
        self.assertEqual(len(dct), 3)
        
        for name,value in dct.items():
            self.assertEqual(len(value), 20)
            if name == 'comp2.normal':
                self.assertTrue(isinstance(value[0], NormalDistribution))
            else:
                self.assertTrue(isinstance(value[0], float))

    def test_get_entry_group(self):
        self.assertEqual(_get_entry_group(self.caseiter), 'openmdao.case_iterator')


if __name__ == "__main__":
    unittest.main()


