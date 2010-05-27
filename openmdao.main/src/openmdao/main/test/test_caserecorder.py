"""
Test for CaseRecorders.
"""

import unittest

from openmdao.main.api import Case, CaseDBRecorder, ListCaseIterator

class CaseRecorderTestCase(unittest.TestCase):

    def test_caseDBrecorder(self):
        outputs = [('z1', 7, None), ('z2', 5.8, None)]
        cases = []
        for i in range(5):
            inputs = [('x', None, i), ('y', None, i*2)]
            cases.append(Case(inputs, outputs))
        iterator = ListCaseIterator(cases)
        
        cr = CaseDBRecorder('mycases')
        
        for case in iterator:
            cr.record(case)
        
        

if __name__ == '__main__':
    unittest.main()

