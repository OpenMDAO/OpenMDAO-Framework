"""
Test for CaseRecorders.
"""

import unittest
import tempfile
import StringIO
import os

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.caseiterators.api import DBCaseIterator, ListCaseIterator
from openmdao.lib.caserecorders.api import DBCaseRecorder, DumpCaseRecorder
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver
from openmdao.main.uncertain_distributions import NormalDistribution
    
class DumpCaseRecorderTestCase(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        driver = top.add('driver', SimpleCaseIterDriver())
        top.add('comp1', ExecComp(exprs=['z=x+y']))
        top.add('comp2', ExecComp(exprs=['z=x+1']))
        top.connect('comp1.z', 'comp2.x')
        driver.workflow.add([top.comp1, top.comp2])
        
        # now create some Cases
        outputs = [('comp1.z', None, None), ('comp2.z', None, None)]
        cases = []
        for i in range(10):
            inputs = [('comp1.x', None, i), ('comp1.y', None, i*2)]
            cases.append(Case(inputs=inputs, outputs=outputs, ident='case%s'%i))
        driver.iterator = ListCaseIterator(cases)

    def test_dumprecorder(self):
        sout = StringIO.StringIO()
        self.top.driver.recorder = DumpCaseRecorder(sout)
        self.top.run()
        expected = [
            'Case: case8',
            '   inputs:',
            '      comp1.x = 8',
            '      comp1.y = 16',
            '   outputs:',
            '      comp1.z = 24.0',
            '      comp2.z = 25.0',
            '   max_retries: None, retries: 0',
            ]
        self.assertTrue('\n'.join(expected) in sout.getvalue())
        
if __name__ == '__main__':
    unittest.main()

