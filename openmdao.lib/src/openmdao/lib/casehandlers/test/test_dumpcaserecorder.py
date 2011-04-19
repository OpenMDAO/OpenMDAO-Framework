"""
Test for CaseRecorders.
"""

import unittest
import tempfile
import StringIO
import os

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import DBCaseIterator, ListCaseIterator
from openmdao.lib.casehandlers.api import DBCaseRecorder, DumpCaseRecorder
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver
from openmdao.main.uncertain_distributions import NormalDistribution
    
class DumpCaseRecorderTestCase(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        driver = top.add('driver', SimpleCaseIterDriver())
        top.add('comp1', ExecComp(exprs=['z=x+y']))
        top.add('comp2', ExecComp(exprs=['z=x+1']))
        top.connect('comp1.z', 'comp2.x')
        driver.workflow.add(['comp1', 'comp2'])
        
        # now create some Cases
        outputs = ['comp1.z', 'comp2.z']
        cases = []
        for i in range(10):
            inputs = [('comp1.x', i), ('comp1.y', i*2)]
            cases.append(Case(inputs=inputs, outputs=outputs, desc='case%s'%i))
        driver.iterator = ListCaseIterator(cases)

    def test_dumprecorder(self):
        sout = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        expected = [
            'Case: case8',
            '   id: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   inputs:',
            '      comp1.x = 8',
            '      comp1.y = 16',
            '   outputs:',
            '      comp1.z = 24.0',
            '      comp2.z = 25.0',
            '   max_retries: None, retries: None',
            ]
        lines = sout.getvalue().split('\n')
        index = lines.index('Case: case8')
        for i in range(len(expected)):
            if expected[i].startswith('   id:'):
                self.assertTrue(lines[index+i].startswith('   id:'))
            else:
                self.assertEqual(lines[index+i], expected[i])
        
if __name__ == '__main__':
    unittest.main()

