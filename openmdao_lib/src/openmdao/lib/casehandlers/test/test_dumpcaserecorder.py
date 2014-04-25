"""
Test for CaseRecorders.
"""

import unittest
import tempfile
import StringIO
import os
import sys

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import DBCaseIterator, ListCaseIterator
from openmdao.lib.casehandlers.api import DBCaseRecorder, DumpCaseRecorder
from openmdao.lib.drivers.sensitivity import SensitivityDriver
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.util.testutil import assert_raises
    
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
            cases.append(Case(inputs=inputs, outputs=outputs, label='case%s'%i))
        driver.iterator = ListCaseIterator(cases)

    def test_bad_recorder(self):
        try:
            self.top.driver.recorders = DumpCaseRecorder()
        except Exception as err:
            self.assertTrue(str(err).startswith("The 'recorders' trait of a SimpleCaseIterDriver"))
            self.assertTrue(str(err).endswith(" was specified."))
        else:
            self.fail("Exception expected")
        
        
    def test_dumprecorder(self):
        sout1 = StringIO.StringIO()
        sout2 = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout1), 
                                     DumpCaseRecorder(sout2)]
        self.top.run()
        expected = [
            'Case: case8',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   timestamp: 1383239074.309192',
            '   inputs:',
            '      comp1.x: 8',
            '      comp1.y: 16',
            '   outputs:',
            '      comp1.z: 24.0',
            '      comp2.z: 25.0',
            ]
        
        for sout in [sout1, sout2]:
            lines = sout.getvalue().split('\n')
            index = lines.index('Case: case8')
            for i in range(len(expected)):
                if expected[i].startswith('   uuid:'):
                    self.assertTrue(lines[index+i].startswith('   uuid:'))
                elif expected[i].startswith('   timestamp:'):
                    self.assertTrue(lines[index+i].startswith('   timestamp:'))
                else:
                    self.assertEqual(lines[index+i], expected[i])
        
    def test_multiple_objectives(self):
        sout = StringIO.StringIO()
        self.top.add('driver', SensitivityDriver())
        self.top.driver.workflow.add(['comp1', 'comp2'])
        self.top.driver.add_parameter(['comp1.x'], low=-100, high=100)
        self.top.driver.add_objective('comp1.z')
        self.top.driver.add_objective('comp2.z')
        
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        expected = [
            'Case: ',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   timestamp: 1383239074.309192',
            '   inputs:',
            '      comp1.x: [0.0]',
            '   outputs:',
            '      Objective_0: 0.0',
            '      Objective_1: 1.0',
            '      driver.workflow.itername: 1',
            ]
        
        lines = sout.getvalue().split('\n')

        for i in range(len(expected)):
            if expected[i].startswith('   uuid:'):
                self.assertTrue(lines[i].startswith('   uuid:'))
            elif expected[i].startswith('   timestamp:'):
                self.assertTrue(lines[i].startswith('   timestamp:'))
            else:
                self.assertEqual(lines[i], expected[i])

    def test_close(self):
        sout1 = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout1)]
        self.top.driver.recorders[0].close()
        self.top.run()
        self.assertEqual(sout1.getvalue(), '')


if __name__ == '__main__':
    unittest.main()

