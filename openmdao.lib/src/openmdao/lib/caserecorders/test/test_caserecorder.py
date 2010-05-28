"""
Test for CaseRecorders.
"""

import unittest
import tempfile
import StringIO

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.api import DBCaseIterator, DBCaseRecorder, DumpCaseRecorder, ListCaseIterator
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver

    
class CaseRecorderTestCase(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        top.add_container('comp1', ExecComp(exprs=['z=x+y']))
        top.add_container('comp2', ExecComp(exprs=['z=x+1']))
        top.connect('comp1.z', 'comp2.x')
        driver = top.add_container('driver', SimpleCaseIterDriver())
        
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
            'ident: case8',
            'retries: 0',
            'max_retries: None',
            'inputs:',
            '    comp1.x = 8',
            '    comp1.y = 16',
            'outputs:',
            '    comp1.z = 24.0',
            '    comp2.z = 25.0'
            ]
        self.assertTrue('\n'.join(expected) in sout.getvalue())
        
        
    def test_caseDBrecorder(self):
        self.top.driver.recorder = DBCaseRecorder()  # db file defaults to ':memory:'
        self.top.run()
        
        # now use the DB as the CaseIterator
        self.top.driver.iterator = DBCaseIterator()
        self.top.driver.iterator.connection = self.top.driver.recorder.connection
        sout = StringIO.StringIO()
        self.top.driver.recorder = DumpCaseRecorder(sout)
        self.top.run()
        sout.getvalue()

if __name__ == '__main__':
    unittest.main()

