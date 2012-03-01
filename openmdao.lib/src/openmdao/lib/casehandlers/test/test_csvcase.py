"""
Test for CSVCaseRecorder and CSVCaseIterator.
"""
import unittest

from openmdao.lib.casehandlers.csvcase import CSVCaseIterator, CSVCaseRecorder
from openmdao.lib.drivers.api import SimpleCaseIterDriver, CaseIteratorDriver
from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp


class DBCaseRecorderTestCase(unittest.TestCase):

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

if __name__ == '__main__':
    unittest.main()
