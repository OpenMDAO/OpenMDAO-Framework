"""
Test for CaseRecorders.
"""

import unittest
import tempfile

from openmdao.main.api import Component, Assembly, Case, DBCaseRecorder, ListCaseIterator, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver

    
class CaseRecorderTestCase(unittest.TestCase):

    def setup_model(self):
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


    def test_caseDBrecorder(self):
        self.setup_model()
        self.top.driver.recorder = DBCaseRecorder()  # db file defaults to ':memory:'
        self.top.run()
        
        # now use the DB as the CaseIterator
        self.top.driver.iterator = DBCaseIterator()

if __name__ == '__main__':
    unittest.main()

