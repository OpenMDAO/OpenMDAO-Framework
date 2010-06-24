"""
Test for CaseRecorders.
"""

import unittest
import tempfile
import StringIO
import os

from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.api import DBCaseIterator, DBCaseRecorder, DumpCaseRecorder, ListCaseIterator
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver

    
class CaseRecorderTestCase(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        driver = top.add('driver', SimpleCaseIterDriver())
        top.add('comp1', ExecComp(exprs=['z=x+y']))
        top.add('comp2', ExecComp(exprs=['z=x+1']))
        top.connect('comp1.z', 'comp2.x')
        
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
        
        
    def test_inoutDB(self):
        """This test runs some cases, puts them in a DB using a DBCaseRecorder,
        then runs the model again using the same cases, pulled out of the DB
        by a DBCaseIterator.  Finally the cases are dumped to a string after
        being run for the second time.
        """
        self.top.driver.recorder = DBCaseRecorder()
        self.top.run()
        
        # now use the DB as source of Cases
        self.top.driver.iterator = DBCaseIterator()
        
        # since the db is in memory in this test, use the connection from the
        # DBCaseRecorder to get access to the same tables
        self.top.driver.iterator._connection = self.top.driver.recorder._connection
        
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
            '   max_retries: None, retries: 1',
            ]
        self.assertTrue('\n'.join(expected) in sout.getvalue())
    
    def test_file_db(self):
        self.top.driver.recorder = DBCaseRecorder('recorder_db')
        self.top.run()
        
    
    def test_tables_already_exist(self):
        recorder = DBCaseRecorder('junk_dbfile')
        recorder._connection.close()
        recorder = DBCaseRecorder('junk_dbfile')
        recorder._connection.close()
        os.remove('junk_dbfile')
        

if __name__ == '__main__':
    unittest.main()

