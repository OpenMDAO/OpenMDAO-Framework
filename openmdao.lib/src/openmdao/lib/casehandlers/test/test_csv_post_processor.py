import bson
import json
import os.path
import re
import sys
import unittest

from numpy import array


from struct import unpack
#from cStringIO import StringIO
import StringIO

from openmdao.main.api import Assembly, Component, Case, VariableTree, set_as_top
from openmdao.main.datatypes.api import Array, Instance, List, VarTree
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import CaseDataset, \
                                          JSONCaseRecorder, BSONCaseRecorder
from openmdao.lib.drivers.api import SensitivityDriver, CaseIteratorDriver, \
                                     SLSQPdriver
from openmdao.util.testutil import assert_raises

from openmdao.lib.casehandlers.api import caseset_query_to_csv
from openmdao.lib.casehandlers.api import CSVCaseIterator, CSVCaseRecorder, \
                                          DumpCaseRecorder
from openmdao.main.datatypes.api import Array, Str, Bool, VarTree
from openmdao.lib.drivers.api import SimpleCaseIterDriver
from openmdao.main.api import Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.util.testutil import assert_raises
from openmdao.main.test.test_vartree import DumbVT


class CSVPostProcessorTestCase(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        driver = top.add('driver', SimpleCaseIterDriver())
        top.add('comp1', ExecComp(exprs=['z=x+y']))
        top.add('comp2', ExecComp(exprs=['z=x+1']))
        top.connect('comp1.z', 'comp2.x')
        top.comp1.add('a_string', Str("Hello',;','", iotype='out'))
        top.comp1.add('a_array', Array(array([1.0, 3.0, 5.5]), iotype='out'))
        top.comp1.add('x_array', Array(array([1.0, 1.0, 1.0]), iotype='in'))
        top.comp1.add('b_bool', Bool(False, iotype='in'))
        top.comp1.add('vt', VarTree(DumbVT(), iotype='out'))
        driver.workflow.add(['comp1', 'comp2'])

        # now create some Cases
        outputs = ['comp1.z', 'comp2.z', 'comp1.a_string', 'comp1.a_array[2]']
        cases = []
        for i in range(10):
            inputs = [('comp1.x', i+0.1), ('comp1.y', i*2 + .1),
                      ('comp1.x_array[1]', 99.88), ('comp1.b_bool', True)]
            cases.append(Case(inputs=inputs, outputs=outputs))

        Case.set_vartree_inputs(driver, cases)
        driver.add_responses(sorted(outputs))

        self.filename_json = "openmdao_test_csv_case_iterator.json"
        self.filename = "openmdao_test_csv_case_iterator.csv"

    def tearDown(self):
        for recorder in self.top.recorders:
            recorder.close()
        if os.path.exists(self.filename):
            os.remove(self.filename)

    def test_inoutCSV_using_JSON(self):

        #This test runs some cases, puts them in a file using a
        #JSONCaseRecorder, converts that to CSV, then runs the model again using the same cases,
        #pulled out of the CSV file by a CSVCaseIterator.  Finally the cases
        #are dumped to a string after being run for the second time.

        self.top.recorders = [JSONCaseRecorder(self.filename_json)]
        self.top.recorders[0].num_backups = 0
        self.top.run()

        cds = CaseDataset(self.filename_json, 'json')

        q = cds.data # is Query object

        caseset_query_to_csv( q )

        print cds.data.var_names().fetch()



    def qqq_test_inoutCSV(self):

        #This test runs some cases, puts them in a CSV file using a
        #CSVCaseRecorder, then runs the model again using the same cases,
        #pulled out of the CSV file by a CSVCaseIterator.  Finally the cases
        #are dumped to a string after being run for the second time.

        self.top.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.recorders[0].num_backups = 0
        self.top.run()

        # now use the CSV recorder as source of Cases
        cases = [case for case in self.top.recorders[0].get_iterator()]
        Case.set_vartree_inputs(self.top.driver, cases)

        sout = StringIO.StringIO()
        self.top.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        expected = [
            'Case:',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   timestamp: 1383239074.309192',
            '   inputs:',
            '      comp1.b_bool: True',
            '      comp1.x: 8.1',
            '      comp1.x_array[1]: 99.88',
            '      comp1.y: 16.1',
            '   outputs:',
            '      Response(comp1.a_array[2]): 5.5',
            "      Response(comp1.a_string): Hello',;','",
            '      Response(comp1.z): 24.2',
            '      Response(comp2.z): 25.2',
            ]
#        print sout.getvalue()
        lines = sout.getvalue().split('\n')
        count = 0
        for index, line in enumerate(lines):
            if line.startswith('Case:'):
                count += 1
                if count != 9:
                    continue
                for i in range(len(expected)):
                    if expected[i].startswith('   uuid:'):
                        self.assertTrue(lines[index+i].startswith('   uuid:'))
                    elif expected[i].startswith('   timestamp:'):
                        self.assertTrue(lines[index+i].startswith('   timestamp:'))
                    else:
                        self.assertEqual(lines[index+i], expected[i])
                break
        else:
            self.fail("couldn't find the expected Case")



class TestCase(unittest.TestCase):

    def setUp(self):
        path = os.path.join(os.path.dirname(__file__), 'sellar.json')
        self.cds = CaseDataset(path, 'json')

    def tearDown(self):
        self.cds = None

    def qqqtest_csv_post_processor(self):
        caseset_query_to_csv( self.cds.data )

if __name__ == '__main__':
    unittest.main()

