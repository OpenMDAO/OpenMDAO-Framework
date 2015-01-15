import os.path
import unittest
import StringIO
import tempfile
import shutil

from numpy import array, isnan

from openmdao.main.api import Assembly, Case, set_as_top
from openmdao.main.datatypes.api import Str, Bool, Array, VarTree
from openmdao.main.test.test_vartree import DumbVT

from openmdao.lib.casehandlers.api import JSONCaseRecorder
from openmdao.lib.casehandlers.api import CSVCaseIterator, CaseDataset, caseset_query_to_csv

from openmdao.lib.drivers.api import SimpleCaseIterDriver

from openmdao.test.execcomp import ExecComp


class CSVPostProcessorTestCase(unittest.TestCase):

    def setUp(self):
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_csv-')
        os.chdir(self.tempdir)

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
            i = float(i)
            inputs = [('comp1.x', i+0.1), ('comp1.y', i*2 + .1),
                      ('comp1.x_array[1]', 99.88), ('comp1.b_bool', True)]
            cases.append(Case(inputs=inputs, outputs=outputs))

        Case.set_vartree_inputs(driver, cases)
        driver.add_responses(sorted(outputs))

        self.filename_json = "openmdao_test_csv_case_iterator.json"
        self.filename_csv = "openmdao_test_csv_case_iterator.csv"

    def tearDown(self):
        for recorder in self.top.recorders:
            recorder.close()
        os.chdir(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass

    def test_simple(self):
        # Make sure the CSV file can be read and has the correct number of cases

        self.top.recorders = [JSONCaseRecorder(self.filename_json)]
        self.top.recorders[0].num_backups = 0
        self.top.run()

        cds = CaseDataset(self.filename_json, 'json')
        data = cds.data.fetch()  # results
        caseset_query_to_csv( data, self.filename_csv)

        cases = [case for case in CSVCaseIterator(filename=self.filename_csv)]
        self.assertEqual(len(cases), 10)

    def test_flatten(self):
        # try it after creating some Cases
        # more rigorous checking of the csv

        outputs = ['comp1.a_array', 'comp1.vt']
        inputs = [('comp1.x_array', array([2.0, 2.0, 2.0]))]
        cases = [Case(inputs=inputs, outputs=outputs)]
        self.top.driver.clear_parameters()
        Case.set_vartree_inputs(self.top.driver, cases)
        self.top.driver.clear_responses()
        self.top.driver.add_responses(outputs)
        self.top.recorders = [JSONCaseRecorder(self.filename_json)]
        self.top.recorders[0].num_backups = 0
        self.top.run()

        cds = CaseDataset(self.filename_json, 'json')
        data = cds.data.fetch()  # results
        caseset_query_to_csv( data, self.filename_csv)

        # check recorded cases
        cases = [case for case in CSVCaseIterator(filename=self.filename_csv)]
        sout = StringIO.StringIO()
        for case in cases:
            print >>sout, case

        expected = \
'''Case:
   uuid: 07280785-9b76-11e4-800d-20c9d0478eff
   timestamp: 1421189195.646824
   parent_uuid: 0720c385-9b76-11e4-b796-20c9d0478eff
   inputs:
      comp1.x_array[0]: 2.0
      comp1.x_array[1]: 2.0
      comp1.x_array[2]: 2.0
   outputs:
      _pseudo_4.out0[0]: 1.0
      _pseudo_4.out0[1]: 3.0
      _pseudo_4.out0[2]: 5.5
      _pseudo_5.out0.data: 
      _pseudo_5.out0.v1: 1.0
      _pseudo_5.out0.v2: 2.0
      _pseudo_5.out0.vt2.data: 
      _pseudo_5.out0.vt2.vt3.a: 1.0
      _pseudo_5.out0.vt2.vt3.b: 12.0
      _pseudo_5.out0.vt2.vt3.data: 
      _pseudo_5.out0.vt2.x: -1.0
      _pseudo_5.out0.vt2.y: -2.0
      comp1.a_array[0]: 1.0
      comp1.a_array[1]: 3.0
      comp1.a_array[2]: 5.5
      comp1.a_string: Hello',;','
      comp1.derivative_exec_count: 0.0
      comp1.exec_count: 1.0
      comp1.itername: 1-comp1
      comp1.vt.data: 
      comp1.vt.v1: 1.0
      comp1.vt.v2: 2.0
      comp1.vt.vt2.data: 
      comp1.vt.vt2.vt3.a: 1.0
      comp1.vt.vt2.vt3.b: 12.0
      comp1.vt.vt2.vt3.data: 
      comp1.vt.vt2.x: -1.0
      comp1.vt.vt2.y: -2.0
      comp1.z: 0.0
      comp2.derivative_exec_count: 0.0
      comp2.exec_count: 1.0
      comp2.itername: 1-comp2
      comp2.z: 1.0
      driver.workflow.itername: 1

'''

        print sout.getvalue()
        lines = sout.getvalue().split('\n')
        expected_lines = expected.split('\n')
        for index, line in enumerate(lines):
            if line.startswith('Case:'):
                for i in range(len(expected_lines)):
                    if expected_lines[i].startswith('   uuid:'):
                        self.assertTrue(lines[index+i].startswith('   uuid:'))
                    elif expected_lines[i].startswith('   parent_uuid:'):
                        self.assertTrue(lines[index+i].startswith('   parent_uuid:'))
                    elif expected_lines[i].startswith('   timestamp:'):
                        self.assertTrue(lines[index+i].startswith('   timestamp:'))
                    else:
                        self.assertEqual(lines[index+i], expected_lines[i])
                break
        else:
            self.fail("couldn't find the expected Case")

    def test_nested(self):
        # Direct comparison of a csv file to a reference. Use the nested case
        # from the JSON file test.

        self.generate_and_compare('nested')

    def test_multiobj(self):
        # Direct comparison of a csv file to a reference. Use the multiobj case
        # from the JSON file test.

        self.generate_and_compare('multiobj')

    def generate_and_compare(self, name):


        directory = os.path.abspath(os.path.dirname(__file__))
        name = os.path.join(directory, name)

        cds = CaseDataset(name + '.json', 'json')
        data = cds.data.fetch()
        caseset_query_to_csv(data, self.filename_csv)

        with open(name + '.csv', 'r') as inp1:
            expected = inp1.readlines()
        with open(self.filename_csv, 'r') as inp2:
            actual = inp2.readlines()

        # Strip off trailing whitespace (newlines and carriage returns)
        # Don't check time-stamp because some OS round it.
        for exp, act in zip(expected, actual):
            # skip timestamps, and uuids
            items2 = act.rstrip().split(",")[1:-3]
            for i, item1 in enumerate(exp.rstrip().split(",")[1:-3]):
                item2 = items2[i]
                try:  # (str).isnumeric() only works on unicode
                    item1, item2 = float(item1), float(item2)
                    # nan equality check fails by definition
                    if isnan(item1) and isnan(item2):
                        continue
                    self.assertAlmostEqual(item1, item2)
                except (ValueError, TypeError):
                    self.assertEqual(item1, item2)


if __name__ == '__main__':
    unittest.main()
