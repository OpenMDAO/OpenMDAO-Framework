"""
Test for CSVCaseRecorder and CSVCaseIterator.
"""
import glob, os, time
import StringIO
import unittest


from openmdao.lib.casehandlers.api import CSVCaseIterator, CSVCaseRecorder, \
                                          ListCaseIterator, ListCaseRecorder, \
                                          DumpCaseRecorder
from openmdao.lib.datatypes.api import Array, Str, Bool, VarTree
from openmdao.lib.drivers.api import SimpleCaseIterDriver, CaseIteratorDriver
from openmdao.main.api import Component, Assembly, Case, set_as_top
from openmdao.main.numpy_fallback import array
from openmdao.test.execcomp import ExecComp
from openmdao.util.testutil import assert_raises
from openmdao.main.test.test_vartree import DumbVT
    

class CSVCaseRecorderTestCase(unittest.TestCase):

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
            cases.append(Case(inputs=inputs, outputs=outputs, label='case%s'%i))
        driver.iterator = ListCaseIterator(cases)
        
        self.filename = "openmdao_test_csv_case_iterator.csv"
        
    def tearDown(self):
        for recorder in self.top.driver.recorders:
            recorder.close()
        if os.path.exists(self.filename):
            os.remove(self.filename)

    def test_inoutCSV(self):
        
        #This test runs some cases, puts them in a CSV file using a CSVCaseRecorder,
        #then runs the model again using the same cases, pulled out of the CSV file
        #by a CSVCaseIterator.  Finally the cases are dumped to a string after
        #being run for the second time.
        
        self.top.driver.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.driver.recorders[0].num_backups = 0
        self.top.run()
        
        # now use the CSV recorder as source of Cases
        self.top.driver.iterator = self.top.driver.recorders[0].get_iterator()
        
        sout = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        expected = [
            'Case: case8',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   inputs:',
            '      comp1.b_bool: True',
            '      comp1.x: 8.1',
            '      comp1.x_array[1]: 99.88',
            '      comp1.y: 16.1',
            '   outputs:',
            #"      comp1.a_list: [1, 'one', 1.0]",
            "      comp1.a_array[2]: 5.5",
            "      comp1.a_string: Hello',;','",
            '      comp1.z: 24.2',
            '      comp2.z: 25.2',
            ]
        lines = sout.getvalue().split('\n')
        for index, line in enumerate(lines):
            if line.startswith('Case: case8'):
                for i in range(len(expected)):
                    if expected[i].startswith('   uuid:'):
                        self.assertTrue(lines[index+i].startswith('   uuid:'))
                    else:
                        self.assertEqual(lines[index+i], expected[i])
                break
        else:
            self.fail("couldn't find the expected Case")
            
    def test_inoutCSV_delimiter(self):
        
        #Repeat test above using semicolon delimiter and ' as quote char.
        
        self.top.driver.recorders = [CSVCaseRecorder(filename=self.filename, delimiter=';', \
                                                     quotechar="'")]
        self.top.driver.recorders[0].num_backups = 0
        self.top.run()

        attrs = self.top.driver.recorders[0].get_attributes()
        self.assertTrue("Inputs" in attrs.keys())
        self.assertTrue({'name': 'filename',
                         'id': 'filename',
                         'type': 'str',
                         'connected': '',
                         'value': 'openmdao_test_csv_case_iterator.csv',
                         'desc': 'Name of the CSV file to be output.'} in attrs['Inputs'])
        self.assertTrue({'name': 'append',
                         'id': 'append',
                         'type': 'bool',
                         'connected': '',
                         'value': 'False',
                         'desc': 'Set to True to append to the existing CSV file.'} in attrs['Inputs'])
        self.assertTrue({'name': 'delimiter',
                         'id': 'delimiter',
                         'type': 'str',
                         'connected': '',
                         'value': ';',
                         'desc': 'CSV delimiter. Default is ",".'} in attrs['Inputs'])
        

        # now use the DB as source of Cases
        self.top.driver.iterator = self.top.driver.recorders[0].get_iterator()
        
        sout = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        expected = [
            'Case: case8',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   inputs:',
            '      comp1.b_bool: True',
            '      comp1.x: 8.1',
            '      comp1.x_array[1]: 99.88',
            '      comp1.y: 16.1',
            '   outputs:',
            "      comp1.a_array[2]: 5.5",
            "      comp1.a_string: Hello',;','",
            '      comp1.z: 24.2',
            '      comp2.z: 25.2',
            ]
        lines = sout.getvalue().split('\n')
        for index, line in enumerate(lines):
            if line.startswith('Case: case8'):
                for i in range(len(expected)):
                    if expected[i].startswith('   uuid:'):
                        self.assertTrue(lines[index+i].startswith('   uuid:'))
                    else:
                        self.assertEqual(lines[index+i], expected[i])
                break
        else:
            self.fail("couldn't find the expected Case")
            
            
    def test_CSVCaseIterator_read_external_file_with_header(self):
        
        # Without a label column
        
        csv_data = ['"comp1.x", "comp1.y", "comp2.b_string"\n',
                    '33.5, 76.2, "Hello There"\n'
                    '3.14159, 0, "Goodbye z"\n'
                    ]
        
        outfile = open(self.filename, 'w')
        outfile.writelines(csv_data)
        outfile.close()
        
        self.top.comp2.add('b_string', Str("Hello',;','", iotype='in'))
        
        
        sout = StringIO.StringIO()
        self.top.driver.iterator = CSVCaseIterator(filename=self.filename)
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        
        self.assertEqual(self.top.comp1.x, 3.14159)
        self.assertEqual(self.top.comp1.y, 0.0)
        self.assertEqual(self.top.comp2.b_string, "Goodbye z")
        
        # Gui pane stuff
        
        attrs = self.top.driver.iterator.get_attributes()
        self.assertTrue("Inputs" in attrs.keys())
        self.assertTrue({'name': 'filename',
                         'type': 'str',
                         'connected': '',
                         'value': 'openmdao_test_csv_case_iterator.csv',
                         'desc': 'Name of the CSV file to be iterated.'} in attrs['Inputs'])
        self.assertTrue({'name': 'headers',
                         'type': 'NoneType',
                         'connected': '',
                         'value': 'None',
                         'desc': 'Optional dictionary of header labels, where the key is the column number.'} in attrs['Inputs'])
        
        # With a label column
        
        csv_data = ['"label", "comp1.x", "comp1.y", "comp2.b_string"\n',
                    '"case1", 33.5, 76.2, "Hello There"\n'
                    ]
        
        outfile = open(self.filename, 'w')
        outfile.writelines(csv_data)
        outfile.close()
        
        self.top.driver.iterator = CSVCaseIterator(filename=self.filename)
        self.top.driver.recorders = [ListCaseRecorder()]
        self.top.run()
        
        it = self.top.driver.recorders[0].get_iterator()
        case1 = it.pop()
        self.assertEqual(case1.label, 'case1')
        
    def test_CSVCaseIterator_read_external_file_without_header(self):
        
        # Without a label column
        
        csv_data = ['33.5, 76.2, "Hello There"\n'
                    '3.14159, 0, "Goodbye z"\n'
                    ]
        
        outfile = open(self.filename, 'w')
        outfile.writelines(csv_data)
        outfile.close()
        
        header_dict = { 0 : "comp1.x",
                        1 : "comp1.y",
                        2 : "comp2.b_string",
                        }
        
        self.top.comp2.add('b_string', Str("Hello',;','", iotype='in'))
        
        
        sout = StringIO.StringIO()
        self.top.driver.iterator = CSVCaseIterator(filename=self.filename, \
                                                   headers=header_dict)
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        
        self.assertEqual(self.top.comp1.x, 3.14159)
        self.assertEqual(self.top.comp1.y, 0.0)
        self.assertEqual(self.top.comp2.b_string, "Goodbye z")
        
        # With a label column
        
        csv_data = ['"case1", 33.5, 76.2, "Hello There"\n'
                    ]
        
        header_dict = { 0 : "label",
                        1 : "comp1.x",
                        2 : "comp1.y",
                        3 : "comp2.b_string",
                        }
        
        outfile = open(self.filename, 'w')
        outfile.writelines(csv_data)
        outfile.close()
        
        self.top.driver.iterator = CSVCaseIterator(filename=self.filename, \
                                                   headers=header_dict)
        self.top.driver.recorders = [ListCaseRecorder()]
        self.top.run()
        
        it = self.top.driver.recorders[0].get_iterator()
        case1 = it.pop()
        self.assertEqual(case1.label, 'case1')
        
        
    def test_inoutCSV_empty_inputs(self):
        
        # now create some Cases
        outputs = ['comp1.z']
        cases = []
        for i in range(10):
            cases.append(Case(inputs=[], outputs=outputs, label='case%s'%i))
        self.top.driver.iterator = ListCaseIterator(cases)
            
        self.top.driver.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.driver.recorders[0].num_backups = 0
        self.top.run()

        # now use the CSV recorder as source of Cases
        self.top.driver.iterator = self.top.driver.recorders[0].get_iterator()
        
        sout = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        expected = [
            'Case: case8',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   outputs:',
            '      comp1.z: 0.0',
            ]
        lines = sout.getvalue().split('\n')
        for index, line in enumerate(lines):
            if line.startswith('Case: case8'):
                for i in range(len(expected)):
                    if expected[i].startswith('   uuid:'):
                        self.assertTrue(lines[index+i].startswith('   uuid:'))
                    else:
                        self.assertEqual(lines[index+i], expected[i])
                break
        else:
            self.fail("couldn't find the expected Case")

    def test_sorting(self):
        # Make sure outputs are sorted
            
        rec = CSVCaseRecorder(filename=self.filename)
        rec.num_backups = 0
        rec.startup()
        rec.record(Case(inputs=[('comp1.x',2.0),('comp1.y',4.3),('comp2.x',1.9)]))
        rec.close()
        
        outfile = open(self.filename, 'r')
        csv_data = outfile.readlines()
        outfile.close()

        line = '"label","/INPUTS","comp1.x","comp1.y","comp2.x","/OUTPUTS","/METADATA","retries","max_retries","parent_uuid","msg"\r\n'
        self.assertEqual(csv_data[0], line)
        line = '"","",2.0,4.3,1.9,"","","","","",""\r\n'
        self.assertEqual(csv_data[1], line)

    def test_flatten(self):
        # create some Cases
        outputs = ['comp1.a_array', 'comp1.vt']
        inputs = [('comp1.x_array', array([2.0, 2.0, 2.0]))]
        self.top.driver.iterator = ListCaseIterator([Case(inputs=inputs, outputs=outputs, label='case1')])
        self.top.driver.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.driver.recorders[0].num_backups = 0
        self.top.run()
        
        # now use the CSV recorder as source of Cases
        self.top.driver.iterator = self.top.driver.recorders[0].get_iterator()
        
        sout = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.run()
        expected = [
            'Case: case1',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   inputs:',
            '      comp1.x_array[0]: 2.0',
            '      comp1.x_array[1]: 2.0',
            '      comp1.x_array[2]: 2.0',
            '   outputs:',
            "      comp1.a_array[0]: 1.0",
            "      comp1.a_array[1]: 3.0",
            "      comp1.a_array[2]: 5.5",
            "      comp1.vt.v1: 1.0",
            "      comp1.vt.v2: 2.0",
            "      comp1.vt.vt2.vt3.a: 1.0",
            "      comp1.vt.vt2.vt3.b: 12.0",
            "      comp1.vt.vt2.x: -1.0",
            "      comp1.vt.vt2.y: -2.0",
            ]
        lines = sout.getvalue().split('\n')
        for index, line in enumerate(lines):
            if line.startswith('Case: case1'):
                for i in range(len(expected)):
                    if expected[i].startswith('   uuid:'):
                        self.assertTrue(lines[index+i].startswith('   uuid:'))
                    else:
                        self.assertEqual(lines[index+i], expected[i])
                break
        else:
            self.fail("couldn't find the expected Case")
        

    def test_CSVCaseRecorder_messages(self):
        rec = CSVCaseRecorder(filename=self.filename)
        rec.startup()
        rec.record(Case(inputs=[('comp1.x',2.0),('comp1.y',4.3),('comp2.x',1.9)]))
        try:
            rec.record(Case(inputs=[('comp1.x',2.0),('comp2.x',1.9)]))
        except Exception as err:
            self.assertEqual(str(err), "number of data points doesn't match header size in CSV recorder")
        else:
            self.fail("Exception expected")
        finally:
            rec.close()
        
        ## BAN - took this test out because only types with a flattener function
        ##       will be returned by the Case, so incompatible types just won't
        ##       be seen by the CSVCaseRecorder at all.  Need to discuss with
        ##       users (and Ken) to see if this is reasonable.
        #self.top.comp2.add('a_slot', Slot(object, iotype='in'))
        #self.top.driver.recorders = [CSVCaseRecorder(filename=self.filename)]

        #case = Case(inputs=[('comp2.a_slot', None)])
        #try:
            #self.top.driver.recorders[0].record(case)
        #except ValueError, err:
            #msg = "CSV format does not support variables of type <type 'NoneType'>"
            #self.top.driver.recorders[0].close()
            #self.assertEqual(msg, str(err))
        #else:
            #self.top.driver.recorders[0].close()
            #self.fail('ValueError Expected')
        
    def test_close(self):
        self.top.driver.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.driver.recorders[0].num_backups = 0
        self.top.run()
        case = Case(inputs=[('comp2.a_slot', None)])
        assert_raises(self, 'self.top.driver.recorders[0].record(case)',
                      globals(), locals(), RuntimeError,
                      'Attempt to record on closed recorder')
        
    def test_csvbackup(self):
        
        # Cleanup from any past failures
        parts = self.filename.split('.')
        backups = glob.glob(''.join(parts[:-1]) + '_*')
        for item in backups:
            os.remove(item)
        
        self.top.driver.recorders = [CSVCaseRecorder(filename=self.filename)]

        # Run twice, two backups.
        self.top.driver.recorders[0].num_backups = 2
        self.top.run()
        # Granularity on timestamp is 1 second.
        time.sleep(1)
        self.top.run()
        backups = glob.glob(''.join(parts[:-1]) + '_*')
        self.assertEqual(len(backups), 2)

        # Set backups to 1 and rerun. Should delete down to 1 backup.
        self.top.driver.recorders[0].num_backups = 1
        self.top.run()
        backups = glob.glob(''.join(parts[:-1]) + '_*')
        self.assertEqual(len(backups), 1)
        
        for item in backups:
            os.remove(item)
        backups = glob.glob(''.join(parts[:-1]) + '_*')
        
        self.top.driver.recorders[0].num_backups = 0

    def test_iterate_twice(self):

        self.top.driver.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.run()
        
        data = self.top.driver.recorders[0].get_iterator()
        
        for case in data:
            keys1 = case.keys()
        
        for case in data:
            keys2 = case.keys()
        
        self.assertEqual(keys1, keys2)
        
if __name__ == '__main__':
    unittest.main()
