"""
Test for CSVCaseRecorder and CSVCaseIterator.
"""
import glob, os, time
import StringIO
import unittest

from numpy import array

from openmdao.lib.casehandlers.api import CSVCaseIterator, CSVCaseRecorder, \
                                          DumpCaseRecorder
from openmdao.main.datatypes.api import Array, Str, Bool, VarTree
from openmdao.lib.drivers.api import SimpleCaseIterDriver
from openmdao.main.api import Assembly, Case, set_as_top
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
            cases.append(Case(inputs=inputs, outputs=outputs))

        Case.set_vartree_inputs(driver, cases)
        driver.add_responses(sorted(outputs))

        self.filename = "openmdao_test_csv_case_iterator.csv"

    def tearDown(self):
        for recorder in self.top.recorders:
            recorder.close()
        if os.path.exists(self.filename):
            os.remove(self.filename)

    def test_inoutCSV(self):

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
            "      _pseudo_0: 5.5",
            "      _pseudo_1: Hello',;','",
            '      _pseudo_2: 24.2',
            '      _pseudo_3: 25.2',
            ]
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

    def test_inoutCSV_delimiter(self):

        #Repeat test above using semicolon delimiter and ' as quote char.

        self.top.recorders = [CSVCaseRecorder(filename=self.filename,
                                              delimiter=';', quotechar="'")]
        self.top.recorders[0].num_backups = 0
        self.top.run()

        attrs = self.top.recorders[0].get_attributes()
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
        self.top.driver.iterator = self.top.recorders[0].get_iterator()

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
            "      _pseudo_0: 5.5",
            "      _pseudo_1: Hello',;','",
            '      _pseudo_2: 24.2',
            '      _pseudo_3: 25.2',
            ]
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


    def test_CSVCaseIterator_read_external_file_with_header(self):

        csv_data = ['"comp1.x", "comp1.y", "comp2.b_string"\n',
                    '33.5, 76.2, "Hello There"\n'
                    '3.14159, 0, "Goodbye z"\n'
                    ]

        outfile = open(self.filename, 'w')
        outfile.writelines(csv_data)
        outfile.close()

        self.top.comp2.add('b_string', Str("Hello',;','", iotype='in'))


        sout = StringIO.StringIO()
        cases = [case for case in CSVCaseIterator(filename=self.filename)]
        self.top.driver.clear_parameters()
        Case.set_vartree_inputs(self.top.driver, cases)
        self.top.recorders = [DumpCaseRecorder(sout)]
        self.top.run()

        self.assertEqual(self.top.comp1.x, 3.14159)
        self.assertEqual(self.top.comp1.y, 0.0)
        self.assertEqual(self.top.comp2.b_string, "Goodbye z")

        # Gui pane stuff

        iterator = CSVCaseIterator(filename=self.filename)
        attrs = iterator.get_attributes()
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

    def test_CSVCaseIterator_read_external_file_without_header(self):

        csv_data = ['33.5, 76.2, "Hello There"\n'
                    '3.14159, 0, "Goodbye z"\n'
                    ]

        outfile = open(self.filename, 'w')
        outfile.writelines(csv_data)
        outfile.close()

        header_dict = {0 : "comp1.x",
                       1 : "comp1.y",
                       2 : "comp2.b_string"}

        self.top.comp2.add('b_string', Str("Hello',;','", iotype='in'))


        sout = StringIO.StringIO()
        cases = [case for case in CSVCaseIterator(filename=self.filename,
                                                  headers=header_dict)]
        self.top.driver.clear_parameters()
        Case.set_vartree_inputs(self.top.driver, cases)
        self.top.recorders = [DumpCaseRecorder(sout)]
        self.top.run()

        self.assertEqual(self.top.comp1.x, 3.14159)
        self.assertEqual(self.top.comp1.y, 0.0)
        self.assertEqual(self.top.comp2.b_string, "Goodbye z")

    def test_inoutCSV_empty_inputs(self):
        from nose import SkipTest
        raise SkipTest("New case drivers don't execute without inputs.")

        # now create some Cases
        outputs = ['comp1.z']
        cases = []
        for i in range(10):
            cases.append(Case(inputs=[], outputs=outputs))
        self.top.driver.clear_parameters()
        Case.set_vartree_inputs(self.top.driver, cases)
        self.top.driver.clear_responses()
        self.top.driver.add_responses(outputs)

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
            '   timestamp: 1383239019.152071',
            '   outputs:',
            '      Response_0: 0.0',
            '      driver.workflow.itername: 9',
            ]
        lines = sout.getvalue().split('\n')
        count = 0
        for index, line in enumerate(lines):
            if line.startswith('Case: '):
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

    def test_sorting(self):
        # Make sure outputs are sorted

        rec = CSVCaseRecorder(filename=self.filename)
        rec.num_backups = 0
        rec.startup()
        rec.register(self, ['comp1.x', 'comp1.y', 'comp2.x', ], [])
        rec.record(self, [2.0, 4.3, 1.9], [], None, '', '')
        rec.close()

        outfile = open(self.filename, 'r')
        csv_data = outfile.readlines()
        outfile.close()

        line = '"timestamp","/INPUTS","comp1.x","comp1.y","comp2.x",' \
               '"/OUTPUTS","/METADATA","uuid","parent_uuid","msg"\r\n'
        self.assertEqual(csv_data[0], line)
        line = '"",2.0,4.3,1.9,"","","","",""\r\n'
        self.assertTrue(csv_data[1].endswith(line))

    def test_flatten(self):
        # create some Cases

        outputs = ['comp1.a_array', 'comp1.vt']
        inputs = [('comp1.x_array', array([2.0, 2.0, 2.0]))]
        cases = [Case(inputs=inputs, outputs=outputs)]
        self.top.driver.clear_parameters()
        Case.set_vartree_inputs(self.top.driver, cases)
        self.top.driver.clear_responses()
        self.top.driver.add_responses(outputs)
        self.top.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.recorders[0].num_backups = 0
        self.top.run()

        # check recorded cases
        cases = [case for case in self.top.recorders[0].get_iterator()]
        sout = StringIO.StringIO()
        for case in cases:
            print >>sout, case
        expected = [
            'Case:',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   timestamp: 1383238593.781986',
            '   inputs:',
            '      comp1.x_array[0]: 2.0',
            '      comp1.x_array[1]: 2.0',
            '      comp1.x_array[2]: 2.0',
            '   outputs:',
            '      _pseudo_4[0]: 1.0',
            '      _pseudo_4[1]: 3.0',
            '      _pseudo_4[2]: 5.5',
            '      _pseudo_5.data: ',
            '      _pseudo_5.v1: 1.0',
            '      _pseudo_5.v2: 2.0',
            '      _pseudo_5.vt2.data: ',
            '      _pseudo_5.vt2.vt3.a: 1.0',
            '      _pseudo_5.vt2.vt3.b: 12.0',
            '      _pseudo_5.vt2.vt3.data: ',
            '      _pseudo_5.vt2.x: -1.0',
            '      _pseudo_5.vt2.y: -2.0',
            '      comp1.a_array[0]: 1.0',
            '      comp1.a_array[1]: 3.0',
            '      comp1.a_array[2]: 5.5',
            '      comp1.vt.data: ',
            '      comp1.vt.v1: 1.0',
            '      comp1.vt.v2: 2.0',
            '      comp1.vt.vt2.data: ',
            '      comp1.vt.vt2.vt3.a: 1.0',
            '      comp1.vt.vt2.vt3.b: 12.0',
            '      comp1.vt.vt2.vt3.data: ',
            '      comp1.vt.vt2.x: -1.0',
            '      comp1.vt.vt2.y: -2.0',
            '      driver.workflow.itername: 1',
            ]

        lines = sout.getvalue().split('\n')
        for index, line in enumerate(lines):
            if line.startswith('Case:'):
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

        # now use the CSV recorder as source of Cases
        self.top.driver.clear_parameters()
        Case.set_vartree_inputs(self.top.driver, cases)
        sout = StringIO.StringIO()
        self.top.recorders = [DumpCaseRecorder(sout)]  # Dump not flattened.
        self.top.run()
        expected = [
            'Case:',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   timestamp: 1383238593.781986',
            '   inputs:',
            '      comp1.x_array[0]: 2.0',
            '      comp1.x_array[1]: 2.0',
            '      comp1.x_array[2]: 2.0',
            '   outputs:',
            '      _pseudo_4: [ 1.   3.   5.5]',
            '      _pseudo_5: <openmdao.main.test.test_vartree.DumbVT object',
            '      driver.workflow.itername: 1',
            ]
        lines = sout.getvalue().split('\n')
        for index, line in enumerate(lines):
            if line.startswith('Case:'):
                for i in range(len(expected)):
                    if expected[i].startswith('   uuid:'):
                        self.assertTrue(lines[index+i].startswith('   uuid:'))
                    elif expected[i].startswith('   timestamp:'):
                        self.assertTrue(lines[index+i].startswith('   timestamp:'))
                    elif expected[i].startswith('      _pseudo_5:'):
                        self.assertTrue(lines[index+i].startswith(expected[i]))
                    else:
                        self.assertEqual(lines[index+i], expected[i])
                break
        else:
            self.fail("couldn't find the expected Case")

    def test_CSVCaseRecorder_messages(self):
        rec = CSVCaseRecorder(filename=self.filename)
        rec.startup()
        rec.register(self, ['comp1.x', 'comp1.y', 'comp2.x'], [])
        rec.record(self, [2.0, 4.3, 1.9], [], None, '', '')
        try:
            rec.record(self, [2.0, 1.9], [], None, '', '')
        except Exception as err:
            self.assertEqual(str(err),
                             "number of data points (9) doesn't match"
                             " header size (10) in CSV recorder")
        else:
            self.fail("Exception expected")
        finally:
            rec.close()

        ## BAN - took this test out because only types with a flattener function
        ##       will be returned by the Case, so incompatible types just won't
        ##       be seen by the CSVCaseRecorder at all.  Need to discuss with
        ##       users (and Ken) to see if this is reasonable.
        #self.top.comp2.add('a_slot', Slot(object, iotype='in'))
        #self.top.recorders = [CSVCaseRecorder(filename=self.filename)]

        #case = Case(inputs=[('comp2.a_slot', None)])
        #try:
            #self.top.recorders[0].record(case)
        #except ValueError, err:
            #msg = "CSV format does not support variables of type <type 'NoneType'>"
            #self.top.recorders[0].close()
            #self.assertEqual(msg, str(err))
        #else:
            #self.top.recorders[0].close()
            #self.fail('ValueError Expected')

    def test_close(self):
        self.top.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.recorders[0].num_backups = 0
        self.top.run()
        outputs = ['comp1.z', 'comp2.z', 'comp1.a_string',
                   'comp1.a_array[2]', 'driver.workflow.itername']
        inputs = ['comp1.x', 'comp1.y', 'comp1.x_array[1]', 'comp1.b_bool']
        self.top.recorders[0].register(self, inputs, outputs)
        outputs = [0, 0, 'world', 0, '1']
        inputs = [0.1, 2 + .1, 99.88, True]
        code = "self.top.recorders[0].record(self, inputs, outputs, None, '', '')"
        assert_raises(self, code, globals(), locals(), RuntimeError,
                      'Attempt to record on closed recorder')

    def test_csvbackup(self):

        # Cleanup from any past failures
        parts = self.filename.split('.')
        backups = glob.glob(''.join(parts[:-1]) + '_*')
        for item in backups:
            os.remove(item)

        self.top.recorders = [CSVCaseRecorder(filename=self.filename)]

        # Run twice, two backups.
        self.top.recorders[0].num_backups = 2
        self.top.run()
        # Granularity on timestamp is 1 second.
        time.sleep(1)
        self.top.run()
        backups = glob.glob(''.join(parts[:-1]) + '_*')
        self.assertEqual(len(backups), 2)

        # Set backups to 1 and rerun. Should delete down to 1 backup.
        self.top.recorders[0].num_backups = 1
        self.top.run()
        backups = glob.glob(''.join(parts[:-1]) + '_*')
        self.assertEqual(len(backups), 1)

        for item in backups:
            os.remove(item)
        backups = glob.glob(''.join(parts[:-1]) + '_*')

        self.top.recorders[0].num_backups = 0

    def test_iterate_twice(self):

        self.top.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.recorders[0].num_backups = 0
        self.top.run()

        data = self.top.recorders[0].get_iterator()

        for case in data:
            keys1 = case.keys()

        for case in data:
            keys2 = case.keys()

        self.assertEqual(keys1, keys2)

    def test_save(self):
        # Check that a recorder can be saved to an egg.
        self.top.recorders = [CSVCaseRecorder(filename=self.filename)]
        self.top.recorders[0].num_backups = 0
        self.top.recorders[0].startup()
        try:
            self.top.save_to_egg('top', '1')
        finally:
            for name in glob.glob('top*.egg'):
                os.remove(name)


if __name__ == '__main__':
    unittest.main()
