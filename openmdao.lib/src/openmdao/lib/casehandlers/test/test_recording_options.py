"""
Test for CaseRecorders.
"""

import unittest
import StringIO

from openmdao.main.api import Assembly, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import JSONCaseRecorder, CaseDataset
from openmdao.lib.drivers.sensitivity import SensitivityDriver


class RecordingOptionsTestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())

        self.top.add('comp1', ExecComp(exprs=['z=x+y']))
        self.top.add('comp2', ExecComp(exprs=['z=x+1']))
        self.top.connect('comp1.z', 'comp2.x')

        self.top.add('driver', SensitivityDriver())
        self.top.driver.workflow.add(['comp1', 'comp2'])
        self.top.driver.add_parameter(['comp1.x'], low=-100, high=100)
        self.top.driver.add_objective('comp1.z')
        self.top.driver.add_objective('comp2.z')

    def test_no_recorder(self):
        # verify recording options are ignored if there are no recorders
        #    (i.e. the Assembly runs without errors)
        
        self.top.recorders = []
        self.top.run()
        self.assertEqual(self.top.comp1.z, 0.0)
        self.assertEqual(self.top.comp2.z, 1.0)

    def test_default_options(self):
        # verify default options:
        #        save_problem_formulation = True
        #        includes = ['*']
        #        excludes = []
        
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.run()

        sout.seek(0) # need to go back to the front of the "file"
        cds = CaseDataset(sout, 'json')
        vnames = cds.data.var_names().fetch()
        expected = ['_driver_id', '_id', '_parent_id',  
				u'_pseudo_0.out0',  u'_pseudo_1.out0',
				u'comp1.derivative_exec_count', u'comp1.exec_count', 
				u'comp1.itername', u'comp1.x', u'comp1.z', 
				u'comp2.derivative_exec_count', u'comp2.exec_count', 
				u'comp2.itername', u'comp2.z', u'driver.workflow.itername', 
				'error_message', 'error_status', 'timestamp']


        self.assertFalse(set(vnames).symmetric_difference(set(expected)))
        
        # Specific variables.
        names = [ 'comp1.x', 'comp2.z', 'comp1.z']
        vnames = cds.data.vars(names).var_names().fetch()
        self.assertEqual(vnames, names)

        cases = cds.data.vars(names).fetch()
        self.assertEqual(len(cases), 1)
        self.assertEqual(len(cases[0]), len(names))

        iteration_case_1 = {
            "comp1.x": 0.0,
            "comp1.z": 0.0,
            "comp2.z": 1.0,
        }
        for name, val in zip(names, cases[0]):
            self.assertAlmostEqual(val, iteration_case_1[name])

    def test_problem_formulation_only(self):
        # verify options with no includes:
        #        save_problem_formulation = True
        #        includes = []
        #        excludes = []
        
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recording_options.save_problem_formulation = True
        self.top.recording_options.includes = []
        self.top.run()

        sout.seek(0) # need to go back to the front of the "file"
        cds = CaseDataset(sout, 'json')
        vnames = cds.data.var_names().fetch()
        expected = ['_driver_id', '_id', '_parent_id', u'_pseudo_0.out0',
                    u'_pseudo_1.out0', u'comp1.x', 'error_message', 'error_status', 'timestamp']

        self.assertFalse(set(vnames).symmetric_difference(set(expected)))
        
        # Specific variables.
        names = [ 'comp1.x',]
        vnames = cds.data.vars(names).var_names().fetch()
        self.assertFalse(set(vnames).symmetric_difference(set(names)))

        cases = cds.data.vars(names).fetch()
        self.assertEqual(len(cases), 1)
        self.assertEqual(len(cases[0]), len(names))

        iteration_case_1 = {
            "comp1.x": 0.0,
        }
        for name, val in zip(names, cases[0]):
            self.assertAlmostEqual(val, iteration_case_1[name])

    def test_includes_only(self):
        # verify options with includes but not problem formulation:
        #        save_problem_formulation = False
        #        includes = ['comp2*']
        #        excludes = []

        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recording_options.save_problem_formulation = False
        self.top.recording_options.includes = ['comp2*']
        self.top.run()

        sout.seek(0) # need to go back to the front of the "file"
        cds = CaseDataset(sout, 'json')
        vnames = cds.data.var_names().fetch()
        expected = ['_driver_id', '_id', '_parent_id', u'comp2.derivative_exec_count', 
                    u'comp2.exec_count', u'comp2.itername', u'comp2.z', 'error_message', 
                    'error_status', 'timestamp']
        self.assertFalse(set(vnames) - set(expected))
        
        
        constants = cds.simulation_info['constants'].keys()
        expected = [u'comp2.directory', u'comp2.force_fd', u'comp2.missing_deriv_policy']
        self.assertFalse(set(constants) - set(expected))
        
        
        # Specific variables.
        names = [ 'comp2.z']
        vnames = cds.data.vars(names).var_names().fetch()
        self.assertEqual(vnames, names)

        cases = cds.data.vars(names).fetch()
        self.assertEqual(len(cases), 1)
        self.assertEqual(len(cases[0]), len(names))

        iteration_case_1 = {
            "comp2.z": 1.0,
        }
        for name, val in zip(names, cases[0]):
            self.assertAlmostEqual(val, iteration_case_1[name])

    def test_options_with_excludes(self):
        # verify options with excludes:
        #        save_problem_formulation = True
        #        includes = ['*']
        #        excludes = ['*directory', '*force_fd', '*missing_deriv_policy', '*gradient_options*']
        
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recording_options.excludes = [
            '*directory',
            '*force_fd',
            '*missing_deriv_policy',
            '*gradient_options*'
        ]
        self.top.run()

        sout.seek(0) # need to go back to the front of the "file"
        cds = CaseDataset(sout, 'json')
        
        constants = cds.simulation_info['constants'].keys()
        expected = [u'recording_options.save_problem_formulation', 
			u'recording_options.includes', u'comp1.y', u'recording_options.excludes']
        self.assertFalse(set(constants) - set(expected))
     

    def test_options_with_includes_excludes(self):
        # verify options with includes and excludes (excludes are processed after includes):
        #        save_problem_formulation = True
        #        includes = ['comp1']
        #        excludes = ['*directory', '*force_fd', '*missing_deriv_policy']
        
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recording_options.includes = ['comp1*']
        self.top.recording_options.excludes = [
            '*directory',
            '*force_fd',
            '*missing_deriv_policy'
        ]
        self.top.run()
        sout.seek(0) # need to go back to the front of the "file"
        cds = CaseDataset(sout, 'json')
        
        constants = cds.simulation_info['constants'].keys()
        expected = [u'comp1.y']
        self.assertFalse(set(constants) - set(expected))


        vnames = cds.data.var_names().fetch()
        expected = ['_driver_id', '_id', '_parent_id', u'_pseudo_0.out0', u'_pseudo_1.out0', 
                    u'comp1.derivative_exec_count', u'comp1.exec_count', u'comp1.itername', 
                    u'comp1.x', u'comp1.z', 'error_message', 'error_status', 'timestamp']
        
        #self.assertFalse(set(vnames) - set(expected))
        self.assertFalse(set(vnames).symmetric_difference(set(expected)))
    
        # Specific variables are there
        names = [ 'comp1.z', 'comp1.x']
        vnames = cds.data.vars(names).var_names().fetch()
        self.assertEqual(vnames, names)

        cases = cds.data.vars(names).fetch()
        self.assertEqual(len(cases), 1)
        self.assertEqual(len(cases[0]), len(names))

        iteration_case_1 = {
            "comp1.x": 0.0,
            "comp1.z": 0.0,
        }
        for name, val in zip(names, cases[0]):
            self.assertAlmostEqual(val, iteration_case_1[name])



if __name__ == '__main__':
    unittest.main()
