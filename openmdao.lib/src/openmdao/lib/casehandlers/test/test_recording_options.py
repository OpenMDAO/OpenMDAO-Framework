"""
Test for CaseRecorders.
"""

import unittest
import StringIO

from openmdao.main.api import Assembly, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import DumpCaseRecorder
from openmdao.lib.casehandlers.api import JSONCaseRecorder, verify_json
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

        verify_json( self, sout, 'test_default_options.json')
        #print sout.getvalue()

    def test_problem_formulation_only(self):
        """ verify options with no includes:
                save_problem_formulation = True
                includes = []
                excludes = []
        """
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recording_options.save_problem_formulation = True
        self.top.recording_options.includes = []
        self.top.run()

        #print sout.getvalue()
        verify_json( self, sout, 'test_problem_formulation_only.json')
        #self.verify_case_dump(expected, sout)

    def test_includes_only(self):
        """ verify options with includes but not problem formulation:
                save_problem_formulation = False
                includes = ['comp2*']
                excludes = []
        """
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recording_options.save_problem_formulation = False
        self.top.recording_options.includes = ['comp2*']
        self.top.run()

        #print sout.getvalue()
        verify_json( self, sout, 'test_includes_only.json')
        #self.verify_case_dump(expected, sout)

    def test_options_with_excludes(self):
        """ verify options with excludes:
                save_problem_formulation = True
                includes = ['*']
                excludes = ['*directory', '*force_fd', '*missing_deriv_policy', '*gradient_options*']
        """
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recording_options.excludes = [
            '*directory',
            '*force_fd',
            '*missing_deriv_policy',
            '*gradient_options*'
        ]
        self.top.run()

        # print sout.getvalue()
        self.verify_case_dump(expected, sout)

    def test_options_with_includes_excludes(self):
        """ verify options with includes and excludes (excludes are processed after includes):
                save_problem_formulation = True
                includes = ['comp1']
                excludes = ['*directory', '*force_fd', '*missing_deriv_policy']
        """
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recording_options.includes = ['comp1*']
        self.top.recording_options.excludes = [
            '*directory',
            '*force_fd',
            '*missing_deriv_policy'
        ]
        self.top.run()

        expected = """\
Constants:
   comp1.y: 0.0
Case:
   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe
   timestamp: 1383239074.309192
   inputs:
      comp1.x: 0.0
   outputs:
      Objective(comp1.z): 0.0
      Objective(comp2.z): 1.0
      comp1.derivative_exec_count: 0
      comp1.exec_count: 1
      comp1.itername: 1-comp1
      comp1.z: 0.0
"""

        # print sout.getvalue()
        self.verify_case_dump(expected, sout)


if __name__ == '__main__':
    unittest.main()
