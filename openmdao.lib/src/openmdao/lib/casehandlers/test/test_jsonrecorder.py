import unittest
import StringIO

from openmdao.main.api import Assembly, Case, set_as_top
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import JSONCaseRecorder
from openmdao.lib.drivers.sensitivity import SensitivityDriver
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver


class TestCase(unittest.TestCase):

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
            cases.append(Case(inputs=inputs, outputs=outputs))

        Case.set_vartree_inputs(driver, cases)
        driver.add_responses(outputs)

    def test_bad_recorder(self):
        try:
            self.top.recorders = JSONCaseRecorder()
        except Exception as err:
            self.assertTrue(str(err).startswith("The 'recorders' trait of an Assembly"))
            self.assertTrue(str(err).endswith(" was specified."))
        else:
            self.fail("Exception expected")

    def test_jsonrecorder(self):
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.run()

        expected_constants = """\
{
    "constants": {
        "comp1.directory": "", 
        "comp1.force_execute": false, 
        "comp1.force_fd": false, 
        "comp1.missing_deriv_policy": "error", 
        "comp2.directory": "", 
        "comp2.force_execute": false, 
        "comp2.force_fd": false, 
        "comp2.missing_deriv_policy": "error", 
        "directory": "", 
        "driver.case_inputs.comp1.x": [
            0, 
            1, 
            2, 
            3, 
            4, 
            5, 
            6, 
            7, 
            8, 
            9
        ], 
        "driver.case_inputs.comp1.y": [
            0, 
            2, 
            4, 
            6, 
            8, 
            10, 
            12, 
            14, 
            16, 
            18
        ], 
        "driver.directory": "", 
        "driver.force_execute": true, 
        "driver.force_fd": false, 
        "driver.gradient_options.derivative_direction": "auto", 
        "driver.gradient_options.fd_form": "forward", 
        "driver.gradient_options.fd_step": 1e-06, 
        "driver.gradient_options.fd_step_type": "absolute", 
        "driver.gradient_options.force_fd": false, 
        "driver.gradient_options.gmres_maxiter": 100, 
        "driver.gradient_options.gmres_tolerance": 1e-09, 
        "excludes": [], 
        "force_execute": false, 
        "force_fd": false, 
        "includes": [
            "*"
        ], 
        "missing_deriv_policy": "assume_zero"
    }
}"""
        expected_case = """\
{
    "case": {
        "Response_0": 24.0, 
        "Response_1": 25.0, 
        "comp1.x": 8.0, 
        "comp1.y": 16.0, 
        "comp1.z": 24.0, 
        "comp2.z": 25.0, 
        "driver.workflow.itername": "9", 
        "parent_uuid": "", 
        "timestamp": 1400599776.232577, 
        "uuid": "8cda6057-e033-11e3-8031-005056000100"
    }
}"""
#        print sout.getvalue()
        lines = sout.getvalue().split('\n')

        expected = expected_constants.split('\n')
        for i in range(len(expected)):
            self.assertEqual(lines[i], expected[i])

        expected = expected_case.split('\n')
        start = 0
        for i in range(10):
            index = start + lines[start:].index('{')
            start = index + 1
        for i in range(len(expected)):
            if expected[i].startswith('        "uuid":'):
                self.assertTrue(lines[index+i].startswith('        "uuid":'))
            elif expected[i].startswith('        "timestamp":'):
                self.assertTrue(lines[index+i].startswith('        "timestamp":'))
            else:
                self.assertEqual(lines[index+i], expected[i])

    def test_multiple_objectives(self):
        sout = StringIO.StringIO()
        self.top.add('driver', SensitivityDriver())
        self.top.driver.workflow.add(['comp1', 'comp2'])
        self.top.driver.add_parameter(['comp1.x'], low=-100, high=100)
        self.top.driver.add_objective('comp1.z')
        self.top.driver.add_objective('comp2.z')

        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.run()

        expected = """\
{
    "constants": {
        "comp1.directory": "", 
        "comp1.force_execute": false, 
        "comp1.force_fd": false, 
        "comp1.missing_deriv_policy": "error", 
        "comp1.y": 0.0, 
        "comp2.directory": "", 
        "comp2.force_execute": false, 
        "comp2.force_fd": false, 
        "comp2.missing_deriv_policy": "error", 
        "directory": "", 
        "driver.directory": "", 
        "driver.force_execute": true, 
        "driver.force_fd": false, 
        "driver.gradient_options.derivative_direction": "auto", 
        "driver.gradient_options.fd_form": "forward", 
        "driver.gradient_options.fd_step": 1e-06, 
        "driver.gradient_options.fd_step_type": "absolute", 
        "driver.gradient_options.force_fd": false, 
        "driver.gradient_options.gmres_maxiter": 100, 
        "driver.gradient_options.gmres_tolerance": 1e-09, 
        "excludes": [], 
        "force_execute": false, 
        "force_fd": false, 
        "includes": [
            "*"
        ], 
        "missing_deriv_policy": "assume_zero"
    }
}
{
    "case": {
        "Objective_0": 0.0, 
        "Objective_1": 1.0, 
        "comp1.x": 0.0, 
        "comp1.z": 0.0, 
        "comp2.z": 1.0, 
        "driver.workflow.itername": "1", 
        "parent_uuid": "", 
        "timestamp": 1400604700.192612, 
        "uuid": "03c312b8-e03f-11e3-803d-005056000100"
    }
}"""
#        print sout.getvalue()
        lines = sout.getvalue().split('\n')
        expected = expected.split('\n')
        for i in range(len(expected)):
            if expected[i].startswith('        "uuid":'):
                self.assertTrue(lines[i].startswith('        "uuid":'))
            elif expected[i].startswith('        "timestamp":'):
                self.assertTrue(lines[i].startswith('        "timestamp":'))
            else:
                self.assertEqual(lines[i], expected[i])

    def test_close(self):
        sout = StringIO.StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recorders[0].close()
        self.top.run()
        self.assertEqual(sout.getvalue(), '')


if __name__ == '__main__':
    unittest.main()

