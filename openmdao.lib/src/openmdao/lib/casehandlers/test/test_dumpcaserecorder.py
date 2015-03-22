"""
Test for CaseRecorders.
"""

import unittest
import StringIO

from openmdao.main.api import Assembly, Case, set_as_top, VariableTree, Component
from openmdao.main.datatypes.api import VarTree, Float
from openmdao.lib.drivers.conmindriver import CONMINdriver
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import DumpCaseRecorder
from openmdao.lib.drivers.sensitivity import SensitivityDriver
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver


def float_value(str):
    try:
        val = float(str)
    except ValueError:
        return None
    return val


class TestContainer(VariableTree):

    dummy1 = Float(desc='default value of 0.0') #this value is being grabbed by the optimizer
    dummy2 = Float(11.0)


class TestComponent(Component):

    dummy_data = VarTree(TestContainer(), iotype='in')
    x = Float(iotype='out')

    def execute(self):
        self.x = (self.dummy_data.dummy1-3)**2 - self.dummy_data.dummy2


class TestAssembly(Assembly):

    def configure(self):
        self.add('dummy_top', TestContainer())
        self.add('comp', TestComponent())
        self.add('driver', CONMINdriver())

        self.driver.workflow.add(['comp'])
        #self.driver.iprint = 4 #debug verbosity
        self.driver.add_objective('comp.x')
        self.driver.add_parameter('comp.dummy_data.dummy1', low=-10.0, high=10.0)

class TestCase(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        for recorder in self.top.recorders:
            recorder.close()

    def test_dumprecorder(self):
        self.top = set_as_top(TestAssembly())
        sout1 = StringIO.StringIO()
        sout2 = StringIO.StringIO()
        self.top.recorders = [DumpCaseRecorder(sout1), DumpCaseRecorder(sout2)]
        self.top.run()

        expected_constants = """\
Constants:
   comp.directory:
   comp.force_fd: False
   comp.missing_deriv_policy: error
   directory:
   driver.conmin_diff: False
   driver.ct: -0.1
   driver.ctl: -0.01
   driver.ctlmin: 0.001
   driver.ctmin: 0.004
   driver.dabfun: 0.001
   driver.delfun: 0.001
   driver.directory:
   driver.fdch: 0.01
   driver.fdchm: 0.01
   driver.force_fd: False
   driver.gradient_options.atol: 1e-09
   driver.gradient_options.derivative_direction: auto
   driver.gradient_options.directional_fd: False
   driver.gradient_options.fd_form: forward
   driver.gradient_options.fd_step: 1e-06
   driver.gradient_options.fd_step_type: absolute
   driver.gradient_options.force_fd: False
   driver.gradient_options.iprint: 0
   driver.gradient_options.lin_solver: scipy_gmres
   driver.gradient_options.maxiter: 100
   driver.gradient_options.rtol: 1e-09
   driver.icndir: 0.0
   driver.iprint: 0
   driver.itmax: 10
   driver.itrm: 3
   driver.linobj: False
   driver.phi: 5.0
   driver.theta: 1.0
   force_fd: False
   missing_deriv_policy: assume_zero
   recording_options.excludes: []
   recording_options.includes: ['*']
   recording_options.save_problem_formulation: True"""


        expected_case ="""\
Case:
   uuid: 66aaa5dc-9c1c-11e4-8009-20c9d0478eff
   timestamp: 1421260652.344388
   inputs:
      comp.dummy_data.dummy1: 2.28846229958
   outputs:
      _pseudo_0.out0: -10.4937141009
      comp.derivative_exec_count: 0
      comp.exec_count: 11
      comp.itername: 9-comp
      comp.x: -10.4937141009
      driver.workflow.itername: 9"""

        # print sout1.getvalue()
        expected = expected_constants.split('\n')
        for sout in [sout1, sout2]:
            lines = sout.getvalue().split('\n')
            lines = [line.rstrip() for line in lines]
            for i in range(len(expected)):
                self.assertEqual(lines[i].rstrip(), expected[i])

        expected = expected_case.split('\n')
        for sout in [sout1, sout2]:
            lines = sout.getvalue().split('\n')
            lines = [line.rstrip() for line in lines]
            start = 0
            for i in range(9):
                index = start + lines[start:].index('Case:')
                start = index + 1
            for i in range(len(expected)):
                prefix, value = expected[i].split(':')
                if prefix.lstrip() in ['uuid', 'timestamp', 'comp.exec_count']:
                    # these values vary, just check proper prefix & indentation
                    self.assertTrue(lines[index+i].startswith(prefix+':'))
                else:
                    expected_float = float_value(value)
                    if expected_float:
                        self.assertTrue(lines[index+i].startswith(prefix+':'))
                        _, value = lines[index+i].split(':')
                        self.assertAlmostEqual(float_value(value), expected_float)
                    else:
                        self.assertEqual(lines[index+i], expected[i])


class DumpCaseRecorderTestCase(unittest.TestCase):

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
            i = float(i)
            inputs = [('comp1.x', i), ('comp1.y', i*2)]
            cases.append(Case(inputs=inputs, outputs=outputs))

        Case.set_vartree_inputs(driver, cases)
        driver.add_responses(outputs)

    def test_bad_recorder(self):
        try:
            self.top.recorders = DumpCaseRecorder()
        except Exception as err:
            self.assertTrue(str(err).startswith("The 'recorders' trait of an Assembly"))
            self.assertTrue(str(err).endswith(" was specified."))
        else:
            self.fail("Exception expected")

    def test_multiple_objectives(self):
        sout = StringIO.StringIO()
        self.top.add('driver', SensitivityDriver())
        self.top.driver.workflow.add(['comp1', 'comp2'])
        self.top.driver.add_parameter(['comp1.x'], low=-100, high=100)
        self.top.driver.add_objective('comp1.z')
        self.top.driver.add_objective('comp2.z')

        self.top.recorders = [DumpCaseRecorder(sout)]
        self.top.run()

        expected = """\
Constants:
   comp1.directory:
   comp1.force_fd: False
   comp1.missing_deriv_policy: error
   comp1.y: 0.0
   comp2.directory:
   comp2.force_fd: False
   comp2.missing_deriv_policy: error
   directory:
   driver.directory:
   driver.force_fd: False
   driver.gradient_options.atol: 1e-09
   driver.gradient_options.derivative_direction: auto
   driver.gradient_options.directional_fd: False
   driver.gradient_options.fd_form: forward
   driver.gradient_options.fd_step: 1e-06
   driver.gradient_options.fd_step_type: absolute
   driver.gradient_options.force_fd: False
   driver.gradient_options.iprint: 0
   driver.gradient_options.lin_solver: scipy_gmres
   driver.gradient_options.maxiter: 100
   driver.gradient_options.rtol: 1e-09
   force_fd: False
   missing_deriv_policy: assume_zero
   recording_options.excludes: []
   recording_options.includes: ['*']
   recording_options.save_problem_formulation: True
Case:
   uuid: 4a80208f-9c20-11e4-800b-20c9d0478eff
   timestamp: 1421262323.082445
   inputs:
      comp1.x: 0.0
   outputs:
      _pseudo_2.out0: 0.0
      _pseudo_3.out0: 1.0
      comp1.derivative_exec_count: 0
      comp1.exec_count: 1
      comp1.itername: 1-comp1
      comp1.z: 0.0
      comp2.derivative_exec_count: 0
      comp2.exec_count: 1
      comp2.itername: 1-comp2
      comp2.z: 1.0
      driver.workflow.itername: 1
"""

        # print sout.getvalue()
        expected = expected.split('\n')
        lines = sout.getvalue().split('\n')
        lines = [line.rstrip() for line in lines]
        for i in range(len(expected)):
            if expected[i].startswith('   uuid:'):
                self.assertTrue(lines[i].startswith('   uuid:'))
            elif expected[i].startswith('   timestamp:'):
                self.assertTrue(lines[i].startswith('   timestamp:'))
            else:
                self.assertEqual(lines[i].rstrip(), expected[i])

    def test_close(self):
        sout1 = StringIO.StringIO()
        self.top.recorders = [DumpCaseRecorder(sout1)]
        self.top.recorders[0].close()
        self.top.run()
        self.assertEqual(sout1.getvalue(), '')


if __name__ == '__main__':
    unittest.main()
