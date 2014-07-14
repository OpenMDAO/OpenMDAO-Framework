"""
Tests the capability to use the case recorders to dump out larger amounts of information
from a driver's workflow.
"""
import StringIO
import unittest

from openmdao.lib.casehandlers.api import DumpCaseRecorder
from openmdao.main.api import Component, Assembly, Driver, set_as_top
from openmdao.main.datatypes.api import Float, List, Str


class Basic_Component(Component):
    ''' Basic building block'''

    x1 = Float(0.0, iotype='in', units='cm')
    y1 = Float(iotype='out', units='m')

    def execute(self):
        ''' pretty simple'''
        self.y1 = self.x1 + 1


class Nest_Me(Assembly):

    def configure(self):
        ''' add some comps'''
        self.add('comp1', Basic_Component())
        self.add('comp2', Basic_Component())
        self.add('comp3', Basic_Component())
        self.driver.workflow.add(['comp1', 'comp2', 'comp3'])

        self.connect('comp1.y1', 'comp2.x1')
        self.connect('comp2.y1', 'comp3.x1')


class Complex_Comp(Component):
    ''' Basic building block'''

    list_str = List(Str, iotype='in')
    string = Str('Testing', iotype='out')

    def execute(self):
        ''' pretty simple'''
        pass


class Run_N(Driver):
    def __init__(self, iter_stop, *args, **kwargs):
        super(Run_N, self).__init__(*args, **kwargs)
        self._iter_count = 0
        self._iter_stop = iter_stop

    def execute(self):
        Driver.execute(self)

    def start_iteration(self):
        super(Run_N, self).start_iteration()
        self._iter_count = 0

    def continue_iteration(self):
        return self._iter_count <= self._iter_stop

    def run_iteration(self):
        super(Run_N, self).run_iteration()
        self._iter_count += 1


class Data_Dump_TestCase(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())

    def tearDown(self):
        self.top = None

    def test_nested_assy_match_all(self):
        self.top.add('comp1', Basic_Component())
        self.top.add('nested', Nest_Me())
        self.top.driver.workflow.add('nested')
        self.top.nested.add('doublenest', Nest_Me())
        self.top.nested.driver.workflow.add('doublenest')

        sout = StringIO.StringIO()
        self.top.recorders = [DumpCaseRecorder(sout)]
        self.top.run()

        expected = """\
Constants:
   comp1.directory:
   comp1.force_fd: False
   comp1.missing_deriv_policy: error
   comp1.x1: 0.0
   directory:
   driver.directory:
   driver.force_fd: False
   driver.gradient_options.derivative_direction: auto
   driver.gradient_options.directional_fd: False
   driver.gradient_options.fd_blocks: []
   driver.gradient_options.fd_form: forward
   driver.gradient_options.fd_step: 1e-06
   driver.gradient_options.fd_step_type: absolute
   driver.gradient_options.force_fd: False
   driver.gradient_options.gmres_maxiter: 100
   driver.gradient_options.gmres_tolerance: 1e-09
   excludes: []
   force_fd: False
   includes: ['*']
   missing_deriv_policy: assume_zero
   nested.comp1.directory:
   nested.comp1.force_fd: False
   nested.comp1.missing_deriv_policy: error
   nested.comp1.x1: 0.0
   nested.comp2.directory:
   nested.comp2.force_fd: False
   nested.comp2.missing_deriv_policy: error
   nested.comp3.directory:
   nested.comp3.force_fd: False
   nested.comp3.missing_deriv_policy: error
   nested.directory:
   nested.doublenest.comp1.directory:
   nested.doublenest.comp1.force_fd: False
   nested.doublenest.comp1.missing_deriv_policy: error
   nested.doublenest.comp1.x1: 0.0
   nested.doublenest.comp2.directory:
   nested.doublenest.comp2.force_fd: False
   nested.doublenest.comp2.missing_deriv_policy: error
   nested.doublenest.comp3.directory:
   nested.doublenest.comp3.force_fd: False
   nested.doublenest.comp3.missing_deriv_policy: error
   nested.doublenest.directory:
   nested.doublenest.driver.directory:
   nested.doublenest.driver.force_fd: False
   nested.doublenest.driver.gradient_options.derivative_direction: auto
   nested.doublenest.driver.gradient_options.directional_fd: False
   nested.doublenest.driver.gradient_options.fd_blocks: []
   nested.doublenest.driver.gradient_options.fd_form: forward
   nested.doublenest.driver.gradient_options.fd_step: 1e-06
   nested.doublenest.driver.gradient_options.fd_step_type: absolute
   nested.doublenest.driver.gradient_options.force_fd: False
   nested.doublenest.driver.gradient_options.gmres_maxiter: 100
   nested.doublenest.driver.gradient_options.gmres_tolerance: 1e-09
   nested.doublenest.excludes: []
   nested.doublenest.force_fd: False
   nested.doublenest.includes: ['*']
   nested.doublenest.missing_deriv_policy: assume_zero
   nested.driver.directory:
   nested.driver.force_fd: False
   nested.driver.gradient_options.derivative_direction: auto
   nested.driver.gradient_options.directional_fd: False
   nested.driver.gradient_options.fd_blocks: []
   nested.driver.gradient_options.fd_form: forward
   nested.driver.gradient_options.fd_step: 1e-06
   nested.driver.gradient_options.fd_step_type: absolute
   nested.driver.gradient_options.force_fd: False
   nested.driver.gradient_options.gmres_maxiter: 100
   nested.driver.gradient_options.gmres_tolerance: 1e-09
   nested.excludes: []
   nested.force_fd: False
   nested.includes: ['*']
   nested.missing_deriv_policy: assume_zero
Case:
   uuid: 87008694-e042-11e3-8005-005056000100
   timestamp: 1400606208.869153
   parent_uuid: 87001ef0-e042-11e3-8004-005056000100
   outputs:
      nested.doublenest.comp1.derivative_exec_count: 0
      nested.doublenest.comp1.exec_count: 1
      nested.doublenest.comp1.itername: 1-nested.1-doublenest.1-comp1
      nested.doublenest.comp1.y1: 1.0
      nested.doublenest.comp2.derivative_exec_count: 0
      nested.doublenest.comp2.exec_count: 1
      nested.doublenest.comp2.itername: 1-nested.1-doublenest.1-comp2
      nested.doublenest.comp2.y1: 101.0
      nested.doublenest.comp3.derivative_exec_count: 0
      nested.doublenest.comp3.exec_count: 1
      nested.doublenest.comp3.itername: 1-nested.1-doublenest.1-comp3
      nested.doublenest.comp3.y1: 10101.0
      nested.doublenest.driver.workflow.itername: 1-nested.1-doublenest.1
Case:
   uuid: 87001ef0-e042-11e3-8004-005056000100
   timestamp: 1400606208.869368
   parent_uuid: 870019e8-e042-11e3-8003-005056000100
   outputs:
      nested.comp1.derivative_exec_count: 0
      nested.comp1.exec_count: 1
      nested.comp1.itername: 1-nested.1-comp1
      nested.comp1.y1: 1.0
      nested.comp2.derivative_exec_count: 0
      nested.comp2.exec_count: 1
      nested.comp2.itername: 1-nested.1-comp2
      nested.comp2.y1: 101.0
      nested.comp3.derivative_exec_count: 0
      nested.comp3.exec_count: 1
      nested.comp3.itername: 1-nested.1-comp3
      nested.comp3.y1: 10101.0
      nested.doublenest.derivative_exec_count: 0
      nested.doublenest.exec_count: 1
      nested.doublenest.itername: 1-nested.1-doublenest
      nested.driver.workflow.itername: 1-nested.1
Case:
   uuid: 870019e8-e042-11e3-8003-005056000100
   timestamp: 1400606208.869599
   outputs:
      driver.workflow.itername: 1
      nested.derivative_exec_count: 0
      nested.exec_count: 1
      nested.itername: 1-nested"""

        expected = expected.split('\n')
#        print sout.getvalue()
        lines = sout.getvalue().split('\n')
        for line, template in zip(lines, expected):
            line = line.rstrip()
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            elif template.startswith('   timestamp:'):
                self.assertTrue(line.startswith('   timestamp:'))
            elif template.startswith('   parent_uuid:'):
                self.assertTrue(line.startswith('   parent_uuid:'))
            else:
                self.assertEqual(line, template)

    def test_nested_assy_match_wildcard(self):
        self.top.add('comp1', Basic_Component())
        self.top.add('nested', Nest_Me())
        self.top.driver.workflow.add('nested')
        self.top.nested.add('doublenest', Nest_Me())
        self.top.nested.driver.workflow.add('doublenest')

        sout = StringIO.StringIO()
        self.top.recorders = [DumpCaseRecorder(sout)]
        self.top.run()

        expected = """\
Constants:
   comp1.directory:
   comp1.force_fd: False
   comp1.missing_deriv_policy: error
   comp1.x1: 0.0
   directory:
   driver.directory:
   driver.force_fd: False
   driver.gradient_options.derivative_direction: auto
   driver.gradient_options.directional_fd: False
   driver.gradient_options.fd_blocks: []
   driver.gradient_options.fd_form: forward
   driver.gradient_options.fd_step: 1e-06
   driver.gradient_options.fd_step_type: absolute
   driver.gradient_options.force_fd: False
   driver.gradient_options.gmres_maxiter: 100
   driver.gradient_options.gmres_tolerance: 1e-09
   excludes: []
   force_fd: False
   includes: ['*']
   missing_deriv_policy: assume_zero
   nested.comp1.directory:
   nested.comp1.force_fd: False
   nested.comp1.missing_deriv_policy: error
   nested.comp1.x1: 0.0
   nested.comp2.directory:
   nested.comp2.force_fd: False
   nested.comp2.missing_deriv_policy: error
   nested.comp3.directory:
   nested.comp3.force_fd: False
   nested.comp3.missing_deriv_policy: error
   nested.directory:
   nested.doublenest.comp1.directory:
   nested.doublenest.comp1.force_fd: False
   nested.doublenest.comp1.missing_deriv_policy: error
   nested.doublenest.comp1.x1: 0.0
   nested.doublenest.comp2.directory:
   nested.doublenest.comp2.force_fd: False
   nested.doublenest.comp2.missing_deriv_policy: error
   nested.doublenest.comp3.directory:
   nested.doublenest.comp3.force_fd: False
   nested.doublenest.comp3.missing_deriv_policy: error
   nested.doublenest.directory:
   nested.doublenest.driver.directory:
   nested.doublenest.driver.force_fd: False
   nested.doublenest.driver.gradient_options.derivative_direction: auto
   nested.doublenest.driver.gradient_options.directional_fd: False
   nested.doublenest.driver.gradient_options.fd_blocks: []
   nested.doublenest.driver.gradient_options.fd_form: forward
   nested.doublenest.driver.gradient_options.fd_step: 1e-06
   nested.doublenest.driver.gradient_options.fd_step_type: absolute
   nested.doublenest.driver.gradient_options.force_fd: False
   nested.doublenest.driver.gradient_options.gmres_maxiter: 100
   nested.doublenest.driver.gradient_options.gmres_tolerance: 1e-09
   nested.doublenest.excludes: []
   nested.doublenest.force_fd: False
   nested.doublenest.includes: ['*']
   nested.doublenest.missing_deriv_policy: assume_zero
   nested.driver.directory:
   nested.driver.force_fd: False
   nested.driver.gradient_options.derivative_direction: auto
   nested.driver.gradient_options.directional_fd: False
   nested.driver.gradient_options.fd_blocks: []
   nested.driver.gradient_options.fd_form: forward
   nested.driver.gradient_options.fd_step: 1e-06
   nested.driver.gradient_options.fd_step_type: absolute
   nested.driver.gradient_options.force_fd: False
   nested.driver.gradient_options.gmres_maxiter: 100
   nested.driver.gradient_options.gmres_tolerance: 1e-09
   nested.excludes: []
   nested.force_fd: False
   nested.includes: ['*']
   nested.missing_deriv_policy: assume_zero
Case:
   uuid: 84c2195c-e043-11e3-8008-005056000100
   timestamp: 1400606634.601406
   parent_uuid: 84c1b666-e043-11e3-8007-005056000100
   outputs:
      nested.doublenest.comp1.derivative_exec_count: 0
      nested.doublenest.comp1.exec_count: 1
      nested.doublenest.comp1.itername: 1-nested.1-doublenest.1-comp1
      nested.doublenest.comp1.y1: 1.0
      nested.doublenest.comp2.derivative_exec_count: 0
      nested.doublenest.comp2.exec_count: 1
      nested.doublenest.comp2.itername: 1-nested.1-doublenest.1-comp2
      nested.doublenest.comp2.y1: 101.0
      nested.doublenest.comp3.derivative_exec_count: 0
      nested.doublenest.comp3.exec_count: 1
      nested.doublenest.comp3.itername: 1-nested.1-doublenest.1-comp3
      nested.doublenest.comp3.y1: 10101.0
      nested.doublenest.driver.workflow.itername: 1-nested.1-doublenest.1
Case:
   uuid: 84c1b666-e043-11e3-8007-005056000100
   timestamp: 1400606634.601622
   parent_uuid: 84c1b0f8-e043-11e3-8006-005056000100
   outputs:
      nested.comp1.derivative_exec_count: 0
      nested.comp1.exec_count: 1
      nested.comp1.itername: 1-nested.1-comp1
      nested.comp1.y1: 1.0
      nested.comp2.derivative_exec_count: 0
      nested.comp2.exec_count: 1
      nested.comp2.itername: 1-nested.1-comp2
      nested.comp2.y1: 101.0
      nested.comp3.derivative_exec_count: 0
      nested.comp3.exec_count: 1
      nested.comp3.itername: 1-nested.1-comp3
      nested.comp3.y1: 10101.0
      nested.doublenest.derivative_exec_count: 0
      nested.doublenest.exec_count: 1
      nested.doublenest.itername: 1-nested.1-doublenest
      nested.driver.workflow.itername: 1-nested.1
Case:
   uuid: 84c1b0f8-e043-11e3-8006-005056000100
   timestamp: 1400606634.601857
   outputs:
      driver.workflow.itername: 1
      nested.derivative_exec_count: 0
      nested.exec_count: 1
      nested.itername: 1-nested"""

        expected = expected.split('\n')
#        print sout.getvalue()
        lines = sout.getvalue().split('\n')

        for line, template in zip(lines, expected):
            line = line.rstrip()
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            elif template.startswith('   timestamp:'):
                self.assertTrue(line.startswith('   timestamp:'))
            elif template.startswith('   parent_uuid:'):
                self.assertTrue(line.startswith('   parent_uuid:'))
            else:
                self.assertEqual(line, template)

    def test_exclude_pseudocomps(self):
        # Pseudocomp comes from unit conversion
        self.top.add('comp1', Basic_Component())
        self.top.driver.workflow.add('comp1')
        self.top.add('comp2', Basic_Component())
        self.top.driver.workflow.add('comp2')
        self.top.connect('comp1.y1', 'comp2.x1')

        sout = StringIO.StringIO()
        self.top.recorders = [DumpCaseRecorder(sout)]
        self.top.run()

        expected = """\
Constants:
   comp1.directory:
   comp1.force_fd: False
   comp1.missing_deriv_policy: error
   comp1.x1: 0.0
   comp2.directory:
   comp2.force_fd: False
   comp2.missing_deriv_policy: error
   directory:
   driver.directory:
   driver.force_fd: False
   driver.gradient_options.derivative_direction: auto
   driver.gradient_options.directional_fd: False
   driver.gradient_options.fd_blocks: []
   driver.gradient_options.fd_form: forward
   driver.gradient_options.fd_step: 1e-06
   driver.gradient_options.fd_step_type: absolute
   driver.gradient_options.force_fd: False
   driver.gradient_options.gmres_maxiter: 100
   driver.gradient_options.gmres_tolerance: 1e-09
   excludes: []
   force_fd: False
   includes: ['*']
   missing_deriv_policy: assume_zero
Case:
   uuid: 0d85f3c5-e044-11e3-8001-005056000100
   timestamp: 1400606864.054437
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 1
      comp1.itername: 1-comp1
      comp1.y1: 1.0
      comp2.derivative_exec_count: 0
      comp2.exec_count: 1
      comp2.itername: 1-comp2
      comp2.y1: 101.0
      driver.workflow.itername: 1"""

        expected = expected.split('\n')
#        print sout.getvalue()
        lines = sout.getvalue().split('\n')

        for line, template in zip(lines, expected):
            line = line.rstrip()
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            elif template.startswith('   timestamp:'):
                self.assertTrue(line.startswith('   timestamp:'))
            elif 'gradient_options' in template:
                self.assertEqual(line.split('<')[0], template.split('<')[0])
            else:
                self.assertEqual(line, template)

    def test_more_datatypes(self):
        self.top.add('comp1', Complex_Comp())
        self.top.driver.workflow.add('comp1')

        sout = StringIO.StringIO()
        self.top.recorders = [DumpCaseRecorder(sout)]
        self.top.run()

        expected = """\
Constants:
   comp1.directory:
   comp1.force_fd: False
   comp1.list_str: []
   comp1.missing_deriv_policy: error
   directory:
   driver.directory:
   driver.force_fd: False
   driver.gradient_options.derivative_direction: auto
   driver.gradient_options.directional_fd: False
   driver.gradient_options.fd_blocks: []
   driver.gradient_options.fd_form: forward
   driver.gradient_options.fd_step: 1e-06
   driver.gradient_options.fd_step_type: absolute
   driver.gradient_options.force_fd: False
   driver.gradient_options.gmres_maxiter: 100
   driver.gradient_options.gmres_tolerance: 1e-09
   excludes: []
   force_fd: False
   includes: ['*']
   missing_deriv_policy: assume_zero
Case:
   uuid: 4f3ca163-e044-11e3-8002-005056000100
   timestamp: 1400606974.301962
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 1
      comp1.itername: 1-comp1
      comp1.string: Testing
      driver.workflow.itername: 1"""

        expected = expected.split('\n')
#        print sout.getvalue()
        lines = sout.getvalue().split('\n')

        for line, template in zip(lines, expected):
            line = line.rstrip()
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            elif template.startswith('   timestamp:'):
                self.assertTrue(line.startswith('   timestamp:'))
            elif 'gradient_options' in template:
                self.assertEqual(line.split('<')[0], template.split('<')[0])
            else:
                self.assertEqual(line, template)

    def test_workflow_itername(self):
        # top
        #     comp1
        #     driverA
        #         comp1
        #         comp2
        #     driverB
        #         comp2
        #         subassy
        #             comp3
        top = Assembly()
        top.add('comp1', Basic_Component())
        top.add('driverA', Run_N(4))
        top.add('comp2', Basic_Component())
        top.add('driverB', Run_N(3))

        sub = top.add('subassy', Assembly())
        sub.add('comp3', Basic_Component())
        sub.driver.workflow.add('comp3')

        top.driver.workflow.add(('comp1', 'driverA', 'driverB'))
        sout = StringIO.StringIO()
        top.recorders = [DumpCaseRecorder(sout)]

        top.driverA.workflow.add(('comp1', 'comp2'))
        top.driverB.workflow.add(('comp2', 'subassy'))

        top.run()

        expected = [
         'driverA.workflow.itername: 1-driverA.1',
         'driverA.workflow.itername: 1-driverA.2',
         'driverA.workflow.itername: 1-driverA.3',
         'driverA.workflow.itername: 1-driverA.4',
         'driverA.workflow.itername: 1-driverA.5',
         'subassy.driver.workflow.itername: 1-driverB.1-subassy.1',
         'driverB.workflow.itername: 1-driverB.1',
         'subassy.driver.workflow.itername: 1-driverB.2-subassy.1',
         'driverB.workflow.itername: 1-driverB.2',
         'subassy.driver.workflow.itername: 1-driverB.3-subassy.1',
         'driverB.workflow.itername: 1-driverB.3',
         'subassy.driver.workflow.itername: 1-driverB.4-subassy.1',
         'driverB.workflow.itername: 1-driverB.4',
         'driver.workflow.itername: 1'
         ]
        lines = [l.strip() for l in sout.getvalue().split('\n')
                                 if 'workflow.itername' in l]
        for i, line in enumerate(lines):
            self.assertEqual(line, expected[i])


if __name__ == '__main__':
    unittest.main()
