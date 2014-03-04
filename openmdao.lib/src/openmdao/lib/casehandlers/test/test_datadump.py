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
        self.top.printvars = ['*']
        self.top.run()

        expected = """\
Case: 
   uuid: 9f9407fc-9f20-11e3-9b48-005056000100
   timestamp: 1393444821.630434
   inputs:
      comp1.directory: 
      comp1.force_execute: False
      comp1.force_fd: False
      comp1.missing_deriv_policy: error
      comp1.x1: 0.0
      directory: 
      driver.directory: 
      driver.force_execute: True
      driver.force_fd: False
      driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x2fffd10>
      force_execute: False
      force_fd: False
      missing_deriv_policy: assume_zero
      nested.comp1.directory: 
      nested.comp1.force_execute: False
      nested.comp1.force_fd: False
      nested.comp1.missing_deriv_policy: error
      nested.comp1.x1: 0.0
      nested.comp2.directory: 
      nested.comp2.force_execute: False
      nested.comp2.force_fd: False
      nested.comp2.missing_deriv_policy: error
      nested.comp2.x1: 100.0
      nested.comp3.directory: 
      nested.comp3.force_execute: False
      nested.comp3.force_fd: False
      nested.comp3.missing_deriv_policy: error
      nested.comp3.x1: 10100.0
      nested.directory: 
      nested.doublenest.comp1.directory: 
      nested.doublenest.comp1.force_execute: False
      nested.doublenest.comp1.force_fd: False
      nested.doublenest.comp1.missing_deriv_policy: error
      nested.doublenest.comp1.x1: 0.0
      nested.doublenest.comp2.directory: 
      nested.doublenest.comp2.force_execute: False
      nested.doublenest.comp2.force_fd: False
      nested.doublenest.comp2.missing_deriv_policy: error
      nested.doublenest.comp2.x1: 100.0
      nested.doublenest.comp3.directory: 
      nested.doublenest.comp3.force_execute: False
      nested.doublenest.comp3.force_fd: False
      nested.doublenest.comp3.missing_deriv_policy: error
      nested.doublenest.comp3.x1: 10100.0
      nested.doublenest.directory: 
      nested.doublenest.driver.directory: 
      nested.doublenest.driver.force_execute: True
      nested.doublenest.driver.force_fd: False
      nested.doublenest.driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x3017110>
      nested.doublenest.force_execute: False
      nested.doublenest.force_fd: False
      nested.doublenest.missing_deriv_policy: assume_zero
      nested.doublenest.printvars: []
      nested.driver.directory: 
      nested.driver.force_execute: True
      nested.driver.force_fd: False
      nested.driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x2fffe90>
      nested.force_execute: False
      nested.force_fd: False
      nested.missing_deriv_policy: assume_zero
      nested.printvars: []
      printvars: ['*']
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 0
      comp1.itername: 
      comp1.y1: 0.0
      derivative_exec_count: 0
      driver.derivative_exec_count: 0
      driver.exec_count: 1
      driver.itername: 
      exec_count: 1
      itername: 
      nested.comp1.derivative_exec_count: 0
      nested.comp1.exec_count: 1
      nested.comp1.itername: 1-1.1-1
      nested.comp1.y1: 1.0
      nested.comp2.derivative_exec_count: 0
      nested.comp2.exec_count: 1
      nested.comp2.itername: 1-1.1-2
      nested.comp2.y1: 101.0
      nested.comp3.derivative_exec_count: 0
      nested.comp3.exec_count: 1
      nested.comp3.itername: 1-1.1-3
      nested.comp3.y1: 10101.0
      nested.derivative_exec_count: 0
      nested.doublenest.comp1.derivative_exec_count: 0
      nested.doublenest.comp1.exec_count: 1
      nested.doublenest.comp1.itername: 1-1.1-4.1-1
      nested.doublenest.comp1.y1: 1.0
      nested.doublenest.comp2.derivative_exec_count: 0
      nested.doublenest.comp2.exec_count: 1
      nested.doublenest.comp2.itername: 1-1.1-4.1-2
      nested.doublenest.comp2.y1: 101.0
      nested.doublenest.comp3.derivative_exec_count: 0
      nested.doublenest.comp3.exec_count: 1
      nested.doublenest.comp3.itername: 1-1.1-4.1-3
      nested.doublenest.comp3.y1: 10101.0
      nested.doublenest.derivative_exec_count: 0
      nested.doublenest.driver.derivative_exec_count: 0
      nested.doublenest.driver.exec_count: 1
      nested.doublenest.driver.itername: 1-1.1-4
      nested.doublenest.driver.workflow.itername: 1-1.1-4.1
      nested.doublenest.exec_count: 1
      nested.doublenest.itername: 1-1.1-4
      nested.driver.derivative_exec_count: 0
      nested.driver.exec_count: 1
      nested.driver.itername: 1-1
      nested.exec_count: 1
      nested.itername: 1-1
Case: 
   uuid: 9f9cc202-9f20-11e3-9b48-005056000100
   timestamp: 1393444821.687524
   inputs:
      comp1.directory: 
      comp1.force_execute: False
      comp1.force_fd: False
      comp1.missing_deriv_policy: error
      comp1.x1: 0.0
      directory: 
      driver.directory: 
      driver.force_execute: True
      driver.force_fd: False
      driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x2fffd10>
      force_execute: False
      force_fd: False
      missing_deriv_policy: assume_zero
      nested.comp1.directory: 
      nested.comp1.force_execute: False
      nested.comp1.force_fd: False
      nested.comp1.missing_deriv_policy: error
      nested.comp1.x1: 0.0
      nested.comp2.directory: 
      nested.comp2.force_execute: False
      nested.comp2.force_fd: False
      nested.comp2.missing_deriv_policy: error
      nested.comp2.x1: 100.0
      nested.comp3.directory: 
      nested.comp3.force_execute: False
      nested.comp3.force_fd: False
      nested.comp3.missing_deriv_policy: error
      nested.comp3.x1: 10100.0
      nested.directory: 
      nested.doublenest.comp1.directory: 
      nested.doublenest.comp1.force_execute: False
      nested.doublenest.comp1.force_fd: False
      nested.doublenest.comp1.missing_deriv_policy: error
      nested.doublenest.comp1.x1: 0.0
      nested.doublenest.comp2.directory: 
      nested.doublenest.comp2.force_execute: False
      nested.doublenest.comp2.force_fd: False
      nested.doublenest.comp2.missing_deriv_policy: error
      nested.doublenest.comp2.x1: 100.0
      nested.doublenest.comp3.directory: 
      nested.doublenest.comp3.force_execute: False
      nested.doublenest.comp3.force_fd: False
      nested.doublenest.comp3.missing_deriv_policy: error
      nested.doublenest.comp3.x1: 10100.0
      nested.doublenest.directory: 
      nested.doublenest.driver.directory: 
      nested.doublenest.driver.force_execute: True
      nested.doublenest.driver.force_fd: False
      nested.doublenest.driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x3017110>
      nested.doublenest.force_execute: False
      nested.doublenest.force_fd: False
      nested.doublenest.missing_deriv_policy: assume_zero
      nested.doublenest.printvars: []
      nested.driver.directory: 
      nested.driver.force_execute: True
      nested.driver.force_fd: False
      nested.driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x2fffe90>
      nested.force_execute: False
      nested.force_fd: False
      nested.missing_deriv_policy: assume_zero
      nested.printvars: []
      printvars: ['*']
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 0
      comp1.itername: 
      comp1.y1: 0.0
      derivative_exec_count: 0
      driver.derivative_exec_count: 0
      driver.exec_count: 1
      driver.itername: 
      exec_count: 1
      itername: 
      nested.comp1.derivative_exec_count: 0
      nested.comp1.exec_count: 1
      nested.comp1.itername: 1-1.1-1
      nested.comp1.y1: 1.0
      nested.comp2.derivative_exec_count: 0
      nested.comp2.exec_count: 1
      nested.comp2.itername: 1-1.1-2
      nested.comp2.y1: 101.0
      nested.comp3.derivative_exec_count: 0
      nested.comp3.exec_count: 1
      nested.comp3.itername: 1-1.1-3
      nested.comp3.y1: 10101.0
      nested.derivative_exec_count: 0
      nested.doublenest.comp1.derivative_exec_count: 0
      nested.doublenest.comp1.exec_count: 1
      nested.doublenest.comp1.itername: 1-1.1-4.1-1
      nested.doublenest.comp1.y1: 1.0
      nested.doublenest.comp2.derivative_exec_count: 0
      nested.doublenest.comp2.exec_count: 1
      nested.doublenest.comp2.itername: 1-1.1-4.1-2
      nested.doublenest.comp2.y1: 101.0
      nested.doublenest.comp3.derivative_exec_count: 0
      nested.doublenest.comp3.exec_count: 1
      nested.doublenest.comp3.itername: 1-1.1-4.1-3
      nested.doublenest.comp3.y1: 10101.0
      nested.doublenest.derivative_exec_count: 0
      nested.doublenest.driver.derivative_exec_count: 0
      nested.doublenest.driver.exec_count: 1
      nested.doublenest.driver.itername: 1-1.1-4
      nested.doublenest.exec_count: 1
      nested.doublenest.itername: 1-1.1-4
      nested.driver.derivative_exec_count: 0
      nested.driver.exec_count: 1
      nested.driver.itername: 1-1
      nested.driver.workflow.itername: 1-1.1
      nested.exec_count: 1
      nested.itername: 1-1
Case: 
   uuid: 9fa4902c-9f20-11e3-9b48-005056000100
   timestamp: 1393444821.738698
   inputs:
      comp1.directory: 
      comp1.force_execute: False
      comp1.force_fd: False
      comp1.missing_deriv_policy: error
      comp1.x1: 0.0
      directory: 
      driver.directory: 
      driver.force_execute: True
      driver.force_fd: False
      driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x2fffd10>
      force_execute: False
      force_fd: False
      missing_deriv_policy: assume_zero
      nested.comp1.directory: 
      nested.comp1.force_execute: False
      nested.comp1.force_fd: False
      nested.comp1.missing_deriv_policy: error
      nested.comp1.x1: 0.0
      nested.comp2.directory: 
      nested.comp2.force_execute: False
      nested.comp2.force_fd: False
      nested.comp2.missing_deriv_policy: error
      nested.comp2.x1: 100.0
      nested.comp3.directory: 
      nested.comp3.force_execute: False
      nested.comp3.force_fd: False
      nested.comp3.missing_deriv_policy: error
      nested.comp3.x1: 10100.0
      nested.directory: 
      nested.doublenest.comp1.directory: 
      nested.doublenest.comp1.force_execute: False
      nested.doublenest.comp1.force_fd: False
      nested.doublenest.comp1.missing_deriv_policy: error
      nested.doublenest.comp1.x1: 0.0
      nested.doublenest.comp2.directory: 
      nested.doublenest.comp2.force_execute: False
      nested.doublenest.comp2.force_fd: False
      nested.doublenest.comp2.missing_deriv_policy: error
      nested.doublenest.comp2.x1: 100.0
      nested.doublenest.comp3.directory: 
      nested.doublenest.comp3.force_execute: False
      nested.doublenest.comp3.force_fd: False
      nested.doublenest.comp3.missing_deriv_policy: error
      nested.doublenest.comp3.x1: 10100.0
      nested.doublenest.directory: 
      nested.doublenest.driver.directory: 
      nested.doublenest.driver.force_execute: True
      nested.doublenest.driver.force_fd: False
      nested.doublenest.driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x3017110>
      nested.doublenest.force_execute: False
      nested.doublenest.force_fd: False
      nested.doublenest.missing_deriv_policy: assume_zero
      nested.doublenest.printvars: []
      nested.driver.directory: 
      nested.driver.force_execute: True
      nested.driver.force_fd: False
      nested.driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x2fffe90>
      nested.force_execute: False
      nested.force_fd: False
      nested.missing_deriv_policy: assume_zero
      nested.printvars: []
      printvars: ['*']
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 0
      comp1.itername: 
      comp1.y1: 0.0
      derivative_exec_count: 0
      driver.derivative_exec_count: 0
      driver.exec_count: 1
      driver.itername: 
      driver.workflow.itername: 1
      exec_count: 1
      itername: 
      nested.comp1.derivative_exec_count: 0
      nested.comp1.exec_count: 1
      nested.comp1.itername: 1-1.1-1
      nested.comp1.y1: 1.0
      nested.comp2.derivative_exec_count: 0
      nested.comp2.exec_count: 1
      nested.comp2.itername: 1-1.1-2
      nested.comp2.y1: 101.0
      nested.comp3.derivative_exec_count: 0
      nested.comp3.exec_count: 1
      nested.comp3.itername: 1-1.1-3
      nested.comp3.y1: 10101.0
      nested.derivative_exec_count: 0
      nested.doublenest.comp1.derivative_exec_count: 0
      nested.doublenest.comp1.exec_count: 1
      nested.doublenest.comp1.itername: 1-1.1-4.1-1
      nested.doublenest.comp1.y1: 1.0
      nested.doublenest.comp2.derivative_exec_count: 0
      nested.doublenest.comp2.exec_count: 1
      nested.doublenest.comp2.itername: 1-1.1-4.1-2
      nested.doublenest.comp2.y1: 101.0
      nested.doublenest.comp3.derivative_exec_count: 0
      nested.doublenest.comp3.exec_count: 1
      nested.doublenest.comp3.itername: 1-1.1-4.1-3
      nested.doublenest.comp3.y1: 10101.0
      nested.doublenest.derivative_exec_count: 0
      nested.doublenest.driver.derivative_exec_count: 0
      nested.doublenest.driver.exec_count: 1
      nested.doublenest.driver.itername: 1-1.1-4
      nested.doublenest.exec_count: 1
      nested.doublenest.itername: 1-1.1-4
      nested.driver.derivative_exec_count: 0
      nested.driver.exec_count: 1
      nested.driver.itername: 1-1
      nested.exec_count: 1
      nested.itername: 1-1
"""
        expected = expected.split('\n')
        lines = sout.getvalue().split('\n')

        for line, template in zip(lines, expected):
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            elif template.startswith('   timestamp:'):
                self.assertTrue(line.startswith('   timestamp:'))
            elif 'gradient_options' in template:
                self.assertEqual(line.split('<')[0], template.split('<')[0])
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
        self.top.printvars = ['*comp1*']
        self.top.run()

        expected = """\
Case: 
   uuid: 4e5bcd76-9f24-11e3-9a3b-005056000100
   timestamp: 1393446403.352580
   inputs:
      comp1.directory: 
      comp1.force_execute: False
      comp1.force_fd: False
      comp1.missing_deriv_policy: error
      comp1.x1: 0.0
      nested.comp1.directory: 
      nested.comp1.force_execute: False
      nested.comp1.force_fd: False
      nested.comp1.missing_deriv_policy: error
      nested.comp1.x1: 0.0
      nested.doublenest.comp1.directory: 
      nested.doublenest.comp1.force_execute: False
      nested.doublenest.comp1.force_fd: False
      nested.doublenest.comp1.missing_deriv_policy: error
      nested.doublenest.comp1.x1: 0.0
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 0
      comp1.itername: 
      comp1.y1: 0.0
      nested.comp1.derivative_exec_count: 0
      nested.comp1.exec_count: 1
      nested.comp1.itername: 1-1.1-1
      nested.comp1.y1: 1.0
      nested.doublenest.comp1.derivative_exec_count: 0
      nested.doublenest.comp1.exec_count: 1
      nested.doublenest.comp1.itername: 1-1.1-4.1-1
      nested.doublenest.comp1.y1: 1.0
      nested.doublenest.driver.workflow.itername: 1-1.1-4.1
Case: 
   uuid: 4e5cb2a4-9f24-11e3-9a3b-005056000100
   timestamp: 1393446403.358414
   inputs:
      comp1.directory: 
      comp1.force_execute: False
      comp1.force_fd: False
      comp1.missing_deriv_policy: error
      comp1.x1: 0.0
      nested.comp1.directory: 
      nested.comp1.force_execute: False
      nested.comp1.force_fd: False
      nested.comp1.missing_deriv_policy: error
      nested.comp1.x1: 0.0
      nested.doublenest.comp1.directory: 
      nested.doublenest.comp1.force_execute: False
      nested.doublenest.comp1.force_fd: False
      nested.doublenest.comp1.missing_deriv_policy: error
      nested.doublenest.comp1.x1: 0.0
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 0
      comp1.itername: 
      comp1.y1: 0.0
      nested.comp1.derivative_exec_count: 0
      nested.comp1.exec_count: 1
      nested.comp1.itername: 1-1.1-1
      nested.comp1.y1: 1.0
      nested.doublenest.comp1.derivative_exec_count: 0
      nested.doublenest.comp1.exec_count: 1
      nested.doublenest.comp1.itername: 1-1.1-4.1-1
      nested.doublenest.comp1.y1: 1.0
      nested.driver.workflow.itername: 1-1.1
Case: 
   uuid: 4e5d8f94-9f24-11e3-9a3b-005056000100
   timestamp: 1393446403.364069
   inputs:
      comp1.directory: 
      comp1.force_execute: False
      comp1.force_fd: False
      comp1.missing_deriv_policy: error
      comp1.x1: 0.0
      nested.comp1.directory: 
      nested.comp1.force_execute: False
      nested.comp1.force_fd: False
      nested.comp1.missing_deriv_policy: error
      nested.comp1.x1: 0.0
      nested.doublenest.comp1.directory: 
      nested.doublenest.comp1.force_execute: False
      nested.doublenest.comp1.force_fd: False
      nested.doublenest.comp1.missing_deriv_policy: error
      nested.doublenest.comp1.x1: 0.0
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 0
      comp1.itername: 
      comp1.y1: 0.0
      driver.workflow.itername: 1
      nested.comp1.derivative_exec_count: 0
      nested.comp1.exec_count: 1
      nested.comp1.itername: 1-1.1-1
      nested.comp1.y1: 1.0
      nested.doublenest.comp1.derivative_exec_count: 0
      nested.doublenest.comp1.exec_count: 1
      nested.doublenest.comp1.itername: 1-1.1-4.1-1
      nested.doublenest.comp1.y1: 1.0
"""
        expected = expected.split('\n')
        lines = sout.getvalue().split('\n')

        for line, template in zip(lines, expected):
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            elif template.startswith('   timestamp:'):
                self.assertTrue(line.startswith('   timestamp:'))
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
        self.top.printvars = ['*']
        self.top.run()

        expected = """\
Case: 
   uuid: c06b420c-9f24-11e3-b076-005056000100
   timestamp: 1393446594.716126
   inputs:
      comp1.directory: 
      comp1.force_execute: False
      comp1.force_fd: False
      comp1.missing_deriv_policy: error
      comp1.x1: 0.0
      comp2.directory: 
      comp2.force_execute: False
      comp2.force_fd: False
      comp2.missing_deriv_policy: error
      comp2.x1: 100.0
      directory: 
      driver.directory: 
      driver.force_execute: True
      driver.force_fd: False
      driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x3a76bf0>
      force_execute: False
      force_fd: False
      missing_deriv_policy: assume_zero
      printvars: ['*']
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 1
      comp1.itername: 1-1
      comp1.y1: 1.0
      comp2.derivative_exec_count: 0
      comp2.exec_count: 1
      comp2.itername: 1-2
      comp2.y1: 101.0
      derivative_exec_count: 0
      driver.derivative_exec_count: 0
      driver.exec_count: 1
      driver.itername: 
      driver.workflow.itername: 1
      exec_count: 1
      itername: 
"""
        expected = expected.split('\n')
        lines = sout.getvalue().split('\n')

        for line, template in zip(lines, expected):
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
        self.top.printvars = ['*']
        self.top.run()

        expected = """\
Case: 
   uuid: fdcac244-9f24-11e3-83f2-005056000100
   timestamp: 1393446697.681743
   inputs:
      comp1.directory: 
      comp1.force_execute: False
      comp1.force_fd: False
      comp1.list_str: []
      comp1.missing_deriv_policy: error
      directory: 
      driver.directory: 
      driver.force_execute: True
      driver.force_fd: False
      driver.gradient_options: <openmdao.main.driver.GradientOptions object at 0x3c77f50>
      force_execute: False
      force_fd: False
      missing_deriv_policy: assume_zero
      printvars: ['*']
   outputs:
      comp1.derivative_exec_count: 0
      comp1.exec_count: 1
      comp1.itername: 1-1
      comp1.string: Testing
      derivative_exec_count: 0
      driver.derivative_exec_count: 0
      driver.exec_count: 1
      driver.itername: 
      driver.workflow.itername: 1
      exec_count: 1
      itername: 
"""
        expected = expected.split('\n')
        lines = sout.getvalue().split('\n')

        for line, template in zip(lines, expected):
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

        lines = [l.strip() for l in sout.getvalue().split('\n') if 'itername' in l]
        self.assertEqual(lines, ['driverA.workflow.itername: 1-1.1',
                                 'driverA.workflow.itername: 1-1.2',
                                 'driverA.workflow.itername: 1-1.3',
                                 'driverA.workflow.itername: 1-1.4',
                                 'driverA.workflow.itername: 1-1.5',
                                 'subassy.driver.workflow.itername: 1-2.1-2.1',
                                 'driverB.workflow.itername: 1-2.1',
                                 'driverB.workflow.itername: 1-2.2',
                                 'driverB.workflow.itername: 1-2.3',
                                 'driverB.workflow.itername: 1-2.4',
                                 'driver.workflow.itername: 1'])


if __name__ == '__main__':
    unittest.main()
