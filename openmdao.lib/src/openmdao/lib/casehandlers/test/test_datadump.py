"""
Tests the capability to use the case recorders to dump out larger amounts of information
from a driver's workflow.
"""
import StringIO
import unittest

from openmdao.lib.casehandlers.api import DumpCaseRecorder
from openmdao.lib.datatypes.api import Float, List, Str
from openmdao.main.api import Component, Assembly, Driver, Run_Once, Case, set_as_top

class Basic_Component(Component):
    ''' Basic building block'''
    
    x1 = Float(0.0, iotype='in')
    y1 = Float(iotype='out')
    
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
        
        self.connect('comp1.y1','comp2.x1')
        self.connect('comp2.y1','comp3.x1')
        
        
class Complex_Comp(Component):
    ''' Basic building block'''
    
    list_str = List(Str, iotype='in')
    
    string = Str('Testing', iotype='out')
    
    def execute(self):
        ''' pretty simple'''
        
        pass
        
class Run_N(Run_Once):
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
        
    def post_iteration(self):
        self.record_case()
        

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
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.driver.printvars = ['*']
        self.top.run()
        expected = [
            'Case: ',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   inputs:',
            '      driver.directory: ',
            '      driver.force_execute: True',
            "      driver.printvars: ['*']",
            '      nested.comp1.directory: ',
            '      nested.comp1.force_execute: False',
            '      nested.comp1.x1: 0.0',
            '      nested.comp2.directory: ',
            '      nested.comp2.force_execute: False',
            '      nested.comp2.x1: 1.0',
            '      nested.comp3.directory: ',
            '      nested.comp3.force_execute: False',
            '      nested.comp3.x1: 2.0',
            '      nested.directory: ',
            '      nested.doublenest.comp1.directory: ',
            '      nested.doublenest.comp1.force_execute: False',
            '      nested.doublenest.comp1.x1: 0.0',
            '      nested.doublenest.comp2.directory: ',
            '      nested.doublenest.comp2.force_execute: False',
            '      nested.doublenest.comp2.x1: 1.0',
            '      nested.doublenest.comp3.directory: ',
            '      nested.doublenest.comp3.force_execute: False',
            '      nested.doublenest.comp3.x1: 2.0',
            '      nested.doublenest.directory: ',
            '      nested.doublenest.force_execute: False',
            '      nested.force_execute: False',
            '   outputs:',
            '      driver.derivative_exec_count: 0',
            '      driver.exec_count: 1',
            '      driver.itername: ',
            '      driver.workflow.itername: 1',
            '      nested.comp1.derivative_exec_count: 0',
            '      nested.comp1.exec_count: 1',
            '      nested.comp1.itername: 1-1.1-1',
            '      nested.comp1.y1: 1.0',
            '      nested.comp2.derivative_exec_count: 0',
            '      nested.comp2.exec_count: 1',
            '      nested.comp2.itername: 1-1.1-2',
            '      nested.comp2.y1: 2.0',
            '      nested.comp3.derivative_exec_count: 0',
            '      nested.comp3.exec_count: 1',
            '      nested.comp3.itername: 1-1.1-3',
            '      nested.comp3.y1: 3.0',
            '      nested.derivative_exec_count: 0',
            '      nested.doublenest.comp1.derivative_exec_count: 0',
            '      nested.doublenest.comp1.exec_count: 1',
            '      nested.doublenest.comp1.itername: 1-1.1-4.1-1',
            '      nested.doublenest.comp1.y1: 1.0',
            '      nested.doublenest.comp2.derivative_exec_count: 0',
            '      nested.doublenest.comp2.exec_count: 1',
            '      nested.doublenest.comp2.itername: 1-1.1-4.1-2',
            '      nested.doublenest.comp2.y1: 2.0',
            '      nested.doublenest.comp3.derivative_exec_count: 0',
            '      nested.doublenest.comp3.exec_count: 1',
            '      nested.doublenest.comp3.itername: 1-1.1-4.1-3',
            '      nested.doublenest.comp3.y1: 3.0',
            '      nested.doublenest.derivative_exec_count: 0',
            '      nested.doublenest.exec_count: 1',
            '      nested.doublenest.itername: 1-1.1-4',
            '      nested.exec_count: 1',
            '      nested.itername: 1-1',
            ]
        lines = sout.getvalue().split('\n')
        
        for line, template in zip(lines, expected):
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            else:
                self.assertEqual(line, template)
        
    def test_nested_assy_match_wildcard(self):
        
        self.top.add('comp1', Basic_Component())
        self.top.add('nested', Nest_Me())
        self.top.driver.workflow.add('nested')
        self.top.nested.add('doublenest', Nest_Me())
        self.top.nested.driver.workflow.add('doublenest')
        
        sout = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.driver.printvars = ['*comp1*']
        self.top.run()
        expected = [
            'Case: ',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   inputs:',
            '      nested.comp1.directory: ',
            '      nested.comp1.force_execute: False',
            '      nested.comp1.x1: 0.0',
            '      nested.doublenest.comp1.directory: ',
            '      nested.doublenest.comp1.force_execute: False',
            '      nested.doublenest.comp1.x1: 0.0',
            '   outputs:',
            '      driver.workflow.itername: 1',
            '      nested.comp1.derivative_exec_count: 0',
            '      nested.comp1.exec_count: 1',
            '      nested.comp1.itername: 1-1.1-1',
            '      nested.comp1.y1: 1.0',
            '      nested.doublenest.comp1.derivative_exec_count: 0',
            '      nested.doublenest.comp1.exec_count: 1',
            '      nested.doublenest.comp1.itername: 1-1.1-4.1-1',
            '      nested.doublenest.comp1.y1: 1.0',
            ]
        lines = sout.getvalue().split('\n')
        
        for line, template in zip(lines, expected):
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
            else:
                self.assertEqual(line, template)

    def test_more_datatypes(self):
        
        self.top.add('comp1', Complex_Comp())
        self.top.driver.workflow.add('comp1')
        
        sout = StringIO.StringIO()
        self.top.driver.recorders = [DumpCaseRecorder(sout)]
        self.top.driver.printvars = ['*']
        self.top.run()
        expected = [
            'Case: ',
            '   uuid: ad4c1b76-64fb-11e0-95a8-001e8cf75fe',
            '   inputs:',
            '      comp1.directory: ',
            '      comp1.force_execute: False',
            '      comp1.list_str: []',
            '      driver.directory: ',
            '      driver.force_execute: True',
            "      driver.printvars: ['*']",
            '   outputs:',
            '      comp1.derivative_exec_count: 0',
            '      comp1.exec_count: 1',
            '      comp1.itername: 1-1',
            '      comp1.string: Testing',
            '      driver.derivative_exec_count: 0',
            '      driver.exec_count: 1',
            '      driver.itername: ',
            '      driver.workflow.itername: 1',
            ]
        lines = sout.getvalue().split('\n')
        
        for line, template in zip(lines, expected):
            if template.startswith('   uuid:'):
                self.assertTrue(line.startswith('   uuid:'))
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
        sout_sub = StringIO.StringIO()
        sub.driver.recorders = [DumpCaseRecorder(sout_sub)]
        sub.add('comp3', Basic_Component())
        sub.driver.workflow.add('comp3')

        top.driver.workflow.add(('comp1', 'driverA', 'driverB'))
        sout = StringIO.StringIO()
        top.driver.recorders = [DumpCaseRecorder(sout)]
        
        top.driverA.workflow.add(('comp1', 'comp2'))
        soutA = StringIO.StringIO()
        top.driverA.recorders = [DumpCaseRecorder(soutA)]
        top.driverB.workflow.add(('comp2', 'subassy'))
        soutB = StringIO.StringIO()
        top.driverB.recorders = [DumpCaseRecorder(soutB)]

        top.run()

        lines = [l.strip() for l in sout.getvalue().split('\n') if 'itername' in l]
        linesA = [l.strip() for l in soutA.getvalue().split('\n') if 'itername' in l]
        linesB = [l.strip() for l in soutB.getvalue().split('\n') if 'itername' in l]
        lines_sub = [l.strip() for l in sout_sub.getvalue().split('\n') if 'itername' in l]
        
        self.assertEqual(lines, ['driver.workflow.itername: 1'])
        self.assertEqual(linesA, ['driverA.workflow.itername: 1-1.1',
                                  'driverA.workflow.itername: 1-1.2',
                                  'driverA.workflow.itername: 1-1.3',
                                  'driverA.workflow.itername: 1-1.4',
                                  'driverA.workflow.itername: 1-1.5'])
        self.assertEqual(linesB, ['driverB.workflow.itername: 1-2.1',
                                  'driverB.workflow.itername: 1-2.2',
                                  'driverB.workflow.itername: 1-2.3',
                                  'driverB.workflow.itername: 1-2.4'])
        self.assertEqual(lines_sub, ['driver.workflow.itername: 1-2.1-2.1'])

        
if __name__ == '__main__':
    unittest.main()
