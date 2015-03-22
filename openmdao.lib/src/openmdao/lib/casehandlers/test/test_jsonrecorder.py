import bson
import json
import os
import tempfile
import re
import shutil
import sys
import unittest
import copy

from struct import unpack
from cStringIO import StringIO

from openmdao.main import __version__
from openmdao.main.api import Assembly, Component, Case, VariableTree, set_as_top
from openmdao.main.datatypes.api import Array, Instance, List, VarTree
from openmdao.test.execcomp import ExecComp
from openmdao.lib.casehandlers.api import JSONCaseRecorder, BSONCaseRecorder, verify_json, CaseDataset

from openmdao.lib.drivers.api import SensitivityDriver, CaseIteratorDriver, \
                                     SLSQPdriver
from openmdao.util.testutil import assert_raises


class TExecComp(ExecComp):
    data = Instance(iotype='in', desc='Used to check bad JSON data')


class Loads(VariableTree):
    Fx = Array()
    Fy = Array()
    Fz = Array()


class LoadsArray(VariableTree):
    loads = List(Loads)


class LoadsComp(Component):
    loads_in  = VarTree(LoadsArray(), iotype='in')
    loads_out = VarTree(LoadsArray(), iotype='out')

    def execute(self):
        self.loads_out = self.loads_in

class ComplexClass:
    def __init__(self, realpart, imagpart):
        self.r = realpart
        self.i = imagpart

class ClassWithNestedObjects:
    def __init__(self, cx1, cx2):
        self.cx1 = cx1
        self.cx2 = cx2

class CompWithNestedPythonObjects(Component):

    cx_nested_in = Instance(ClassWithNestedObjects(ComplexClass(1.0,2.0),ComplexClass(3.0,4.0)),
            iotype="in", desc="nested complex number")
    cx_nested_out = Instance(ClassWithNestedObjects(ComplexClass(5.0,6.0),ComplexClass(7.0,8.0)),
            iotype="out", desc="nested complex number")

    def execute(self):
        self.cx_nested_out = copy.deepcopy(self.cx_nested_in)


class TestCaseForNestedObjects(unittest.TestCase):

    def setUp(self):
        self.top = set_as_top(Assembly())
        self.top.add('comp1', CompWithNestedPythonObjects())
        self.top.driver.workflow.add(['comp1'])

    def tearDown(self):
        self.top = None

    def test_jsonrecorder_with_nest_object(self):
        # make sure bug 87740468 is fixed
        sout = StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]

        self.top.run()
        self.assertTrue(isinstance(self.top.comp1.cx_nested_in.cx1, ComplexClass)) # should NOT be a dict


class TestCase(unittest.TestCase):

    def setUp(self):
        self.top = top = set_as_top(Assembly())
        driver = top.add('driver', CaseIteratorDriver())
        top.add('comp1', TExecComp(exprs=['z=x+y']))
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

        self.tempdir = tempfile.mkdtemp(prefix='test_jsonrecorder-')

    def tearDown(self):
        try:
            shutil.rmtree(self.tempdir)
        except OSError:
            pass
        self.top = None

    def test_jsonrecorder_norun(self):
        # test ability to get model data from case recorder
        #    without calling run()

        sout = StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.record_configuration()

        if os.environ.get('REGEN_JSON_FILES'):
            with open('jsonrecorder_norun.new', 'w') as out:
                out.write(sout.getvalue())
        verify_json(self, sout, 'jsonrecorder_norun.json')

    def test_jsonrecorder(self):
        sout = StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.run()

        if os.environ.get('REGEN_JSON_FILES'):
            with open('jsonrecorder.new', 'w') as out:
                out.write(sout.getvalue())
        verify_json(self, sout, 'jsonrecorder.json')

    def test_multiple_objectives(self):
        sout = StringIO()
        self.top.add('driver', SensitivityDriver())
        self.top.driver.workflow.add(['comp1', 'comp2'])
        self.top.driver.add_parameter(['comp1.x'], low=-100, high=100)
        self.top.driver.add_objective('comp1.z')
        self.top.driver.add_objective('comp2.z')

        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.run()

        if os.environ.get('REGEN_JSON_FILES'):
            with open('multiobj.new', 'w') as out:
                out.write(sout.getvalue())
        verify_json(self, sout, 'multiobj.json')

    def test_nested(self):
        asm3 = Assembly()
        asm3.add('comp1', ExecComp(exprs=['z=x+y']))
        driver = asm3.add('driver', SLSQPdriver())
        driver.workflow.add('comp1')
        driver.add_parameter('comp1.y', low=-1, high=1, start=0)
        driver.add_objective('comp1.z')
        driver.add_constraint('comp1.z >= 0')
        asm3.create_passthrough('comp1.x')
        asm3.create_passthrough('comp1.z')

        asm2 = Assembly()
        asm2.add('comp1', ExecComp(exprs=['z=x+y']))
        asm2.add('asm3', asm3)
        asm2.connect('comp1.z', 'asm3.x')
        driver = asm2.add('driver', SLSQPdriver())
        driver.workflow.add(('comp1', 'asm3'))
        driver.add_parameter('comp1.y', low=-1, high=1, start=0)
        driver.add_objective('asm3.z')
        driver.add_constraint('comp1.z >= 0')
        asm2.create_passthrough('comp1.x')
        asm2.create_passthrough('asm3.z')

        asm1 = set_as_top(Assembly())
        asm1.add('comp1', ExecComp(exprs=['z=x+y']))
        asm1.add('asm2', asm2)
        asm1.connect('comp1.z', 'asm2.x')
        driver = asm1.add('driver', SLSQPdriver())
        driver.workflow.add(('comp1', 'asm2'))
        driver.add_parameter('comp1.y', low=-1, high=1, start=0)
        driver.add_objective('asm2.z')
        driver.add_constraint('comp1.z >= 0')

        sout = StringIO()
        asm1.recorders = [JSONCaseRecorder(sout)]
        asm1.run()

        if os.environ.get('REGEN_JSON_FILES'):
            with open('nested.new', 'w') as out:
               out.write(sout.getvalue())
        verify_json(self, sout, 'nested.json')

    def test_close(self):
        sout = StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.recorders[0].close()
        self.top.run()
        self.assertEqual(sout.getvalue(), '')

    def test_badval(self):
        sout = StringIO()
        self.top.recorders = [JSONCaseRecorder(sout)]
        self.top.comp1.data = self.test_badval.__func__.__code__
        assert_raises(self, 'self.top.run()', globals(), locals(), RuntimeError,
                      "JSON write failed for simulation_info.constants:"
                      " keys ['comp1.data']: <code object test_badval at")

    def test_bsonrecorder(self):
        # Verify bson output can be read and that it matches json.
        bson_out = StringIO()
        json_out = StringIO()
        self.top.recorders = [BSONCaseRecorder(bson_out),
                              JSONCaseRecorder(json_out)]
        self.top.run()

        json_run = json.loads(json_out.getvalue())

        inp = StringIO(bson_out.getvalue())
        reclen = unpack('<L', inp.read(4))[0]
        data = inp.read(reclen)
        obj = bson.loads(data)  # simulation_info
        keys = sorted(obj.keys())
        self.assertEqual(keys, sorted(json_run['simulation_info'].keys()))
        for key in keys:
            # graph sometimes serializes with nodes in differant order
            # between json and bson. The graphs are still equivalent, but the
            # assertion below will fail
            if key not in ('uuid', 'graph',):
                self.assertEqual(obj[key], json_run['simulation_info'][key])

        driver_count = 1
        case_count = 1
        data = inp.read(4)
        while data:
            reclen = unpack('<L', data)[0]
            data = inp.read(reclen)
            obj = bson.loads(data)  # driver_info or iteration_case
            keys = sorted(obj.keys())

            if '_driver_id' in obj:  # iteration_case
                case = 'iteration_case_%s' % case_count
                self.assertEqual(keys, sorted(json_run[case].keys()))
                for key in keys:
                    if key not in ('_driver_id', '_id', '_parent_id',
                                   'timestamp'):
                        self.assertEqual(obj[key], json_run[case][key])
                case_count += 1
            else:  # driver_info
                driver = 'driver_info_%s' % driver_count
                self.assertEqual(keys, sorted(json_run[driver].keys()))
                for key in keys:
                    if key not in ('_id',):
                        self.assertEqual(obj[key], json_run[driver][key])
                driver_count += 1

            data = inp.read(4)

    def test_vtree(self):
        top = Assembly()
        sub = top.add('sub', Assembly())
        sub.add('comp', LoadsComp())
        sub.driver.workflow.add('comp')
        sub.create_passthrough('comp.loads_in')
        sub.create_passthrough('comp.loads_out')
        top.driver.workflow.add('sub')


        jsonfile = os.path.join(self.tempdir, 'test_vtree.json')
        old_json_file = os.path.join(os.path.dirname(__file__), 'vtree.json')
        top.recorders = [JSONCaseRecorder(jsonfile)]

        loads = Loads()
        loads.Fx = [1, 2, 3]
        loads.Fy = [4, 5, 6]
        loads.Fz = [7, 8, 9]
        arr = LoadsArray()
        arr.loads = [loads]
        top.sub.loads_in = arr

        top.run()

        cdsnew = CaseDataset(jsonfile, 'json')
        cdsold = CaseDataset(old_json_file, 'json')


        cdsold.data.vars('sub.comp.loads_out').fetch()[0][0]['loads'][0]['Fx'] == cdsnew.data.vars('sub.comp.loads_out').fetch()[0][0]['loads'][0]['Fx']
        cdsold.data.vars('sub.comp.loads_out').fetch()[1][0]['loads'][0]['Fz'] == cdsnew.data.vars('sub.comp.loads_out').fetch()[1][0]['loads'][0]['Fz']


if __name__ == '__main__':
    unittest.main()
