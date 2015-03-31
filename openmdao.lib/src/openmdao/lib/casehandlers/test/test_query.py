"""
Test query of case recorder file.
"""

import glob
import os.path
import tempfile
import shutil
import unittest

from math import isnan

import numpy as np

from openmdao.main.api import Assembly, Component, VariableTree, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree
from openmdao.lib.casehandlers.api import CaseDataset, \
                                          JSONCaseRecorder, BSONCaseRecorder
from openmdao.lib.drivers.api import FixedPointIterator, SLSQPdriver
from openmdao.lib.optproblems import sellar
from openmdao.util.testutil import assert_rel_error


class States(VariableTree):
    y = Array([0.0, 0.0])


class Globals(VariableTree):
    z1 = Float(0.0)
    z2 = Float(0.0)


class Half(Component):
    z2a = Float(0.0, iotype='in')
    z2b = Float(0.0, iotype='out')

    def execute(self):
        self.z2b = 0.5*self.z2a


class SellarMDF(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with FixedPointIterator.
    """

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        # Sub assembly
        sub = self.add('sub', Assembly())

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        sub.add('driver', FixedPointIterator())
        sub.add('dis1', sellar.Discipline1())
        sub.add('dis2', sellar.Discipline2())
        sub.driver.workflow.add(['dis1', 'dis2'])

        # Make all connections
        sub.connect('dis1.y1', 'dis2.y1')
        sub.connect('dis1.z1', 'dis2.z1')

        # Iteration loop
        sub.driver.add_parameter('dis1.y2')
        sub.driver.add_constraint('dis2.y2 = dis1.y2')

        # Solver settings
        sub.driver.max_iteration = 100
        sub.driver.tolerance = .00001
        sub.driver.print_convergence = False

        # Subassy boundaries
        sub.add('globals', VarTree(Globals(), iotype='in'))
        sub.add('states', VarTree(States(), iotype='out'))
        sub.connect('globals.z1', 'dis1.z1')
        # Note, dis1.z2 is connected by input-input conn
        sub.connect('globals.z2', 'dis1.z2')
        sub.connect('globals.z2', 'dis2.z2')
        sub.create_passthrough('dis1.x1')
        sub.connect('dis1.y1', 'states.y[0]')
        sub.connect('dis2.y2', 'states.y[1]')

        # Global Optimization
        self.add('driver', SLSQPdriver())
        self.driver.gradient_options.force_fd = True
        #self.driver.iprint = 3

        # Extra comp
        self.add('half', Half())
        self.connect('half.z2b', 'sub.globals.z2')

        self.driver.workflow.add(['half', 'sub'])

        # Add Parameters to optimizer
        self.driver.add_parameter('sub.globals.z1', low=-10.0, high=10.0)
        self.driver.add_parameter('half.z2a', low=0.0, high=10.0)
        self.driver.add_parameter('sub.x1', low=0.0, high=10.0)

        # Optimization parameters
        self.driver.add_objective('(sub.x1)**2 + sub.globals.z2 + sub.states.y[0] + math.exp(-sub.states.y[1])')

        self.driver.add_constraint('3.16 < sub.states.y[0]')
        self.driver.add_constraint('sub.states.y[1] < 24.0')

        self.sub.globals.z1 = 5.0
        self.half.z2a = 2.0
        self.sub.x1 = 1.0


def create_files():
    """ Create/update test data files. """
    prob = set_as_top(SellarMDF())
    #prob.name = "top"
    prob.recorders = [JSONCaseRecorder('sellar_json.new'),
                      BSONCaseRecorder('sellar_bson.new'),
                      ]
    prob.run()


class TestCase(unittest.TestCase):

    def setUp(self):
        create_files()  # Uncomment to create 'sellar.new'
        path = os.path.join(os.path.dirname(__file__), 'sellar.json')
        self.cds = CaseDataset(path, 'json')
        self.startdir = os.getcwd()
        self.tempdir = tempfile.mkdtemp(prefix='test_query-')
        os.chdir(self.tempdir)

    def tearDown(self):
        self.cds = None
        os.chdir(self.startdir)
        if not os.environ.get('OPENMDAO_KEEPDIRS', False):
            try:
                shutil.rmtree(self.tempdir)
            except OSError:
                pass


    def test_query(self):
        # Full dataset.
        vnames = self.cds.data.var_names().fetch()
        expected = ['_driver_id', '_id', '_parent_id', u'_pseudo_0', u'_pseudo_0.out0', u'_pseudo_1', u'_pseudo_1.out0', u'_pseudo_2', u'_pseudo_2.out0', u'driver.workflow.itername', 'error_message', 'error_status', u'half.derivative_exec_count', u'half.exec_count', u'half.itername', u'half.z2a', u'half.z2b', u'sub._pseudo_0', u'sub._pseudo_0.out0', u'sub.derivative_exec_count', u'sub.dis1.derivative_exec_count', u'sub.dis1.exec_count', u'sub.dis1.itername', u'sub.dis1.y1', u'sub.dis1.y2', u'sub.dis2.derivative_exec_count', u'sub.dis2.exec_count', u'sub.dis2.itername', u'sub.dis2.y2', u'sub.driver.workflow.itername', u'sub.exec_count', u'sub.globals.z1', u'sub.itername', u'sub.states', u'sub.states.y[0]', u'sub.states.y[1]', u'sub.x1', 'timestamp']

        self.assertEqual(vnames, expected)

        cases = self.cds.data.fetch()
        self.assertEqual(len(cases), 242)
        self.assertEqual(len(cases[0]), len(expected))

        # Specific variables.
        names = ['half.z2a', 'sub.globals.z1', 'sub.x1']
        vnames = self.cds.data.vars(names).var_names().fetch()
        self.assertEqual(vnames, names)

        cases = self.cds.data.vars(names).fetch()
        self.assertEqual(len(cases), 242)
        self.assertEqual(len(cases[0]), len(names))

        iteration_case_242 = {
            "half.z2a": -9.1082956132942171e-13,
            "sub.globals.z1": 1.9776387704500034,
            "sub.x1": 1.7062385906271342e-14
        }
        for name, val in zip(names, cases[-1]):
            self.assertAlmostEqual(val, iteration_case_242[name])

        # Transposed.
        vars = self.cds.data.local().vars(names).by_variable().fetch()
        self.assertEqual(len(vars), len(names))
        for name in ('half.z2a', 'sub.x1'):
            self.assertEqual(len(vars[name]), 242)
            self.assertTrue(not isnan(vars[name][-1]))
        for name in ('sub.globals.z1', 'sub.x1'):
            self.assertEqual(len(vars[name]), 242)
            assert_rel_error(self, vars[name][-1], iteration_case_242[name], 0.001)

    def test_parent(self):
        # Full dataset names by specifying a top-level case.
        parent = 'a00d3f9e-86ba-11e4-8001-20c9d0478eff'  # iteration_case_6

        vnames = self.cds.data.parent_case(parent).var_names().fetch()
        expected = [u'half.z2a', u'half.z2b', u'sub.dis1.y1',
                    u'sub.itername', u'_pseudo_1', u'sub.dis1.itername',
                    u'sub.globals.z1', u'sub.dis1.derivative_exec_count', u'sub.dis1.y2',
                    u'half.exec_count', u'sub.driver.workflow.itername',
                    u'sub.states.y[1]', u'sub._pseudo_0.out0',
                    u'sub.derivative_exec_count', u'half.derivative_exec_count',
                    'error_status', 'timestamp', u'sub.exec_count', u'sub.states',
                    u'half.itername', u'sub.dis1.exec_count', u'sub.states.y[0]',
                    u'sub.dis2.y2', u'_pseudo_1.out0', u'driver.workflow.itername',
                    u'_pseudo_0', u'_pseudo_2', u'_pseudo_0.out0', u'_pseudo_2.out0',
                    '_id', u'sub.dis2.derivative_exec_count', 'error_message',
                    '_driver_id', u'sub.dis2.itername', u'sub.dis2.exec_count',
                    u'sub.x1', u'sub._pseudo_0', '_parent_id']

        #####self.assertEqual(vnames, expected)


        cases = self.cds.data.parent_case(parent).fetch()
        self.assertEqual(len(cases), 6)
        self.assertEqual(len(cases[0]), len(expected))

        iteration_case_1 = {
            "sub._pseudo_0": 10.176871642217915,
            "sub.dis1.derivative_exec_count": 0,
            "sub.dis1.exec_count": 1,
            "sub.dis1.itername": "1-sub.1-dis1",
            "sub.dis1.y1": 26.8,
            "sub.dis1.y2": 1.0,
            "sub.dis2.derivative_exec_count": 0,
            "sub.dis2.exec_count": 1,
            "sub.dis2.itername": "1-sub.1-dis2",
            "sub.dis2.y2": 11.176871642217915,
            "sub.driver.workflow.itername": "1-sub.1"
        }
        self.verify(vnames, cases[0], iteration_case_1)

        iteration_case_6 = {
            # Data from parent.
            "_pseudo_0": 26.803946487677322,
            "_pseudo_1": -21.643929454616536,
            "_pseudo_2": -13.019645649693533,
            "driver.workflow.itername": "1",
            "half.derivative_exec_count": 0,
            "half.exec_count": 1,
            "half.itername": "1-half",
            "half.z2a": 2.0,
            "half.z2b": 1.0,
            "sub.derivative_exec_count": 0,
            "sub.exec_count": 1,
            "sub.globals.z1": 5.0,
            "sub.itername": "1-sub",
            "sub.states": {
                "y": [
                    24.803929454616537,
                    10.980354350306467
                ]
            },
            "sub.states.y[0]": 24.803929454616537,
            "sub.states.y[1]": 10.980354350306467,
            "sub.x1": 1.0,

            # Last data from sub.
            "sub._pseudo_0": 1.6233891457773097e-06,
            "sub.dis1.derivative_exec_count": 0,
            "sub.dis1.exec_count": 5,
            "sub.dis1.itername": "1-sub.5-dis1",
            "sub.dis1.y1": 24.803929454616537,
            "sub.dis1.y2": 10.980352726917321,
            "sub.dis2.derivative_exec_count": 0,
            "sub.dis2.exec_count": 5,
            "sub.dis2.itername": "1-sub.5-dis2",
            "sub.dis2.y2": 10.980354350306467,
            "sub.driver.workflow.itername": "1-sub.5"
        }
        self.verify(vnames, cases[-1], iteration_case_6)

    def verify(self, names, case, expected):
        for name, value in expected.items():
            i = names.index(name)
            if isinstance(value, float):
                assert_rel_error(self, case[i], value, 0.001)
            elif isinstance(value, dict):
                self.verify(value.keys(), case[name], expected[name])
            elif isinstance(value, np.ndarray) or isinstance(value, list):
                for i, val in enumerate(value):
                    assert_rel_error(self, case[name][i], value[i], 0.001)
            else:
                self.assertEqual(case[i], value)

    def test_driver(self):
        # Dataset of a driver.
        vnames = self.cds.data.driver('sub.driver').var_names().fetch()
        expected = ['_driver_id', '_id', '_parent_id', 'error_message',
				'error_status', u'sub._pseudo_0', u'sub._pseudo_0.out0',
				'sub.dis1.derivative_exec_count', u'sub.dis1.exec_count',
				u'sub.dis1.itername', u'sub.dis1.y1', u'sub.dis1.y2',
				'sub.dis2.derivative_exec_count', u'sub.dis2.exec_count',
				u'sub.dis2.itername', u'sub.dis2.y2',
				u'sub.driver.workflow.itername', 'timestamp']
        self.assertEqual(vnames, expected)

        cases = self.cds.data.driver('sub.driver').fetch()
        self.assertEqual(len(cases), 184)
        self.assertEqual(len(cases[0]), len(expected))

    def test_bson(self):
        # Simple check of _BSONReader.
        names = ['half.z2a', 'sub.globals.z1', 'sub.x1']

        path = os.path.join(os.path.dirname(__file__), 'sellar.json')
        json_cases = CaseDataset(path, 'json').data.vars(names).fetch()

        path = os.path.join(os.path.dirname(__file__), 'sellar.bson')
        bson_cases = CaseDataset(path, 'bson').data.vars(*names).fetch()

        for json_case, bson_case in zip(json_cases, bson_cases):
            for json_val, bson_val in zip(json_case, bson_case):
                if isnan(json_val):
                    self.assertTrue(isnan(bson_val))
                else:
                    self.assertEqual(bson_val, json_val)

    def test_json(self):
        # Simple check of _JSONReader.
        path = os.path.join(os.path.dirname(__file__), 'jsonrecorder.json')
        cases = CaseDataset(path, 'json').data.fetch()
        self.assertEqual(len(cases), 10)

        path = os.path.join(os.path.dirname(__file__), 'truncated.json')
        cases = CaseDataset(path, 'json').data.fetch()
        self.assertEqual(len(cases), 7)

    def test_restore(self):
        # Restore from case, run, verify outputs match expected.
        top = set_as_top(SellarMDF())
        #top.name = 'top'
        top.recorders = [JSONCaseRecorder()]
        top.run()
        assert_rel_error(self, top.sub.globals.z1, 1.977639, .0001)
        assert_rel_error(self, top.half.z2a, 0., .0001)
        assert_rel_error(self, top.sub.x1, 0., .0001)
        assert_rel_error(self, top.sub.states.y[0], 3.160004, .0001)
        assert_rel_error(self, top.sub.states.y[1], 3.755280, .0001)
        assert_rel_error(self, top.driver.eval_objective(), 3.18339413394, .0001)

        cds = CaseDataset('cases.json', 'json')
        cases = cds.data.fetch()
        n_orig = len(cases)  # Typically 142

        top = set_as_top(SellarMDF())
        top._setup()
        cds.restore(top, cases[-1]['_id'])
        top.recorders = [JSONCaseRecorder('cases.restored')]
        top.run()
        assert_rel_error(self, top.sub.globals.z1, 1.977639, .0001)
        assert_rel_error(self, top.half.z2a, 0., .0001)
        assert_rel_error(self, top.sub.x1, 0., .0001)
        assert_rel_error(self, top.sub.states.y[0], 3.160000, .0001)
        assert_rel_error(self, top.sub.states.y[1], 3.755278, .0001)
        assert_rel_error(self, top.driver.eval_objective(), 3.18339397762, .0001)

        cases = CaseDataset('cases.restored', 'json').data.fetch()
        # Exact case counts are unreliable, just assure restore was quicker.
        self.assertTrue(len(cases) < n_orig/4)   # Typically 15

    def test_write(self):
        # Read in a dataset and write out a selected portion of it.
        path = os.path.join(os.path.dirname(__file__), 'jsonrecorder.json')
        cases = CaseDataset(path, 'json').data.fetch()
        self.assertEqual(len(cases), 10)
        self.assertEqual(len(cases[0]),19)

        names = ('comp1.x', 'comp1.y', 'comp1.z', 'comp2.z')
        CaseDataset(path, 'json').data.vars(names).write('cases.reduced')
        reduced = CaseDataset('cases.reduced', 'json').data.fetch()
        self.assertEqual(len(reduced), 10)
        self.assertEqual(len(reduced[0]), 10)


if __name__ == '__main__':
    unittest.main()
