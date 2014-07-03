"""
Test query of case recorder file.
"""

import glob
import os.path
import unittest

from math import isnan

from openmdao.main.api import Assembly, set_as_top
from openmdao.main.datatypes.api import Array
from openmdao.lib.casehandlers.api import CaseDataset, \
                                          JSONCaseRecorder, BSONCaseRecorder
from openmdao.lib.drivers.api import SLSQPdriver
from openmdao.lib.optproblems import sellar
from openmdao.util.testutil import assert_rel_error


class SellarCO(Assembly):
    """ Solution of the sellar analytical problem using CO. """

    global_des_var_targets = Array([5.0, 2.0], iotype='in')
    local_des_var_targets = Array([1.0], iotype='in')
    coupling_var_targets = Array([3.16, 0.0], iotype='in')

    def __init__(self, ext='.new'):
        self._ext = ext
        super(SellarCO, self).__init__()

    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339"""

        self.recorders = [JSONCaseRecorder('sellar_json'+self._ext),
                          BSONCaseRecorder('sellar_bson'+self._ext)]

        # Global Optimization
        self.add('driver', SLSQPdriver())
        self.add('localopt1', SLSQPdriver())
        self.add('localopt2', SLSQPdriver())
        self.driver.workflow.add(['localopt2',
                                  'localopt1'])

        # Local Optimization 1
        self.add('dis1', sellar.Discipline1())

        # Local Optimization 2
        self.add('dis2', sellar.Discipline2())

        #Parameters - Global Optimization
        self.driver.add_objective('(local_des_var_targets[0])**2 + '
                                  'global_des_var_targets[1] + '
                                  'coupling_var_targets[0] + '
                                  'math.exp(-coupling_var_targets[1])')

        self.driver.add_parameter('global_des_var_targets[0]', low=-10.0, high=10.0)
        self.driver.add_parameter('global_des_var_targets[1]', low=0.0, high=10.0)
        self.driver.add_parameter('local_des_var_targets[0]', low=0.0, high=10.0)
        self.driver.add_parameter('coupling_var_targets[0]', low=3.16, high=10.0)
        self.driver.add_parameter('coupling_var_targets[1]', low=-10.0, high=24.0)

        con1 = '(global_des_var_targets[0] - dis1.z1)**2 + ' \
               '(global_des_var_targets[1] - dis1.z2)**2 + ' \
               '(local_des_var_targets[0] - dis1.x1)**2 + ' \
               '(coupling_var_targets[0] - dis1.y1)**2 + ' \
               '(coupling_var_targets[1] - dis1.y2)**2 <= 0'

        con2 = '(global_des_var_targets[0] - dis2.z1)**2 + ' \
               '(global_des_var_targets[1] - dis2.z2)**2 + ' \
               '(coupling_var_targets[0] - dis2.y1)**2 + ' \
               '(coupling_var_targets[1] - dis2.y2)**2 <= 0'

        self.driver.add_constraint(con1)
        self.driver.add_constraint(con2)

        self.driver.iprint = 0

        # Parameters - Local Optimization 1
        self.localopt1.add_objective('(global_des_var_targets[0] - dis1.z1)**2 + '
                                     '(global_des_var_targets[1] - dis1.z2)**2 + '
                                     '(local_des_var_targets[0] - dis1.x1)**2 + '
                                     '(coupling_var_targets[0] - dis1.y1)**2 + '
                                     '(coupling_var_targets[1] - dis1.y2)**2')

        self.localopt1.add_parameter('dis1.x1', low=0.0, high=10.0)
        self.localopt1.add_parameter('dis1.z1', low=-10.0, high=10.0)
        self.localopt1.add_parameter('dis1.z2', low=0.0, high=10.0)
        self.localopt1.add_parameter('dis1.y2', low=-1e99, high=1e99)
        self.localopt1.add_constraint('dis1.y1 > 3.16')
        self.localopt1.iprint = 0

        # Parameters - Local Optimization 2
        self.localopt2.add_objective('(global_des_var_targets[0] - dis2.z1)**2 + ' \
                                     '(global_des_var_targets[1] - dis2.z2)**2 + ' \
                                     '(coupling_var_targets[0] - dis2.y1)**2 + ' \
                                     '(coupling_var_targets[1] - dis2.y2)**2')
        self.localopt2.add_parameter('dis2.z1', low=-10.0, high=10.0)
        self.localopt2.add_parameter('dis2.z2', low=0.0, high=10.0)
        self.localopt2.add_parameter('dis2.y1', low=-1e99, high=1e99)
        self.localopt2.add_constraint('dis2.y2 < 24.0')
        self.localopt2.iprint = 0


def create_files():
    """ Create/update test data files. """
    prob = set_as_top(SellarCO())

    prob.dis1.z1 = 5.0
    prob.dis2.z1 = 5.0

    prob.dis1.z2 = 2.0
    prob.dis2.z2 = 2.0

    prob.dis1.x1 = 1.0

    prob.dis1.y2 = 1.0
    prob.dis2.y1 = 1.0

    prob.global_des_var_targets = [5.0, 2.0]
    prob.local_des_var_targets = [1.0,]
    prob.coupling_var_targets = [1.0, 1.0]

    prob.run()


class TestCase(unittest.TestCase):

    def setUp(self):
#        create_files()  # Uncomment to create 'sellar.new'
        path = os.path.join(os.path.dirname(__file__), 'sellar.json')
        self.cds = CaseDataset(path, 'json')

    def tearDown(self):
        self.cds = None
        for path in glob.glob(os.path.join(os.path.dirname(__file__),
                                           'sellar_*.restore')):
            os.remove(path)

    def test_query(self):
        # Full dataset.
        vnames = self.cds.data.var_names().fetch()
        expected = [
            '_driver_id', '_id', '_parent_id', '_pseudo_0', '_pseudo_1',
            '_pseudo_2', '_pseudo_3', '_pseudo_4', '_pseudo_5', '_pseudo_6',
            'coupling_var_targets[0]', 'coupling_var_targets[1]',
            'dis1.derivative_exec_count', 'dis1.exec_count', 'dis1.itername',
            'dis1.x1', 'dis1.y1', 'dis1.y2', 'dis1.z1', 'dis1.z2',
            'dis2.derivative_exec_count', 'dis2.exec_count', 'dis2.itername',
            'dis2.y1', 'dis2.y2', 'dis2.z1', 'dis2.z2',
            'driver.workflow.itername', 'error_message', 'error_status',
            'global_des_var_targets[0]', 'global_des_var_targets[1]',
            'local_des_var_targets[0]', 'localopt1.derivative_exec_count',
            'localopt1.error_code', 'localopt1.exec_count',
            'localopt1.itername', 'localopt1.workflow.itername',
            'localopt2.derivative_exec_count', 'localopt2.error_code',
            'localopt2.exec_count', 'localopt2.itername',
            'localopt2.workflow.itername', 'timestamp']
        self.assertEqual(vnames, expected)

        cases = self.cds.data.fetch()
        self.assertEqual(len(cases), 1826)
        self.assertEqual(len(cases[0]), len(expected))

        # Specific variables.
        names = ['dis1.x1', 'dis1.y1', 'dis1.y2', 'dis1.z1', 'dis1.z2']
        vnames = self.cds.data.vars(names).var_names().fetch()
        self.assertEqual(vnames, names)

        cases = self.cds.data.vars(names).fetch()
        self.assertEqual(len(cases), 1826)
        self.assertEqual(len(cases[0]), len(names))

        iteration_case_1826 = {
            "dis1.x1": 0.0094360174925073158,
            "dis1.y1": 3.1600005559017141,
            "dis1.y2": 3.7528188943779459,
            "dis1.z1": 1.9750937573344864,
            "dis1.z2": 0.00013296702313628215,
        }
        for name, val in zip(names, cases[-1]):
            assert_rel_error(self, val, iteration_case_1826[name], 0.001)

        # Local to driver.
        cases = self.cds.data.local().vars(names).fetch()
        self.assertEqual(len(cases), 1826)
        self.assertEqual(len(cases[0]), len(names))
        for val in cases[0]:  # localopt2
            self.assertTrue(isnan(val))
        self.assertEqual(len(cases[-1]), len(names))
        for val in cases[-1]: # top
            self.assertTrue(isnan(val))

        iteration_case_69 = { # localopt1
            "dis1.x1": 1.0,
            "dis1.y1": 27.8,
            "dis1.y2": 1.0,
            "dis1.z1": 5.0,
            "dis1.z2": 2.0,
        }
        for name, val in zip(names, cases[68]):
            self.assertEqual(val, iteration_case_69[name])

        self.assertEqual(cases[68]['dis1.y1'], iteration_case_69['dis1.y1'])

        # Transposed.
        vars = self.cds.data.local().vars(names).by_variable().fetch()
        self.assertEqual(len(vars), len(names))
        self.assertEqual(len(vars[0]), 1826)

        iteration_case_69 = { # localopt1
            "dis1.x1": 1.0,
            "dis1.y1": 27.8,
            "dis1.y2": 1.0,
            "dis1.z1": 5.0,
            "dis1.z2": 2.0,
        }
        for i, var in enumerate(vars):
            self.assertTrue(isnan(var[0]))  # localopt2
            self.assertTrue(isnan(var[-1])) # top
            assert_rel_error(self, var[68], iteration_case_69[names[i]], 0.001)

        self.assertEqual(vars['dis1.y1'][68], iteration_case_69['dis1.y1'])

    def test_parent(self):
        # Full dataset names by specifying a top-level case.
        parent = '796bf5d1-012c-11e4-8e45-005056000100'  # iteration_case_78
        vnames = self.cds.data.parent_case(parent).var_names().fetch()
        expected = [
            '_driver_id', '_id', '_parent_id', '_pseudo_0', '_pseudo_1',
            '_pseudo_2', '_pseudo_3', '_pseudo_4', '_pseudo_5', '_pseudo_6',
            'coupling_var_targets[0]', 'coupling_var_targets[1]',
            'dis1.derivative_exec_count', 'dis1.exec_count', 'dis1.itername',
            'dis1.x1', 'dis1.y1', 'dis1.y2', 'dis1.z1', 'dis1.z2',
            'dis2.derivative_exec_count', 'dis2.exec_count', 'dis2.itername',
            'dis2.y1', 'dis2.y2', 'dis2.z1', 'dis2.z2',
            'driver.workflow.itername', 'error_message', 'error_status',
            'global_des_var_targets[0]', 'global_des_var_targets[1]',
            'local_des_var_targets[0]', 'localopt1.derivative_exec_count',
            'localopt1.error_code', 'localopt1.exec_count',
            'localopt1.itername', 'localopt1.workflow.itername',
            'localopt2.derivative_exec_count', 'localopt2.error_code',
            'localopt2.exec_count', 'localopt2.itername',
            'localopt2.workflow.itername', 'timestamp']
        self.assertEqual(vnames, expected)

        cases = self.cds.data.parent_case(parent).fetch()
        self.assertEqual(len(cases), 78)
        self.assertEqual(len(cases[0]), len(expected))

        iteration_case_1 = {
            "_pseudo_5": 49.0,
            "_pseudo_6": -16.0,
            "dis2.derivative_exec_count": 0,
            "dis2.exec_count": 1,
            "dis2.itername": "1-localopt2.1-dis2",
            "dis2.y1": 1.0,
            "dis2.y2": 8.0,
            "dis2.z1": 5.0,
            "dis2.z2": 2.0,
            "localopt2.workflow.itername": "1-localopt2.1"
        }
        self.verify(vnames, cases[0], iteration_case_1)

        iteration_case_78 = {
            # Data from parent.
            "_pseudo_0": 4.3678794411714428,
            "_pseudo_1": 18.603650790259696,
            "_pseudo_2": 13.004546217033543,
            "coupling_var_targets[0]": 1.0,
            "coupling_var_targets[1]": 1.0,
            "driver.workflow.itername": "1",
            "global_des_var_targets[0]": 5.0,
            "global_des_var_targets[1]": 2.0,
            "local_des_var_targets[0]": 1.0,
            "localopt1.derivative_exec_count": 0,
            "localopt1.error_code": 0,
            "localopt1.exec_count": 1,
            "localopt1.itername": "1-localopt1",
            "localopt2.derivative_exec_count": 0,
            "localopt2.error_code": 0,
            "localopt2.exec_count": 1,
            "localopt2.itername": "1-localopt2",

            # Last data from localopt1.
            "_pseudo_3": 18.603650790259696,
            "_pseudo_4": -2.0914426634988104e-08,
            "dis1.derivative_exec_count": 0,
            "dis1.exec_count": 37,
            "dis1.itername": "1-localopt1.9-dis1",
            "dis1.x1": -2.5171596376597159e-16,
            "dis1.y1": 3.1600000209144268,
            "dis1.y2": 1.2169731313990237,
            "dis1.z1": 1.577422040870375,
            "dis1.z2": 0.91513435217057326,
            "localopt1.workflow.itername": "1-localopt1.9",

            # Last data from localopt2.
            "_pseudo_5": 13.004546217033543,
            "_pseudo_6": -20.961798034144564,
            "dis2.derivative_exec_count": 0,
            "dis2.exec_count": 107,
            "dis2.itername": "1-localopt2.68-dis2",
            "dis2.y1": -2.3511537015279333e-07,
            "dis2.y2": 3.0382019658554338,
            "dis2.z1": 3.0336152281330477,
            "dis2.z2": 0.004101850756019077,
            "localopt2.workflow.itername": "1-localopt2.68"
        }
        self.verify(vnames, cases[-1], iteration_case_78)

    def verify(self, names, case, expected):
        for name, value in expected.items():
            i = names.index(name)
            if isinstance(value, float):
                assert_rel_error(self, case[i], value, 0.001)
            else:
                self.assertEqual(case[i], value)

    def test_driver(self):
        # Dataset of a driver.
        vnames = self.cds.data.driver('localopt1').var_names().fetch()
        expected = [
            '_driver_id', '_id', '_parent_id', '_pseudo_3', '_pseudo_4',
            'dis1.derivative_exec_count', 'dis1.exec_count', 'dis1.itername',
            'dis1.x1', 'dis1.y1', 'dis1.y2', 'dis1.z1', 'dis1.z2',
            'error_message', 'error_status', 'localopt1.workflow.itername',
            'timestamp']
        self.assertEqual(vnames, expected)

        cases = self.cds.data.driver('localopt1').fetch()
        self.assertEqual(len(cases), 735)
        self.assertEqual(len(cases[0]), len(expected))

    def test_bson(self):
        # Simple check of _BSONReader.
        names = ('dis1.x1', 'dis1.y1', 'dis1.y2', 'dis1.z1', 'dis1.z2')

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
        case_id = '7bd00c94-012c-11e4-9566-005056000100'  # iteration_case_1825
        top = set_as_top(SellarCO('.restore'))
        self.cds.restore(top, case_id)

        top.run()

        path = os.path.join(os.path.dirname(__file__), 'sellar_json.restore')
        cds = CaseDataset(path, 'json')
        vnames = cds.data.var_names().fetch()
        cases = cds.data.fetch()
        self.assertEqual(len(cases), 322)

        iteration_case_322 = {
            "_pseudo_0": 3.1833911345239598,
            "_pseudo_1": 7.1329259991714449e-08,
            "_pseudo_2": 1.0159265073590318e-07,
            "coupling_var_targets[0]": 3.159999999989676,
            "coupling_var_targets[1]": 3.7553981923553157,
            "driver.workflow.itername": "24",
            "global_des_var_targets[0]": 1.9776544546085641,
            "global_des_var_targets[1]": -4.9291780851978736e-11,
            "local_des_var_targets[0]": -1.0245069227736856e-09,
            "localopt1.derivative_exec_count": 0,
            "localopt1.error_code": 0,
            "localopt1.exec_count": 320,
            "localopt1.itername": "24-localopt1",
            "localopt2.derivative_exec_count": 0,
            "localopt2.error_code": 0,
            "localopt2.exec_count": 278,
            "localopt2.itername": "24-localopt2"
        }
        self.verify(vnames, cases[-1], iteration_case_322)


if __name__ == '__main__':
    unittest.main()

