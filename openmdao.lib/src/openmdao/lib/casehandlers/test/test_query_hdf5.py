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
from openmdao.lib.casehandlers.api import CaseDatasetHDF5, HDF5CaseRecorder
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
    prob.recorders = [ HDF5CaseRecorder('sellar_hdf5.new')]
    prob.run()



class TestCase(unittest.TestCase):

    def setUp(self):
        create_files()  # Uncomment to create 'sellar.new'
        path = os.path.join(os.path.dirname(__file__), 'sellar.hdf5')
        self.cds = CaseDatasetHDF5(path, 'hdf5')
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
        
        # Check the variable names for the full dataset.
        vnames = self.cds.data.var_names().fetch()
        expected = ['_driver_id', '_driver_name', '_id', '_itername', '_parent_id', '_pseudo_0.out0', 
        '_pseudo_1.out0', '_pseudo_2.out0', 'driver.workflow.itername', 'error_message', 'error_status', 
        'half.derivative_exec_count', 'half.exec_count', 'half.itername', 'half.z2a', 'half.z2b', 
        'sub._pseudo_0.out0', 'sub.derivative_exec_count', 'sub.dis1.derivative_exec_count', 'sub.dis1.exec_count',
         'sub.dis1.itername', 'sub.dis1.y1', 'sub.dis1.y2', 'sub.dis2.derivative_exec_count', 
         'sub.dis2.exec_count', 'sub.dis2.itername', 'sub.dis2.y2', 'sub.driver.workflow.itername', 
         'sub.exec_count', 'sub.globals.z1', 'sub.itername', 'sub.states', 'sub.states.y[0]', 
         'sub.states.y[1]', 'sub.x1', 'timestamp']
        self.assertEqual(vnames, expected)

        # Check the sizes of the full dataset.
        cases = self.cds.data.fetch()
        self.assertEqual(len(cases), 74)
        self.assertEqual(len(cases[0]), len(expected))
        
        # check to see if the driver method works
        cases = self.cds.data.driver('driver').fetch() # the vartree data is only available in the top level driver cases
        self.assertEqual(len(cases), 10)

        # Check to see if vartree reading works
        cases = self.cds.data.driver('driver').fetch() # the vartree data is only available in the top level driver cases
        expected = [ 8.18917514,  6.52651228]
        actual = cases[0]['sub.states']['y']
        for exp, act in zip( expected, actual):
            self.assertAlmostEqual(exp, act)
            
        # Check to see if vars method works
        names = ['half.z2a', 'sub.globals.z1', 'sub.x1']
        vnames = self.cds.data.vars(names).var_names().fetch()
        self.assertEqual(vnames, names)

        cases = self.cds.data.vars(names).fetch()
        self.assertEqual(len(cases), 74)
        self.assertEqual(len(cases[0]), len(names))

        # Check to see if case method works
        cases = self.cds.data.case('3-sub.5').fetch()
        self.assertEqual(1, len(cases))
        
        self.assertAlmostEqual( cases[0]['sub.dis1.y1'], 8.18917513654 )

        iteration_case_3_sub_5_expected = {
            "sub.dis1.y1": 8.18917513654,
            "sub.dis1.y2": 6.52651934578,
            "sub.dis2.y2": 6.52651228123
        }
        for name, exp in iteration_case_3_sub_5_expected.items():
            self.assertAlmostEqual(exp, cases[0][name])

        # Transposed.
        vars = self.cds.data.local().vars(names).by_variable().fetch()
        self.assertEqual(len(vars), len(names))
        for name in ('half.z2a', 'sub.x1'):
            self.assertEqual(len(vars[name]), 242)
            self.assertTrue(not isnan(vars[name][-1]))
        for name in ('sub.globals.z1', 'sub.x1'):
            self.assertEqual(len(vars[name]), 242)
            assert_rel_error(self, vars[name][-1], iteration_case_242[name], 0.001)


        # Just a note if you want to see the iternames for the cases
        #print self.cds.data.vars('_itername').fetch()

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




class TestVarTreeCase(unittest.TestCase):

    def setUp(self):
        path = os.path.join(os.path.dirname(__file__), 'cases__driver_with_vartree.hdf5')
        self.cds = CaseDatasetHDF5(path, 'hdf5')
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
        cases = self.cds.data.fetch()


if __name__ == '__main__':
    unittest.main()
