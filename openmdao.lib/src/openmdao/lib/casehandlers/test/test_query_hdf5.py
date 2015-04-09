"""
Test query of case recorder file.
"""

import os.path
import tempfile
import shutil
import unittest
from unittest import SkipTest

from math import isnan

from openmdao.main.api import Assembly, Component, VariableTree, set_as_top
from openmdao.main.datatypes.api import Array, Float, VarTree
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
        try:
            import h5py
        except ImportError:
            raise SkipTest("this test requires h5py")
        from openmdao.lib.casehandlers.api import CaseDatasetHDF5, HDF5CaseRecorder

        #create_files()  # Uncomment to create 'sellar.new'
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

        #cases = self.cds.data.driver('driver').vars(['_itername',]).by_variable().fetch() # TODO. This works here but fails when run later in this method.
        # when run here, it messes up later queries


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

        expected = ['_driver_id', '_driver_name', '_id', '_itername', '_parent_id', '_pseudo_0.out0',
                    '_pseudo_1.out0', '_pseudo_2.out0', 'driver.workflow.itername', 'error_message',
                    'error_status', 'half.derivative_exec_count', 'half.exec_count', 'half.itername',
                    'half.z2a', 'half.z2b', 'sub._pseudo_0.out0', 'sub.derivative_exec_count',
                    'sub.dis1.derivative_exec_count', 'sub.dis1.exec_count', 'sub.dis1.itername',
                    'sub.dis1.y1', 'sub.dis1.y2', 'sub.dis2.derivative_exec_count', 'sub.dis2.exec_count',
                    'sub.dis2.itername', 'sub.dis2.y2', 'sub.driver.workflow.itername', 'sub.exec_count',
                    'sub.globals.z1', 'sub.itername', 'sub.states', 'sub.states.y[0]', 'sub.states.y[1]',
                    'sub.x1', 'timestamp']
        self.assertEqual(vnames, expected)

        # Check the sizes of the full dataset.
        cases = self.cds.data.fetch()
        self.assertEqual(len(cases), 74)
        self.assertEqual(len(cases[0]), len(expected))

        # check to see if the driver method works
        cases = self.cds.data.driver('driver').fetch()
        self.assertEqual(len(cases), 10)
        vnames = self.cds.data.driver('driver').var_names().fetch()
        expected = ['_driver_id', '_driver_name', '_id', '_itername', '_parent_id',
                    '_pseudo_0.out0', '_pseudo_1.out0', '_pseudo_2.out0', 'driver.workflow.itername',
                    'error_message', 'error_status', 'half.derivative_exec_count', 'half.exec_count',
                    'half.itername', 'half.z2a', 'half.z2b', 'sub.derivative_exec_count', 'sub.exec_count',
                    'sub.globals.z1', 'sub.itername', 'sub.states', 'sub.states.y[0]', 'sub.states.y[1]',
                    'sub.x1', 'timestamp']
        self.assertEqual(vnames, expected)

        # Check to see if vars method works
        names = ['half.z2a', 'sub.globals.z1', 'sub.x1']
        vnames = self.cds.data.vars(names).var_names().fetch()
        self.assertEqual(vnames, names)

        cases = self.cds.data.vars(names).fetch()
        self.assertEqual(len(cases), 74)
        self.assertEqual(len(cases[0]), len(names))

        # Check to see if the vars and driver method work together
        cases = self.cds.data.driver('driver').vars('sub.x1').fetch()
        expected = [[1.0], [1.0], [-2.2204460492503131e-15], [1.5833286366705896e-15],
                    [-1.4920636625846083e-15], [-7.076328627021676e-15], [-4.4055932092474378e-15],
                    [-4.0009363277664916e-15], [1.34802409522104e-14], [4.941980752962757e-15]]

        for exp, act in zip( expected, cases):
            self.assertAlmostEqual(exp[0], act[0])

        #TODO: the order that this returns is not the correct order
        cases = self.cds.data.driver('driver').vars(['sub.x1', 'half.z2a']).by_variable().fetch()
        expected = [[1.0, 1.0, -2.2204460492503131e-15, 1.5833286366705896e-15, -1.4920636625846083e-15,
                     -7.076328627021676e-15, -4.4055932092474378e-15, -4.0009363277664916e-15,
                     1.34802409522104e-14, 4.941980752962757e-15], [2.0, 2.0, 1.3982923058163501,
                    0.66069127222525592, -1.1213252548714081e-13, -8.3705631699668261e-13,
                    -2.5667509986838707e-15, 1.7620750445643237e-15, -8.9255408786136536e-15, -8.2170907529898209e-15]]

        for i in range(2):
            for exp, act in zip( expected[i], cases[i]):
                self.assertAlmostEqual(exp, act)


        # Check to see if vartree reading works
        cases = self.cds.data.driver('driver').fetch() # the vartree data is only available in the top level driver cases
        expected = [ 24.80392945,  10.98035435]
        actual = cases[0]['sub.states']['y']
        for exp, act in zip( expected, actual):
            self.assertAlmostEqual(exp, act)

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

        ## Transposed. TODO
        #cases = self.cds.data.vars(names).fetch()
        #vars = self.cds.data.driver('driver').by_variable().fetch()
        #self.assertEqual(len(vars), len(names))
        #for name in ('half.z2a', 'sub.x1'):
            #self.assertEqual(len(vars[name]), 1)
            #self.assertTrue(not isnan(vars[name][-1]))
        #for name in ('sub.globals.z1', 'sub.x1'):
            #self.assertEqual(len(vars[name]), 242)
            #assert_rel_error(self, vars[name][-1], iteration_case_242[name], 0.001)


        # Just a note if you want to see the iternames for the cases
        #print self.cds.data.vars('_itername').fetch()






if __name__ == '__main__':
    unittest.main()
