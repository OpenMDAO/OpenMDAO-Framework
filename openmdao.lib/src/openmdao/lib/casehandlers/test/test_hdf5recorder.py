import unittest
import tempfile
import shutil
import os

from nose import SkipTest

from openmdao.lib.drivers.api import SLSQPdriver
from openmdao.lib.drivers.api import FixedPointIterator, SLSQPdriver
from openmdao.lib.optproblems import sellar
from openmdao.lib.drivers.api import SLSQPdriver
from openmdao.main.api import Assembly, Component, VariableTree, set_as_top
from openmdao.main.datatypes.api import Array, VarTree, Float, Int, Str
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


class TestSellarMDFCase(unittest.TestCase):

    def setUp(self):
        self.tolerance = 0.001
        self.top = set_as_top(SellarMDF())
        self.tempdir = tempfile.mkdtemp(prefix='test_hdf5recorder-')

    def tearDown(self):
        try:
            shutil.rmtree(self.tempdir)
        except OSError:
            pass
        self.top = None

    def test_sellarMDF_hdf5_recording(self):

        try:
            import h5py
        except ImportError:
            raise SkipTest("this test requires h5py")
        from openmdao.lib.casehandlers.api import HDF5CaseRecorder

        hdf5_cases_filename = 'sellarMDF.hdf5'
        hdf5_cases_filepath = os.path.join(self.tempdir, hdf5_cases_filename)
        self.top.recorders = [HDF5CaseRecorder(hdf5_cases_filepath)]
        self.top.run()

        ### Check to see if the values written make sense ###

        hdf5_cases_file = h5py.File(hdf5_cases_filepath,'r')

        # Check some values in the driver section
        driver_info_1 = hdf5_cases_file['/driver_info_1/']
        self.assertEqual( driver_info_1['ineq_constraints'].value[0], "3.16 < sub.states.y[0]" )
        driver_info_2 = hdf5_cases_file['/driver_info_2/']
        self.assertEqual( driver_info_2['eq_constraints'].value, "dis2.y2 = dis1.y2" )

        # Check some values in the simulation info section
        simulation_info = hdf5_cases_file['/simulation_info/']
        self.assertAlmostEqual( simulation_info['constants/driver.accuracy'].value, 1e-06 )
        self.assertEqual( simulation_info['constants/driver.iout'].value, 6 )
        self.assertEqual( simulation_info['constants/missing_deriv_policy'].value, 'assume_zero')
        expected = '{"directed": true, "graph": [], "nodes": [{"comp": true, "id": "sub"}, {"comp": true, "driver": true, "id": "driver"}, {"comp": true, "id": "half"}, {"comp": true, "pseudo": "constraint", "id": "_pseudo_1"}, {"comp": true, "pseudo": "objective", "id": "_pseudo_0"}, {"comp": true, "pseudo": "constraint", "id": "_pseudo_2"}], "links": [{"source": 0, "target": 3}, {"source": 0, "target": 4}, {"source": 0, "target": 5}, {"source": 1, "target": 0}, {"source": 1, "target": 2}, {"source": 2, "target": 0}, {"source": 3, "target": 1}, {"source": 4, "target": 1}, {"source": 5, "target": 1}], "multigraph": false}'
        self.assertEqual( simulation_info['comp_graph'].value, expected )

        # Check some values in the iteration cases section
        driver_grp = hdf5_cases_file['/iteration_cases/driver/']

        # How many iterations?
        #num_iterations = get_number_of_iterations(driver_grp)

        # compare expected to actual for case with itername '6' for some items
        for key in driver_grp:
            if key.startswith( "iteration_case_"):
                if driver_grp[key]['metadata']['_itername'][0] == '6':
                    iteration_case_name = key

        iteration_data = driver_grp['%s/data/' % iteration_case_name ]
        # check a float array
        actual = iteration_data['array_of_floats' ].value
        expected = [  3.20119761e+00,   3.76541140e+00,  -2.02345886e+01,  -8.37056317e-13,  -4.18528158e-13,
                      1.98270572e+00,   3.17803953e+00,  -7.07632863e-15,  -1.80395299e-02]
        for exp, act in zip(expected, actual):
            assert_rel_error(self, exp, act, self.tolerance)

        # check a string array
        actual = iteration_data['array_of_strs' ].value
        expected = ['6-sub', '6', '6-half']
        for exp, act in zip(expected, actual):
            self.assertEqual( exp, act )

        # check a vartree
        actual = iteration_data['sub.states/y' ].value
        expected = [ 3.17803953,  3.7654114]
        for exp, act in zip(expected, actual):
            assert_rel_error(self, exp, act, self.tolerance)


# class CompWithStringOutput(Component):
#     n = Int(0, iotype='in')
#     s = Str('', iotype='out')

#     def execute(self):
#         self.s = 'q' * self.n


# class StringOutput(Assembly):
#     """ Optimization of the Sellar problem using MDF
#     Disciplines coupled with FixedPointIterator.
#     """

#     def configure(self):

#         comp = self.add('comp', CompWithStringOutput())
#         self.driver.workflow.add( 'comp' )



# class TestSettingStringLengthCase(unittest.TestCase):

#     def setUp(self):
#         self.top = set_as_top(StringOutput())

#     def tearDown(self):
#         self.top = None

#     def test_setting_string_length(self):
#         hdf5_cases_filename = 'string_output.hdf5'
#         self.top.comp.n = 90
#         self.top.recorders = [HDF5CaseRecorder(hdf5_cases_filename)]

#         # Check to see that an error is thrown if a string is too long
#         try:
#             self.top.run()
#         except ValueError, err:
#             self.assertEqual(str(err), "string will not fit in space allocated for in HDF5 file")
#         else:
#             self.fail("expected ValueError with message: string will not fit in space allocated for in HDF5 file")


if __name__ == '__main__':
    unittest.main()
