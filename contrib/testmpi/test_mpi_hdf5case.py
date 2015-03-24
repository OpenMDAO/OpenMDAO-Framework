from openmdao.lib.casehandlers.api import JSONCaseRecorder, BSONCaseRecorder, verify_json, CaseDataset, HDF5CaseRecorder
import time
import sys

import numpy as np

from openmdao.test.mpiunittest import MPITestCase
from openmdao.util.testutil import assert_rel_error
from openmdao.main.test.simpledriver import SimpleDriver

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.interfaces import implements, ISolver
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.mpiwrap import MPI
from openmdao.lib.drivers.iterate import FixedPointIterator
from openmdao.lib.drivers.newton_solver import NewtonSolver
from openmdao.test.execcomp import ExecComp
from openmdao.main.test.test_derivatives import SimpleDriver

from openmdao.lib.optproblems import sellar

import h5py

from openmdao.test.mpiunittest import MPITestCase, MPIContext

import logging

logger = logging.getLogger()
logger.level = logging.DEBUG

class SellarMDF(Assembly):
    """ Optimization of the Sellar problem using MDF
    Disciplines coupled with FixedPointIterator.
    """
    def configure(self):
        """ Creates a new Assembly with this problem

        Optimal Design at (1.9776, 0, 0)

        Optimal Objective = 3.18339
        """

        self.add('driver', FixedPointIterator())

        # Inner Loop - Full Multidisciplinary Solve via fixed point iteration
        C1 = self.add('C1', sellar.Discipline1())
        C2 = self.add('C2', sellar.Discipline2())

        self.driver.workflow.add(['C1','C2'])

        #not relevant to the iteration. Just fixed constants
        C1.z1 = C2.z1 = 1.9776
        C1.z2 = C2.z2 = 0
        C1.x1 = 0

        # Solver settings
        self.driver.max_iteration = 5
        self.driver.tolerance = 1.e-15
        self.driver.print_convergence = False

def get_value_from_hdf5_file( names, values, name ):

    idx = np.nonzero( names == name )[0][0]
    return values[idx]


def create_serial_file():

        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C1.y2 = C2.y2')

        hdf5_filename = 'test_sellar_params1_serial.hdf5'
        top.recorders = [HDF5CaseRecorder(hdf5_filename)]

        top.run()

#create_serial_file()  # Uncomment to create 'test_sellar_params1_serial.hdf5' and 'cases__driver.hdf5'

class MPITests1(MPITestCase):

    N_PROCS = 2


    def test_sellar_params1(self):
        stream_handler = logging.StreamHandler(sys.stdout)
        logger.addHandler(stream_handler)

        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C1.y2 = C2.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        hdf5_filename = 'test_sellar_params1_parallel.hdf5'
        top.recorders = [HDF5CaseRecorder(hdf5_filename)]

        top.run()

        if self.comm.rank == 0:
            for name, expval in expected.items():
                val = top.get(name)
                print 'asserts'
                assert_rel_error(self, val, expval, 0.001)


        if self.comm.rank == 0:
            hdf5_file = h5py.File(hdf5_filename,'r')
            float_names = hdf5_file['iteration_cases/driver/float_names'].value
            float_values = hdf5_file['iteration_cases/driver/iteration_case_6/data/array_of_floats'].value
            if self.comm.rank == 0:
                for name, expval in expected.items():
                    val = get_value_from_hdf5_file(float_names, float_values, name)
                    assert_rel_error(self, val, expval, 0.001)

            val = get_value_from_hdf5_file(float_names, float_values, '_pseudo_0.out0')
            assert_rel_error(self, val, 0.0, 0.001)

            int_names = hdf5_file['iteration_cases/driver/int_names'].value
            int_values = hdf5_file['iteration_cases/driver/iteration_case_6/data/array_of_ints'].value

            self.assertTrue( get_value_from_hdf5_file( int_names, int_values, 'C1.exec_count' ) > 0 )
            self.assertTrue( get_value_from_hdf5_file( int_names, int_values, 'C2.exec_count' ) > 0 )


            # Check the iternames
            for i in range(6):
                metadata = hdf5_file['iteration_cases/driver/iteration_case_%d/metadata' % (i+1)]
                self.assertTrue(  metadata['_itername'][0] == '%d' % (i+1) )


            logging.getLogger().info(  get_value_from_hdf5_file( int_names, int_values, 'C2.derivative_exec_count' ))





            str_names = hdf5_file['iteration_cases/driver/str_names'].value
            str_values = hdf5_file['iteration_cases/driver/iteration_case_6/data/array_of_strs'].value

            for name in str_names:
                self.assertTrue( len( get_value_from_hdf5_file( str_names, str_values, name) ) > 0 )



        if self.comm.rank == 0:
            # Read values from files to verify
            print 'iteration_cases'
            # logging.getLogger().info(dir(hdf5_file['iteration_cases']['driver']['float_names']))

            expected = ['_pseudo_0.out0', 'C1.y1', 'C1.y2', 'C2.y2']
            actual = hdf5_file['iteration_cases']['driver']['float_names'].value
            for exp, act in zip( expected, actual ):
                logging.getLogger().info( (exp, act) )
                self.assertTrue( act == exp )

            expected = [ -1.57732043e-06,   3.15986146e+00,   3.75520149e+00,   3.75519992e+00]
            logging.getLogger().info(hdf5_file['iteration_cases/driver/float_names'].value)
            logging.getLogger().info(hdf5_file['iteration_cases']['driver']['iteration_case_6']['data']['array_of_floats'].value)
            logging.getLogger().info(hdf5_file['iteration_cases']['driver']['iteration_case_6']['metadata'].value)

            print hdf5_file['iteration_cases']

            #          GROUP "data" {
            # DATASET "array_of_floats" {



    def test_sellar_compare_to_serial_case_recording(self):
        stream_handler = logging.StreamHandler(sys.stdout)
        logger.addHandler(stream_handler)

        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C1.y2 = C2.y2')

        hdf5_filename = 'test_sellar_params1_parallel.hdf5'
        top.recorders = [HDF5CaseRecorder(hdf5_filename)]

        top.run()

        if self.comm.rank == 0:
            # Open up and compare the non-metadata values in cases__driver_serial.hdf5 and cases__driver.hdf5






# top = set_as_top(SellarMDF())

# top.driver.max_iteration = 30
# top.driver.max_iteration = 1
# top.driver.add_parameter('C2.y1', low=-1e99, high=1e99)
# top.driver.add_constraint('C1.y1 = C2.y1')
# top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
# top.driver.add_constraint('C2.y2 = C1.y2')

# expected = { 'C1.y1': 3.1598617768014536, 'C2.y2': 3.7551999159927316 }

# if MPI:
#     top.recorders = [HDF5CaseRecorder('SellarMDF_parallel.hdf5')]
# else:
#     top.recorders = [HDF5CaseRecorder('SellarMDF_serial.hdf5')]

# top.run()


if __name__ == '__main__':
   from openmdao.test.mpiunittest import mpirun_tests
   mpirun_tests()

