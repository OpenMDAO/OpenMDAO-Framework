from unittest import SkipTest
import numpy as np

from openmdao.lib.drivers.iterate import FixedPointIterator
from openmdao.lib.optproblems import sellar
from openmdao.main.api import Assembly, set_as_top
from openmdao.test.mpiunittest import MPITestCase
from openmdao.util.testutil import assert_rel_error

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
    """Get the value from an array whose keys are given in another array.

    Args:
       names (numpy array of str):  Keys of the value array.
       values (numpy array of str/int/float):  Array of values.
       name (str):  Key into the value array.

    Returns:
       str/int/float.  Returns the value from values using the key name::

    """

    idx = np.nonzero( names == name )[0][0]
    return values[idx]


def create_serial_file():
    '''Create the HDF5 case record files for the serial case. Only run as needed'''

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

    def setUp(self):
        try:
            import h5py
        except ImportError:
            raise SkipTest("this test requies h5py")
        from openmdao.lib.casehandlers.api import HDF5CaseRecorder

        self.tolerance = 0.001

    def test_sellar_params1(self):

        global error_tolerance

        from openmdao.lib.casehandlers.api import HDF5CaseRecorder
        import h5py

        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C1.y2 = C2.y2')

        hdf5_filename = 'test_sellar_params1_parallel.hdf5'
        top.recorders = [HDF5CaseRecorder(hdf5_filename)]

        top.run()

        # Only do the testing on rank 0 process
        if self.comm.rank == 0:
            # Just check to make sure model returns the expected results
            expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, self.tolerance)

            # read the HDF5 file that was recorded to
            hdf5_file = h5py.File(hdf5_filename,'r')

            # How many iterations were there?
            num_iterations = 0
            for key in hdf5_file['iteration_cases/driver/']:
                if key.startswith( "iteration_case_"):
                    num_iterations += 1

            #   Were the expected results in the final case ?
            float_names = hdf5_file['iteration_cases/driver/float_names'].value
            float_values = hdf5_file['iteration_cases/driver/iteration_case_%d/data/array_of_floats' % num_iterations].value
            for name, expval in expected.items():
                val = get_value_from_hdf5_file(float_names, float_values, name)
                assert_rel_error(self, val, expval, self.tolerance)

            # Check to see that the constraint value is close to zero
            val = get_value_from_hdf5_file(float_names, float_values, '_pseudo_0.out0')
            assert_rel_error(self, val, 0.0, self.tolerance)

            # Check to see that the exec_count matches the iteration count
            int_names = hdf5_file['iteration_cases/driver/int_names'].value
            int_values = hdf5_file['iteration_cases/driver/iteration_case_%d/data/array_of_ints' % num_iterations].value
            self.assertEqual( get_value_from_hdf5_file( int_names, int_values, 'C1.exec_count' ) , num_iterations )
            self.assertEqual( get_value_from_hdf5_file( int_names, int_values, 'C2.exec_count' ) , num_iterations )

            # Check the iternames
            for i in range(num_iterations):
                metadata = hdf5_file['iteration_cases/driver/iteration_case_%d/metadata' % (i+1)]
                self.assertEqual(  metadata['_itername'][0], '%d' % (i+1) )

            # make sure all the strings recorded have positive length
            str_names = hdf5_file['iteration_cases/driver/str_names'].value
            str_values = hdf5_file['iteration_cases/driver/iteration_case_%d/data/array_of_strs' % num_iterations].value
            for name in str_names:
                self.assertTrue( len( get_value_from_hdf5_file( str_names, str_values, name) ) > 0 )


    def test_sellar_compare_to_serial_case_recording(self):

        from openmdao.lib.casehandlers.api import HDF5CaseRecorder
        import h5py

        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C1.y2 = C2.y2')

        hdf5_filename = 'test_sellar_params1_parallel.hdf5'
        top.recorders = [HDF5CaseRecorder(hdf5_filename)]

        top.run()

        if self.comm.rank == 0:
            # Open up and compare the non-metadata values in cases__driver_serial.hdf5 and cases__driver.hdf5
            parallel_hdf5_cases_file = 'test_sellar_params1_parallel__driver.hdf5'
            serial_hdf5_cases_file = 'cases__driver_serial.hdf5'
            parallel_hdf5_file = h5py.File(parallel_hdf5_cases_file,'r')
            serial_hdf5_file = h5py.File(serial_hdf5_cases_file,'r')

            num_iterations = 0
            for key in serial_hdf5_file:
                if key.startswith( "iteration_case_"):
                    num_iterations += 1

            for i in range(num_iterations): # check each of the iterations
                parallel_float_values = parallel_hdf5_file['/iteration_case_%d/data/array_of_floats' % (i+1)].value
                serial_float_values = serial_hdf5_file['/iteration_case_%d/data/array_of_floats' % (i+1)].value
                for parallel_value, serial_value in zip( parallel_float_values, serial_float_values ):
                    assert_rel_error(self, parallel_value, serial_value, self.tolerance)


if __name__ == '__main__':
   from openmdao.test.mpiunittest import mpirun_tests
   mpirun_tests()

