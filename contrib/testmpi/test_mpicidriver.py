import time
from unittest import TestCase
import numpy as np

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array, Str, List
from openmdao.main.mpiwrap import MPI, MPIContext
from openmdao.lib.drivers.mpicasedriver import MPICaseDriver
from openmdao.lib.drivers.simplecid import SimpleCaseIterDriver
from openmdao.test.mpiunittest import MPITestCase
from openmdao.util.testutil import assert_rel_error


class ABCDArrayComp(Component):
    delay = Float(0.01, iotype='in')
    in_string = Str(iotype='in')
    out_string = Str(iotype='out')
    in_list = List(iotype='in')
    out_list = List(iotype='out')

    def __init__(self, arr_size=9):
        super(ABCDArrayComp, self).__init__()
        self.add_trait('a', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('b', Array(np.ones(arr_size, float), iotype='in'))
        self.add_trait('c', Array(np.ones(arr_size, float), iotype='out'))
        self.add_trait('d', Array(np.ones(arr_size, float), iotype='out'))

    def execute(self):
        time.sleep(self.delay)
        self.c = self.a + self.b
        self.d = self.a - self.b
        self.out_string = self.in_string + '_' + self.name
        self.out_list = self.in_list[:]+[1.5]
        #self.dump()

    def dump(self):
        print self.name,':'
        print "%s.a = %s" % (self.name, self.a)
        print "%s.b = %s" % (self.name, self.b)
        print "%s.c = %s" % (self.name, self.c)
        print "%s.d = %s" % (self.name, self.d)

def model_setup(num_inputs, mpi=True):
    vsize = 5   # array var size

    # a comp feeds 3 parallel comps which feed
    # another comp
    top = set_as_top(Assembly())
    if mpi:
        drv = MPICaseDriver()
    else:
        drv = SimpleCaseIterDriver()
    driver = top.add("driver", drv)
    top.add("C1", ABCDArrayComp(vsize))
    top.add("C2", ABCDArrayComp(vsize))
    top.add("C3", ABCDArrayComp(vsize))
    top.add("C4", ABCDArrayComp(vsize))
    top.add("C5", ABCDArrayComp(vsize))
    top.driver.workflow.add(['C1', 'C2', 'C3', 'C4', 'C5'])
    top.connect('C1.c', 'C2.a')
    top.connect('C1.out_string', 'C2.in_string')
    top.connect('C1.out_list', 'C4.in_list')

    top.connect('C1.d', 'C3.b')
    top.connect('C1.c', 'C4.a')
    top.connect('C2.out_string', 'C5.in_string')
    top.connect('C3.d', 'C5.b')
    top.connect('C4.c', 'C5.a')
    top.connect('C4.out_list', 'C5.in_list')

    # set up parameters and responses for CaseDriver
    driver.add_parameter("C1.a")
    driver.add_parameter("C1.b")
    driver.add_parameter("C1.in_string")
    driver.add_parameter("C1.in_list")

    driver.add_response("C5.c")
    driver.add_response("C5.d")
    driver.add_response("C5.out_string")
    driver.add_response("C5.out_list")

    # set up inputs
    a = 3.0
    b = 7.0
    s = 'abcdefghijklmnopqrstuvwxyz'
    l = [1, 2, 3]
    avals=[]; bvals=[]; svals=[]; lvals=[]
    for i in range(num_inputs):
        avals.append(np.ones(vsize, float) * a); a += 1.
        bvals.append(np.ones(vsize, float) * b); b += 1.
        svals.append(s[:i])
        lvals.append(l);
        l = l + [i+4]

    driver.case_inputs.C1.a = avals
    driver.case_inputs.C1.b = bvals
    driver.case_inputs.C1.in_string = svals
    driver.case_inputs.C1.in_list = lvals

    return top


# class MPITests1(MPITestCase):
#
#     N_PROCS = 2
#
#     def test_fan_out_in(self):
#         size = 5   # array var size
#
#         # a comp feeds two parallel comps which feed
#         # another comp
#         top = set_as_top(Assembly())
#         top.add("C1", ABCDArrayComp(size))
#         top.add("C2", ABCDArrayComp(size))
#         top.add("C3", ABCDArrayComp(size))
#         top.add("C4", ABCDArrayComp(size))
#         top.driver.workflow.add(['C1', 'C2', 'C3', 'C4'])
#         top.connect('C1.c', 'C2.a')
#         top.connect('C1.d', 'C3.b')
#         top.connect('C2.c', 'C4.a')
#         top.connect('C3.d', 'C4.b')
#
#         top.C1.a = np.ones(size, float) * 3.0
#         top.C1.b = np.ones(size, float) * 7.0
#
#         top.run()
#
#         with MPIContext():
#             self.assertTrue(all(top.C4.a==np.ones(size, float)*11.))
#             self.assertTrue(all(top.C4.b==np.ones(size, float)*5.))
#
#         # Piggyback testing of the is_variable_local function.
#         system = top.driver.workflow._system
#         self.assertTrue(system.is_variable_local('C1.c') is True)
#
#         # Exclusive or - you either got C2 or C3.
#         self.assertTrue(system.is_variable_local('C2.a') != system.is_variable_local('C3.a'))
#         self.assertTrue(system.is_variable_local('C2.c') != system.is_variable_local('C3.c'))


class MPITests3(MPITestCase):

    N_PROCS = 6

    def test_fan_out_in_noflats(self):
        top = model_setup(100)
        driver = top.driver
        top.run()

        # self.assertTrue(all(top.C5.a==np.ones(vsize, float)*11.))
        # self.assertTrue(all(top.C5.b==np.ones(vsize, float)*5.))
        #
        # self.assertEqual(top.C5.out_string, 'foo_C1_C2_C5')
        # self.assertEqual(top.C5.out_list, [1, 1, 1, 1.5, 1.5, 1.5])


class SerialTests(TestCase):

    def test_fan_out_in_noflats_serial(self):
        top = model_setup(100, mpi=False)
        driver = top.driver
        top.run()
        

if __name__ == '__main__':
    from openmdao.test.mpiunittest import mpirun_tests
    mpirun_tests()
