import os
import sys
import time
import tempfile
import shutil
from unittest import TestCase
import numpy as np

from openmdao.main.api import Assembly, Component, set_as_top, Driver, SimulationRoot
from openmdao.main.datatypes.api import Float, Array, Str, List, File, FileRef, Int
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

def model_par3_setup(num_inputs, mpi=True):
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
    a = np.ones(vsize, float) * 3.0
    b = np.ones(vsize, float) * 7.0
    s = 'abcdefghijklmnopqrstuvwxyz'
    avals=[]; bvals=[]; svals=[]; lvals=[]
    for i in range(num_inputs):
        avals.append(a); a += 1.
        bvals.append(b); b += 1.
        svals.append(s[:i])
        lvals.append(range(i+1));

    driver.case_inputs.C1.a = avals
    driver.case_inputs.C1.b = bvals
    driver.case_inputs.C1.in_string = svals
    driver.case_inputs.C1.in_list = lvals

    # expected results
    expected = {}
    for name in driver.get_responses():
        expected[name] = []

        for i in range(num_inputs):
            if name == 'C5.c':
                val = (b + np.ones(vsize,float)) * 2.
            elif name == 'C5.d':
                val = a * 2.
            elif name == 'C5.out_string':
                val = s[:i]+'_C1_C2_C5'
            elif name == 'C5.out_list':
                val = range(i+1)+[1.5,1.5,1.5]

            expected[name].append(val)

    return top, expected


class MPITests9(MPITestCase):

    N_PROCS = 7

    def test_par3_2divs_1leftover(self):
        num_inputs = 17
        top, expected = model_par3_setup(num_inputs)
        driver = top.driver
        top.run()

        self.assertEqual(self.N_PROCS/3, driver._num_parallel_subs)

        for name, expval in expected.items():
                val = driver.case_outputs.get(name)
                if driver.workflow._system.mpi.comm != MPI.COMM_NULL:
                    for v1, v2 in zip(expval, val):
                        if isinstance(v1, np.ndarray):
                            self.assertTrue(all(v1==v2))
                        else:
                            self.assertEqual(v1, v2)
                else:
                    self.assertEqual(val, [])

class MPITests5(MPITestCase):

    N_PROCS = 5

    def test_par3_2leftovers(self):
        num_inputs = 17
        top, expected = model_par3_setup(num_inputs)
        driver = top.driver
        top.run()

        self.assertEqual(self.N_PROCS/3, driver._num_parallel_subs)

        for name, expval in expected.items():
                val = driver.case_outputs.get(name)
                if driver.workflow._system.mpi.comm != MPI.COMM_NULL:
                    for v1, v2 in zip(expval, val):
                        if isinstance(v1, np.ndarray):
                            self.assertTrue(all(v1==v2))
                        else:
                            self.assertEqual(v1, v2)
                else:
                    self.assertEqual(val, [])

class FComp(Component):

    infile = File(iotype='in', local_path='input')
    outfile = File(FileRef('output'), iotype='out')
    outval = Int(iotype='out')

    def execute(self):
        """ Runs code and sets `outfile`. """
        with open(self.infile.abspath()) as f:
            s = f.read().strip()
            val = int(s.split()[1])

        self.outval = val + 1

        fname = self.outfile.abspath()
        print "about to open",fname;sys.stdout.flush()
        with open(fname, 'w') as f:
            print "writing %s %d %d to %s" % (self.name, self.outval,
                                              MPI.COMM_WORLD.rank,fname)
            sys.stdout.flush()

            f.write("%s %d %d\n" % (self.name, self.outval, MPI.COMM_WORLD.rank))
            print "wrote %s %d %d to %s" % (self.name, self.outval,
                                              MPI.COMM_WORLD.rank,fname)
            sys.stdout.flush()


class FComp2(Component):

    inval = Int(iotype='in')
    outfile = File(FileRef('output'), iotype='out')
    outval = Int(iotype='out')

    def execute(self):
        self.outval = self.inval + 1
        fname = self.outfile.abspath()
        print "about to open",fname;sys.stdout.flush()
        with open(fname, 'w') as f:
            print "writing %s %d %d to %s" % (self.name, self.outval,
                                              MPI.COMM_WORLD.rank,fname)
            sys.stdout.flush()
            f.write("%s %d %d\n" % (self.name, self.outval, MPI.COMM_WORLD.rank))
            print "wrote %s %d %d to %s" % (self.name, self.outval,
                                              MPI.COMM_WORLD.rank,fname)
            sys.stdout.flush()


# FIXME: handling FileRefs in MPI still doesn't work
# class FileTestCase(MPITestCase):
#     """ Test the ExternalCode component. """
#
#     N_PROCS = 2
#
#     def setUp(self):
#         self.startdir = os.getcwd()
#         self.tempdir = tempfile.mkdtemp(prefix='test_distfile-')
#         os.chdir(self.tempdir)
#         SimulationRoot.chroot(self.tempdir)
#
#     def tearDown(self):
#         SimulationRoot.chroot(self.startdir)
#         os.chdir(self.startdir)
#         if not os.environ.get('OPENMDAO_KEEPDIRS', False):
#             try:
#                 shutil.rmtree(self.tempdir)
#             except OSError:
#                 pass
#
#     def test_normal(self):
#         print "CWD:",os.getcwd()
#         top = Assembly()
#         print "CWD:",os.getcwd()
#         top.create_instance_dir = 'rank'
#         driver = top.add("driver", SimpleCaseIterDriver())
#
#         top.add('C1', FComp2())
#         top.add('C2', FComp())
#         top.add('C3', FComp())
#         top.add('C4', FComp())
#         top.add('C5', FComp())
#
#         driver.add_parameter("C1.inval")
#         driver.add_response("C4.outval")
#         driver.add_response("C5.outval")
#
#         invals = [5,4,3,2,1]
#         driver.case_inputs.C1.inval = invals
#
#         top.C1.create_instance_dir = True
#         top.C2.create_instance_dir = True
#         top.C3.create_instance_dir = True
#         top.C4.create_instance_dir = True
#
#         top.driver.workflow.add(['C1','C2','C3'])
#         top.connect('C1.outfile', 'C2.infile')
#         top.connect('C1.outfile', 'C3.infile')
#         top.connect('C3.outfile', 'C4.infile')
#         top.connect('C2.outfile', 'C5.infile')
#
#         print "CWD:",os.getcwd()
#         top.run()
#         print "run done";sys.stdout.flush()
#         print driver.case_outputs.C4.outval
#
#         for i,val in enumerate(invals):
#             self.assertEqual(driver.case_outputs.C4.outval[i],
#                              val+3)
#


class SerialTests(TestCase):

    def test_fan_out_in_noflats_serial(self):
        num_inputs = 6
        top, expected = model_par3_setup(num_inputs, mpi=False)
        driver = top.driver
        top.run()

        for name, expval in expected.items():
            val = driver.case_outputs.get(name)
            for v1, v2 in zip(expval, val):
                if isinstance(v1, np.ndarray):
                    self.assertTrue(all(v1==v2))
                else:
                    self.assertEqual(v1, v2)


if __name__ == '__main__':
    from openmdao.test.mpiunittest import mpirun_tests
    mpirun_tests()
