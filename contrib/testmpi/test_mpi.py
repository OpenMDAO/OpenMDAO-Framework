
from unittest import TestCase
import time

import numpy as np

from openmdao.lib.drivers.iterate import FixedPointIterator
from openmdao.lib.drivers.newton_solver import NewtonSolver
from openmdao.lib.optproblems import sellar

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array, Str, List
from openmdao.main.interfaces import implements, ISolver
from openmdao.main.mpiwrap import MPI, MPIContext
from openmdao.main.test.simpledriver import SimpleDriver
from openmdao.test.execcomp import ExecComp
from openmdao.test.mpiunittest import MPITestCase
from openmdao.util.testutil import assert_rel_error


class NoDerivSimpleDriver(SimpleDriver):
    def requires_derivs(self):
        return False

class NoDerivSimpleDriverSetter(NoDerivSimpleDriver):
    def __init__(self, *args, **kwargs):
        super(NoDerivSimpleDriverSetter, self).__init__(*args, **kwargs)
        self.vals = []

    def execute(self):
        self.set_parameters(self.vals)
        self.run_iteration()


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

    def dump(self, comm):
        print "%d: %s.a = %s" % (comm.rank, self.name, self.a)
        print "%d: %s.b = %s" % (comm.rank, self.name, self.b)
        print "%d: %s.c = %s" % (comm.rank, self.name, self.c)
        print "%d: %s.d = %s" % (comm.rank, self.name, self.d)

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

class SellarMDFwithDerivs(Assembly):
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
        C1 = self.add('C1', sellar.Discipline1_WithDerivatives())
        C2 = self.add('C2', sellar.Discipline2_WithDerivatives())

        self.driver.workflow.add(['C1','C2'])

        #not relevant to the iteration. Just fixed constants
        C1.z1 = C2.z1 = 1.9776
        C1.z2 = C2.z2 = 0
        C1.x1 = 0

        # Solver settings
        self.driver.max_iteration = 5
        self.driver.tolerance = 1.e-15
        self.driver.print_convergence = False


class MPITests1(MPITestCase):

    N_PROCS = 2

    def test_sellar_params1(self):
        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C1.y2 = C2.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()

        if self.comm.rank == 0:
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, 0.001)

    def test_sellar_params2(self):
        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')

        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C2.y2 = C1.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()

        if self.comm.rank == 0:
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, 0.001)

    def test_simple_opaque(self):
        size = 5

        # 2 parallel comps feeding another comp
        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())
        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2'])
        top.connect('C1.c', 'C2.a')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0
        top.C2.b = np.ones(size, float) * 5.0

        top.run()

        if self.comm.rank == 0:
            self.assertTrue(all(top.C2.a==np.ones(size, float)*10.))
            self.assertTrue(all(top.C2.b==np.ones(size, float)*5.))
            self.assertTrue(all(top.C2.c==np.ones(size, float)*15.))
            self.assertTrue(all(top.C2.d==np.ones(size, float)*5.))

    def test_fan_in(self):
        size = 5

        # 2 parallel comps feeding another comp
        top = set_as_top(Assembly())
        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3'])
        top.connect('C1.c', 'C3.a')
        top.connect('C2.d', 'C3.b')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0
        top.C2.a = np.ones(size, float) * 4.0
        top.C2.b = np.ones(size, float) * 5.0

        top.run()

        self.assertTrue(all(top.C3.a==np.ones(size, float)*10.))
        self.assertTrue(all(top.C3.b==np.ones(size, float)*-1.))
        self.assertTrue(all(top.C3.c==np.ones(size, float)*9.))
        self.assertTrue(all(top.C3.d==np.ones(size, float)*11.))

    def test_fan_in_simpledriver(self):
        size = 5

        # 2 parallel comps feeding another comp
        top = set_as_top(Assembly())
        top.add('driver', SimpleDriver())

        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3'])
        top.connect('C1.c', 'C3.a')
        top.connect('C2.d', 'C3.b')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0
        top.C2.a = np.ones(size, float) * 4.0
        top.C2.b = np.ones(size, float) * 5.0

        top.driver.add_parameter('C1.a', low=-1000, high=1000)
        top.driver.add_parameter('C2.a', low=-1000, high=1000)
        top.driver.add_objective('C3.d')

        top.run()

        # top.C1.dump(self.comm)
        # top.C2.dump(self.comm)

        if self.comm.rank == 0:
            self.assertTrue(all(top.C3.a==np.ones(size, float)*10.))
            self.assertTrue(all(top.C3.b==np.ones(size, float)*-1.))
            self.assertTrue(all(top.C3.c==np.ones(size, float)*9.))
            self.assertTrue(all(top.C3.d==np.ones(size, float)*11.))

    def test_fan_in_simpledriver_noderiv(self):
        size = 5

        # 2 parallel comps feeding another comp
        top = set_as_top(Assembly())
        top.add('driver', NoDerivSimpleDriver())

        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3'])
        top.connect('C1.c', 'C3.a')
        top.connect('C2.d', 'C3.b')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0
        top.C2.a = np.ones(size, float) * 4.0
        top.C2.b = np.ones(size, float) * 5.0

        top.driver.add_parameter('C1.a', low=-1000, high=1000)
        top.driver.add_parameter('C2.a', low=-1000, high=1000)
        top.driver.add_objective('C3.d')

        top.run()

        self.assertTrue(all(top.C3.a==np.ones(size, float)*10.))
        self.assertTrue(all(top.C3.b==np.ones(size, float)*-1.))
        self.assertTrue(all(top.C3.c==np.ones(size, float)*9.))
        self.assertTrue(all(top.C3.d==np.ones(size, float)*11.))

    def test_fan_in_simpledriver_setting_params(self):
        size = 5

        # 2 parallel comps feeding another comp
        top = set_as_top(Assembly())
        top.add('driver', NoDerivSimpleDriverSetter())

        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3'])
        top.connect('C1.c', 'C3.a')
        top.connect('C2.d', 'C3.b')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0
        top.C2.a = np.ones(size, float) * 4.0
        top.C2.b = np.ones(size, float) * 5.0

        top.driver.add_parameter('C1.a', low=-1000, high=1000)
        top.driver.add_parameter('C2.a', low=-1000, high=1000)
        top.driver.add_objective('C3.d')
        top.driver.vals = [-1.,-1.,-1.,-1.,-1,9.,9.,9.,9.,9.]

        top.run()

        #if self.comm.rank == 0:
        #    from openmdao.util.dotgraph import plot_graph, plot_graphs, plot_system_tree
        #    plot_graphs(top, prefix="works")

        self.assertTrue(all(top.C3.a==np.ones(size, float)*6.))
        self.assertTrue(all(top.C3.b==np.ones(size, float)*4.))
        self.assertTrue(all(top.C3.c==np.ones(size, float)*10.))
        self.assertTrue(all(top.C3.d==np.ones(size, float)*2.))


    def test_fan_out_in(self):
        size = 5   # array var size

        # a comp feeds two parallel comps which feed
        # another comp
        top = set_as_top(Assembly())
        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.add("C4", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3', 'C4'])
        top.connect('C1.c', 'C2.a')
        top.connect('C1.d', 'C3.b')
        top.connect('C2.c', 'C4.a')
        top.connect('C3.d', 'C4.b')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0

        top.run()

        with MPIContext():
            self.assertTrue(all(top.C4.a==np.ones(size, float)*11.))
            self.assertTrue(all(top.C4.b==np.ones(size, float)*5.))

        # Piggyback testing of the is_variable_local function.
        system = top.driver.workflow._system

        # Only lowest rank has vars that are on all proceses
        if self.comm.rank == 0:
            self.assertTrue(system.is_variable_local('C1.c'))
            self.assertTrue(system.is_variable_local('C2.a'))
            self.assertTrue(system.is_variable_local('C3.b'))
            self.assertTrue(system.is_variable_local('C1.exec_count'))
        else:
            self.assertFalse(system.is_variable_local('C1.c'))
            self.assertFalse(system.is_variable_local('C2.a'))
            self.assertFalse(system.is_variable_local('C3.b'))
            self.assertFalse(system.is_variable_local('C1.exec_count'))

        # Exclusive or - you either got C2 or C3 on a given process.
        self.assertTrue(system.is_variable_local('C2.c') != system.is_variable_local('C3.d'))
        self.assertTrue(system.is_variable_local('C4.a') != system.is_variable_local('C4.b'))


    def test_fan_out_in_force_serial(self):
        size = 5  # array var size

        top = set_as_top(Assembly())
        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.add("C4", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3', 'C4'])
        top.connect('C1.c', 'C2.a')
        top.connect('C1.d', 'C3.b')
        top.connect('C2.c', 'C4.a')
        top.connect('C3.d', 'C4.b')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0

        top.driver.system_type = 'serial'

        top.run()

        if self.comm.rank == 0:
            self.assertTrue(all(top.C4.a==np.ones(size, float)*11.))
            self.assertTrue(all(top.C4.b==np.ones(size, float)*5.))

    def test_serial_under_par(self):

        class MyDriver(SimpleDriver):

            implements(ISolver)

            def execute(self):
               # Direct uvec setting
                uvec = self._system.vec['u']
                #print uvec.keys()
                # Only can interact with the var that is in our node
                for num in [1.0, 2.0, 3.0]:
                    if 'comp1.x' in uvec:
                        uvec['comp1.x'] = num
                        #print "SETTING", 'comp1.x', uvec['comp1.x']
                    if 'comp2.x' in uvec:
                        uvec['comp2.x'] = num
                        #print "SETTING", 'comp2.x', uvec['comp2.x']

                    self.run_iteration()

            def requires_derivs(self):
                return False

        top = set_as_top(Assembly())
        top.add('driver', MyDriver())
        top.add('comp1', ExecComp(['y = 2.0*x']))
        top.add('comp2', ExecComp(['y = 1.0*x']))
        top.driver.workflow.add(['comp1', 'comp2'])
        top.driver.add_parameter('comp1.x', low=-100, high=100)
        top.driver.add_parameter('comp2.x', low=-100, high=100)
        top.driver.add_constraint('comp1.y = comp2.x')
        top.driver.add_constraint('comp2.y = comp1.x')

        top.run()

        self.assertTrue(top.comp1.x==3.0)
        self.assertTrue(top.comp2.x==3.0)


class MPITests2(MPITestCase):

    N_PROCS = 2

    def test_sellar_cyclic(self):

        top = set_as_top(SellarMDF())

        top.connect('C1.y1','C2.y1')
        top.connect('C2.y2', 'C1.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()

        if self.comm.rank == 0:
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, 0.001)

    def test_sellar_parallel(self):

        top = set_as_top(SellarMDF())

        top.driver.max_iteration = 25
        top.driver.add_parameter('C2.y1', low=-1e99, high=1e99)
        top.driver.add_constraint('C1.y1 = C2.y1')
        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C2.y2 = C1.y2')

        expected = { 'C1.y1': 3.1598617768014536, 'C2.y2': 3.7551999159927316 }

        top.run()

        # gather the values back to the rank 0 process and compare to expected
        dist_answers = top._system.mpi.comm.gather([(k[0],v) for k,v in top._system.vec['u'].items()],
                                                   root=0)
        if self.comm.rank == 0:
            for answers in dist_answers:
                for name, val in answers:
                    if name in expected:
                        #print self.comm.rank, name, val[0]
                        assert_rel_error(self, val[0], expected[name], 0.001)
                        del expected[name]

            if expected:
                self.fail("expected values %s were not found" % expected.keys())

        # Piggyback testing of the is_variable_local function.
        system = top.driver.workflow._system

        # Only lowest rank has vars that are on all proceses
        if self.comm.rank == 0:
            self.assertTrue(system.is_variable_local('_pseudo_0.out0'))

            # Params on lowest rank
            self.assertTrue(system.is_variable_local('C1.y2'))
            self.assertTrue(system.is_variable_local('C2.y1'))
        else:
            self.assertFalse(system.is_variable_local('_pseudo_0.out0'))
            self.assertFalse(system.is_variable_local('C1.y2'))
            self.assertFalse(system.is_variable_local('C2.y1'))

        # Exclusive or - you either got C2 or C3 on a given process.
        self.assertTrue(system.is_variable_local('C1.y1') != system.is_variable_local('C2.y2'))
        self.assertTrue(system.is_variable_local('C1.exec_count') != system.is_variable_local('C2.exec_count'))

    def test_sellar_Newton_parallel(self):

        top = set_as_top(SellarMDFwithDerivs())
        top.replace('driver', NewtonSolver())

        top.driver.add_parameter('C2.y1', low=-1e99, high=1e99)
        top.driver.add_constraint('C1.y1 = C2.y1')
        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C2.y2 = C1.y2')

        expected = { 'C1.y1': 3.1598617768014536, 'C2.y2': 3.7551999159927316 }

        top.driver.iprint = 0
        top.driver.max_iteration = 20
        top.run()
        # print top.C1.y1, top.C2.y1
        # print top.C1.y2, top.C2.y2

        # gather the values back to the rank 0 process and compare to expected
        dist_answers = top._system.mpi.comm.gather([(k[0],v) for k,v in top._system.vec['u'].items()],
                                                   root=0)
        if self.comm.rank == 0:
            for answers in dist_answers:
                for name, val in answers:
                    if name in expected:
                        #print self.comm.rank, name, val[0]
                        assert_rel_error(self, val[0], expected[name], 0.001)
                        del expected[name]

            if expected:
                self.fail("not all expected values were found")


class MPITests3(MPITestCase):

    N_PROCS = 3

    def test_fan_out_in_noflats(self):
        size = 5   # array var size

        # a comp feeds 3 parallel comps which feed
        # another comp
        top = set_as_top(Assembly())
        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.add("C4", ABCDArrayComp(size))
        top.add("C5", ABCDArrayComp(size))
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

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0

        top.C1.in_string = 'foo'
        top.C1.in_list = [1, 1, 1]

        top.run()

        self.assertTrue(all(top.C5.a==np.ones(size, float)*11.))
        self.assertTrue(all(top.C5.b==np.ones(size, float)*5.))

        self.assertEqual(top.C5.out_string, 'foo_C1_C2_C5')
        self.assertEqual(top.C5.out_list, [1, 1, 1, 1.5, 1.5, 1.5])


class TestCaseSerial(TestCase):
    def test_sellar_p_serial(self):

        top = set_as_top(SellarMDF())

        top.driver.add_parameter('C2.y1', low=-1e99, high=1e99)
        top.driver.add_constraint('C1.y1 = C2.y1')
        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C2.y2 = C1.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()

        #top._system.dump()

        #from openmdao.util.dotgraph import plot_graph, plot_system_tree
        #plot_graph(top.driver.workflow._reduced_graph, 'rgraph.pdf')
        #plot_system_tree(top._system, 'system.pdf')
        for name, expval in expected.items():
            val = top.get(name)
            assert_rel_error(self, val, expval, 0.001)

    def test_fan_in_simpledriver(self):
        size = 5

        # 2 parallel comps feeding another comp
        top = set_as_top(Assembly())
        top.add('driver', NoDerivSimpleDriver())

        top.add("C1", ABCDArrayComp(size))
        top.add("C2", ABCDArrayComp(size))
        top.add("C3", ABCDArrayComp(size))
        top.driver.workflow.add(['C1', 'C2', 'C3'])
        top.connect('C1.c', 'C3.a')
        top.connect('C2.d', 'C3.b')

        top.C1.a = np.ones(size, float) * 3.0
        top.C1.b = np.ones(size, float) * 7.0
        top.C2.a = np.ones(size, float) * 4.0
        top.C2.b = np.ones(size, float) * 5.0

        top.driver.add_parameter('C1.a', low=-1000, high=1000)
        top.driver.add_objective('C3.d')

        top.run()

        self.assertTrue(all(top.C3.a==np.ones(size, float)*10.))
        self.assertTrue(all(top.C3.b==np.ones(size, float)*-1.))
        self.assertTrue(all(top.C3.c==np.ones(size, float)*9.))
        self.assertTrue(all(top.C3.d==np.ones(size, float)*11.))


if __name__ == '__main__':
    from openmdao.test.mpiunittest import mpirun_tests
    mpirun_tests()
