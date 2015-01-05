
from unittest import TestCase
import time

import numpy as np

from openmdao.test.mpiunittest import MPITestCase, collective_assert_rel_error
from openmdao.util.testutil import assert_rel_error
from openmdao.main.test.simpledriver import SimpleDriver

from openmdao.main.api import Assembly, Component, set_as_top, Driver
from openmdao.main.datatypes.api import Float, Array
from openmdao.main.mpiwrap import MPI
from openmdao.lib.drivers.iterate import FixedPointIterator
from openmdao.test.execcomp import ExecComp
from openmdao.main.test.test_derivatives import SimpleDriver

from openmdao.lib.optproblems import sellar

class NoDerivSimpleDriver(SimpleDriver):
    def requires_derivs(self):
        return False
        
class NoDerivSimpleDriverSetter(NoDerivSimpleDriver):
    def __init__(self, *args, **kwargs):
        super(NoDerivSimpleDriverSetter, self).__init__(*args, **kwargs)
        self.vals = []
        
    def execute(self):
        self.set_parameters(self.vals)
        self.workflow.run()

        
class ABCDArrayComp(Component):
    delay = Float(0.01, iotype='in')

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

        Optimal Objective = 3.18339"""

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

        self.collective_assertTrue(all(top.C3.a==np.ones(size, float)*10.))
        self.collective_assertTrue(all(top.C3.b==np.ones(size, float)*-1.))
        self.collective_assertTrue(all(top.C3.c==np.ones(size, float)*9.))
        self.collective_assertTrue(all(top.C3.d==np.ones(size, float)*11.))

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

        # if self.comm.rank == 0:
        #     from openmdao.util.dotgraph import plot_system_tree
        #     plot_system_tree(top._system)
            
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

        self.collective_assertTrue(all(top.C3.a==np.ones(size, float)*10.))
        self.collective_assertTrue(all(top.C3.b==np.ones(size, float)*-1.))
        self.collective_assertTrue(all(top.C3.c==np.ones(size, float)*9.))
        self.collective_assertTrue(all(top.C3.d==np.ones(size, float)*11.))

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

        print top.C3.a
        
        self.collective_assertTrue(all(top.C3.a==np.ones(size, float)*6.))
        self.collective_assertTrue(all(top.C3.b==np.ones(size, float)*4.))
        self.collective_assertTrue(all(top.C3.c==np.ones(size, float)*10.))
        self.collective_assertTrue(all(top.C3.d==np.ones(size, float)*2.))


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
        
        self.collective_assertTrue(all(top.C4.a==np.ones(size, float)*11.))
        self.collective_assertTrue(all(top.C4.b==np.ones(size, float)*5.))

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

            def pre_iteration(self):
               # Direct uvec setting
                uvec = self._system.vec['u']
                print uvec.keys()
                # Only can interact with the var that is in our node
                if 'branch0_comp0.x' in uvec:
                    uvec['branch0_comp0.x'] = 1.0
                if 'branch1_comp0.x' in uvec:
                    uvec['branch1_comp0.x'] = 1.0

            def requires_derivs(self):
                return False               
        
        top = set_as_top(Assembly())
        top.add('driver', MyDriver())
        top.add('branch0_comp0', ExecComp(['y = 2.0*x']))
        top.add('branch0_comp1', ExecComp(['y = 1.0*x']))
        top.add('branch1_comp0', ExecComp(['y = 3.0*x']))
        top.add('branch1_comp1', ExecComp(['y = 1.0*x']))
        top.add('sum', ExecComp(['y = x1 + x2']))
        top.driver.workflow.add(['branch0_comp0', 'branch0_comp1',
                                 'branch1_comp0', 'branch1_comp1',
                                 'sum'])
        top.driver.add_parameter('branch0_comp0.x', low=-100, high=100)
        top.driver.add_parameter('branch1_comp0.x', low=-100, high=100)
        top.driver.add_objective('sum.y')
        
        # These lock up the process
        #top.driver.add_constraint('branch1_comp1.y < branch0_comp0.x')
        #top.driver.add_constraint('branch0_comp1.y < branch1_comp0.x')

        # So do these
        #top.driver.add_constraint('branch1_comp0.x < sum.y')
        #top.driver.add_constraint('branch0_comp0.x < sum.y')
        
        top.connect('branch0_comp0.y', 'branch0_comp1.x')
        top.connect('branch1_comp0.y', 'branch1_comp1.x')
        top.connect('branch0_comp1.y', 'sum.x1')
        top.connect('branch1_comp1.y', 'sum.x2')
        
        top.run()
        print top.sum.y
        # from openmdao.util.dotgraph import plot_system_tree
        # plot_system_tree(top.driver.workflow._system)

        self.collective_assertTrue(top.sum.y==5.0)
        
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

        top.driver.add_parameter('C2.y1', low=-1e99, high=1e99)
        top.driver.add_constraint('C1.y1 = C2.y1')
        top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
        top.driver.add_constraint('C2.y2 = C1.y2')

        expected = { 'C1.y1': 3.160068, 'C2.y2': 3.755315 }

        top.run()
        
        #top._system.dump()

        if self.comm.rank == 0:
            #from openmdao.util.dotgraph import plot_graph, plot_system_tree
            #plot_graph(top.driver.workflow._reduced_graph, 'rgraph.pdf')
            #plot_system_tree(top._system, 'system.pdf')
            for name, expval in expected.items():
                val = top.get(name)
                assert_rel_error(self, val, expval, 0.001)

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

    
# FIXME: running this file as main currently doesn't work...
# if __name__ == '__main__':
#     import unittest
#     unittest.main()
