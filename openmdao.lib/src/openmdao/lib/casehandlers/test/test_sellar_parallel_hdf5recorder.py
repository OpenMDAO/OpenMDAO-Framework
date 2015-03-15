from openmdao.lib.casehandlers.api import JSONCaseRecorder, BSONCaseRecorder, verify_json, CaseDataset, HDF5CaseRecorder
import time

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
        self.driver.max_iteration = 10
        self.driver.tolerance = 1.e-15
        #self.driver.tolerance = 1.e-1
        self.driver.print_convergence = False


top = set_as_top(SellarMDF())

top.driver.max_iteration = 30
top.driver.max_iteration = 1
top.driver.add_parameter('C2.y1', low=-1e99, high=1e99)
top.driver.add_constraint('C1.y1 = C2.y1')
top.driver.add_parameter('C1.y2', low=-1.e99, high=1.e99)
top.driver.add_constraint('C2.y2 = C1.y2')

expected = { 'C1.y1': 3.1598617768014536, 'C2.y2': 3.7551999159927316 }

if MPI:
    top.recorders = [HDF5CaseRecorder('SellarMDF_parallel.hdf5')]
else:
    top.recorders = [HDF5CaseRecorder('SellarMDF_serial.hdf5')]

top.run()


# from openmdao.util.dotgraph import plot_graph, plot_system_tree
# plot_graph(top.driver.workflow._reduced_graph, 'rgraph.pdf')
# plot_system_tree(top._system, 'system.pdf')

# gather the values back to the rank 0 process and compare to expected
# dist_answers = top._system.mpi.comm.gather([(k[0],v) for k,v in top._system.vec['u'].items()], 
#                                            root=0)
# if self.comm.rank == 0:
#     for answers in dist_answers:
#         for name, val in answers:
#             if name in expected:
#                 #print self.comm.rank, name, val[0]
#                 assert_rel_error(self, val[0], expected[name], 0.001)
#                 del expected[name]

#     if expected:
#         self.fail("expected values %s were not found" % expected.keys())
