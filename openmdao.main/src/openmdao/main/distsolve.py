"""
This is a simple iteration driver that basically runs a workflow, passing the
output to the input for the next iteration. Relative change and number of
iterations are used as termination criteria.
"""
from openmdao.main.driver import Driver
from openmdao.main.datatypes.api import Float, Int, Enum
from openmdao.main.cyclicflow import CyclicWorkflow
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.main.interfaces import IHasParameters, IHasEqConstraints, \
                                     ISolver, implements
from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions

from openmdao.main.mpiwrap import mpiprint, MPI

if not MPI:
    from numpy.linalg import norm

# @add_delegate(HasParameters, HasEqConstraints, HasStopConditions)
# class FixedPointIterator(Driver):
#     """ A simple fixed point iteration driver, which runs a workflow and passes
#     the value from the output to the input for the next iteration. Relative
#     change and number of iterations are used as termination criteria. This type
#     of iteration is also known as Gauss-Seidel."""

#     implements(IHasParameters, IHasEqConstraints, ISolver)

#     # pylint: disable-msg=E1101
#     max_iteration = Int(25, iotype='in', desc='Maximum number of '
#                                          'iterations before termination.')

#     tolerance = Float(1.0e-3, iotype='in', desc='Absolute convergence '
#                                             'tolerance between iterations.')

#     norm_order = Enum('Infinity', ['Infinity', 'Euclidean'],
#                        desc='For multivariable iteration, type of norm '
#                                    'to use to test convergence.')

#     def __init__(self):
#         super(FixedPointIterator, self).__init__()
#         self.current_iteration = 0
#         self.workflow = CyclicWorkflow()
#         self.normval = 1.e99
#         self.norm0 = 1.e99

#         # user either the petsc norm or numpy.linalg norm
#         if MPI:
#             self.norm = self._mpi_norm

#     def execute(self):
#         """ Executes an iterative solver """
#         self.current_iteration = 0
#         if MPI:
#             if self.workflow._system.mpi.comm == MPI.COMM_NULL:
#                 return
#         else:
#             if self.norm_order == 'Infinity':
#                 self._norm_order = float('inf')
#             else:
#                 self._norm_order = self.norm_order

#         super(FixedPointIterator, self).execute()

#     def start_iteration(self):
#         """ Commands run before any iterations """
#         self.current_iteration = 0
#         self.normval = 1.e99
#         self.norm0 = 1.e99
#         system = self.workflow._system
#         system.vec['u'].set_from_scope(self.parent)
#         self.run_iteration()
#         self.normval = self.norm()
#         self.norm0 = self.normval if self.normval != 0.0 else 1.0

#     def run_iteration(self):
#         system = self.workflow._system
#         system.vec['u'].array += system.vec['f'].array[:]
#         system.run(self.workflow._iterbase())

#     def continue_iteration(self):
#         return self.current_iteration < self.max_iteration and \
#                self.normval > self.tolerance # and self.normval/self.norm0 > self.rtol:

#     def post_iteration(self):
#         """Runs after each iteration"""
#         self.normval = self.norm()
#         self.current_iteration += 1
#         #mpiprint("iter %d, norm = %s" % (self.current_iteration, self.normval))

#     def _mpi_norm(self):
#         """ Compute the norm of the f Vec using petsc. """
#         fvec = self.workflow._system.vec['f']
#         fvec.petsc_vec.assemble()
#         return fvec.petsc_vec.norm()

#     def norm(self):
#         """ Compute the norm using numpy.linalg. """
#         return norm(self.workflow._system.vec['f'].array, 
#                     self._norm_order)

