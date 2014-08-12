"""
This is a simple iteration driver that basically runs a workflow, passing the
output to the input for the next iteration. Relative change and number of
iterations are used as termination criteria.
"""
from openmdao.main.driver import Driver
from openmdao.main.datatypes.api import Float, Int
from openmdao.main.cyclicflow import CyclicWorkflow
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.main.interfaces import IHasParameters, IHasEqConstraints, \
                                     ISolver, implements
from openmdao.util.decorators import add_delegate

from openmdao.main.mpiwrap import mpiprint, MPI

@add_delegate(HasParameters, HasEqConstraints)
class MPISolver(Driver):
    """ Base Driver class for Drivers iterating over a workflow
    of distributed components using MPI.
    """

    implements(IHasParameters, IHasEqConstraints, ISolver)

    # pylint: disable-msg=E1101
    max_iteration = Int(99, iotype='in', desc='Maximum number of '
                                         'iterations before termination.')

    tolerance = Float(1.0e-11, iotype='in', desc='Absolute convergence '
                                            'tolerance between iterations.')

    # norm_order = Enum('Infinity', ['Infinity', 'Euclidean'],
    #                    desc='For multivariable iteration, type of norm '
    #                                'to use to test convergence.')

    def __init__(self):
        super(MPISolver, self).__init__()
        self.current_iteration = 0
        self.normval = 1.e99
        self.norm0 = 1.e99
        self.workflow = CyclicWorkflow()

    def execute(self):
        """ Executes an iterative solver """
        self.current_iteration = 0
        system = self.workflow._system
        if MPI and system.mpi.comm == MPI.COMM_NULL:
            return
        super(MPISolver, self).execute()

    def _norm(self):
        """ Computes the norm that must be driven to zero """
        raise NotImplementedError("_norm method not implemented")

    def run_iteration(self):
        """ Operation executed in each iteration """
        raise NotImplementedError("run_iteration method not implemented")

    def start_iteration(self):
        """ Commands run before any iterations """
        self.current_iteration = 0
        self.normval = 1.e99
        self.norm0 = 1.e99
        self.pairs = self._get_param_constraint_pairs()
        #mpiprint("PAIRS: %s" % self.pairs)
        system = self.workflow._system
        system.vec['u'].set_from_scope(self.parent)
        #mpiprint("initial u vector: %s" % system.vec['u'].items())
        self.run_iteration()
        self.normval = self._norm()
        self.norm0 = self.normval if self.normval != 0.0 else 1.0

    def continue_iteration(self):
        return self.current_iteration < self.max_iteration and \
               self.normval > self.tolerance # and self.normval/self.norm0 > self.rtol:

    def pre_iteration(self):
        """Runs before each iteration."""
        pass

    def post_iteration(self):
        """Runs after each iteration"""
        self.normval = self._norm()
        self.current_iteration += 1
        #mpiprint("iter %d, norm = %s" % (self.current_iteration, self.normval))

class MPINonlinearSolver(MPISolver):
    """ A base class for distributed nonlinear solvers """

    def _norm(self):
        """ Computes the norm of the f Vec """
        system = self.workflow._system
        #mpiprint("NORM for %s" % system.name)
        #for n in ['block_size','local_size','owner_range','owner_ranges','size','sizes']:
        #    mpiprint("f.%s = %s" % (n,getattr(system.vec['f'].petsc_vec,n)))
        #mpiprint("f vector (pre-apply_F): %s" % system.vec['f'].items())
        #system.run(self.workflow._iterbase())
        #mpiprint("f vector (post-apply_F): %s" % system.vec['f'].items())

        system.vec['f'].petsc_vec.assemble()
        #mpiprint("f vector (post-assemble): %s" % system.vec['f'].items())
        return system.vec['f'].petsc_vec.norm()

    def run_iteration(self):
        #mpiprint("NLGS running")
        self.workflow._system.vec['u'].array[:] -= self.workflow._system.vec['f'].array[:]
        self.workflow._system.run(self.workflow._iterbase())
        #mpiprint("updating u vector with residuals")
        self.add_constraint_residuals()
        #mpiprint("UVEC: %s" % self.workflow._system.vec['u'].items())

    def add_constraint_residuals(self):
        uvec = self.workflow._system.vec['u']
        for param, cnst, sign in self.pairs:
            if sign < 0:
                uvec[param][:] += uvec[cnst]
            else:
                uvec[param][:] -= uvec[cnst]


# class MPINonlinearGS(MPINonlinearSolver):
#     """ Nonlinear block Gauss Seidel """

#     def run_iteration(self):
#         """ Solve each subsystem in series """
#         mpiprint("NLGS running")
#         system = self.workflow._system
#         for subsystem in system.local_subsystems:
#             #mpiprint("=== system U vector before %s run: %s" % (subsystem.name, system.vec['u'].items()))
#             system.scatter('u','p', subsystem)
#             subsystem.run()
#             #mpiprint("=== system U vector after %s run: %s" % (subsystem.name, system.vec['u'].items()))

#         # TODO: look at collecting params into contiguous view and
#         # constraints as well so we can do a fast numpy update with no
#         # copying.

#         # add current value of constraint residual to params
#         self.add_constraint_residuals()


# class MPINonlinearJacobi(MPINonlinearSolver):
#     """ Nonlinear block Jacobi """

#     def run_iteration(self):
#         """ Solve each subsystem in parallel """
#         system = self.workflow._system
#         system.scatter('u','p')
#         for subsystem in system.local_subsystems:
#             subsystem.run()

#         # add current value of constraint residual to params
#         self.add_constraint_residuals()


# class MPILinearSolver(MPISolver):
#     """ A base class for linear solvers """

#     def _norm(self):
#         """ Computes the norm of the linear residual """
#         system = self.workflow._system
#         system.apply_dFdpu(system.variables.keys())
#         system.rhs_vec.array[:] *= -1.0
#         system.rhs_vec.array[:] += system.rhs_buf.array[:]
#         system.rhs_vec.petsc.assemble()
#         return system.rhs_vec.petsc.norm()

#     def start_iteration(self):
#         """ Stores the rhs and initial sol vectors """
#         system = self.workflow._system
#         system.rhs_buf.array[:] = system.rhs_vec.array[:]
#         system.sol_buf.array[:] = system.sol_vec.array[:]
#         norm = self._norm()
#         norm0 = norm if norm != 0.0 else 1.0
#         return norm0, norm

#     def end_iteration(self):
#         """ Copy the sol vector ; used for KSP solver """
#         system = self.workflow._system
#         system.sol_vec.array[:] = system.sol_buf.array[:]


# class MPIIdentity(MPILinearSolver):
#     """ Identity mapping; no preconditioning """

#     def execute(self):
#         """ Just copy the rhs to the sol vector """
#         system = self.workflow._system
#         system.sol_vec.array[:] = system.rhs_vec.array[:]
