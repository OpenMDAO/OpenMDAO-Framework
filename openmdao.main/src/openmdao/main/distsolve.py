"""
This is a simple iteration driver that basically runs a workflow, passing the
output to the input for the next iteration. Relative change and number of
iterations are used as termination criteria.
"""
import numpy

from openmdao.main.driver import Driver
from openmdao.main.datatypes.api import Float, Int, Enum
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
        self._itercount = 0
        self.normval = 1.e99
        self.norm0 = 1.e99
        self.workflow = CyclicWorkflow()

    def execute(self):
        """ Executes an iterative solver """
        self._itercount = 0
        system = self.workflow.get_subsystem()
        if system.mpi.comm == MPI.COMM_NULL:
            return
        super(MPISolver, self).execute()

    def _norm(self):
        """ Computes the norm that must be driven to zero """
        raise NotImplementedError("_norm method not implemented")

    def run_iteration(self):
        """ Operation executed in each iteration """
        raise NotImplementedError("run_iteration method not implemented")

    def start_iteration(self):
        """ Commands run before iteration """
        self._itercount = 0
        self.normval = 1.e99
        self.norm0 = 1.e99
        system = self.workflow.get_subsystem()
        system.vec['u'].set_from_scope(self.parent)
        #mpiprint("initial u vector: %s" % system.vec['u'].items())
        self.normval = self._norm()
        self.norm0 = self.normval if self.normval != 0.0 else 1.0

    def continue_iteration(self):
        return self._itercount < self.max_iteration and self.normval > self.tolerance # and self.normval/self.norm0 > self.rtol:

    def post_iteration(self):
        self.normval = self._norm()
        self._itercount += 1
        mpiprint("iter %d, norm = %s" % (self._itercount, self.normval))
        
class MPINonlinearSolver(MPISolver):
    """ A base class for dostribited nonlinear solvers """

    def _norm(self):
        """ Computes the norm of the f Vec """
        system = self.workflow.get_subsystem()
        #mpiprint("@@@ NORM for %s" % system.name)
        #for n in ['block_size','local_size','owner_range','owner_ranges','size','sizes']:
        #    mpiprint("@@@ f.%s = %s" % (n,getattr(system.vec['f'].petsc_vec,n)))
        #mpiprint("@@@ f vector (pre-apply_F): %s" % system.vec['f'].items())
        system.apply_F()
        #mpiprint("@@@ f vector (post-apply_F): %s" % system.vec['f'].items())

        # account for duplication of the same values in the global
        # f vector
        # if system.mpi.comm.size > system.mpi.comm.rank+1:
        #     mpiprint("@@@ size = %d,  rank = %d" % (system.mpi.comm.size,system.mpi.comm.rank))
        #     temp = system.vec['f'].array.copy()
        #     system.vec['f'].array[:] = numpy.zeros(system.vec['f'].array.size)
        system.vec['f'].petsc_vec.assemble()
        #mpiprint("@@@ f vector (post-assemble): %s" % system.vec['f'].items())
        val = system.vec['f'].petsc_vec.norm()
        # if system.mpi.comm.size > system.mpi.comm.rank+1:
        #     system.vec['f'].array[:] = temp
        #mpiprint("@@@ norm = %s" % val)
        system.vec['f'].petsc_vec.assemble()
        return val


class MPINonlinearGS(MPINonlinearSolver):
    """ Nonlinear block Gauss Seidel """

    def run_iteration(self):
        """ Solve each subsystem in series """
        mpiprint("NLGS running")
        system = self.workflow.get_subsystem()
        for subsystem in system.subsystems:
            #mpiprint("=== system U vector before %s run: %s" % (subsystem.name, system.vec['u'].items()))
            system.scatter('u','p', subsystem)
            subsystem.run()
            #mpiprint("=== system U vector after %s run: %s" % (subsystem.name, system.vec['u'].items()))


class MPINonlinearJacobi(MPINonlinearSolver):
    """ Nonlinear block Jacobi """

    def run_iteration(self):
        """ Solve each subsystem in parallel """
        system = self._system
        system.scatter('u','p')
        for subsystem in system.subsystems:
            subsystem.run()
