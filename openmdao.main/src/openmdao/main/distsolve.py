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
        system = self.workflow._system
        system.vec['u'].set_from_scope(self.parent)
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
        fvec = self.workflow._system.vec['f']
        fvec.petsc_vec.assemble()
        return fvec.petsc_vec.norm()

    def run_iteration(self):
        system = self.workflow._system
        system.vec['u'].array += system.vec['f'].array[:]
        system.run(self.workflow._iterbase())
