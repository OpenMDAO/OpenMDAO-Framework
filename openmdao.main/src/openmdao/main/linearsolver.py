""" Linear solvers that are used to solve for the gradient of an OpenMDAO System.
"""

from scipy.sparse.linalg import gmres, LinearOperator

class LinearSolver(object):
    """ A base class for linear solvers """

    def _norm(self):
        """ Computes the norm of the linear residual """

        pass
        #system = self._system
        #system.rhs_vec.array[:] = 0.0
        #system.apply_dFdpu(system.variables.keys())
        #system.rhs_vec.array[:] *= -1.0
        #system.rhs_vec.array[:] += system.rhs_buf.array[:]
        #system.rhs_vec.petsc.assemble()
        #return system.rhs_vec.petsc.norm()

    def _initialize(self):
        """ Stores the rhs and initial sol vectors """

        pass
        #system = self._system
        #system.rhs_buf.array[:] = system.rhs_vec.array[:]
        #system.sol_buf.array[:] = system.sol_vec.array[:]
        #if system.kwargs['LN_ilimit'] > 1:
            #norm = self._norm()
        #else:
            #norm = 1.0
        #norm0 = norm if norm != 0.0 else 1.0
        #return norm0, norm

    def _finalize(self):
        """ Copy the sol vector ; used for KSP solver """

        pass
        #system = self._system
        #system.sol_vec.array[:] = system.sol_buf.array[:]

class ScipyGMRES(LinearSolver):
    """ Scipy's GMRES Solver. This is a serial solver, so should never be used
    in an MPI setting."""

    def __init__(self, system):
        """ Set up ScipyGMRES object """

    def solve(self):
        """ Run GMRES solver """

        system = self._system
        self.ksp.setTolerances(max_it=ilimit, atol=atol, rtol=rtol)
        self._initialize()
        self.ksp.solve(system.rhs_buf, system.sol_buf)
        self._finalize()

    def mult(self, arg):
        """ GMRES Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = arg[:]
        system.apply_dFdpu(system.variables.keys())
        return system.rhs_vec.array[:]

