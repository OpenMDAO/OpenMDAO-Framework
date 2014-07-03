""" Linear solvers that are used to solve for the gradient of an OpenMDAO System.
"""

# pylint: disable=E0611, F0401
from numpy import zeros

from scipy.sparse.linalg import gmres, LinearOperator

from openmdao.util.log import logger

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
        self._system = system
        self.inputs = None

    def solve(self, inputs, outputs):
        """ Run GMRES solver """

        system = self._system

        # Size the problem
        num_input = system.vec['p'].array.size
        num_output = system.vec['u'].array.size - num_input
        n_edge = system.vec['f'].array.size

        J = zeros((num_input, num_output))
        A = LinearOperator((n_edge, n_edge),
                           matvec=self.mult,
                           dtype=float)

        if system.mode == 'adjoint':
            temp = inputs
            inputs = outputs
            outputs = temp

        # Forward mode, solve linear system for each parameter
        j = 0
        for param in inputs:

            if isinstance(param, tuple):
                param = param[0]

            i1, i2 = system.get_var_info()

            if isinstance(i1, list):
                in_range = i1
            else:
                in_range = range(i1, i2)

            for irhs in in_range:

                RHS = zeros((n_edge, 1))
                RHS[irhs, 0] = 1.0

                # Call GMRES to solve the linear system
                dx, info = gmres(A, RHS,
                                 tol=options.gmres_tolerance,
                                 maxiter=options.gmres_maxiter)
                if info > 0:
                    msg = "ERROR in calc_gradient in '%s': gmres failed to converge " \
                          "after %d iterations for parameter '%s' at index %d"
                    logger.error(msg, wflow.parent.get_pathname(), info, param, irhs)
                elif info < 0:
                    msg = "ERROR in calc_gradient in '%s': gmres failed " \
                          "for parameter '%s' at index %d"
                    logger.error(msg, wflow.parent.get_pathname(), param, irhs)

                i = 0
                for item in outputs:
                    try:
                        k1, k2 = bounds[item]
                    except KeyError:
                        i += wflow.get_width(item)
                        continue

                    if isinstance(k1, list):
                        J[i:i+(len(k1)), j] = dx[k1]
                        i += len(k1)
                    else:
                        J[i:i+(k2-k1), j] = dx[k1:k2]
                        i += k2-k1

                j += 1

        #print inputs, '\n', outputs, '\n', J
        return J

    def mult(self, arg):
        """ GMRES Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = arg[:]
        system.apply_dFdpu(system.variables.keys())
        return system.rhs_vec.array[:]

