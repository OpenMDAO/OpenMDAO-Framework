""" Linear solvers that are used to solve for the gradient of an OpenMDAO System.
(Not to be confused with the OpenMDAO Solver classes.)
"""

# pylint: disable=E0611, F0401
import numpy as np
from scipy.sparse.linalg import gmres, LinearOperator

from openmdao.main.mpiwrap import MPI
from openmdao.util.log import logger

if MPI:
    from petsc4py import PETSc
else:
    class PETSc(object):
        # Dummy class so things parse.
        pass

class LinearSolver(object):
    """ A base class for linear solvers """

    def __init__(self, system):
        """ Set up any LinearSolver object """
        self._system = system
        self.options = system.options
        self.inputs = None

class ScipyGMRES(LinearSolver):
    """ Scipy's GMRES Solver. This is a serial solver, so should never be used
    in an MPI setting."""

    def solve(self, inputs, outputs):
        """ Run GMRES solver to return a Jacobian of outputs with respect to
        inputs."""

        system = self._system
        options = self.options
        self.inputs = inputs

        # Size the problem
        num_input = system.get_size(inputs)
        num_output = system.get_size(outputs)
        n_edge = system.vec['f'].array.size

        J = np.zeros((num_output, num_input))
        RHS = np.zeros((n_edge, 1))
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

            indices = system.vec['u'].indices(param)
            i1 = indices[0]
            if len(indices) == 2:
                i2 = indices[1]
            else:
                i2 = i1 + 1

            for irhs in range(i1, i2):

                RHS[irhs, 0] = 1.0

                # Call GMRES to solve the linear system
                dx, info = gmres(A, RHS,
                                 tol=options.gmres_tolerance,
                                 maxiter=options.gmres_maxiter)
                print 'dx', dx
                if info > 0:
                    msg = "ERROR in calc_gradient in '%s': gmres failed to converge " \
                          "after %d iterations for parameter '%s' at index %d"
                    logger.error(msg, system.name, info, param, irhs)
                elif info < 0:
                    msg = "ERROR in calc_gradient in '%s': gmres failed " \
                          "for parameter '%s' at index %d"
                    logger.error(msg, system.name, param, irhs)

                RHS[irhs, 0] = 0.0

                i = 0
                for item in outputs:

                    if isinstance(item, tuple):
                        item = item[0]

                    indices = system.vec['u'].indices(item)
                    k1 = indices[0]
                    if len(indices) == 2:
                        k2 = indices[1]
                    else:
                        k2 = k1 + 1

                    if system.mode == 'forward':
                        J[i:i+(k2-k1), j] = dx[k1:k2]
                    else:
                        J[j, i:i+(k2-k1)] = dx[k1:k2]
                    i += k2-k1

                j += 1

        #print inputs, '\n', outputs, '\n', J
        #print 'dx', dx
        return J

    def newton(self):
        """ Solve the coupled equations for a new state vector that nulls the
        residual. Used by the Newton solvers."""

        system = self._system
        options = self.options

        # Size the problem
        n_edge = system.vec['df'].array.size

        A = LinearOperator((n_edge, n_edge),
                           matvec=self.mult,
                           dtype=float)

        # Call GMRES to solve the linear system
        dx, info = gmres(A, system.vec['f'].array,
                         tol=options.gmres_tolerance,
                         maxiter=options.gmres_maxiter)

        if info > 0:
            msg = "ERROR in calc_gradient in '%s': gmres failed to converge " \
                  "after %d iterations for parameter '%s' at index %d"
            logger.error(msg, system.name, info, param, irhs)
        elif info < 0:
            msg = "ERROR in calc_gradient in '%s': gmres failed " \
                  "for parameter '%s' at index %d"
            logger.error(msg, system.name, param, irhs)

        system.vec['df'].array[:] = -dx

        print 'dx', dx

    def mult(self, arg):
        """ GMRES Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = arg[:]
        system.rhs_vec.array[:] = 0
        system.applyJ()

        # Extra equation for all requested inputs.
        for varname in system._in_nodes:

            if isinstance(varname, tuple):
                varname = varname[0]

            system.rhs_vec[varname] += system.sol_vec[varname]

        print 'arg, result', arg, system.rhs_vec.array[:]
        return system.rhs_vec.array[:]


class PETSc_KSP(LinearSolver):
    """ PETSc's KSP solver with preconditioning """

    def __init__(self, system):
        """ Set up KSP object """
        super(PETSc_KSP, self).__init__(system)

        lsize = np.sum(system.local_var_sizes[system.mpi.rank, :])
        size = np.sum(system.local_var_sizes)
        jac_mat = PETSc.Mat().createPython([(lsize, size), (lsize, size)],
                                           comm=system.mpi.comm)
        jac_mat.setPythonContext(self)
        jac_mat.setUp()

        self.ksp = PETSc.KSP().create(comm=system.mpi.comm)
        self.ksp.setOperators(jac_mat)
        self.ksp.setType('fgmres')
        self.ksp.setGMRESRestart(1000)
        self.ksp.setPCSide(PETSc.PC.Side.RIGHT)

        pc_mat = self.ksp.getPC()
        pc_mat.setType('python')
        pc_mat.setPythonContext(self)

        # Set these in the system
        system.sol_buf = PETSc.Vec().createWithArray(np.zeros(size),
                                                     comm=system.mpi.comm)
        system.rhs_buf = PETSc.Vec().createWithArray(np.zeros(size),
                                                     comm=system.mpi.comm)

    def solve(self, inputs, outputs):
        """ Run KSP solver to return a Jacobian of outputs with respect to
        inputs."""
        system = self._system

        self.inputs = inputs

        # Size the problem
        num_input = system.get_size(inputs)
        num_output = system.get_size(outputs)

        J = np.zeros((num_output, num_input))

        if system.mode == 'adjoint':
            temp = inputs
            inputs = outputs
            outputs = temp

        self.ksp.setTolerances(max_it=10, atol=1e-10, rtol=1e-6)

        # Forward mode, solve linear system for each parameter
        j = 0
        for param in inputs:

            if isinstance(param, tuple):
                param = param[0]

            indices = system.vec['u'].indices(param)
            i1 = indices[0]
            if len(indices) == 2:
                i2 = indices[1]
            else:
                i2 = i1 + 1

            for irhs in range(i1, i2):

                system.rhs_vec.array[:] = 0.0
                system.sol_vec.array[:] = 0.0
                system.rhs_buf.array[:] = 0.0
                system.sol_buf.array[:] = 0.0

                ind = np.sum(system.local_var_sizes[:, :irhs])
                ind_set = PETSc.IS().createGeneral([ind], comm=system.mpi.comm)
                if system.app_ordering is not None:
                    ind_set = system.app_ordering.app2petsc(ind_set)
                ind = ind_set.indices[0]
                system.rhs_buf.setValue(ind, 1.0, addv=False)

                # Call PetSC KSP to solve the linear system
                self.ksp.solve(system.rhs_buf, system.sol_buf)
                system.sol_vec.array[:] = system.sol_buf.array[:]

                system.rhs_vec.petsc_vec.setValue(ind, 0.0, addv=False)
                dx = system.sol_vec.array
                print 'dx', dx

                i = 0
                for item in outputs:

                    if isinstance(item, tuple):
                        item = item[0]

                    indices = system.vec['u'].indices(item)
                    k1 = indices[0]
                    if len(indices) == 2:
                        k2 = indices[1]
                    else:
                        k2 = k1 + 1

                    if system.mode == 'forward':
                        J[i:i+(k2-k1), j] = dx[k1:k2]
                    else:
                        J[j, i:i+(k2-k1)] = dx[k1:k2]
                    i += k2-k1

                j += 1

        #print inputs, '\n', outputs, '\n', J
        return J


    def mult(self, mat, sol_vec, rhs_vec):
        """ KSP Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = sol_vec.array[:]
        system.applyJ()

        # Extra equation for all requested inputs.
        for varname in system._in_nodes:

            if isinstance(varname, tuple):
                varname = varname[0]

            system.rhs_vec[varname] += system.sol_vec[varname]

        rhs_vec.array[:] = system.rhs_vec.array[:]
        print 'arg, result', sol_vec.array, rhs_vec.array

    def apply(self, mat, sol_vec, rhs_vec):
        """ Applies preconditioner """

        system = self._system

        # TODO - Preconditioning is not supported yet, so mimic an Identity
        # matrix.
        rhs_vec.array[:] = sol_vec.array[:]

        #system.rhs_vec.array[:] = sol_vec.array[:]
        #system.solve_precon()
        #rhs_vec.array[:] = system.sol_vec.array[:]

