""" Linear solvers that are used to solve for the gradient of an OpenMDAO System.
(Not to be confused with the OpenMDAO Solver classes.)
"""

# pylint: disable=E0611, F0401
import numpy as np
from scipy.sparse.linalg import gmres, LinearOperator

from openmdao.main.mpiwrap import MPI, mpiprint
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
    """ Scipy's GMRES Solver. This is a serial solver, so 
    it should never be used in an MPI setting.
    """

    def solve(self, inputs, outputs):
        """ Run GMRES solver to return a Jacobian of outputs 
        with respect to inputs.
        """

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

            for irhs in indices:

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
                    nk = len(indices)

                    if system.mode == 'forward':
                        J[i:i+nk, j] = dx[indices]
                    else:
                        J[j, i:i+nk] = dx[indices]
                    i += nk

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

        #print 'dx', dx

    def mult(self, arg):
        """ GMRES Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = arg[:]
        system.rhs_vec.array[:] = 0
        system.applyJ()

        #print 'arg, result', arg, system.rhs_vec.array[:]
        return system.rhs_vec.array[:]


class PETSc_KSP(LinearSolver):
    """ PETSc's KSP solver with preconditioning """

    class Monitor(object):
        """ Prints output from PETSc's KSP solvers """

        def __init__(self, ksp):
            """ Stores pointer to the ksp solver """
            self._ksp = ksp
            self._norm0 = 1.0

        def __call__(self, ksp, counter, norm):
            """ Store norm if first iteration, and print norm """
            if counter == 0 and norm != 0.0:
                self._norm0 = norm
            mpiprint("%d: norm=%f" % (counter, norm))

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
        self.ksp.setMonitor(self.Monitor(self))

        pc_mat = self.ksp.getPC()
        pc_mat.setType('python')
        pc_mat.setPythonContext(self)

        # Set these in the system
        #mpiprint("KSP: creating sol buf, size %d" % lsize)
        system.sol_buf = PETSc.Vec().createWithArray(np.zeros(lsize),
                                                     comm=system.mpi.comm)
        #mpiprint("KSP: creating rhs buf, size %d" % lsize)
        system.rhs_buf = PETSc.Vec().createWithArray(np.zeros(lsize),
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

            for irhs in indices:

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

                # FIXME: john doesn't appear to need to call assemble
                # before calling ksp.solve, but if we don't, petsc
                # complains
                system.rhs_buf.assemble()

                # Call PetSC KSP to solve the linear system
                self.ksp.solve(system.rhs_buf, system.sol_buf)
                system.sol_vec.array[:] = system.sol_buf.array[:]

                # this seems pretty expensive to make a setValue call
                # on two different petsc vectors for each iteration,
                # and John doesn't do it as far as I can tell, so we
                # may want to revisit this later
                system.rhs_vec.petsc_vec.setValue(ind, 0.0, addv=False)
                system.rhs_vec.petsc_vec.assemble()
                #system.rhs_vec.array[irhs] = 0.0
                dx = system.sol_vec.array
                mpiprint('%s:\n      dx = %s' % (system.name, dx))

                i = 0

                for item in outputs:

                    if isinstance(item, tuple):
                        item = item[0]

                    indices = system.vec['u'].indices(item)
                    nk = len(indices)

                    if system.mode == 'forward':
                        J[i:i+nk, j] = dx[indices]
                    else:
                        J[j, i:i+nk] = dx[indices]
                    i += nk

                j += 1

        #mpiprint("returning from KSP.solve for system %s, rank %d" % (system.name, system.mpi.comm.rank))
        #print inputs, '\n', outputs, '\n', J
        return J


    def mult(self, mat, sol_vec, rhs_vec):
        """ KSP Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = sol_vec.array[:]
        system.applyJ()

        # # Extra equation for all requested inputs.
        # for varname in system._in_nodes:

        #     if isinstance(varname, tuple):
        #         varname = varname[0]

        #     system.rhs_vec[varname] += system.sol_vec[varname]

        mpiprint('result = %s' % system.rhs_vec.array[:])

        rhs_vec.array[:] = system.rhs_vec.array[:]
        #print 'arg, result', sol_vec.array, rhs_vec.array

    def apply(self, mat, sol_vec, rhs_vec):
        """ Applies preconditioner """

        system = self._system

        # TODO - Preconditioning is not supported yet, so mimic an Identity
        # matrix.
        rhs_vec.array[:] = sol_vec.array[:]

        #system.rhs_vec.array[:] = sol_vec.array[:]
        #system.solve_precon()
        #rhs_vec.array[:] = system.sol_vec.array[:]

