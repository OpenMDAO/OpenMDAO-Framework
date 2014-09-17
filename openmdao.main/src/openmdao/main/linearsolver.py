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

    def _norm(self):
        """ Computes the norm of the linear residual """
        system = self._system
        system.rhs_vec.array[:] = 0.0
        system.apply_J()
        system.rhs_vec.array[:] *= -1.0
        system.rhs_vec.array[:] += system.rhs_buf.array[:]

        if MPI:
            system.rhs_vec.petsc.assemble()
            return system.rhs_vec.petsc.norm()
        else:
            return norm(rhs_vec)


class ScipyGMRES(LinearSolver):
    """ Scipy's GMRES Solver. This is a serial solver, so
    it should never be used in an MPI setting.
    """

    def solve(self, inputs, outputs, return_format='array'):
        """ Run GMRES solver to return a Jacobian of outputs
        with respect to inputs.
        """

        system = self._system
        options = self.options

        # Size the problem
        # TODO - Support for array slice inputs/outputs
        try:
            num_input = system.get_size(inputs)
            num_output = system.get_size(outputs)
        except KeyError as exc:
            if '[' in str(exc):
                msg = 'Array slice inputs and outputs currently not supported.'
                raise RuntimeError(msg)
            else:
                raise

        n_edge = system.vec['f'].array.size

        RHS = np.zeros((n_edge, 1))
        A = LinearOperator((n_edge, n_edge),
                           matvec=self.mult,
                           dtype=float)

        if return_format == 'dict':
            J = {}
            for okey in outputs:
                J[okey] = {}
                for ikey in inputs:
                    if isinstance(ikey, tuple):
                        ikey = ikey[0]
                    J[okey][ikey] = None
        else:
            J = np.zeros((num_output, num_input))


        if system.mode == 'adjoint':
            outputs, inputs = inputs, outputs

        # If Forward mode, solve linear system for each parameter
        # If Reverse mode, solve linear system for each requested output
        j = 0
        for param in inputs:

            if isinstance(param, tuple):
                param = param[0]

            in_indices = system.vec['u'].indices(param)
            jbase = j

            for irhs in in_indices:

                RHS[irhs, 0] = 1.0

                # Call GMRES to solve the linear system
                dx, info = gmres(A, RHS,
                                 tol=options.atol,
                                 maxiter=options.maxiter)
                #mpiprint('dx', dx)
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

                    out_indices = system.vec['u'].indices(item)
                    nk = len(out_indices)

                    if return_format == 'dict':
                        if system.mode == 'forward':
                            if J[item][param] is None:
                                J[item][param] = np.zeros((nk, len(in_indices)))
                            J[item][param][:, j-jbase] = dx[out_indices]
                        else:
                            if J[param][item] is None:
                                J[param][item] = np.zeros((nk, len(in_indices)))
                            J[param][item][j-jbase, :] = dx[out_indices]

                    else:
                        if system.mode == 'forward':
                            J[i:i+nk, j] = dx[out_indices]
                        else:
                            J[j, i:i+nk] = dx[out_indices]
                        i += nk

                j += 1

        #print inputs, '\n', outputs, '\n', J
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

        #print 'newton start vec', system.vec['f'].array[:]
        # Call GMRES to solve the linear system
        dx, info = gmres(A, system.vec['f'].array,
                         tol=options.atol,
                         maxiter=options.maxiter)

        if info > 0:
            msg = "ERROR in calc_gradient in '%s': gmres failed to converge " \
                  "after %d iterations for parameter '%s' at index %d"
            logger.error(msg, system.name, info, param, irhs)
        elif info < 0:
            msg = "ERROR in calc_gradient in '%s': gmres failed " \
                  "for parameter '%s' at index %d"
            logger.error(msg, system.name, param, irhs)

        system.vec['df'].array[:] = -dx

        #print 'newton solution vec', system.vec['df'].array[:]

    def mult(self, arg):
        """ GMRES Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = arg[:]
        system.rhs_vec.array[:] = 0
        system.applyJ()

        #mpiprint ('arg, result', arg, system.rhs_vec.array[:])
        return system.rhs_vec.array[:]


class PETSc_KSP(LinearSolver):
    """ PETSc's KSP solver with preconditioning. MPI is supported."""

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

        # # Set these in the system
        # #mpiprint("KSP: creating sol buf, size %d" % lsize)
        system.sol_buf = PETSc.Vec().createWithArray(np.zeros(lsize),
                                                     comm=system.mpi.comm)
        # #mpiprint("KSP: creating rhs buf, size %d" % lsize)
        system.rhs_buf = PETSc.Vec().createWithArray(np.zeros(lsize),
                                                     comm=system.mpi.comm)

    def solve(self, inputs, outputs, return_format='dict'):
        """Returns a nested dict of sensitivities if return_format == 'dict'.
        """

        if return_format == 'dict':
            return self._J_dict_solve(inputs, outputs)
        else:
            raise RuntimeError("unsupported solve return_format '%s'" % return_format)

    def _J_dict_solve(self, inputs, outputs):
        """Returns a dict of sensitivities for given
        inputs and outputs.
        """
        system = self._system
        options = self.options
        name2collapsed = system.scope.name2collapsed

        inputs = [_detuple(x) for x in inputs]
        outputs = [_detuple(x) for x in outputs]

        J = {}
        for okey in outputs:
            J[okey] = {}
            for ikey in inputs:
                J[okey][ikey] = None

        if system.mode == 'adjoint':
            outputs, inputs = inputs, outputs

        self.ksp.setTolerances(max_it=options.maxiter,
                               atol=options.atol,
                               rtol=options.rtol)

        j = 0
        for param in inputs:
            param_tup = name2collapsed[param]
            param_size = system.get_size(param)

            jbase = j

            for irhs in xrange(param_size):
                solvec = system._compute_derivatives(param_tup, irhs)

                for out in outputs:
                    out_size = system.get_size(out)

                    if system.mode == 'forward':
                        if out in solvec:
                            if J[out][param] is None:
                                J[out][param] = np.zeros((out_size, param_size))
                            J[out][param][:, j-jbase] = solvec[out]
                        else:
                            del J[out][param]
                    else:
                        if out in solvec:
                            if J[param][out] is None:
                                J[param][out] = np.zeros((out_size, param_size))
                            J[param][out][j-jbase, :] = solvec[out]
                        else:
                            del J[param][out]

                j += 1

        return J

    def newton(self):
        """ Solve the coupled equations for a new state vector that nulls the
        residual. Used by the Newton solvers."""

        system = self._system
        options = self.options

        self.ksp.setTolerances(max_it=options.maxiter,
                               atol=options.atol,
                               rtol=options.rtol)

        system.rhs_vec.array[:] = system.vec['f'].array[:]
        #print 'newton start vec', system.vec['f'].array[:]

        system.sol_buf.array[:] = system.sol_vec.array[:]
        system.rhs_buf.array[:] = system.rhs_vec.array[:]

        system.ln_solver.ksp.solve(system.rhs_buf, system.sol_buf)

        system.vec['df'].array[:] = -system.sol_buf.array[:]

        #print 'newton solution vec', system.vec['df'].array[:]

    def mult(self, mat, sol_vec, rhs_vec):
        """ KSP Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = sol_vec.array[:]
        system.rhs_vec.array[:] = 0.0

        system.applyJ()

        rhs_vec.array[:] = system.rhs_vec.array[:]
        #print 'arg, result', sol_vec.array, rhs_vec.array
        # mpiprint('names = %s' % system.sol_vec.keys())
        # mpiprint('arg = %s, result=%s' % (sol_vec.array, rhs_vec.array))

    def apply(self, mat, sol_vec, rhs_vec):
        """ Applies preconditioner """

        #system = self._system

        # TODO - Preconditioning is not supported yet, so mimic an Identity
        # matrix.
        rhs_vec.array[:] = sol_vec.array[:]

        #system.rhs_vec.array[:] = sol_vec.array[:]
        #system.solve_precon()
        #rhs_vec.array[:] = system.sol_vec.array[:]


class LinearGS(LinearSolver):
    """ Linear block Gauss Seidel. MPI is not supported yet.
    Serial block solve of D x = b - (L+U) x """

    def solve(self, inputs, outputs, return_format='array'):
        """ Run GMRES solver to return a Jacobian of outputs
        with respect to inputs.
        """

        system = self._system

        # Size the problem
        # TODO - Support for array slice inputs/outputs
        try:
            num_input = system.get_size(inputs)
            num_output = system.get_size(outputs)
        except KeyError as exc:
            if '[' in str(exc):
                msg = 'Array slice inputs and outputs currently not supported.'
                raise RuntimeError(msg)
            else:
                raise

        n_edge = system.vec['f'].array.size

        if return_format == 'dict':
            J = {}
            for okey in outputs:
                J[okey] = {}
                for ikey in inputs:
                    if isinstance(ikey, tuple):
                        ikey = ikey[0]
                    J[okey][ikey] = None
        else:
            J = np.zeros((num_output, num_input))


        if system.mode == 'adjoint':
            outputs, inputs = inputs, outputs

        # If Forward mode, solve linear system for each parameter
        # If Reverse mode, solve linear system for each requested output
        j = 0
        for param in inputs:

            if isinstance(param, tuple):
                param = param[0]

            in_indices = system.vec['u'].indices(param)
            jbase = j

            for irhs in in_indices:

                system.rhs_vec.array[irhs, 0] = 1.0

                # Perform LinearGS solve
                self._solve()
                dx = system.sol_vec.array

                system.rhs_vec.array[irhs, 0] = 0.0

                i = 0
                for item in outputs:

                    if isinstance(item, tuple):
                        item = item[0]

                    out_indices = system.vec['u'].indices(item)
                    nk = len(out_indices)

                    if return_format == 'dict':
                        if system.mode == 'forward':
                            if J[item][param] is None:
                                J[item][param] = np.zeros((nk, len(in_indices)))
                            J[item][param][:, j-jbase] = dx[out_indices]
                        else:
                            if J[param][item] is None:
                                J[param][item] = np.zeros((nk, len(in_indices)))
                            J[param][item][j-jbase, :] = dx[out_indices]

                    else:
                        if system.mode == 'forward':
                            J[i:i+nk, j] = dx[out_indices]
                        else:
                            J[j, i:i+nk] = dx[out_indices]
                        i += nk

                j += 1

        #print inputs, '\n', outputs, '\n', J
        return J

    def _solve(self):
        """ Executes an iterative solver """

        system.rhs_buf.array[:] = system.rhs_vec.array[:]
        system.sol_buf.array[:] = system.sol_vec.array[:]
        options = self.options
        system = self._system

        norm0, norm = 1.0, 1.0
        counter = 0
        while counter < options.maxiter and norm > options.atol and \
              norm/norm0 > options.rtol:

            if system.mode == 'forward':
                for subsystem in system.subsystems(local=True):
                    #system.scatter('lin', subsystem)
                    system.rhs_vec.array[:] = 0.0
                    subsystem.applyJ()
                    system.rhs_vec.array[:] *= -1.0
                    system.rhs_vec.array[:] += system.rhs_buf.array[:]
                    subsystem.applyJ()

            elif system.mode == 'adjoint':
                system.subsystems['local'].reverse()
                for subsystem in system.subsystems(local=True):
                    system.sol_buf.array[:] = system.rhs_buf.array[:]
                    for subsystem2 in system.subsystems['local']:
                        if subsystem is not subsystem2:
                            system.rhs_vec.array[:] = 0.0
                            subsystem2.applyJ(args)
                            #system.scatter('lin', subsystem2)
                            system.sol_buf.array[:] -= system.rhs_vec.array[:]
                    system.rhs_vec.array[:] = system.sol_buf.array[:]
                    subsystem.applyJ()

                system.subsystems['local'].reverse()

            norm = self._norm()
            counter += 1
            print self.name, "Norm: ", norm, counter


def _detuple(x):
    """Return x, or if x is a tuple, x[0]."""
    if isinstance(x, tuple):
        return x[0]
    return x
