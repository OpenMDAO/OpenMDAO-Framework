""" Linear solvers that are used to solve for the gradient of an OpenMDAO System.
(Not to be confused with the OpenMDAO Solver classes.)
"""

# pylint: disable=E0611, F0401
import numpy as np
from scipy.sparse.linalg import gmres, LinearOperator

from openmdao.main.mpiwrap import MPI
from openmdao.util.graph import fix_single_tuple
from openmdao.util.log import logger

try:
    from petsc4py import PETSc
except ImportError:
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
        system.applyJ(system.vector_vars.keys())
        system.rhs_vec.array[:] *= -1.0
        system.rhs_vec.array[:] += system.rhs_buf[:]

        if MPI:
            system.rhs_vec.petsc.assemble()
            return system.rhs_vec.petsc.norm()
        else:
            return np.linalg.norm(system.rhs_vec.array)


class ScipyGMRES(LinearSolver):
    """ Scipy's GMRES Solver. This is a serial solver, so
    it should never be used in an MPI setting.
    """

    def __init__(self, system):
        """ Set up ScipyGMRES object """
        super(ScipyGMRES, self).__init__(system)

        n_edge = system.vec['f'].array.size

        system.rhs_buf = np.zeros((n_edge, ))
        system.sol_buf = np.zeros((n_edge, ))
        self.A = LinearOperator((n_edge, n_edge),
                                matvec=self.mult,
                                dtype=float)

    def calc_gradient(self, inputs, outputs, return_format='array'):
        """ Run GMRES solver to return a Jacobian of outputs
        with respect to inputs.
        """

        system = self._system
        RHS = system.rhs_buf
        A = self.A

        # Size the problem
        num_input = system.get_size(inputs)
        num_output = system.get_size(outputs)

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
        # If Adjoint mode, solve linear system for each requested output
        j = 0
        for param in inputs:

            if isinstance(param, tuple):
                param = param[0]

            in_indices = system.vec['u'].indices(system.scope, param)
            jbase = j

            for irhs in in_indices:

                RHS[irhs] = 1.0

                # Call GMRES to solve the linear system
                dx = self.solve(RHS)

                RHS[irhs] = 0.0

                i = 0
                for item in outputs:

                    if isinstance(item, tuple):
                        item = item[0]

                    out_indices = system.vec['u'].indices(system.scope, item)
                    nk = len(out_indices)

                    if return_format == 'dict':
                        if system.mode == 'forward':
                            if J[item][param] is None:
                                J[item][param] = np.zeros((nk, len(in_indices)))
                            J[item][param][:, j-jbase] = dx[out_indices]
                        else:
                            if J[param][item] is None:
                                J[param][item] = np.zeros((len(in_indices), nk))
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

    def solve(self, arg):
        """ Solve the coupled equations for a new state vector that nulls the
        residual. Used by the Newton solvers."""

        system = self._system
        options = self.options
        A = self.A

        #print system.name, 'Linear solution start vec', system.rhs_vec.array
        # Call GMRES to solve the linear system
        dx, info = gmres(A, arg,
                         tol=options.atol,
                         maxiter=options.maxiter)

        if info > 0:
            msg = "ERROR in calc_gradient in '%s': gmres failed to converge " \
                  "after %d iterations"
            logger.error(msg, system.name, info)
        elif info < 0:
            msg = "ERROR in calc_gradient in '%s': gmres failed"
            logger.error(msg, system.name)

        #print system.name, 'Linear solution vec', -dx
        return dx


    def mult(self, arg):
        """ GMRES Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = arg[:]

        # Start with a clean slate
        system.rhs_vec.array[:] = 0.0
        system.clear_dp()

        if system._parent_system:
            vnames = system._parent_system._relevant_vars
        else:
            vnames = system.flat_vars.keys()
        system.applyJ(vnames)

        #print system.name, 'mult: arg, result', arg, system.rhs_vec.array[:]
        #print system.rhs_vec.keys()
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

    def calc_gradient(self, inputs, outputs, return_format='dict'):
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

        inputs = [fix_single_tuple(x) for x in inputs]
        outputs = [fix_single_tuple(x) for x in outputs]

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

    def solve(self, arg):
        """ Solve the coupled equations for a new state vector that nulls the
        residual. Used by the Newton solvers."""

        system = self._system
        options = self.options

        self.ksp.setTolerances(max_it=options.maxiter,
                               atol=options.atol,
                               rtol=options.rtol)

        system.rhs_vec.array[:] = system.vec['f'].array[:]
        #print 'newton start vec', system.vec['f'].array[:]

        system.sol_buf.array[:] = arg
        system.rhs_buf.array[:] = system.rhs_vec.array[:]

        system.ln_solver.ksp.solve(system.rhs_buf, system.sol_buf)

        system.vec['df'].array[:] = -system.sol_buf.array[:]

        #print 'newton solution vec', system.vec['df'].array[:]
        return system.vec['df'].array[:]

    def mult(self, mat, sol_vec, rhs_vec):
        """ KSP Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = sol_vec.array[:]

        # Start with a clean slate
        system.rhs_vec.array[:] = 0.0
        system.clear_dp()

        if system._parent_system:
            vnames = system._parent_system._relevant_vars
        else:
            vnames = system.flat_vars.keys()
        system.applyJ(vnames)

        rhs_vec.array[:] = system.rhs_vec.array[:]
        # mpiprint('names = %s' % system.sol_vec.keys())
        #mpiprint('arg = %s, result=%s' % (sol_vec.array, rhs_vec.array))
        #mpiprint('df, du, dp', system.vec['df'].array, system.vec['du'].array, system.vec['dp'].array)

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

    def __init__(self, system):
        """ Set up LinearGS object """
        super(LinearGS, self).__init__(system)

        lsize = np.sum(system.local_var_sizes[system.mpi.rank, :])

        system.sol_buf = np.zeros(lsize)
        system.rhs_buf = np.zeros(lsize)

    def calc_gradient(self, inputs, outputs, return_format='array'):
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

            in_indices = system.vec['u'].indices(system.scope, param)
            jbase = j

            for irhs in in_indices:

                system.clear_dp()
                system.sol_vec.array[:] = 0.0
                system.rhs_vec.array[:] = 0.0
                system.rhs_vec.array[irhs] = 1.0

                # Perform LinearGS solve
                dx = self.solve(system.rhs_vec.array)

                #system.rhs_vec.array[irhs] = 0.0

                i = 0
                for item in outputs:

                    if isinstance(item, tuple):
                        item = item[0]

                    out_indices = system.vec['u'].indices(system.scope, item)
                    nk = len(out_indices)

                    if return_format == 'dict':
                        if system.mode == 'forward':
                            if J[item][param] is None:
                                J[item][param] = np.zeros((nk, len(in_indices)))
                            J[item][param][:, j-jbase] = dx[out_indices]
                        else:
                            if J[param][item] is None:
                                J[param][item] = np.zeros((len(in_indices), nk))
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

    def solve(self, arg):
        """ Executes an iterative solver """
        system = self._system

        system.rhs_buf[:] = arg[:]
        system.sol_buf[:] = system.sol_vec.array[:]
        options = self.options
        system = self._system

        norm0, norm = 1.0, 1.0
        counter = 0
        while counter < options.maxiter and norm > options.atol and \
              norm/norm0 > options.rtol:

            if system.mode == 'forward':
                for subsystem in system.subsystems(local=True):
                    system.scatter('du', 'dp', subsystem=subsystem)
                    system.rhs_vec.array[:] = 0.0
                    subsystem.applyJ(system.flat_vars.keys())
                    system.rhs_vec.array[:] *= -1.0
                    system.rhs_vec.array[:] += system.rhs_buf[:]
                    sub_options = options if subsystem.options is None \
                                          else subsystem.options
                    subsystem.solve_linear(sub_options)

            elif system.mode == 'adjoint':

                rev_systems = [item for item in reversed(system.subsystems(local=True))]

                for subsystem in rev_systems:
                    system.sol_buf[:] = system.rhs_buf[:]
                    succs = system.graph.successors(subsystem.name)
                    for subsystem2 in rev_systems:
                        if subsystem is not subsystem2:
                        #if subsystem2.name in succs:
                            system.rhs_vec.array[:] = 0.0
                            args = subsystem.flat_vars.keys()
                            subsystem2.applyJ(args)
                            system.scatter('du', 'dp', subsystem=subsystem2)
                            system.sol_buf[:] -= system.rhs_vec.array[:]
                            system.vec['dp'].array[:] = 0.0
                    system.rhs_vec.array[:] = system.sol_buf[:]
                    subsystem.solve_linear(options)
            norm = self._norm()
            counter += 1

        #print 'return', options.parent.name, np.linalg.norm(system.rhs_vec.array), system.rhs_vec.array
        #print 'Linear solution vec', system.sol_vec.array
        return system.sol_vec.array

