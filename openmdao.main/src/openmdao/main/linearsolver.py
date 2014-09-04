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

    def solve(self, inputs, outputs, return_format='array'):
        """ Run GMRES solver to return a Jacobian of outputs
        with respect to inputs.
        """

        system = self._system
        options = self.options
        self.inputs = inputs

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
                    J[okey][ikey] = None
        else:
            J = np.zeros((num_output, num_input))


        if system.mode == 'adjoint':
            outputs, inputs = inputs, outputs

        # Forward mode, solve linear system for each parameter
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
                                 tol=options.gmres_tolerance,
                                 maxiter=options.gmres_maxiter)
                mpiprint('dx', dx)
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

        mpiprint ('arg, result', arg, system.rhs_vec.array[:])
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
        #self.ksp.setMonitor(self.Monitor(self))

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

    def solve(self, inputs, outputs, return_format='array'):
        """Returns a Jacobian matrix if return_format == 'array',
        or a nested dict of sensitivities if return_format == 'dict'.
        """
        #self.inputs = inputs

        if return_format == 'array':
            return self._J_array_solve(inputs, outputs)
        # elif return_format == 'dict':
        #     return self._J_dict_solve(inputs, outputs)
        else:
            raise RuntimeError("unsupported J return format '%s'" % return_format)

    def _J_array_solve(self, inputs, outputs):
        """Returns a Jacobian matrix for given inputs and outputs."""

        system = self._system
        name2collapsed = system.scope.name2collapsed

        if system.mode == 'adjoint':
            outputs, inputs = inputs, outputs

        inputs = [_detuple(x) for x in inputs]
        outputs = [_detuple(x) for x in outputs]

        uniques = system.get_unique_vars(inputs+outputs)

        # Size the problem
        tot_input_size = system.get_size(inputs)
        tot_output_size = system.get_size(outputs)
        #mpiprint('inputs', inputs, 'size', tot_input_size)
        #mpiprint('outputs', outputs, 'size', tot_output_size)

        J = np.zeros((tot_output_size, tot_input_size))

        #mpiprint('J',str(J))

        # FIXME: these shouldn't be hard-wired here
        self.ksp.setTolerances(max_it=10, atol=1e-10, rtol=1e-6)

        #mpiprint("local_var_sizes: %s" % system.local_var_sizes)

        j = 0
        for param in inputs:

            i = 0
            mpiprint("param, j: %s, %d" % (param,j))
            for ind in system._get_global_indices(name2collapsed[param], uniques[param]):
                solvec = system._compute_derivatives(ind, uniques[param])
                for output in outputs:

                    mpiprint("output, i: %s, %d" % (output, i))
                    if output in solvec:
                        view = solvec[output]
                        if system.mode == 'forward':
                            J[i:i+view.size, j] = view
                        else:
                            J[j, i:i+view.size] = view
                        i += view.size
            j += 1

        return J

    # def _J_dict_solve(self, inputs, outputs):
    #     """Returns a dict of sensitivities for given 
    #     inputs and outputs.
    #     """
    #     system = self.system
    #     uvec = system.vec['u']

    #     # FIXME: these shouldn't be hard-wired here
    #     self.ksp.setTolerances(max_it=10, atol=1e-10, rtol=1e-6)

    #     sens_dict = {}
    #     for output in outputs:
    #         out_size = uvec[output].size

    #         sens_dict[output] = {}
    #         for param in inputs:
    #             param_size = uvec[param].size
    #             sens_dict[output][param] = np.zeros((out_size, param_size))

    #         for ind in system._get_global_indices(output):
    #             solvec = system._compute_derivatives(output, ind)

    #             for param in inputs:
    #                 sens_dict[output][param][ind, :] = solvec[param]  

    #     return sens_dict      

    def mult(self, mat, sol_vec, rhs_vec):
        """ KSP Callback: applies Jacobian matrix. Mode is determined by the
        system."""

        system = self._system
        system.sol_vec.array[:] = sol_vec.array[:]
        system.rhs_vec.array[:] = 0.0

        system.applyJ()

        rhs_vec.array[:] = system.rhs_vec.array[:]
        #print 'arg, result', sol_vec.array, rhs_vec.array
        mpiprint('names = %s' % system.sol_vec.keys())
        mpiprint('arg = %s, result=%s' % (sol_vec.array, rhs_vec.array))

    def apply(self, mat, sol_vec, rhs_vec):
        """ Applies preconditioner """

        #system = self._system

        # TODO - Preconditioning is not supported yet, so mimic an Identity
        # matrix.
        rhs_vec.array[:] = sol_vec.array[:]

        #system.rhs_vec.array[:] = sol_vec.array[:]
        #system.solve_precon()
        #rhs_vec.array[:] = system.sol_vec.array[:]


def _detuple(x):
    """Return x, or if x is a tuple, x[0]."""
    if isinstance(x, tuple):
        return x[0]
    return x
