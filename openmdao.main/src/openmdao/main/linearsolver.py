""" Linear solvers that are used to solve for the gradient of an OpenMDAO System.
(Not to be confused with the OpenMDAO Solver classes.)
"""

import sys
import traceback

# pylint: disable=E0611, F0401
import numpy as np
from scipy.sparse.linalg import gmres, LinearOperator

from openmdao.main.mpiwrap import MPI, PETSc, get_norm
from openmdao.util.graph import fix_single_tuple
from openmdao.util.log import logger


class LinearSolver(object):
    """ A base class for linear solvers """

    def __init__(self, system):
        """ Set up any LinearSolver object """
        self._system = system
        self.options = system.options
        self.custom_jacs = {}

        # A few extra checks if we call calc_gradient from a driver.
        level = 0
        if hasattr(system, '_parent_system') and \
           system._parent_system is not None and \
           hasattr(system._parent_system, '_comp'):
            drv = system._parent_system._comp

            # Figure out base indentation for printing the residual during
            # convergence.
            self.drv_name = drv.name
            if drv.itername == '-driver':
                level = 0
            else:
                level = drv.itername.count('.') + 1

            # Figure out if the user defined custom constraint gradients.
            if hasattr(drv, 'get_constraints'):
                for constraint in drv.get_constraints().values():
                    if constraint.jacs is not None:
                        key = '%s.out0' % constraint.pcomp_name
                        self.custom_jacs[key] = constraint.jacs
            if hasattr(drv, 'get_2sided_constraints'):
                for constraint in drv.get_2sided_constraints().values():
                    if constraint.jacs is not None:
                        key = '%s.out0' % constraint.pcomp_name
                        self.custom_jacs[key] = constraint.jacs

        else:
            self.drv_name = system.name

        self.indent = '   ' * level

    def print_norm(self, driver_string, iteration, res, res0, msg=None, solver='LN'):
        """ Prints out the norm of the residual in a neat readable format.
        """

        if msg is not None:
            form = self.indent + '[%s]    %s: %s   %d | %s'
            print form % (self.drv_name, solver, self.ln_string, iteration, msg)
            return

        form = self.indent + '[%s]    %s: %s   %d | %.9g %.9g'
        print form % (self.drv_name, solver, self.ln_string, iteration, res, res/res0)

    def _norm(self):
        """ Computes the norm of the linear residual """
        system = self._system
        system.rhs_vec.array[:] = 0.0
        system.applyJ(system.vector_vars.keys())
        system.rhs_vec.array[:] *= -1.0
        system.rhs_vec.array[:] += system.rhs_buf[:]

        return get_norm(system.rhs_vec)

    def user_defined_jacobian(self, con, params, J):
        """ Inserts the user-defined Jacobian into the full Jacobian rather
        than doing any calculation. """

        if not isinstance(J, dict):
            msg = 'Only PyOptSparse supports custom Jacobians'
            raise RuntimeError(msg)

        jacs = self.custom_jacs[con]()
        for param in params:

            if param in jacs:
                J[con][param] = jacs[param]

            # Assume zero if user did not explicitly define.
            else:
                psize = self._system.get_size(param)
                csize = self._system.get_size(con)
                J[con][param] = np.zeros((csize, psize))


class ScipyGMRES(LinearSolver):
    """ Scipy's GMRES Solver. This is a serial solver, so
    it should never be used in an MPI setting.
    """

    ln_string = 'GMRES'

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

        if return_format == 'dict':
            J = {}
            for okey in outputs:
                J[okey] = {}
                for ikey in inputs:
                    if isinstance(ikey, tuple):
                        ikey = ikey[0]
                    J[okey][ikey] = None
        else:
            num_input = system.get_size(inputs)
            num_output = system.get_size(outputs)
            J = np.zeros((num_output, num_input))

        if system.mode == 'adjoint':
            outputs, inputs = inputs, outputs

        # If Forward mode, solve linear system for each parameter
        # If Adjoint mode, solve linear system for each requested output
        j = 0
        for param in inputs:

            if isinstance(param, tuple):
                param = param[0]

            in_indices = system.vec['u'].indices(system, param)
            jbase = j

            # Did the user define a custom Jacobian for a constraint?
            if system.mode == 'adjoint' and param in self.custom_jacs:
                self.user_defined_jacobian(param, outputs, J)
                j += len(in_indices)
                continue

            for irhs in in_indices:

                RHS[irhs] = 1.0

                # Call GMRES to solve the linear system
                dx = self.solve(RHS)

                RHS[irhs] = 0.0

                i = 0
                for item in outputs:

                    if isinstance(item, tuple):
                        item = item[0]

                    out_indices = system.vec['u'].indices(system, item)
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

    ln_string = 'KSP'

    # This class object is given to KSP as a callback object for printing the residual.
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

            if self._ksp.options.iprint > 0:
                self._ksp.print_norm(self._ksp.ln_string, counter, norm, self._norm0)

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

        system.rhs_buf = np.zeros((lsize, ))
        system.sol_buf = np.zeros((lsize, ))

        # Set these in the system
        system.sol_buf_petsc = PETSc.Vec().createWithArray(system.sol_buf,
                                                     comm=system.mpi.comm)
        system.rhs_buf_petsc = PETSc.Vec().createWithArray(system.rhs_buf,
                                                     comm=system.mpi.comm)

    def calc_gradient(self, inputs, outputs, return_format='dict'):
        """Returns a nested dict of sensitivities if return_format == 'dict'.
        """

        system = self._system
        options = self.options
        name2collapsed = system.scope.name2collapsed

        inputs = [fix_single_tuple(x) for x in inputs]
        outputs = [fix_single_tuple(x) for x in outputs]

        if return_format == 'dict':
            J = {}
            for okey in outputs:
                J[okey] = {}
                for ikey in inputs:
                    if isinstance(ikey, tuple):
                        ikey = ikey[0]
                    J[okey][ikey] = None
        else:
            num_input = system.get_size(inputs)
            num_output = system.get_size(outputs)
            J = np.zeros((num_output, num_input))

        if system.mode == 'adjoint':
            outputs, inputs = inputs, outputs

        self.ksp.setTolerances(max_it=options.maxiter,
                               atol=options.atol,
                               rtol=options.rtol)

        j = 0
        for param in inputs:

            if isinstance(param, tuple):
                param = param[0]

            param_tup = name2collapsed[param]
            param_size = system.get_size(param)

            jbase = j

            # Did the user define a custom Jacobian for a constraint?
            if system.mode == 'adjoint' and param in self.custom_jacs:
                self.user_defined_jacobian(param, outputs, J)
                j += param_size
                continue

            for irhs in xrange(param_size):

                # Solve the system with PetSC KSP
                solvec = system._compute_derivatives(param_tup, irhs)

                i = 0
                for out in outputs:

                    if isinstance(out, tuple):
                        out = out[0]

                    out_size = system.get_size(out)

                    if return_format == 'dict':
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
                                    J[param][out] = np.zeros((param_size, out_size))
                                J[param][out][j-jbase, :] = solvec[out]
                            else:
                                del J[param][out]

                    else:
                        if out in solvec:
                            nk = len(solvec[out])
                            if system.mode == 'forward':
                                J[i:i+nk, j] = solvec[out]
                            else:
                                J[j, i:i+nk] = solvec[out]
                            i += nk
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

        system.rhs_buf[:] = arg[:]
        self.ksp.solve(system.rhs_buf_petsc, system.sol_buf_petsc)

        #print 'newton solution vec', system.vec['df'].array[:]
        return system.sol_buf[:]

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
        #print 'names = %s' % system.sol_vec.keys()
        #print 'arg = %s, result=%s' % (sol_vec.array, rhs_vec.array)
        #print 'df, du, dp', system.vec['df'].array, system.vec['du'].array, system.vec['dp'].array

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

    ln_string = 'LIN_GS'

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
        # If Reverse mode, solve linear system for each requested output
        j = 0
        for param in inputs:

            if isinstance(param, tuple):
                param = param[0]

            in_indices = system.rhs_vec.indices(system, param)
            nj = len(in_indices)
            jbase = j

            # Did the user define a custom Jacobian for a constraint?
            if system.mode == 'adjoint' and param in self.custom_jacs:
                self.user_defined_jacobian(param, outputs, J)
                j += nj
                continue

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

                    out_indices = system.sol_vec.indices(system, item)
                    nk = len(out_indices)

                    if return_format == 'dict':
                        if system.mode == 'forward':
                            if J[item][param] is None:
                                J[item][param] = np.zeros((nk, nj))
                            J[item][param][:, j-jbase] = dx[out_indices]
                        else:
                            if J[param][item] is None:
                                J[param][item] = np.zeros((nj, nk))
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
        #print "START", system.name

        system.rhs_buf[:] = arg[:]
        system.sol_buf[:] = system.sol_vec.array[:]
        options = self.options
        system = self._system

        norm0, norm = 1.0, 1.0
        counter = 0
        if self.options.iprint > 0:
            self.print_norm(self.ln_string, counter, norm, norm0)

        while counter < options.maxiter and norm > options.atol and \
              norm/norm0 > options.rtol:

            if system.mode == 'forward':
                #print "Start Forward", system.name, system; sys.stdout.flush()
                for subsystem in system.subsystems(local=True):
                    #print subsystem.name; sys.stdout.flush()
                    #print "Z1", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()
                    system.scatter('du', 'dp', subsystem=subsystem)
                    #print "Z2", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()
                    system.rhs_vec.array[:] = 0.0
                    subsystem.applyJ(system.flat_vars.keys())
                    system.rhs_vec.array[:] *= -1.0
                    system.rhs_vec.array[:] += system.rhs_buf[:]
                    sub_options = options if subsystem.options is None \
                                          else subsystem.options
                    #print "Z4", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()
                    subsystem.solve_linear(sub_options)
                    #print "Z5", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array
                    #print subsystem.name, system.rhs_vec.array, system.sol_vec.array; sys.stdout.flush()

                #print "End", system.name; sys.stdout.flush()
            elif system.mode == 'adjoint':
                #print "Start Adjoint", system.name, system; sys.stdout.flush()

                rev_systems = [item for item in reversed(list(system.subsystems(local=True)))]

                for subsystem in rev_systems:
                    #print "Outer", subsystem.name; sys.stdout.flush()
                    system.sol_buf[:] = system.rhs_buf[:]

                    # Instead of a double loop, we can use the graph to only
                    # call applyJ on the component behind us. This led to a
                    # nice speedup.
                    succs = [str(node) for node in system.graph.successors(subsystem.node)]

                    for subsystem2 in rev_systems:
                        if subsystem2.name in succs:
                            #print "Inner", subsystem2.name; sys.stdout.flush()
                            system.rhs_vec.array[:] = 0.0
                            args = subsystem.flat_vars.keys()
                            #print "Z1", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()
                            subsystem2.applyJ(args)
                            #print "Z2", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()
                            system.scatter('du', 'dp', subsystem=subsystem2)
                            #print subsystem2.name, subsystem2.vec['dp'].keys(), subsystem2.vec['du'].keys()
                            #print "Z3", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()
                            system.sol_buf[:] -= system.rhs_vec.array[:]
                            system.vec['dp'].array[:] = 0.0
                            #print "Z4", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()
                    system.rhs_vec.array[:] = system.sol_buf[:]
                    #print "Z5", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()

                    subsystem.solve_linear(options)
                    #print "Z6", system.vec['du'].array, system.vec['dp'].array, system.vec['df'].array; sys.stdout.flush()

            norm = self._norm()
            counter += 1
            if self.options.iprint > 0:
                self.print_norm(self.ln_string, counter, norm, norm0)

        #print 'return', options.parent.name, np.linalg.norm(system.rhs_vec.array), system.rhs_vec.array
        #print 'Linear solution vec', system.sol_vec.array; sys.stdout.flush()
        if len(system.vec['dp'].array) == 0:
            psys = system._parent_system
            #print "pZZ", psys.vec['du'].array, psys.vec['dp'].array, psys.vec['df'].array; sys.stdout.flush()

        return system.sol_vec.array
