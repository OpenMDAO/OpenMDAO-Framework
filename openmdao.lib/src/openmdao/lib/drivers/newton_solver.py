"""
A python Newton solver with line-search adapation of the relaxation parameter.
"""

# pylint: disable=C0103

#public symbols
__all__ = ['NewtonSolver']

import numpy
from openmdao.main.mpiwrap import MPI

npnorm = numpy.linalg.norm
def norm(a, order=None):
    '''This little funct for norm replaces a dependency on scipy
    '''
    return npnorm(numpy.asarray_chkfinite(a), ord=order)

# pylint: disable=E0611, F0401
from openmdao.main.case import Case
from openmdao.main.driver import Driver
from openmdao.main.datatypes.api import Float, Int, Enum
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.main.interfaces import IHasParameters, IHasEqConstraints, \
                                     ISolver, implements
from openmdao.util.decorators import add_delegate


@add_delegate(HasParameters, HasEqConstraints)
class NewtonSolver(Driver):
    ''' Wrapper for some Newton style solvers. Currently supports
    fsolve from scipy.optimize.
    '''

    implements(IHasParameters, IHasEqConstraints, ISolver)

    # pylint: disable=E1101
    atol = Float(1.0e-12, iotype='in', desc='Absolute convergence tolerance')

    rtol = Float(1.0e-10, iotype='in', desc='Relative convergence tolerance')

    max_iteration = Int(20, iotype='in', desc='Maximum number of iterations')

    ls_atol = Float(1.0e-10, iotype='in',
                    desc='Absolute convergence tolerance for line search')

    ls_rtol = Float(0.9, iotype='in',
                    desc='Relative convergence tolerance for line search')

    ls_max_iteration = Int(10, iotype='in',
                          desc='Maximum number of line searches')

    alpha = Float(1.0, iotype='in', low=0.0, high=1.0,
                  desc='Initial over-relaxation factor')

    iprint = Enum(0, [0, 1, 2], iotype='in', desc='set to 1 to print '
                  'convergence. Set to 2 to get backtracking convergence '
                  'as well.')

    def __init__(self):
        super(NewtonSolver, self).__init__()

        # user either the petsc norm or numpy.linalg norm
        if MPI:
            self.norm = self._mpi_norm

    def execute(self):
        """ General Newton's method. """

        if MPI:
            if self.workflow._system.mpi.comm == MPI.COMM_NULL:
                return

        system = self.workflow._system
        options = self.gradient_options
        fvec = system.vec['f']
        dfvec = system.vec['df']
        uvec = system.vec['u']
        iterbase = self.workflow._iterbase()

        # perform an initial run
        self.workflow._system.evaluate(iterbase, case_uuid=Case.next_uuid())

        f_norm = self.norm()
        f_norm0 = f_norm

        if self.iprint == 1:
            print self.name, "Norm: ", f_norm, 0

        itercount = 0
        alpha = self.alpha
        while itercount < self.max_iteration and f_norm > self.atol and \
              f_norm/f_norm0 > self.rtol:

            system.calc_newton_direction(options=options)

            print "LS 1", uvec.array, '+', dfvec.array
            uvec.array += alpha*dfvec.array

            # Just evaluate the model with the new points
            self.workflow._system.evaluate(iterbase, case_uuid=Case.next_uuid())

            f_norm = self.norm()
            if self.iprint == 1:
                print self.name, "Norm: ", f_norm, itercount+1

            itercount += 1
            ls_itercount = 0

            # Backtracking Line Search
            while ls_itercount < self.ls_max_iteration and \
                  f_norm > self.ls_atol and \
                  f_norm/f_norm0 > self.ls_rtol:

                alpha *= 0.5
                uvec.array -= alpha*dfvec.array

                # Just evaluate the model with the new points
                self.workflow._system.evaluate(iterbase,
                                               case_uuid=Case.next_uuid())

                f_norm = self.norm()
                if self.iprint == 2:
                    print "Backtracking Norm: %f, Alpha: %f" % (f_norm, alpha)

                ls_itercount += 1

            # Reset backtracking
            alpha = self.alpha

        # Need to make sure the whole workflow is executed at the final
        # point, not just evaluated.
        self.pre_iteration()
        self.run_iteration()
        self.post_iteration()

        if self.iprint == 1:
            print self.name, "converged"

    def _mpi_norm(self):
        """ Compute the norm of the f Vec using petsc. """
        fvec = self.workflow._system.vec['f']
        fvec.petsc_vec.assemble()
        return fvec.petsc_vec.norm()

    def norm(self):
        """ Compute the norm using numpy.linalg. """
        return norm(self.workflow._system.vec['f'].array)

    def requires_derivs(self):
        """Newtonsolver always requires derivatives."""
        return True
