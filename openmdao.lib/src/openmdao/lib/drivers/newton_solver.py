"""
A python Newton solver with line-search adapation of the relaxation parameter.
"""

# pylint: disable=C0103

#public symbols
__all__ = ['NewtonSolver']

import numpy
from openmdao.main.mpiwrap import MPI, get_norm

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
        nstring = 'NEWTON'

        # perform an initial run
        system.evaluate(iterbase, case_uuid=Case.next_uuid())

        f_norm = get_norm(fvec)

        f_norm0 = f_norm

        if self.iprint > 0:
            self.print_norm(nstring, 0, f_norm, f_norm0)

        itercount = 0
        alpha = self.alpha
        while itercount < self.max_iteration and f_norm > self.atol and \
              f_norm/f_norm0 > self.rtol:

            system.calc_newton_direction(options=options)

            #print "LS 1", uvec.array, '+', dfvec.array
            uvec.array += alpha*dfvec.array

            # Just evaluate the model with the new points
            system.evaluate(iterbase, case_uuid=Case.next_uuid())

            f_norm = get_norm(fvec)
            if self.iprint > 0:
                self.print_norm(nstring, itercount+1, f_norm, f_norm0)

            itercount += 1
            ls_itercount = 0

            # Backtracking Line Search
            while ls_itercount < self.ls_max_iteration and \
                  f_norm > self.ls_atol and \
                  f_norm/f_norm0 > self.ls_rtol:

                alpha *= 0.5
                uvec.array -= alpha*dfvec.array

                # Just evaluate the model with the new points
                system.evaluate(iterbase, case_uuid=Case.next_uuid())

                f_norm = get_norm(fvec)
                if self.iprint> 1:
                    self.print_norm('BK_TKG', itercount+1,
                                    f_norm, f_norm/f_norm0,
                                    indent=1, solver='LS')

                ls_itercount += 1

            # Reset backtracking
            alpha = self.alpha

        # Need to make sure the whole workflow is executed at the final
        # point, not just evaluated.
        self.pre_iteration()
        self.run_iteration()
        self.post_iteration()

        if self.iprint > 0:
            self.print_norm(nstring, itercount, f_norm, f_norm0, msg='Converged')


    def requires_derivs(self):
        """Newtonsolver always requires derivatives."""
        return True
