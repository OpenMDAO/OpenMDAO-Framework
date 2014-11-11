"""
A python Newton solver with line-search adapation of the relaxation parameter.
"""

# pylint: disable=C0103

#public symbols
__all__ = ['NewtonSolver']

import numpy
npnorm = numpy.linalg.norm
def norm(a, order=None):
    '''This little funct for nomr replaces a dependency on scipy
    '''
    return npnorm(numpy.asarray_chkfinite(a), ord=order)

# pylint: disable=E0611, F0401
from openmdao.main.case import Case
from openmdao.main.driver import Driver
from openmdao.main.datatypes.api import Float, Int
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


    def execute(self):
        """ General Newton's method. """

        system = self.workflow._system
        options = self.gradient_options
        fvec = system.vec['f']
        dfvec = system.vec['df']
        uvec = system.vec['u']
        iterbase = self.workflow._iterbase()

        # perform an initial run
        self.workflow._system.evaluate(iterbase, case_uuid=Case.next_uuid())

        f_norm = norm(fvec.array)
        f_norm0 = f_norm
        #print self.name, "Norm: ", f_norm, 0
        #print uvec.array, fvec.array

        itercount = 0
        alpha = self.alpha
        while itercount < self.max_iteration and f_norm > self.atol and \
              f_norm/f_norm0 > self.rtol:

            system.calc_newton_direction(options=options)
            #print "new direction", dfvec.array

            #print "LS 1", uvec.array, '+', dfvec.array
            uvec.array += alpha*dfvec.array
            #for param in self.list_param_targets():
                #p_edge = self.parent.name2collapsed.get(param)
                #uvec[p_edge] += alpha*dfvec[p_edge]

            # Just evaluate the model with the new points
            self.workflow._system.evaluate(iterbase, case_uuid=Case.next_uuid())

            f_norm = norm(fvec.array)
            #print self.name, "Norm: ", f_norm, itercount+1
            #print uvec.array, fvec.array
            itercount += 1

            ls_itercount = 0

            # Backtracking Line Search
            while ls_itercount < self.ls_max_iteration and \
                  f_norm > self.ls_atol and \
                  f_norm/f_norm0 > self.ls_rtol:

                uvec.array -= alpha*dfvec.array
                #for param in self.list_param_targets():
                    #p_edge = self.parent.name2collapsed.get(param)
                    #uvec[p_edge] -= alpha*dfvec[p_edge]
                alpha = alpha/2.0
                uvec.array += alpha*dfvec.array
                #for param in self.list_param_targets():
                    #p_edge = self.parent.name2collapsed.get(param)
                    #uvec[p_edge] += alpha*dfvec[p_edge]

                # Just evaluate the model with the new points
                self.workflow._system.evaluate(iterbase, case_uuid=Case.next_uuid())

                f_norm = npnorm(fvec.array)
                #print "Backtracking Norm: %f, Alpha: %f" % (f_norm, alpha)
                #print uvec.array, fvec.array
                ls_itercount += 1

            # Reset backtracking
            alpha = self.alpha

        # Need to make sure the whole workflow is executed at the final
        # point, not just evaluated.
        self.pre_iteration()
        self.run_iteration()
        self.post_iteration()

        #print self.name, "converged"

    def requires_derivs(self):
        return True
