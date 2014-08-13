"""
A python Newton solver with line-search adapation of the relaxation parameter.
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['NewtonSolver']

from scipy.optimize import fsolve

# this little funct replaces a dependency on scipy
import numpy
npnorm = numpy.linalg.norm
def norm(a, ord=None):
    return npnorm(numpy.asarray_chkfinite(a), ord=ord)

# pylint: disable-msg=E0611, F0401
from openmdao.main.api import Driver, CyclicWorkflow
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

    # pylint: disable-msg=E1101
    atol = Float(1.0e-12, iotype='in', desc='Absolute convergence tolerance')

    rtol = Float(1.0e-10, iotype='in', desc='Relative convergence tolerance')

    max_iteration = Int(20, iotype='in', desc='Maximum number of iterations')

    ls_atol = Float(1.0e-10, iotype='in', desc='Absolute convergence tolerance')

    ls_rtol = Float(0.9, iotype='in', desc='Relative convergence tolerance')

    ls_max_iteration = Int(10, iotype='in',
                          desc='Maximum number of line searches')

    alpha = Float(1.0, iotype='in', low=0.0, high=1.0,
                  desc='Initial over-relaxation factor')

    def __init__(self):

        super(NewtonSolver, self).__init__()
        self.workflow = CyclicWorkflow()

    def check_config(self, strict=False):
        """ This solver requires a CyclicWorkflow. """

        super(NewtonSolver, self).check_config(strict=strict)

        if not isinstance(self.workflow, CyclicWorkflow):
            msg = "The NewtonSolver requires a CyclicWorkflow workflow."
            self.raise_exception(msg, RuntimeError)

    def execute(self):
        """ General Newton's method. """

        system = self.workflow._system
        options = self.gradient_options
        fvec = system.vec['f']
        dfvec = system.vec['df']
        uvec = system.vec['u']

        # perform an initial run
        self.pre_iteration()
        self.run_iteration()
        self.post_iteration()

        f_norm = npnorm(fvec.array)
        f_norm0 = f_norm

        itercount = 0
        alpha = self.alpha
        while itercount < self.max_iteration and f_norm > self.atol and \
              f_norm/f_norm0 > self.rtol:

            system.calc_newton_direction(options=options)

            uvec.array -= alpha*dfvec.array

            self.pre_iteration()
            self.run_iteration()
            self.post_iteration()

            f_norm = npnorm(fvec.array)
            print "Norm:", f_norm
            itercount += 1

            ls_itercount = 0

            # Backtracking Line Search
            while ls_itercount < self.ls_max_iteration and \
                  f_norm > self.ls_atol and \
                  f_norm/f_norm0 > self.ls_rtol:

                uvec.array += alpha*dfvec.array
                alpha = alpha/2.0
                uvec.array -= alpha*dfvec.array

                self.pre_iteration()
                self.run_iteration()
                self.post_iteration()

                f_norm = npnorm(fvec.array)
                print "Backtracking Norm: %f, Alpha: %f" % (f_norm, alpha)
                ls_itercount += 1

        #print "done"