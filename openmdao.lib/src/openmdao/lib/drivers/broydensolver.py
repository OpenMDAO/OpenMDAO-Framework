"""
    solver.py - Solver based on the nonlinear solvers found in Scipy.Optimize.
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['BroydenSolver']

import numpy
from scipy.optimize.nonlin import norm

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import on_trait_change, TraitError
                                 
from openmdao.main.api import Driver, ExpressionList
from openmdao.main.exceptions import RunStopped
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.lib.traits.float import Float
from openmdao.lib.traits.int import Int
from openmdao.lib.traits.enum import Enum
from openmdao.util.decorators import add_delegate

        
@add_delegate(HasParameters, HasEqConstraints)
class BroydenSolver(Driver):
    """ MIMO Newton-Raphson Solver with Bryoden approximation to the Jacobian.
    Algorithms are based on those found in scipy.optimize.
    
    Nonlinear solvers
    =================

    These solvers find x for which F(x)=0. Both x and F are multidimensional.

    All solvers have the parameter iter (the number of iterations to compute), some
    of them have other parameters of the solver, see the particular solver for
    details.

    A collection of general-purpose nonlinear multidimensional solvers.

    broyden2            --  Broyden's second method - the same as broyden1, but
                            updates the inverse Jacobian directly
    broyden3            --  Broyden's second method - the same as broyden2, but
                            instead of directly computing the inverse Jacobian,
                            it remembers how to construct it using vectors, and
                            when computing inv(J)*F, it uses those vectors to
                            compute this product, thus avoding the expensive NxN
                            matrix multiplication.
    excitingmixing      --  The excitingmixing algorithm. J=-1/alpha

    The broyden2 is the best. For large systems, use broyden3; excitingmixing is
    also very effective. The remaining nonlinear solvers from scipy are, in
    their own words, of "mediocre quality" so they were not implemented.
    """ 
    
    # pylint: disable-msg=E1101
    algorithm = Enum('broyden2', ['broyden2', 'broyden3', 'excitingmixing'],
                     iotype = 'in', desc='Algorithm to use. Choose from '
                     'broyden2, broyden3, and excitingmixing')
    
    itmax = Int(10, iotype='in', desc='Maximum number of iterations before '
                'termination')

    alpha = Float(0.4, iotype='in', 
                  desc='Mixing Coefficient')

    alphamax = Float(1.0, iotype='in', 
                  desc='Parameter alphamax (only used for excitingmixing)')

    tol = Float(0.00001, iotype='in', 
                  desc='Convergence tolerance. If the norm of the independent'
                  'vector is lower than this, then terminate successfully')

    def __init__(self):
        
        super(BroydenSolver, self).__init__()
        
        self.xin = numpy.zeros(0,'d')
        self.F = numpy.zeros(0,'d')
        
        
    def execute(self):
        """Solver execution."""
        
        # get the initial values of the independents
        independents = self.get_parameters().values()
        self.xin = numpy.zeros(len(independents),'d')
            
        # perform an initial run for self-consistency
        self.run_iteration()

        # get initial dependents
        dependents = self.get_eq_constraints().values()
        self.F = numpy.zeros(len(dependents),'d')
                
        # pick solver algorithm
        if self.algorithm == 'broyden2':
            self.execute_broyden2()
        elif self.algorithm == 'broyden3':
            self.execute_broyden3()
        elif self.algorithm == 'excitingmixing':
            self.execute_excitingmixing()
            
                
    def execute_broyden2(self):
        """from scipy: Broyden's second method.

        Updates inverse Jacobian by an optimal formula.
        There is NxN matrix multiplication in every iteration.

        The best norm |F(x)|=0.003 achieved in ~20 iterations.
        """ 
        
        xm = self.xin.T
        Fxm = numpy.matrix(self.F).T
        Gm = -self.alpha*numpy.matrix(numpy.identity(len(self.xin)))
        
        for n in range(self.itmax):
            
            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            deltaxm = -Gm*Fxm
            xm = xm + deltaxm.T

            # update the new independents in the model
            self.set_parameters(xm.flat)

            # run the model
            self.run_iteration()

            # get dependents
            for i, v in enumerate(self.get_eq_constraints().values()):
                term = v.evaluate()
                self.F[i] = term[0] - term[1]

            # successful termination if independents are below tolerance
            if norm(self.F) < self.tol:
                return
 
            Fxm1 = numpy.matrix(self.F).T
            deltaFxm = Fxm1 - Fxm
            Fxm = Fxm1.copy()
            Gm = Gm + (deltaxm-Gm*deltaFxm)*deltaFxm.T/norm(deltaFxm)**2


    def execute_broyden3(self):
        """from scipy: Broyden's second (sic) method.

        Updates inverse Jacobian by an optimal formula.
        The NxN matrix multiplication is avoided.

        The best norm |F(x)|=0.003 achieved in ~20 iterations.
        """
        
        zy = []
        
        def updateG(z, y):
            """G:=G+z*y.T'"""
            zy.append((z, y))
            
        def Gmul(f):
            """G=-alpha*1+z*y.T+z*y.T ..."""
            s = -self.alpha*f
            for z, y in zy:
                s = s + z*(y.T*f)
            return s

        xm = self.xin.T
        Fxm = numpy.matrix(self.F).T

        for n in range(self.itmax):
            
            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            deltaxm = Gmul(-Fxm)
            xm = xm + deltaxm.T

            # update the new independents in the model
            self.set_parameters(xm.flat)

            # run the model
            self.run_iteration()

            # get dependents
            for i, v in enumerate(self.get_eq_constraints().values()):
                term = v.evaluate()
                self.F[i] = term[0] - term[1]

            # successful termination if independents are below tolerance
            if norm(self.F) < self.tol:
                return
 
            Fxm1 = numpy.matrix(self.F).T
            deltaFxm = Fxm1 - Fxm
            Fxm = Fxm1.copy()
            updateG(deltaxm - Gmul(deltaFxm), deltaFxm/norm(deltaFxm)**2)


    def execute_excitingmixing(self):
        """from scipy: The excitingmixing method.

        J=-1/alpha

        The best norm |F(x)|=0.005 achieved in ~140 iterations.
        
        Note: Scipy uses 0.1 as the default value for alpha for this algorithm.
        Ours is set at 0.4, which is appropriate for Broyden2 and Broyden3, so
        adjust it accordingly if there are problems.
        """
    
        xm = self.xin.copy()
        beta = numpy.array([self.alpha]*len(xm))
        Fxm = self.F.T.copy()
    
        for n in range(self.itmax):

            if self._stop:
                self.raise_exception('Stop requested', RunStopped)

            deltaxm = beta*Fxm
            xm = xm + deltaxm

            # update the new independents in the model
            self.set_parameters(xm.flat)

            # run the model
            self.run_iteration()

            # get dependents
            for i, v in enumerate(self.get_eq_constraints().values()):
                term = v.evaluate()
                self.F[i] = term[0] - term[1]

            # successful termination if independents are below tolerance
            if norm(self.F) < self.tol:
                return
 
            Fxm1 = self.F.T
            
            for i in range(len(xm)):
                if Fxm1[i]*Fxm[i] > 0:
                    beta[i] = beta[i] + self.alpha
                    if beta[i] > self.alphamax:
                        beta[i] = self.alphamax
                else:
                    beta[i] = self.alpha
                
            Fxm = Fxm1.copy()

# end broydensolver.py