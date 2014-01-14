"""
Newton Krylov solver based around Scipy's newton method.
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['NewtonKrylov']

import logging

try:
    import numpy
    from scipy.optimize import newton
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))

# pylint: disable-msg=E0611, F0401
from openmdao.main.api import Driver, CyclicWorkflow   
from openmdao.main.datatypes.api import Float, Int, Enum, Bool
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasEqConstraints
from openmdao.main.interfaces import IHasParameters, IHasEqConstraints, \
                                     ISolver, implements
from openmdao.util.decorators import add_delegate, stub_if_missing_deps


@stub_if_missing_deps('numpy', 'scipy')
@add_delegate(HasParameters, HasEqConstraints)
class NewtonKrylov(Driver):
    
    implements(IHasParameters, IHasEqConstraints, ISolver)
    
    # pylint: disable-msg=E1101
    tolerance = Float(1.48e-8, iotype='in', desc='Global convergence tolerance')
    
    max_iteration = Int(50, iotype='in', desc='Maximum number of iterations')
    
    method = Enum('newton', ['newton'], iotype='in', 
                  desc='Solution method (currently only newton from scipy optimize)')
    
    print_convergence = Bool(True, iotype='in', desc='Set to False to ' +\
                             'suppress output of convergence history.')
    
    def __init__(self):
        
        super(NewtonKrylov, self).__init__()
        self.workflow = CyclicWorkflow()
        
    def check_config(self):
        """ This solver requires a CyclicWorkflow. """
        
        super(NewtonKrylov, self).check_config()
        
        if not isinstance(self.workflow, CyclicWorkflow):
            msg = "The NewtonKrylov requires a CyclicWorkflow workflow."
            self.raise_exception(msg, RuntimeError)
        
    def execute(self):
        """ Pick our solver method. """

        # perform an initial run
        self.pre_iteration()
        self.run_iteration()
        self.post_iteration()
        
        # Only one choice now
        self.execute_Newton()
            
    def execute_Newton(self):
        """ Solver execution loop: Newton-Krylov. """
        
        x0 = self.workflow.get_independents()
        newton(self._solve_callback, x0, fprime=self._jacobian_callback,
               maxiter=self.max_iteration, tol=self.tolerance)
        
    def _solve_callback(self, vals):
        """Function hook for evaluating our equations."""
        
        self.workflow.set_independents(vals)

        # run the model
        self.pre_iteration()
        self.run_iteration()
        self.post_iteration()
        
        return self.workflow.get_dependents()

    def _jacobian_callback(self, vals):
        """This function is passed to the internal solver to return the
        jacobian of the dependents with respect to the independents."""
        J = self.workflow.calc_gradient()
        print J
        return J