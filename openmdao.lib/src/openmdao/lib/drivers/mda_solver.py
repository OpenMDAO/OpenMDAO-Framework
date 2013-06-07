"""
This solver can converge an MDA that contains a cyclic graph without requiring
the user to break connections and specify independent and dependent variables.
Set "newton" to True to use Newton-Krylov, otherwise, set "newton" to False to
use Gauses-Siedel (aka fixed-point iteration).
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['MDASolver']

import logging

try:
    import numpy
    from scipy.sparse.linalg import gmres, LinearOperator
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))

# pylint: disable-msg=E0611, F0401
from openmdao.main.api import Driver, CyclicWorkflow   
from openmdao.main.datatypes.api import Float, Int, Bool
from openmdao.util.decorators import stub_if_missing_deps


@stub_if_missing_deps('numpy', 'scipy')
class MDASolver(Driver):
    
    tolerance = Float(1.0e-8, iotype='in', desc='Global convergence tolerance')
    
    max_iteration = Int(30, iotype='in', desc='Maximum number of iterations')
    
    newton = Bool(False, iotype='in', desc='Set to True to use a ' + \
                    'Newton-Krylov method. Defaults to False for ' + \
                    'Gauss-Siedel.')
    
    def __init__(self):
        
        super(MDASolver, self).__init__()
        self.workflow = CyclicWorkflow()
        
    def check_config(self):
        """ This solver requires a CyclicWorkflow. """
        
        super(MDASolver, self).check_config()
        
        if not isinstance(self.workflow, CyclicWorkflow):
            msg = "The MDASolver requires a CyclicWorkflow workflow."
            self.raise_exception(msg, RuntimeError)
        
    def execute(self):
        """ Pick our solver method. """
        
        if self.newton:
            self.execute_Newton()
        else:
            self.execute_Gauss_Seidel()
            
    def execute_Gauss_Seidel(self):
        """ Solver execution loop: fixed point iteration. """
        
        # Find dimension of our problem.
        self.workflow.initialize_residual()
        
        # Initial Run
        self.run_iteration()
        
        # Initial residuals
        norm = numpy.linalg.norm(self.workflow.calculate_residuals())
        print "Residual vector norm:\n", norm
        
        # Loop until the residuals converge
        iter_num = 0
        while (norm > self.tolerance) and (iter_num < self.max_iteration):
            
            # Pull values across severed edges
            for edge in self.workflow._severed_edges:
                src, target = edge
                self.parent.set(target, self.parent.get(src), force=True)
            
            # Run all components
            self.run_iteration()
            
            # New residuals
            norm = numpy.linalg.norm(self.workflow.calculate_residuals())
            print "Residual vector norm:\n", norm
            
            iter_num += 1
            self.record_case()
            
    def execute_Newton(self):
        """ Solver execution loop: Newton-Krylov. """
        
        # Find dimension of our problem.
        nEdge = self.workflow.initialize_residual()
        
        A = LinearOperator((nEdge, nEdge),
                           matvec=self.workflow.matvecFWD,
                           dtype=float)
            
        # Initial Run
        self.run_iteration()
        
        # Initial residuals
        norm = numpy.linalg.norm(self.workflow.calculate_residuals())
        print "Residual vector norm:\n", norm
        
        # Loop until convergence of residuals
        iter_num = 0
        while (norm > self.tolerance) and (iter_num < self.max_iteration):
            
            # Each comp calculates its own derivatives at the current
            # point. (i.e., linearizes)
            self.workflow.calc_derivatives(first=True)
            
            # Call GMRES to solve the linear system
            dv, info = gmres(A, -self.workflow.res,
                             tol=self.tolerance,
                             maxiter=100)
            
            # Increment the model input edges by dv
            self.workflow.set_new_state(dv)
            
            # Run all components
            self.run_iteration()
            
            # New residuals
            norm = numpy.linalg.norm(self.workflow.calculate_residuals())
            print "Residual vector norm:\n", norm
            
            iter_num += 1
            self.record_case()
            
            
            