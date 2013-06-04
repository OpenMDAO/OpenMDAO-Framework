"""
This solver can converge an MDA that contains a cyclic graph without requiring
the user to break connections and specify independent and dependent
variables
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['MDASolver']

import logging

try:
    import numpy
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
else:
    # this little funct replaces a dependency on scipy
    npnorm = numpy.linalg.norm
    def norm(a, ord=None):
        return npnorm(numpy.asarray_chkfinite(a), ord=ord)

from scipy.sparse.linalg import gmres, LinearOperator
    
# pylint: disable-msg=E0611, F0401
from openmdao.main.api import Driver, CyclicWorkflow   
from openmdao.main.datatypes.api import Float, Int, Bool
from openmdao.util.decorators import stub_if_missing_deps


@stub_if_missing_deps('numpy')
class MDASolver(Driver):
    
    tolerance = Float(1.0e-8, iotype='in', desc='Global convergence tolerance')
    
    max_iteration = Int(30, iotype='in', desc='Maximum number of iterations')
    
    Newton = Bool(False, iotype='in', desc='Set to True to use a ' + \
                    'Newton method.')
    
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
        
        if self.Newton:
            self.execute_Newton()
        else:
            self.execute_Gauss_Seidel()
            
    def execute_Gauss_Seidel(self):
        """ Solver execution loop: fixed point iteration. """
        
        # Find dimension of our problem.
        self.workflow.get_dimensions()
        
        # Initial Run
        self.workflow.run()
        
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
            self.workflow.run()
            
            # New residuals
            norm = numpy.linalg.norm(self.workflow.calculate_residuals())
            print "Residual vector norm:\n", norm
            
            iter_num += 1
            self.record_case()
            
    def execute_Newton(self):
        """ Solver execution loop: Newton-Krylov. """
        
        # Find dimension of our problem.
        nEdge = self.workflow.get_dimensions()
        
        self.arg = numpy.zeros((nEdge, 1))
        A = LinearOperator((nEdge, nEdge),
                           matvec=self.matvecFWD,
                           dtype=float)
            
        # Initial Run
        self.workflow.run()
        
        # Initial residuals
        norm = numpy.linalg.norm(self.workflow.calculate_residuals())
        print "Residual vector norm:\n", norm
        
        # Loop until convergence of residuals
        iter_num = 0
        while (norm > self.tolerance) and (iter_num < self.max_iteration):
            
            # Each comp calculates its own derivatives at the current
            # point.
            for comp in self.workflow.__iter__():
                comp.linearize()
            
            # Call GMRES to solve the linear system
            dv, info = gmres(A, -self.workflow.res,
                             tol=self.tolerance,
                             maxiter=100)
            
            # Increment the model input edges by dv
            self.workflow.set_new_state(dv)
            
            self.workflow.run()
            
            # New residuals
            norm = numpy.linalg.norm(self.workflow.calculate_residuals())
            print "Residual vector norm:\n", norm
            
            iter_num += 1
            self.record_case()
            
    def matvecFWD(self, arg):
        '''Callback function for performing the matrix vector product.'''
        
        # Bookkeeping dictionaries
        inputs = {}
        outputs = {}
        
        # Start with zero-valued dictionaries cotaining keys for all inputs
        for comp in self.workflow.__iter__():
            name = comp.name
            inputs[name] = {}
            outputs[name] = {}
            
        # Fill input dictionaries with values from input arg.
        for edge in self.workflow.get_interior_edges():
            src, target = edge
            i1, i2 = self.workflow.bounds[edge]
            
            parts = src.split('.')
            comp_name = parts[0]
            var_name = '.'.join(parts[1:])
            
            outputs[comp_name][var_name] = arg[i1:i2]
            inputs[comp_name][var_name] = arg[i1:i2]
            
            parts = target.split('.')
            comp_name = parts[0]
            var_name = '.'.join(parts[1:])
            
            inputs[comp_name][var_name] = arg[i1:i2]
            
        # Call ApplyJ on each component
        for comp in self.workflow.__iter__():
            name = comp.name
            comp.applyJ(inputs[name], outputs[name])
            
        # Poke results into the return vector
        result = numpy.zeros(len(arg))
        for edge in self.workflow.get_interior_edges():
            src, target = edge
            i1, i2 = self.workflow.bounds[edge]
        
            parts = src.split('.')
            comp_name = parts[0]
            var_name = '.'.join(parts[1:])
            
            result[i1:i2] = outputs[comp_name][var_name]
        
        return result
        
            
            