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
    
    use_Jacobian = Bool(False, iotype='in', desc='Set to True to use a ' + \
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
        
        if self.use_Jacobian:
            self.execute_Newton()
        else:
            self.execute_Gauss_Seidel()
            
            
    def execute_Gauss_Seidel(self):
        """ Solver execution loop: fixed point iteration. """
        
        # Find dimension of our problem.
        edges = self.workflow.get_interior_edges()
        
        nEdge = 0
        self.bounds = {}
        for edge in edges:
            
            src = edge[0]
            
            val = self.parent.get(src)
            if isinstance(val, float):
                width = 1
            elif isinstance(val, numpy.ndarray):
                shape = val.shape
                if len(shape) == 2:
                    width = shape[0]*shape[1]
                else:
                    width = shape[0]
            else:
                msg = "Variable %s is of type %s. " % (src, type(val)) + \
                      "This type is not supported by the MDA Solver."
                self.raise_exception(msg, RuntimeError)
                
            self.bounds[edge] = (nEdge, nEdge+width)
            nEdge += width
            
        self.res = numpy.zeros((nEdge, 1))
        
        # Initial Run
        self.workflow.run()
        
        # Initial residuals
        for edge in edges:
            src, target = edge
            src_val = self.parent.get(src)
            target_val = self.parent.get(target)
            
            i1, i2 = self.bounds[edge]
            self.res[i1:i2] = src_val - target_val
            
        print "Residual vector:\n", self.res
        norm = numpy.linalg.norm(self.res)
        
        # Loop until convergence of residuals
        iter_num = 0
        while (norm > self.tolerance) and (iter_num < self.max_iteration):
            
            # Apply residuals to the model
            for edge in edges:
                src, target = edge
                self.parent.set(target, self.parent.get(src), force=True)
            
            self.workflow.run()
            
            # Get residuals
            for edge in edges:
                src, target = edge
                src_val = self.parent.get(src)
                target_val = self.parent.get(target)
                
                i1, i2 = self.bounds[edge]
                self.res[i1:i2] = src_val - target_val
                
            print "Residual vector:\n", self.res
            norm = numpy.linalg.norm(self.res)
            
            iter_num += 1
            self.record_case()
            
    def execute_Newton(self):
        """ Solver execution loop: Newton-Krylov. """
        
        # Find dimension of our problem.
        edges = self.workflow.get_interior_edges()
        
        nEdge = 0
        self.bounds = {}
        for edge in edges:
            
            src = edge[0]
            
            val = self.parent.get(src)
            if isinstance(val, float):
                width = 1
            elif isinstance(val, numpy.ndarray):
                shape = val.shape
                if len(shape) == 2:
                    width = shape[0]*shape[1]
                else:
                    width = shape[0]
            else:
                msg = "Variable %s is of type %s. " % (src, type(val)) + \
                      "This type is not supported by the MDA Solver."
                self.raise_exception(msg, RuntimeError)
                
            self.bounds[edge] = (nEdge, nEdge+width)
            nEdge += width
            
        print nEdge
        self.res = numpy.zeros((nEdge, 1))
        self.arg = numpy.zeros((nEdge, 1))
        A = LinearOperator((nEdge, nEdge),
                           matvec=self.matvecFWD,
                           dtype=float)
            
        # Initial Run
        self.workflow.run()
        
        # Initial residuals
        for edge in edges:
            src, target = edge
            src_val = self.parent.get(src)
            target_val = self.parent.get(target)
            
            i1, i2 = self.bounds[edge]
            self.res[i1:i2] = src_val - target_val
            
        print "Residual vector:\n", self.res
        norm = numpy.linalg.norm(self.res)
        
        # Loop until convergence of residuals
        iter_num = 0
        while (norm > self.tolerance) and (iter_num < self.max_iteration):
            
            # Each comp calculates its own derivatives at the current
            # point.
            for comp in self.workflow.__iter__():
                comp.linearize()
            
            dv, info = gmres(A, -self.res,
                             tol=self.tolerance,
                             maxiter=100)
            print "dv", dv
            
            # Apply new state to model
            for edge in edges:
                src, target = edge
                i1, i2 = self.bounds[edge]
                if i2-i1 > 1:
                    new_val = self.parent.get(target) + dv[i1:i2]
                else:
                    new_val = self.parent.get(target) + float(dv[i1:i2])
                    
                self.parent.set(target, new_val, force=True)
            
            self.workflow.run()
            
            # Get residuals
            for edge in edges:
                src, target = edge
                src_val = self.parent.get(src)
                target_val = self.parent.get(target)
                
                i1, i2 = self.bounds[edge]
                self.res[i1:i2] = src_val - target_val
                
            print "Residual vector:\n", self.res
            norm = numpy.linalg.norm(self.res)
            
            iter_num += 1
            self.record_case()
            
    def matvecFWD(self, arg):
        '''Callback function for performing the matrix vector product.'''
        print "arg from GMRES", arg
        
        # Bookkeeping dictionaries
        inputs = {}
        outputs = {}
        
        # Start with zero-valued dictionaries cotaining keys for all inputs
        # and outputs.
        for comp in self.workflow.__iter__():
            name = comp.name
            val = 0.0
            inputs[name] = {key : val for key in \
                            comp.list_inputs()+comp.list_outputs()}
            outputs[name] = {key : val for key in comp.list_outputs()}
            
        # Fill input dictionaries with values from input arg.
        for edge in self.workflow.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]
            
            parts = src.split('.')
            comp_name = parts[0]
            var_name = '.'.join(parts[1:])
            
            outputs[comp_name][var_name] = arg[i1:i2]
            
            parts = target.split('.')
            comp_name = parts[0]
            var_name = '.'.join(parts[1:])
            
            inputs[comp_name][var_name] = arg[i1:i2]
            
        # Call ApplyJ on each component
        results = {}
        for comp in self.workflow.__iter__():
            name = comp.name
            results[name] = comp.applyJ(inputs[name])
            
        # Poke results into the return vector
        result = numpy.zeros(len(arg))
        for edge in self.workflow.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]
        
            parts = src.split('.')
            comp_name = parts[0]
            var_name = '.'.join(parts[1:])
            
            result[i1:i2] = results[comp_name][var_name]
        
        print "matvec result", result
        return result
        
            
            