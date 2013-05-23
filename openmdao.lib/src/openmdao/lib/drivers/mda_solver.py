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
    
# pylint: disable-msg=E0611, F0401
from openmdao.main.api import Driver, CyclicWorkflow   
from openmdao.main.datatypes.api import Float, Int
from openmdao.util.decorators import stub_if_missing_deps


@stub_if_missing_deps('numpy')
class MDASolver(Driver):
    
    tolerance = Float(1.0e-8, iotype='in', desc='Global convergence tolerance')
    
    max_iteration = Int(30, iotype='in', desc='Maximum number of iterations')
    
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
        """ Solver execution loop. """
        
        # Find dimension of our problem.
        edges = self.workflow.get_interior_edges()
        
        nEdge = 0
        bounds = {}
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
                
            bounds[edge] = (nEdge, nEdge+width)
            nEdge += width
            
        print nEdge
        res = numpy.zeros((nEdge, 1))
            
        # Initial Run
        self.workflow.run()
        
        # Initial residuals
        for edge in edges:
            src, target = edge
            src_val = self.parent.get(src)
            target_val = self.parent.get(target)
            
            i1, i2 = bounds[edge]
            res[i1:i2] = src_val - target_val
            
        print "Residual vector:\n", res
        norm = numpy.linalg.norm(res)
        
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
                
                i1, i2 = bounds[edge]
                res[i1:i2] = src_val - target_val
                
            print "Residual vector:\n", res
            norm = numpy.linalg.norm(res)
            
            iter_num += 1
            