""" A workflow that contains cyclic graphs. Note that a special solver is
required to converge this workflow in order to execute it. """

import networkx as nx
from networkx.algorithms.components import strongly_connected_components

try:
    from numpy import ndarray, zeros
except ImportError as err:
    logging.warn("In %s: %r", __file__, err)
    from openmdao.main.numpy_fallback import ndarray, zeros
    
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.seqentialflow import SequentialWorkflow

__all__ = ['CyclicWorkflow']

# SequentialWorkflow gives us the add and remove methods.
class CyclicWorkflow(SequentialWorkflow):
    """A CyclicWorkflow consists of a collection of Components that contains
    loops in the graph.
    """   
    
    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        
        super(CyclicWorkflow, self).__init__(parent, scope, members)
        self.config_changed()
        
    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        self._collapsed_graph = None
        self._topsort = None
        self._severed_edges = None
        self.res = None
        self.bounds = None
        
    def check_config(self):
        """Any checks we need. For now, drivers are not allowed. You can get
        around this by placing them in an assembly."""         
        
        for comp in self.get_components():
            if has_interface(comp, IDriver):
                msg = 'Subdriver not supported in a cyclicflow. Please ' \
                      'place it in a subassembly.'
                self.scope.raise_exception(msg, RuntimeError)
               
    def add(self, compnames, index=None, check=False):
        """ Add new component(s) to the workflow by name. """
        
        super(CyclicWorkflow, self).add(compnames, index, check)
        self.config_changed()

    def remove(self, compname):
        """Remove a component from this Workflow by name."""
        
        super(CyclicWorkflow, self).remove(compname)
        self.config_changed()
        
    def __iter__(self):
        """Iterate through the nodes in some proper order."""
        
        # resolve all of the components up front so if there's a problem it'll
        # fail early and not waste time running components
        scope = self.scope
        return [getattr(scope, n) for n in self._get_topsort()].__iter__()

    def _get_topsort(self):
        """ Return a sorted list of components in the workflow.
        """
        
        if self._topsort is None:
            graph = nx.DiGraph(self._get_collapsed_graph())
            
            cyclic = True
            self._severed_edges = []
            while cyclic:
                
                try:
                    self._topsort = nx.topological_sort(graph)
                    cyclic = False
                    
                except nx.NetworkXUnfeasible:
                    strong = strongly_connected_components(graph)
                    
                    # We may have multiple loops. We only deal with one at
                    # a time because multiple loops create some non-unique
                    # paths.
                    strong = strong[0]
                    
                    # Break one edge of the loop.
                    # For now, just break the first edge.
                    # TODO: smarter ways to choose edge to break.
                    graph.remove_edge(strong[-1], strong[0])
                    
                    # Keep a list of the edges we break, so that a solver
                    # can use them as its independents/dependents.
                    depgraph = self._parent.parent._depgraph
                    edge_set = depgraph.get_interior_edges([strong[-1], 
                                                            strong[0]])
                    
                    out_set = set(depgraph.var_edges(strong[-1]))
                    
                    # Our cut is directional, so only include that direction.
                    edges = edge_set.intersection(out_set)      
                
                    self._severed_edges += list(edges)
                
        return self._topsort
    
    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it. This graph can be cyclic."""
        
        # Cached
        if self._collapsed_graph:
            return self._collapsed_graph
        
        # Parent assembly's graph
        scope = self.scope
        graph = scope._depgraph.copy_graph()
        
        contents = self.get_components()
        
        # add any dependencies due to ExprEvaluators
        for comp in contents:
            graph.add_edges_from([tup for tup in comp.get_expr_depends()])
        
        # this way avoids a deep copy of edge/node data    
        collapsed_graph = nx.DiGraph(graph)  

        cnames = set(self._names)
        self._collapsed_graph = collapsed_graph.subgraph(cnames)
        return self._collapsed_graph
    
    def get_interior_edges(self):
        """ Returns an alphabetical list of all output edges that are
        interior to the set of components supplied."""
        
        names = self.get_names()
        edge_list = self._parent.parent._depgraph.get_interior_edges(names)
        return sorted(list(edge_list))
    
    def get_dimensions(self):
        """Returns some bookkeeping items that describe the dimension of the
        interior edge connections. Also creates the residual array.
        """
        edges = self.get_interior_edges()
        
        nEdge = 0
        self.bounds = {}
        for edge in edges:
            
            src = edge[0]
            
            val = self._parent.parent.get(src)
            if isinstance(val, float):
                width = 1
            elif isinstance(val, ndarray):
                shape = val.shape
                if len(shape) == 2:
                    width = shape[0]*shape[1]
                else:
                    width = shape[0]
            else:
                msg = "Variable %s is of type %s. " % (src, type(val)) + \
                      "This type is not supported by the MDA Solver."
                self.scope.raise_exception(msg, RuntimeError)
                
            self.bounds[edge] = (nEdge, nEdge+width)
            nEdge += width
            
        if self.res is None:
            self.res = zeros((nEdge, 1))
            
        return edges, nEdge, self.bounds
    
    def calculate_residuals(self):
        """Caclulate and return the vector of residuals based on the current
        state of the system in our workflow."""
        
        for edge in self.get_interior_edges():
            src, target = edge
            src_val = self._parent.parent.get(src)
            target_val = self._parent.parent.get(target)
            
            i1, i2 = self.bounds[edge]
            self.res[i1:i2] = src_val - target_val
    
        return self.res