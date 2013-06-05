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
                    depgraph = self.scope._depgraph
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
    
    def initialize_residual(self):
        """Creates the array that stores the residual. Also returns the
        number of edges.
        """
        edges = self.get_interior_edges()
        
        nEdge = 0
        self.bounds = {}
        for edge in edges:
            
            src = edge[0]
            
            val = self.scope.get(src)
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
            
        # Initialize the residual vector on the first time through, and also
        # if for some reason the number of edges has changed.
        if self.res is None or nEdge != self.res.shape[0]:
            self.res = zeros((nEdge, 1))
            
        return nEdge
    
    def calculate_residuals(self):
        """Caclulate and return the vector of residuals based on the current
        state of the system in our workflow."""
        
        for edge in self.get_interior_edges():
            src, target = edge
            src_val = self.scope.get(src)
            target_val = self.scope.get(target)
            
            i1, i2 = self.bounds[edge]
            self.res[i1:i2] = src_val - target_val
    
        return self.res
    
    def set_new_state(self, dv):
        """Adds a vector of new values to the current model state at the
        input edges.
        
        dv: ndarray (nEdge, 1)
            Array of values to add to the model inputs.
        """
        
        # Apply new state to model
        for edge in self._severed_edges:
            deflatten = False
            src, target = edge
                
            i1, i2 = self.bounds[edge]
            if i2-i1 > 1:
                old_val = self.scope.get(target)
                if old_val.shape[0] > old_val.shape[1]:
                    old_val = old_val.T[0]
                    deflatten = True
                new_val = old_val + dv[i1:i2]
            else:
                new_val = self.scope.get(target) + \
                          float(dv[i1:i2])
                
            if deflatten:
                new_val = new_val.reshape([i2-i1, 1])
            self.scope.set(target, new_val, force=True)
            
            # Prevent OpenMDAO from stomping on our poked input.
            parts = target.split('.')
            comp_name = parts[0]
            var_name = '.'.join(parts[1:])
            comp = self.scope.get(comp_name)
            valids = comp._valid_dict
            valids[var_name] = True
            
            #(An alternative way to prevent the stomping. This is more
            #concise, but setting an output and allowing OpenMDAO to pull it
            #felt hackish.)
            #self.scope.set(src, new_val, force=True)

    def matvecFWD(self, arg):
        '''Callback function for performing the matrix vector product of the
        workflow's full Jacobian with an incoming vector arg.'''
        
        # Bookkeeping dictionaries
        inputs = {}
        outputs = {}
        
        # Start with zero-valued dictionaries cotaining keys for all inputs
        for comp in self.__iter__():
            name = comp.name
            inputs[name] = {}
            outputs[name] = {}
            
        # Fill input dictionaries with values from input arg.
        for edge in self.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]
            
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
        for comp in self.__iter__():
            name = comp.name
            comp.applyJ(inputs[name], outputs[name])
            
        # Poke results into the return vector
        result = zeros(len(arg))
        for edge in self.get_interior_edges():
            src, target = edge
            i1, i2 = self.bounds[edge]
        
            parts = src.split('.')
            comp_name = parts[0]
            var_name = '.'.join(parts[1:])
            
            result[i1:i2] = outputs[comp_name][var_name]
        
        return result
        
