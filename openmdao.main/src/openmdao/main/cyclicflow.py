""" A workflow that contains cyclic graphs. Note that a special solver is
required to converge this workflow in order to execute it. """

import networkx as nx
from networkx.algorithms.components import strongly_connected_components

from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.sequentialflow import SequentialWorkflow

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
        self._workflow_graph = None
        self._topsort = None
        self._severed_edges = []
        self._hidden_edges = set()
        self.res = None
        
    def check_config(self):
        """Any checks we need. For now, drivers are not allowed. You can get
        around this by placing them in an assembly."""         
        
        super(CyclicWorkflow, self).check_config()
        
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
                    edge_set = set(depgraph.get_directional_interior_edges(strong[-1], 
                                                                            strong[0]))
                    
                    self._severed_edges += list(edge_set)
                
        return self._topsort
    
    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it. This graph can be cyclic."""
        
        # Cached
        if self._workflow_graph is None:
       
            contents = self.get_components()
           
            # get the parent assembly's component graph
            scope = self.scope
            compgraph = scope._depgraph.component_graph()
            graph = compgraph.subgraph([c.name for c in contents])
           
            # add any dependencies due to ExprEvaluators
            for comp in contents:
                graph.add_edges_from([tup for tup in comp.get_expr_depends()])
                
            self._workflow_graph = graph
       
        return self._workflow_graph
    
