""" A workflow that contains cyclic graphs. Note that a special solver is
required to converge this workflow in order to execute it. """

import networkx as nx
from networkx.algorithms.components import strongly_connected_components

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
        return [getattr(scope, n) for n in self._get_sort()].__iter__()

    def _get_sort(self):
        """ Return a sorted list of components in the workflow.
        """
        
        if self._topsort is None:
            graph = nx.DiGraph(self._get_collapsed_graph())
            
            cyclic = True
            while cyclic:
                
                try:
                    self._topsort = nx.topological_sort(graph)
                    cyclic = False
                    
                except nx.NetworkXUnfeasible:
                    strong = strongly_connected_components(graph)
                    print strong
                    # Only deal with one loop for now
                    strong = strong[0]
                    
                    # Break one edge of the loop
                    graph.remove_edge(strong[0], strong[-1])
                
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
    