
import logging

from enthought.traits.api import implements
import networkx as nx
from networkx.algorithms.traversal import is_directed_acyclic_graph, strongly_connected_components

from openmdao.main.workflow import Workflow

__all__ = ['Dataflow']

                
class Dataflow(Workflow):
    """
    A Dataflow consists of a collection of Components which are executed in 
    data flow order.
    """

    def __init__(self, scope=None):
        """ Create an empty flow. """
        super(Dataflow, self).__init__(scope=scope)
        self._no_expr_graph = nx.DiGraph()
        
    def __contains__(self, name):
        """Return True if this Dataflow contains a Component with the
        given name.
        """
        return name in self._no_expr_graph
        
    def add_node(self, name):
        """Add the name of a Component to this Dataflow."""
        self._no_expr_graph.add_node(name)
        
    def remove_node(self, name):
        """Remove the name of a Component from this Dataflow."""
        self._no_expr_graph.remove_node(name)
        
    def get_graph(self):
        """Return the Component graph for this Dataflow."""
        return self._no_expr_graph
        
    def connect(self, srccompname, destcompname, srcvarname, destvarname):
        """Add an edge to our Component graph from *srccompname* to *destcompname*.
        The *srcvarname* and *destvarname* args are for data reporting only.
        """
        # if an edge already exists between the two components, 
        # just increment the ref count
        graph = self._no_expr_graph
        try:
            graph[srccompname][destcompname]['refcount'] += 1
        except KeyError:
            graph.add_edge(srccompname, destcompname, refcount=1)
            
        if not is_directed_acyclic_graph(graph):
            # do a little extra work here to give more info to the user in the error message
            strongly_connected = strongly_connected_components(graph)
            refcount = graph[srccompname][destcompname]['refcount'] - 1
            if refcount == 0:
                graph.remove_edge(srccompname, destcompname)
            else:
                graph[srccompname][destcompname]['refcount'] = refcount
            for strcon in strongly_connected:
                if len(strcon) > 1:
                    raise RuntimeError(
                        'circular dependency (%s) would be created by connecting %s to %s' %
                                 (str(strcon), 
                                  '.'.join([srccompname,srcvarname]), 
                                  '.'.join([destcompname,destvarname]))) 
        
    def disconnect(self, comp1name, comp2name):
        """Decrement the ref count for the edge in the dependency graph 
        between the two components or remove the edge if the ref count
        reaches 0.
        """
        refcount = self._no_expr_graph[comp1name][comp2name]['refcount'] - 1
        if refcount == 0:
            self._no_expr_graph.remove_edge(comp1name, comp2name)
        else:
            self._no_expr_graph[comp1name][comp2name]['refcount'] = refcount

            
    def nodes_iter(self):
        """Iterate through the nodes in dataflow order."""
        drivers = self.scope.drivers
        self._drvsorter = None
        
        for n in nx.topological_sort(self._no_expr_graph):
            yield getattr(self.scope, n)