
import logging

from enthought.traits.api import implements
import networkx as nx
from networkx.algorithms.traversal import is_directed_acyclic_graph, strongly_connected_components

from openmdao.main.interfaces import IWorkflow
from openmdao.main.workflow import Workflow
from openmdao.main.component import Component

__all__ = ['Dataflow']

def _is_component(obj):
    return isinstance(obj, Component)
                
class Dataflow(Workflow):
    """
    A Dataflow consists of a collection of Components which are executed in 
    data flow order.
    """

    implements(IWorkflow)
    
    def __init__(self, scope=None, validator=_is_component):
        """ Create an empty flow. """
        super(Dataflow, self).__init__(scope=scope, validator=validator)
        self._no_expr_graph = nx.DiGraph()
        
    def __contains__(self, comp):
        """Return True if this Dataflow contains the given component."""
        return comp.name in self._no_expr_graph
    
    def __len__(self):
        return len(self._no_expr_graph)
        
    def __iter__(self):
        """Iterate through the nodes in dataflow order."""
        for n in nx.topological_sort(self._no_expr_graph):
            yield getattr(self.scope, n)
            
    def add(self, comp):
        """Add the name of a Component to this Dataflow."""
        if self._validator and not self._validator(comp):
            msg = 'Dataflow.add validation failed for type %s' % type(comp)
            if self.scope:
                self.scope.raise_exception(msg, TypeError)
            else:
                raise TypeError(msg)
        else:
            self._no_expr_graph.add_node(comp.name)

        
    def remove(self, comp):
        """Remove the name of a Component from this Dataflow."""
        self._no_expr_graph.remove_node(comp.name)
        
    def connect(self, srcpath, destpath):
        """Add an edge to our Component graph from *srccompname* to *destcompname*.
        The *srcvarname* and *destvarname* args are for data reporting only.
        """
        # if an edge already exists between the two components, 
        # just increment the ref count
        graph = self._no_expr_graph
        srccompname, srcvarname = srcpath.split('.', 1)
        destcompname, destvarname = destpath.split('.', 1)
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

