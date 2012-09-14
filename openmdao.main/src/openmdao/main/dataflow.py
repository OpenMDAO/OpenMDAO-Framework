
import networkx as nx
from networkx.algorithms.components import strongly_connected_components

from openmdao.main.seqentialflow import SequentialWorkflow
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface

__all__ = ['Dataflow']

class Dataflow(SequentialWorkflow):
    """
    A Dataflow consists of a collection of Components which are executed in 
    data flow order.
    """
    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        super(Dataflow, self).__init__(parent, scope, members)
        self.config_changed()

    def __iter__(self):
        """Iterate through the nodes in dataflow order."""
        # resolve all of the components up front so if there's a problem it'll fail early
        # and not waste time running components
        scope = self.scope
        return [getattr(scope, n) for n in self._get_topsort()].__iter__()

    def add(self, compnames, index=None):
        """ Add new component(s) to the workflow by name. """
        super(Dataflow, self).add(compnames, index)
        self.config_changed()

    def remove(self, compname):
        """Remove a component from this Workflow by name."""
        super(Dataflow, self).remove(compname)
        self.config_changed()

    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        self._collapsed_graph = None
        self._topsort = None

    def _get_topsort(self):
        if self._topsort is None:
            graph = self._get_collapsed_graph()
            try:
                self._topsort = nx.topological_sort(graph)
            except nx.NetworkXUnfeasible:
                # do a little extra work here to give more info to the user in the error message
                strcon = strongly_connected_components(graph)
                self.scope.raise_exception('circular dependency found between the following: %s' % str(strcon[0]),
                                           RuntimeError)
        return self._topsort
            
    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it, with additional edges added to it from sub-workflows
        of any Driver components in our workflow, and from any ExprEvaluators
        in any components in our workflow.
        """
        if self._collapsed_graph:
            return self._collapsed_graph
        
        to_add = []
        scope = self.scope
        graph = scope._depgraph.copy_graph()
        
        contents = self.get_components()
        
        # add any dependencies due to ExprEvaluators
        for comp in contents:
            graph.add_edges_from([tup for tup in comp.get_expr_depends()])
            
        collapsed_graph = nx.DiGraph(graph)  # this way avoids a deep copy of edge/node data

        # find all of the incoming and outgoing edges to/from all of the components
        # in each driver's iteration set so we can add edges to/from the driver
        # in our collapsed graph
        cnames = set(self._names)
        removes = set()
        itersets = {}
        for comp in contents:
            cname = comp.name
            if has_interface(comp, IDriver):
                iterset = [c.name for c in comp.iteration_set()]
                itersets[cname] = iterset
                removes.update(iterset)
                for u,v in graph.edges_iter(nbunch=iterset): # outgoing edges
                    if v != cname and v not in iterset:
                        collapsed_graph.add_edge(cname, v)
                for u,v in graph.in_edges_iter(nbunch=iterset): # incoming edges
                    if u != cname and u not in iterset:
                        collapsed_graph.add_edge(u, cname)
        # connect all of the edges from each driver's iterset members to itself
        to_add = []
        for drv,iterset in itersets.items():
            for cname in iterset:
                for u,v in collapsed_graph.edges_iter(cname):
                    if v != drv:
                        to_add.append((drv, v))
                for u,v in collapsed_graph.in_edges_iter(cname):
                    if u != drv:
                        to_add.append((u, drv))
        collapsed_graph.add_edges_from(to_add)
        
        # now add some fake dependencies for degree 0 nodes in an attempt to
        # mimic a SequentialWorkflow in cases where nodes aren't connected.
        # Edges are added from each degree 0 node to all nodes after it in
        # sequence order.
        last = len(self._names)-1
        if last > 0:
            to_add = []
            for i,cname in enumerate(self._names):
                if collapsed_graph.degree(cname) == 0:
                    if i < last:
                        for n in self._names[i+1:]:
                            to_add.append((cname, n))
                    else:
                        for n in self._names[0:i]:
                            to_add.append((n, cname))
            collapsed_graph.add_edges_from(to_add)
        
        self._collapsed_graph = collapsed_graph.subgraph(cnames-removes)
        return self._collapsed_graph
