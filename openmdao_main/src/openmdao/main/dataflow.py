""" A workflow where the execution order is automatically inferred from the
data connections."""

import networkx as nx
from networkx.algorithms.components import strongly_connected_components
from networkx.algorithms.dag import is_directed_acyclic_graph

from openmdao.main.sequentialflow import SequentialWorkflow
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
        # resolve all of the components up front so if there's a problem
        # it will fail early and not waste time running components
        scope = self.scope
        return [getattr(scope, n) for n in self._get_topsort()].__iter__()

    def check_config(self):
        """Check for cyclic graph."""

        super(Dataflow, self).check_config()

        graph = self._get_collapsed_graph()
        if not is_directed_acyclic_graph(graph):
            # do a little extra work here to give more info to the user
            # in the error message
            strcon = strongly_connected_components(graph)
            self.scope.raise_exception('circular dependency found between'
                                       ' the following: %s'
                                       % str(strcon[0]), RuntimeError)

    def config_changed(self):
        """Notifies the Workflow that its configuration (dependencies, etc.)
        has changed.
        """
        super(Dataflow, self).config_changed()
        self._collapsed_graph = None
        self._topsort = None
        self._duplicates = None

    def _get_topsort(self):
        if self._topsort is None:
            graph = self._get_collapsed_graph()
            try:
                self._topsort = nx.topological_sort(graph)
            except nx.NetworkXUnfeasible:
                # do a little extra work here to give more info to the user
                # in the error message
                strcon = strongly_connected_components(graph)
                self.scope.raise_exception('circular dependency found between'
                                           ' the following: %s'
                                           % str(strcon[0]), RuntimeError)
            if self._duplicates:
                self._insert_duplicates()
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
        graph = scope._depgraph

        # find all of the incoming and outgoing edges to/from all of the
        # components in each driver's iteration set so we can add edges to/from
        # the driver in our collapsed graph
        comps = self.get_components(full=True)
        cnames = set([c.name for c in comps])
        removes = set()
        itersets = {}
        graph_with_subs = graph.component_graph()
        collapsed_graph = graph_with_subs.subgraph(cnames)

        for comp in comps:
            cname = comp.name
            if has_interface(comp, IDriver):
                iterset = [c.name for c in comp.iteration_set()]
                itersets[cname] = iterset
                removes.update(iterset)
                for u, v in graph_with_subs.edges_iter(nbunch=iterset):  # outgoing edges
                    if v != cname and v not in iterset and not v.startswith('_pseudo_'):
                        collapsed_graph.add_edge(cname, v)
                for u, v in graph_with_subs.in_edges_iter(nbunch=iterset):  # incoming edges
                    if u != cname and u not in iterset and not u.startswith('_pseudo_'):
                        collapsed_graph.add_edge(u, cname)

        # connect all of the edges from each driver's iterset members to itself
        # For this, we need the graph with the subdriver itersets all still in it.
        to_add = []
        for drv, iterset in itersets.items():
            for cname in iterset:
                for u, v in graph_with_subs.edges_iter(cname):
                    if v != drv:
                        to_add.append((drv, v))
                for u, v in graph_with_subs.in_edges_iter(cname):
                    if u != drv:
                        to_add.append((u, drv))
        collapsed_graph.add_edges_from(to_add)

        collapsed_graph = collapsed_graph.subgraph(cnames-removes)

        # now add some fake dependencies for degree 0 nodes in an attempt to
        # mimic a SequentialWorkflow in cases where nodes aren't connected.
        # Edges are added from each degree 0 node to all nodes after it in
        # sequence order.
        self._duplicates = set()
        last = len(self._names)-1
        if last > 0:
            to_add = []
            for i, cname in enumerate(self._names):
                if collapsed_graph.degree(cname) == 0:
                    if self._names.count(cname) > 1:
                        # Don't introduce circular dependencies.
                        self._duplicates.add(cname)
                    else:
                        if i < last:
                            for n in self._names[i+1:]:
                                to_add.append((cname, n))
                        else:
                            for n in self._names[0:i]:
                                to_add.append((n, cname))
            collapsed_graph.add_edges_from([(u, v) for u, v in to_add
                                            if u in collapsed_graph and v in collapsed_graph])

        self._collapsed_graph = collapsed_graph

        return self._collapsed_graph

    def _insert_duplicates(self):
        """We have some duplicate unconnected components. Adjust order
        to include duplicates in 'sequential' order.
        """
        # Remove (single instance of) duplicates from topo sort.
        topsort = self._topsort
        for cname in self._duplicates:
            topsort.remove(cname)

        # For each name in sequential order, if it's a duplicate we need
        # to insert it after all of its (possibly duplicated) predecessors.
        for i, cname in enumerate(self._names):
            if cname in self._duplicates:
                predecessors = self._names[0:i]
                max_index = -1
                for pname in predecessors:
                    start = 0
                    index = -1
                    for j in range(predecessors.count(pname)):
                        index = topsort.index(pname, start)
                        start = index + 1
                    max_index = max(index, max_index)
                topsort.insert(max_index+1, cname)
