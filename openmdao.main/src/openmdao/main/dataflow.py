""" A workflow where the execution order is automatically inferred from the
data connections."""

#import networkx as nx
from networkx.algorithms.components import strongly_connected_components
from networkx.algorithms.dag import is_directed_acyclic_graph

from openmdao.main.sequentialflow import SequentialWorkflow
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.depgraph import transitive_closure, gsort

__all__ = ['Dataflow']


class Dataflow(SequentialWorkflow):
    """
    A Dataflow consists of a collection of Components which are executed in
    data flow order.
    """
    def __init__(self, parent=None, members=None):
        """ Create an empty flow. """
        super(Dataflow, self).__init__(parent, members)
        self.config_changed()

    def __iter__(self):
        """Iterate through the nodes in dataflow order."""
        # resolve all of the components up front so if there's a problem
        # it will fail early and not waste time running components
        scope = self.scope
        self.get_names()
        return [getattr(scope, n) for n in self._fullnames].__iter__()

    def check_config(self, strict=False):
        """Check for cyclic graph."""

        super(Dataflow, self).check_config(strict=strict)

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

    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it, with additional edges added to it from sub-workflows
        of any Driver components in our workflow, and from any ExprEvaluators
        in any components in our workflow.
        """
        if self._collapsed_graph:
            return self._collapsed_graph

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

        collapsed_graph = collapsed_graph.subgraph(cnames-removes)

        alldeps = transitive_closure(collapsed_graph)
        self._fullnames = gsort(alldeps, self._fullnames)

        self._collapsed_graph = collapsed_graph

        return self._collapsed_graph
