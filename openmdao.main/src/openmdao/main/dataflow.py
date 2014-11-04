""" A workflow where the execution order is automatically inferred from the
data connections."""

from openmdao.main.sequentialflow import SequentialWorkflow
from openmdao.main.interfaces import IDriver
from openmdao.main.mp_support import has_interface
from openmdao.main.depgraph import gsort

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
        self._get_collapsed_graph()
        return [getattr(scope, n) for n in self._fullnames].__iter__()

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
        graph_with_subs = graph.component_graph()
        collapsed_graph = graph_with_subs.subgraph(cnames)
        for comp in comps:
            if has_interface(comp, IDriver):
                comp._collapse_subdrivers(collapsed_graph)

        self._fullnames = gsort(collapsed_graph, self._fullnames)

        self._collapsed_graph = collapsed_graph

        return self._collapsed_graph
