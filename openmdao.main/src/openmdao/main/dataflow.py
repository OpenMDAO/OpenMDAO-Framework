
import networkx as nx

from openmdao.main.workflow import SequentialFlow

__all__ = ['Dataflow']

class Dataflow(SequentialFlow):
    """
    A Dataflow consists of a collection of Components which are executed in 
    data flow order.
    """
    def __iter__(self):
        """Iterate through the nodes in dataflow order."""
        scope = self._parent.parent
        graph = scope.comp_graph.subgraph([n.name for n in self._nodes])
        for n in nx.topological_sort(graph):
            yield getattr(scope, n)
