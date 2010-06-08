
import networkx as nx

from openmdao.main.workflow import SequentialFlow
from openmdao.main.driver import Driver

__all__ = ['Dataflow']

class Dataflow(SequentialFlow):
    """
    A Dataflow consists of a collection of Components which are executed in 
    data flow order.
    """
    def __iter__(self):
        """Iterate through the nodes in dataflow order."""
        # import Driver here to avoid circular import
        scope = self._parent.parent
        graph = self._get_collapsed_graph()
        for n in nx.topological_sort(graph):
            yield getattr(scope, n)

    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it, with additional edges added to it from sub-workflows
        of any Driver components in our workflow.
        """
        # find all of the incoming and outgoing edges to/from all of the components
        # in each driver's iteration set so we can add edges to/from the driver
        # in our collapsed graph
        to_add = []
        scope = self._parent.parent
        graph = scope.comp_graph.graph()
        for comp in self._nodes:
            if isinstance(comp, Driver):
                iterset = [c.name for c in comp.iteration_set()]
                dname = comp.name
                for u,v in graph.edges_iter(nbunch=iterset): # outgoing edges
                    if v not in iterset:
                        to_add.append((dname, v)) # add output edge to collapsed loop
                for u,v in graph.in_edges_iter(nbunch=iterset):
                    if u not in iterset:
                        to_add.append((u, dname)) # add input edge to collapsed loop

        graph = scope.comp_graph.subgraph([n.name for n in self._nodes])
        graph.add_edges_from(to_add)
        return graph
