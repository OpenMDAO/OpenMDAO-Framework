
import networkx as nx
from networkx.algorithms.traversal import strongly_connected_components

from openmdao.main.seqentialflow import SequentialWorkflow
from openmdao.main.driver import Driver

__all__ = ['Dataflow']

class Dataflow(SequentialWorkflow):
    """
    A Dataflow consists of a collection of Components which are executed in 
    data flow order.
    """
    def __init__(self, scope, members=None):
        """ Create an empty flow. """
        super(Dataflow, self).__init__(members)
        self._scope = scope

    def __iter__(self):
        """Iterate through the nodes in dataflow order."""
        # import Driver here to avoid circular import
        scope = self._scope
        graph = self._get_collapsed_graph()
        topsort = nx.topological_sort(graph)
        if topsort is None:
            # do a little extra work here to give more info to the user in the error message
            strongly_connected = strongly_connected_components(graph)
            for strcon in strongly_connected:
                raise RuntimeError('circular dependency (%s) found' % str(strcon))
        for n in topsort:
            yield getattr(scope, n)

    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it, with additional edges added to it from sub-workflows
        of any Driver components in our workflow, and from any Expressions
        or ExpressionLists in any components in our workflow.
        """
        # find all of the incoming and outgoing edges to/from all of the components
        # in each driver's iteration set so we can add edges to/from the driver
        # in our collapsed graph
        to_add = []
        scope = self._scope
        graph = scope.comp_graph.graph().copy()
        for comp in self._nodes:
            # add any dependencies due to Expressions or ExpressionLists
            graph.add_edges_from([tup for tup in comp.get_expr_depends()])
            
        cnames = []
        for comp in self._nodes:
            cname = comp.name
            cnames.append(cname)
            if isinstance(comp, Driver):
                iterset = [c.name for c in comp.iteration_set()]
                for u,v in graph.edges_iter(nbunch=iterset): # outgoing edges
                    if v not in iterset:
                        to_add.append((cname, v)) # add output edge to collapsed loop
                for u,v in graph.in_edges_iter(nbunch=iterset):
                    if u not in iterset:
                        to_add.append((u, cname)) # add input edge to collapsed loop

        graph = graph.subgraph(cnames)
        graph.add_edges_from(to_add)
        return graph
