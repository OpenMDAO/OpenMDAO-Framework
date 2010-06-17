
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
            strcon = strongly_connected_components(graph)
            scope.raise_exception('circular dependency (%s) found' % str(strcon[0]),
                                  RuntimeError)
        for n in topsort:
            yield getattr(scope, n)

    def _get_collapsed_graph(self):
        """Get a dependency graph with only our workflow components
        in it, with additional edges added to it from sub-workflows
        of any Driver components in our workflow, and from any Expressions
        or ExpressionLists in any components in our workflow.
        """
        to_add = []
        scope = self._scope
        graph = scope.comp_graph.graph().copy()
        
        # add any dependencies due to Expressions or ExpressionLists
        for comp in self._nodes:
            graph.add_edges_from([tup for tup in comp.get_expr_depends()])
            
        collapsed_graph = graph.copy()

        # find all of the incoming and outgoing edges to/from all of the components
        # in each driver's iteration set so we can add edges to/from the driver
        # in our collapsed graph
        cnames = set([c.name for c in self._nodes])
        removes = set()
        itersets = {}
        for comp in self._nodes:
            cname = comp.name
            if isinstance(comp, Driver):
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
        last = len(self._nodes)-1
        if last > 0:
            for i,comp in enumerate(self._nodes):
                if graph.degree(comp.name) == 0:
                    if i < last:
                        for n in self._nodes[i+1:]:
                            collapsed_graph.add_edge(comp.name, n.name)
                    else:
                        for n in self._nodes[0:i]:
                            collapsed_graph.add_edge(n.name, comp.name)

        return collapsed_graph.subgraph(cnames-removes)
