
from zope.interface import implements
import networkx as nx
from networkx.algorithms.traversal import strongly_connected_components

from openmdao.main.interfaces import IAssembly, IComponent, IDriver
from openmdao.main.workflow import Workflow
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.interfaces import IWorkflow

__all__ = ['Dataflow']


class Dataflow(Workflow):
    """
    A Dataflow consists of a collection of Components which are executed in 
    data flow order.
    """

    implements(IWorkflow)
    
    def __init__(self, name, parent=None):
        """ Create an empty flow. """
        super(Dataflow, self).__init__(name, parent)

    def nodes_iter(self):
        """Iterate through the nodes in dataflow order, allowing for multiple Driver
        loops within the same Assembly.
        """
        if self.parent and IAssembly.providedBy(self.parent):
            graph = self.parent.get_component_graph().copy()
            pargraph = self.parent.get_component_graph()
            for compname in pargraph:
                obj = getattr(self.parent, compname)
                if IDriver.providedBy(obj):
                    graph.add_edges_from(obj.get_ref_graph().edges())
                    
            # create a new graph with strongly connected components as nodes so we
            # can eliminate cycles and topologically sort the strongly connected components
            strongs = strongly_connected_components(graph)
            strong_dict = {}
            strong_graph = nx.DiGraph()
            for i,strong in enumerate(strongs):
                strong_graph.add_node(i)
                for node in strong:
                    strong_dict[node] = i # map nodes to their strongly connected comp
            for node in graph.nodes():
                for u,v in graph.edges(node):
                    if strong_dict[u] != strong_dict[v]:
                        strong_graph.add_edge(strong_dict[u], strong_dict[v])
            sorted_strongs = nx.topological_sort(strong_graph)
            
            # For each strongly connected component, we need to break the loop somewhere
            # and topologically sort it to get a good ordering.  We'll break the loop
            # by removing edges due to ref variable inputs on the Driver. If there's more
            # than one Driver in a strongly connected component, we may have issues...
            for sccomp in sorted_strongs:
                if len(strongs[sccomp]) == 1:  # no loop, just a single component
                    compname = strongs[sccomp][0]
                    if compname != self.name:
                        yield getattr(self.parent, strongs[sccomp][0])
                else:  # some kind of loop. break it by throwing out driver ref input edges
                    scc_graph = nx.DiGraph()
                    for compname in strongs[sccomp]:
                        scc_graph.add_node(compname)
                    for compname in strongs[sccomp]:
                        obj = getattr(self.parent, compname)
                        if IDriver.providedBy(obj):
                            scc_graph.add_edges_from(obj.get_ref_graph(skip_inputs=True).edges())
                    sorted_names = nx.topological_sort(scc_graph)
                    if sorted_names == None:
                        self.raise_exception('cannot break loop %s into a sortable graph' %
                                             strongs[sccomp], RuntimeError)
                    for compname in sorted_names:
                        if __debug__: self._logger.debug('yielding %s' % compname)
                        if compname != self.name:
                            yield getattr(self.parent, compname)                
        else:
            self.raise_exception('DataFlow requires an Assembly parent', RuntimeError)
    
