
from zope.interface import implements
import networkx as nx
from networkx.algorithms.traversal import strongly_connected_components

from openmdao.main.interfaces import IAssembly, IComponent, IDriver
from openmdao.main.workflow import Workflow
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

    def run (self):
        """Run this Workflow."""
        if self.parent and IAssembly.providedBy(self.parent):
            drivers = [x for x in self.parent.values(pub=False) 
                                  if IDriver.providedBy(x)]
            if len(drivers) > 1:
                self.graph = self.parent._comp_graph().copy()
                for driver in drivers:
                    for refout in driver.get_ref_successors():
                        self.graph.add_edge(driver.name, refout)
                    for refin in driver.get_ref_predecessors():
                        self.graph.add_edge(refin, driver.name)

                #sccs = strongly_connected_components(self.graph)
            else:
                self.graph = self.parent._comp_graph
                self.compnames = set([x.name for x in self.parent.values(pub=False) 
                                       if x is not self and IComponent.providedBy(x)])
                self.execute()
        else:
            self.raise_exception('dataflow requires an Assembly parent',RuntimeError)
            
    def nodes_iter(self):
        """Iterate through the nodes in dataflow order."""
        if self.parent:
            for compname in [cmp for cmp in nx.topological_sort(self.graph) if cmp in self.compnames]:
                self._logger.debug('yielding %s' % compname)
                yield getattr(self.parent, compname)
    
