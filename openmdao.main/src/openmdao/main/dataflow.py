
from zope.interface import implements
import networkx as nx

from openmdao.main.interfaces import IAssembly, IComponent, IDriver
from openmdao.main.workflow import Workflow
from openmdao.main.interfaces import IWorkflow

__all__ = ['DataFlow']


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
            if len(drivers) > 0:
                self.graph = self.parent._dep_graph.copy()
                for driver in drivers:
                    for outdep in driver.get_ref_outputs():
                        self.graph.add_edge(driver.name, outdep.split('.',1)[0])
                    for indep in driver.get_ref_inputs():
                        self.graph.add_edge(indep.split('.',1)[0], driver.name)

                #sccs = nx.strongly_connected_components(self.graph)
                #strong_graph = nx.MultiDiGraph()
                #name2strong = {}
                #for i,strong in enumerate(sccs):
                    #strong_graph.add_node(i)
                    #for name in strong:
                        
                
            else:
                self.graph = self.parent._dep_graph
            self.compnames = [x.name for x in self.parent.values(pub=False) 
                                  if IComponent.providedBy(x) and x is not self]
            self.execute()
        
    def nodes_iter(self):
        """Iterate through the nodes in dataflow order."""
        if self.parent:
            for compname in nx.topological_sort(self.graph):
                if compname in self.compnames and not compname.startswith('#'):
                    yield getattr(self.parent, compname)
    
