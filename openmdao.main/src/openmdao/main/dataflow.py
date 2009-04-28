
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
            if len(drivers) > 1:
                self.graph = self.parent._dep_graph.copy()
                for driver in drivers:
                    for outdep in driver.get_ref_successors():
                        self.graph.add_edge(driver.name, outdep.split('.',1)[0])
                    for indep in driver.get_ref_predecessors():
                        self.graph.add_edge(indep.split('.',1)[0], driver.name)

                #sccs = nx.strongly_connected_components(self.graph)
                #strong_graph = nx.MultiDiGraph()
                #name2strong = {}
                #for i,strong in enumerate(sccs):
                    #strong_graph.add_node(i)
                    #for name in strong:                                       
            else:
                self.graph = self.parent._dep_graph
                if len(self.nodes) > 0:
                    self.compnames = set([x.name for x in self.nodes])
                else:
                    self.compnames = set([x.name for x in self.parent.values(pub=False) 
                                       if IComponent.providedBy(x) and x is not self])
                self.execute()        
        else:
            self.raise_exception('dataflow requires an Assembly parent',RuntimeError)
            
    def nodes_iter(self):
        """Iterate through the nodes in dataflow order."""
        if self.parent:
            sortedvars = nx.topological_sort(self.graph)
            completed = set()
            for varname in sortedvars:
                try:
                    compname, name = varname.split('.',1)
                except ValueError:
                    continue  # skip over boundary vars
                #if compname in self.compnames and not compname.startswith('#'):
                if compname in self.compnames and not compname in completed:
                    completed.add(compname)
                    yield getattr(self.parent, compname)
    
