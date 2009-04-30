
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

    def get_ref_successors(self):
        """Return a set of names of successor Variables based on the 
        contents of our output RefVariables.
        """
        outs = set()
        for refout in [v for v in self.values() if isinstance(v,RefVariable) 
                                 and v.iostatus==OUTPUT]:
            outs.update(refout.get_referenced_varpaths())
        return outs
    
    def get_ref_predecessors(self):
        """Return a set of names of predecessor Variables based on the 
        contents of our input RefVariables.
        """
        ins = set()
        for refin in [v for v in self.values() if isinstance(v,RefVariable) 
                                 and v.iostatus==INPUT]:
            ins.update(refin.get_referenced_varpaths())
        return ins
    def run (self):
        """Run this Workflow."""
        if self.parent and IAssembly.providedBy(self.parent):
            drivers = [x for x in self.parent.values(pub=False) 
                                  if IDriver.providedBy(x)]
            if len(drivers) > 1:
                self.graph = self.parent.get_var_graph().copy()
                for driver in drivers:
                    for refout in [v for v in driver.values() if isinstance(v,RefVariable) 
                                                                    and v.iostatus==OUTPUT]:
                        for refname in refout.get_referenced_varpaths():
                            self.graph.add_edge('.'.join([driver.name,refout.name]),
                                                refname)
                    for refin in [v for v in driver.values() if isinstance(v,RefVariable) 
                                                                     and v.iostatus==INPUT]:
                        for refname in refin.get_referenced_varpaths():
                            self.graph.add_edge(refname, '.'.join([driver.name,refin.name]))

                sccs = nx.strongly_connected_components(self.graph)
            else:
                self.graph = self.parent.get_comp_graph()
                #if len(self.nodes) > 0:
                    #self.compnames = set([x.name for x in self.nodes])
                #else:
                self.compnames = set([x.name for x in self.parent.values(pub=False) 
                                       if x is not self and IComponent.providedBy(x)])
                self.execute()
        else:
            self.raise_exception('dataflow requires an Assembly parent',RuntimeError)
            
    def nodes_iter(self):
        """Iterate through the nodes in dataflow order."""
        if self.parent:
            for compname in [cmp for cmp in nx.topological_sort(self.graph) if cmp in self.compnames]:
                yield getattr(self.parent, compname)
    
