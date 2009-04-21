
from zope.interface import implements

from openmdao.main import Workflow
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

    def nodes_iter(self):
        """Iterate through the nodes in dataflow order."""
        for node in self.parent.get_component_iterator([x.name for x in self.nodes]):
            yield node
    
