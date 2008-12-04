"""
A workflow consists of a list of nodes which are executed in order.
The typical node is a ComponentNode, which simply executes a Component.
"""

from zope.interface import implements

from openmdao.main.component import Component, STATE_RUNNING, STATE_WAITING, RUN_OK
from openmdao.main.interfaces import IWorkflow, IComponent, IDriver

__all__ = ('Workflow')


class Workflow(Component):
    """
    A workflow consists of a list of nodes which are executed in order.
    """

    implements(IWorkflow)
    
    def __init__(self, name, parent=None):
        """ Create an empty flow. """
        Component.__init__(self, name, parent)
        self.nodes = []

    def __len__(self):
        """ Not very meaningful, but it helps boolean tests when using RPyC. """
        return len(self.nodes)

    def add_node(self, node):
        """ Add a new node to the end of the flow. """
        assert IComponent.providedBy(node)
        assert not IDriver.providedBy(node)
        self.nodes.append(node)
        
    def remove_node(self, node):
        nodes = [x for x in self.nodes if x is not node]
        for n in nodes:
            if isinstance(n, Workflow):
                n.remove_node(node)
        self.nodes = nodes

    def execute(self):
        """ Run through the nodes in the workflow list. """
        status = RUN_OK
        for node in self.nodes:
            self.state = STATE_WAITING
            status = node.run()
            self.state = STATE_RUNNING
            if status != RUN_OK:
                return status
            if self._stop:
                return RUN_STOPPED
        return RUN_OK

    def steppable(self):
        """ Return True if it makes sense to 'step' this component. """
        return len(self.nodes) > 1

    def stop(self):
        """
        Stop all nodes.
        We assume it's OK to to call stop() on something that isn't running.
        """
        self._stop = True
        for node in self.nodes:
            node.stop()

    def get_nodes(self):
        """ Return nodes in flow as a tuple. """
        return tuple(self.nodes)

