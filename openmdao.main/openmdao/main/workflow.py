"""
A workflow consists of a list of nodes which are executed in order.
The typical node is a ComponentNode, which simply executes a Component.
"""

from zope.interface import implements

from openmdao.main.component import Component, STATE_RUNNING, STATE_WAITING
from openmdao.main.component import RUN_OK, RUN_STOPPED
from openmdao.main.interfaces import IWorkflow, IComponent, IDriver

__all__ = ['Workflow']


class Workflow(Component):
    """
    A workflow consists of a list of nodes which are executed in order.
    """

    implements(IWorkflow)
    
    def __init__(self, name, parent=None):
        """ Create an empty flow. """
        super(Workflow, self).__init__(name, parent)
        self.nodes = []
        self._iterator = None

    def __len__(self):
        """ Not very meaningful, but it helps boolean tests when using RPyC. """
        return len(self.nodes)

    def add_node(self, node):
        """ Add a new node to the end of the flow. """
        assert IComponent.providedBy(node)
        assert not IDriver.providedBy(node)
        self.nodes.append(node)
        
    def remove_node(self, node):
        """Remove a component from this Workflow and any of its children."""
        nodes = [x for x in self.nodes if x is not node]
        for comp in nodes:
            if isinstance(comp, Workflow):
                comp.remove_node(node)
        self.nodes = nodes

    def execute(self):
        """ Run through the nodes in the workflow list. """
        status = RUN_OK
        for node in self.nodes:
            self.state = STATE_WAITING
            status = node.run()
            self.state = STATE_RUNNING
            if status is not RUN_OK:
                return status
            if self._stop:
                return RUN_STOPPED
        return RUN_OK
    
    def step(self):
        """Run a single component in the Workflow"""
        if self._iterator is None:
            self._iterator = self.nodes.__iter__()
            
        self.state = STATE_WAITING
        node = self._iterator.next()
        try:
            status = node.run()
        except StopIteration, err:
            self._iterator = None
            raise err
        self.state = STATE_RUNNING
        if status is RUN_OK:
            return RUN_STOPPED
        else:
            return status

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

