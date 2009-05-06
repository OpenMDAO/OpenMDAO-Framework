
from zope.interface import implements

from openmdao.main.component import Component, STATE_RUNNING, STATE_WAITING
from openmdao.main.exceptions import RunStopped
from openmdao.main.interfaces import IWorkflow, IComponent, IDriver

__all__ = ['Workflow']


class Workflow(Component):
    """
    A Workflow consists of a list of Components which are executed in 
    some order.
    """

    implements(IWorkflow)
    
    def __init__(self, name, parent=None, add_to_parent=True):
        """ Create an empty flow. """
        super(Workflow, self).__init__(name, parent, add_to_parent=add_to_parent)
        self.nodes = []
        self._iterator = None

    def __len__(self):
        """ Not very meaningful, but it helps boolean tests when using RPyC. """
        return len(self.nodes)

    def add_node(self, node):
        """ Add a new node to the end of the flow. """
        if IComponent.providedBy(node) and not IDriver.providedBy(node):
            self.nodes.append(node)
        else:
            self.raise_exception('%s is either a Driver or is not a Component' % node.get_pathname(),
                                 TypeError)
        
    def remove_node(self, node):
        """Remove a component from this Workflow and any of its children."""
        nodes = [x for x in self.nodes if x is not node]
        for comp in nodes:
            if isinstance(comp, Workflow):
                comp.remove_node(node)
        self.nodes = nodes

    def run (self, force=False):
        """Run this Workflow."""
        self.execute()

    def execute(self):
        """ Run through the nodes in the workflow list. """
        if __debug__: self._logger.debug('execute %s' % self.get_pathname())
        for node in self.nodes_iter():
            self.state = STATE_WAITING
            node.run()
            self.state = STATE_RUNNING
            if self._stop:
                self.raise_exception('Stop requested', RunStopped)
    
    def nodes_iter(self):
        """Iterate through the nodes."""
        for node in self.nodes:
            yield node
    
    def step(self):
        """Run a single component in the Workflow"""
        if self._iterator is None:
            self._iterator = self.nodes_iter()
            
        self.state = STATE_WAITING
        node = self._iterator.next()
        try:
            node.run()
        except StopIteration, err:
            self._iterator = None
            raise err
        self.state = STATE_RUNNING
        self.raise_exception('Step complete', RunStopped)

    def steppable(self):
        """ Return True if it makes sense to 'step' this component. """
        return len(self.nodes) > 1

    def stop(self):
        """
        Stop all nodes.
        We assume it's OK to to call stop() on something that isn't running.
        """
        self._stop = True
        for node in self.nodes_iter():
            node.stop()
