
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

    def _execute_if_needed(self):
        """Override of Component version to force execution even if we have
        no invalid inputs or outputs.
        """
        if __debug__: self._logger.debug('executing %s' % self.get_pathname())
        self.execute()
            
    def execute(self):
        """ Run through the nodes in the workflow list. """
        for node in self.nodes:
            self.state = STATE_WAITING
            node.run()
            self.state = STATE_RUNNING
            if self._stop:
                self.raise_exception('Stop requested', RunStopped)
    
    def step(self):
        """Run a single component in the Workflow"""
        if self._iterator is None:
            self._iterator = self.nodes.__iter__()
            
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
        for node in self.nodes:
            node.stop()

    def get_nodes(self):
        """ Return nodes in flow as a tuple. """
        return tuple(self.nodes)

