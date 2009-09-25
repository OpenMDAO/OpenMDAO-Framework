
from enthought.traits.api import implements

from openmdao.main.container import Container
from openmdao.main.component import Component
from openmdao.main.exceptions import RunStopped

__all__ = ['Workflow']


class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order.
    """

    def __init__(self, scope=None):
        """ Create an empty flow. """
        self.scope = scope
        self.nodes = []
        self._iterator = None
        self._stop = False

    def __len__(self):
        """ Not very meaningful, but it helps boolean tests when using RPyC. """
        return len(self.nodes)

    def add_node(self, node):
        """ Add a new node to the end of the flow. """
        if isinstance(node, Component):
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

    def run(self):
        """ Run through the nodes in the workflow list. """
        #if __debug__: self._logger.debug('execute %s' % self.get_pathname())
        for node in self.nodes_iter():
            node.run()
            if self._stop:
                self.raise_exception('Stop requested', RunStopped)
    
    def nodes_iter(self):
        """Iterate through the nodes."""
        for node in self.nodes:
            yield node
    
    def step(self):
        """Run a single component in the Workflow."""
        if self._iterator is None:
            self._iterator = self.nodes_iter()
            
        node = self._iterator.next()
        try:
            node.run()
        except StopIteration, err:
            self._iterator = None
            raise err
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
