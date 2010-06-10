
from enthought.traits.api import implements

from openmdao.main.interfaces import IWorkflow
from openmdao.main.container import Container
from openmdao.main.component import Component
from openmdao.main.exceptions import RunStopped

__all__ = ['Workflow']

class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order.
    """

    implements(IWorkflow)
    
    def __init__(self, members=None):
        """ Create an workflow. If members is not None,
        iterate through members and add them to the workflow."""
        self._iterator = None
        self._stop = False
        if members:
            for member in members:
                self.add(member)

    def run(self):
        """ Run through the nodes in the workflow list. """
        self._stop = False
        self._iterator = self.__iter__()
        for node in self._iterator:
            node.run()
            if self._stop:
                raise RunStopped('Stop requested')
        self._iterator = None
            
    def step(self):
        """Run a single component in the Workflow."""
        if self._iterator is None:
            self._iterator = self.__iter__()
            
        comp = self._iterator.next()
        try:
            comp.run()
        except StopIteration, err:
            self._iterator = None
            raise err
        raise RunStopped('Step complete')

    def stop(self):
        """
        Stop all nodes.
        We assume it's OK to to call stop() on something that isn't running.
        """
        self._stop = True
        for comp in self.__iter__():
            comp.stop()

    def add(self, comp):
        """ Add a new component to the workflow. """
        raise NotImplemented("This Workflow has no 'add' function")
        
    def remove(self, comp):
        """Remove a component from this Workflow"""
        raise NotImplemented("This Workflow has no 'remove' function")

    def contents(self):
        """Returns a list of all Components in the workflow.
        No ordering is assumed.
        """
        raise NotImplemented("This Workflow has no 'contents' function")

    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        raise NotImplemented("This Workflow has no '__iter__' function")
    
    def __len__(self):
        raise NotImplemented("This Workflow has no '__len__' function")
