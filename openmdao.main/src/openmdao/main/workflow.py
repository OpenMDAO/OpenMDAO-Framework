
from enthought.traits.api import implements

from openmdao.main.interfaces import IWorkflow
from openmdao.main.container import Container
from openmdao.main.component import Component
from openmdao.main.exceptions import RunStopped

__all__ = ['Workflow']

def _is_component(obj):
    return isinstance(obj, Component)

class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order.
    """

    implements(IWorkflow)
    
    def __init__(self, scope=None, validator=_is_component):
        """ Create an empty flow. """
        self.scope = scope
        self._validator = validator
        self._iterator = None
        self._stop = False

    def run(self):
        """ Run through the nodes in the workflow list. """
        #if __debug__: self._logger.debug('execute %s' % self.get_pathname())
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

    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        raise NotImplemented("This Workflow has no '__iter__' function")
    
    def __len__(self):
        raise NotImplemented("This Workflow has no '__len__' function")
    
    def connect(self, srcpath, destpath):
        """Specify a connection between two components in this workflow. The
        names passed in are full pathnames to variables being connected."""
        raise NotImplemented("This Workflow has no 'connect' function")
        
    def disconnect(self, comp1name, comp2name):
        """Disconnect two components in this workflow."""
        pass

    
class SequentialFlow(Workflow):
    """A Workflow that is a simple sequence of components."""
    
    def __init__(self, scope=None, validator=_is_component):
        """ Create an empty flow. """
        super(SequentialFlow, self).__init__(scope=scope, validator=validator)
        self._nodes = []
        
    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return self._nodes.__iter__()
    
    def __len__(self):
        return len(self._nodes)

    def add(self, comp):
        """ Add a new component to the end of the workflow. """
        if self._validator and not self._validator(comp):
            msg = 'Workflow.add validation failed for type %s' % type(comp)
            if self.scope:
                self.scope.raise_exception(msg, TypeError)
            else:
                raise TypeError(msg)
        else:
            self._nodes.append(comp)
        
    def remove(self, comp):
        """Remove a component from this Workflow and any of its children."""
        self._nodes = [x for x in self._nodes if x is not comp]

