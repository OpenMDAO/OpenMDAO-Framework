""" Workflow class definition """

# pylint: disable-msg=E0611,F0401
from openmdao.main.exceptions import RunStopped

__all__ = ['Workflow']

class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order.
    """

    def __init__(self, parent=None, scope=None, members=None):
        """ Create an workflow. If members is not None,
        iterate through members and add them to the workflow.
        
        members: list of str (optional)
        """
        self._iterator = None
        self._stop = False
        self._parent = parent
        self._scope = scope
        if members:
            for member in members:
                if not isinstance(member, basestring):
                    raise TypeError("Components must be added to a workflow by name.")
                self.add(member)

    @property
    def scope(self):
        if self._scope is None and self._parent is not None:
            self._scope = self._parent.parent
        if self._scope is None:
            raise RuntimeError("workflow has no scope!")
        return self._scope
    
    @scope.setter
    def scope(self, scope):
        self._scope = scope
        self.config_changed()
    
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
        for comp in self.__iter__():
            comp.stop()
        self._stop = True

    def add(self, comp):
        """ Add a new component to the workflow by name."""
        raise NotImplemented("This Workflow has no 'add' function")
    
    def config_changed(self):
        """Notifies the Workflow that workflow configuration (dependencies, etc)
        has changed.
        """
        pass
        
    def remove(self, comp):
        """Remove a component from this Workflow by name."""
        raise NotImplemented("This Workflow has no 'remove' function")

    def contents(self):
        """Returns a list of all components in the workflow.
        No ordering is assumed.
        """
        raise NotImplemented("This Workflow has no 'contents' function")

    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        raise NotImplemented("This Workflow has no '__iter__' function")
    
    def __len__(self):
        raise NotImplemented("This Workflow has no '__len__' function")
