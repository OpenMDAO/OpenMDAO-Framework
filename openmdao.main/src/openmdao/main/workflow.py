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
        """Create a Workflow.
        
        parent: Driver (optional)
            The Driver that contains this Workflow.  This option is normally
            passed instead of scope because scope usually isn't known at
            initialization time.  If scope is not provided, it will be
            set to parent.parent, which should be the Assembly that contains
            the parent Driver.
            
        scope: Component (optional)
            The scope can be explicitly specified here, but this is not 
            typically known at initialization time.

        members: list of str (optional)
            A list of names of Components to add to this workflow.
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
        """The scoping Component that is used to resolve the Component names in 
        this Workflow.
        """
        if self._scope is None and self._parent is not None:
            self._scope = self._parent.parent
        if self._scope is None:
            raise RuntimeError("workflow has no scope!")
        return self._scope
    
    @scope.setter
    def scope(self, scope):
        self._scope = scope
        self.config_changed()
    
    def run(self, ffd_order=0):
        """ Run the Components in this Workflow. """
        self._stop = False
        self._iterator = self.__iter__()
        for node in self._iterator:
            node.run(ffd_order=ffd_order)
            if self._stop:
                raise RunStopped('Stop requested')
        self._iterator = None
            
    def step(self, ffd_order=0):
        """Run a single component in this Workflow."""
        if self._iterator is None:
            self._iterator = self.__iter__()
            
        comp = self._iterator.next()
        try:
            comp.run(ffd_order=ffd_order)
        except StopIteration, err:
            self._iterator = None
            raise err
        raise RunStopped('Step complete')

    def calc_derivatives(self, first=False, second=False):
        """ Calculate derivatives and save baseline states for all components
        in this workflow."""
        
        self._stop = False
        self._iterator = self.__iter__()
        for node in self._iterator:
            node.calc_derivatives(first, second)
            if self._stop:
                raise RunStopped('Stop requested')
        self._iterator = None
        
    def stop(self):
        """
        Stop all Components in this Workflow.
        We assume it's OK to to call stop() on something that isn't running.
        """
        for comp in self.get_components():
            comp.stop()
        self._stop = True

    def add(self, comp):
        """ Add a new component to the workflow by name."""
        raise NotImplementedError("This Workflow has no 'add' function")
    
    def config_changed(self):
        """Notifies the Workflow that workflow configuration (dependencies, etc)
        has changed.
        """
        pass
        
    def remove(self, comp):
        """Remove a component from this Workflow by name."""
        raise NotImplementedError("This Workflow has no 'remove' function")

    def get_names(self):
        """Return a list of component names in this workflow."""
        raise NotImplementedError("This Workflow has no 'get_names' function")

    def get_components(self):
        """Returns a list of all component objects in the workflow. No ordering
        is assumed.
        """
        scope = self.scope
        return [getattr(scope, name) for name in self.get_names()]

    def __iter__(self):
        """Returns an iterator over the components in the workflow in
        some order.
        """
        raise NotImplementedError("This Workflow has no '__iter__' function")
    
    def __len__(self):
        raise NotImplementedError("This Workflow has no '__len__' function")
