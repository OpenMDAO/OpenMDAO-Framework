
from enthought.traits.api import implements

from openmdao.main.workflow import Workflow
from openmdao.main.interfaces import IComponent
from openmdao.main.exceptions import RunStopped
from openmdao.main.mp_support import has_interface

__all__ = ['SequentialWorkflow']

class SequentialWorkflow(Workflow):
    """A Workflow that is a simple sequence of components."""
    
    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        self._nodes = []
        super(SequentialWorkflow, self).__init__(parent, scope, members)
        
    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        scope = self.scope
        for name in self._nodes:
            yield getattr(scope, name)
    
    def __len__(self):
        return len(self._nodes)
    
    def __contains__(self, comp):
        return comp in self._nodes
    
    def contents(self):
        """Returns a list of all component objects in the workflow."""
        scope = self.scope
        return [getattr(scope, name) for name in self._nodes]

    def add(self, compnames):
        """ Add new component(s) to the end of the workflow by name. """
        if isinstance(compnames, list) or isinstance(compnames, tuple):
            nodes = compnames
        elif isinstance(compnames, basestring):
            nodes = [compnames]
        else:
            raise TypeError("Components must be added by name to a workflow.")
        for node in nodes:
            if isinstance(node, basestring):
                self._nodes.append(node)
            else:
                raise TypeError("Components must be added by name to a workflow.")
        
    def remove(self, compname):
        """Remove a component from the workflow by name. Do not report an
        error if the specified component is not found.
        """
        if not isinstance(compname, basestring):
            raise TypeError("Components must be removed by name from a workflow.")
        try:
            self._nodes.remove(compname)
        except ValueError:
            pass

    def clear(self):
        """Remove all components from this workflow."""
        self._nodes = []
