
from enthought.traits.api import implements

from openmdao.main.workflow import Workflow
from openmdao.main.interfaces import IComponent, obj_has_interface
from openmdao.main.exceptions import RunStopped

__all__ = ['SequentialWorkflow']

class SequentialWorkflow(Workflow):
    """A Workflow that is a simple sequence of components."""
    
    def __init__(self, members=None):
        """ Create an empty flow. """
        self._nodes = []
        super(SequentialWorkflow, self).__init__(members)
        
    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return self._nodes.__iter__()
    
    def __len__(self):
        return len(self._nodes)
    
    def __contains__(self, comp):
        return comp in self._nodes
    
    def contents(self):
        """Returns a list of all Components in the workflow."""
        return self._nodes[:]

    def add(self, comp):
        """ Add new component(s) to the end of the workflow. """
        if isinstance(comp, list) or isinstance(comp, tuple):
            nodes = comp
        elif obj_has_interface(comp, IComponent):
            nodes = [comp]
        else:
            raise TypeError("adding a non-Component to a workflow (%s)" %
                            type(comp))
        for node in nodes:
            if obj_has_interface(node, IComponent):
                self._nodes.append(node)
            else:
                raise TypeError("Cannot add object of type %s to a workflow" % type(node))
        
    def remove(self, comp):
        """Remove a component from the workflow. Do not report an
        error if the specified component is not found.
        """
        self._nodes = [x for x in self._nodes if x is not comp]

    def clear(self):
        """Remove all components from this workflow."""
        self._nodes = []