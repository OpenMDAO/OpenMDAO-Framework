""" A workflow that allows the user to explicitly specify the execution
order. """

from openmdao.main.workflow import Workflow
from openmdao.main.interfaces import implements, IComponent
from openmdao.main.exceptions import RunStopped
from openmdao.main.mp_support import has_interface

__all__ = ['SequentialWorkflow']

class SequentialWorkflow(Workflow):
    """A Workflow that is a simple sequence of components."""
    
    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        self._names = []
        super(SequentialWorkflow, self).__init__(parent, scope, members)
        
    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return iter(self.get_components())
    
    def __len__(self):
        return len(self._names)
    
    def __contains__(self, comp):
        return comp in self._names
    
    def index(self, comp):
        return self._names.index(comp)

    def __eq__(self, other):
        return type(self) is type(other) and self._names == other._names
    
    def __ne__(self, other):
        return not self.__eq__(other)

    def get_names(self):
        """Return a list of component names in this workflow."""
        return self._names[:]
    
    def add(self, compnames, index=None, check=False):
        """ Add new component(s) to the end of the workflow by name. """
        if isinstance(compnames, basestring):
            nodes = [compnames]
        else:
            nodes = compnames
        try:
            nodeit = iter(nodes)
        except TypeError:
            raise TypeError("Components must be added by name to a workflow.")
        
        for node in nodes:
            if isinstance(node, basestring):
                
                # If we are calling from the GUI, we need to check whether 
                # each node is valid so that we can send a useful error
                # message.
                if check:
                    
                    name = self._parent.parent.name
                    if not name:
                        name = "the top assembly."
                        
                    # Components in subassys are never allowed.
                    if '.' in node:
                        msg = "Component '%s' is not" % node + \
                              " in the scope of %s" % name
                        raise AttributeError(msg)
                        
                    # Does the component really exist?
                    try:
                        target = self._parent.parent.get(node)
                    except AttributeError:
                        msg = "Component '%s'" % node + \
                              " does not exist in %s" % name
                        raise AttributeError(msg)
                    
                    # Don't add yourself to your own workflow
                    if target == self._parent:
                        msg = "You cannot add a driver to its own workflow"
                        raise AttributeError(msg)
                    
                    # Check for circular dependency in driver workflow
                    if hasattr(target, 'iteration_set'):
                        iterset = target.iteration_set()
                        if self._parent in iterset:
                            msg = "Driver recursion loop detected"
                            raise AttributeError(msg)
                    
                if index is None:
                    self._names.append(node)
                else:
                    self._names.insert(index, node)
                    index += 1
            else:
                raise TypeError("Components must be added by name to a workflow.")
            
        # We seem to need this so that our get_attributes is correct for the GUI.
        if check:
            self.config_changed()
        
    def remove(self, compname):
        """Remove a component from the workflow by name. Do not report an
        error if the specified component is not found.
        """
        if not isinstance(compname, basestring):
            raise TypeError("Components must be removed by name from a workflow.")
        try:
            self._names.remove(compname)
        except ValueError:
            pass

    def clear(self):
        """Remove all components from this workflow."""
        self._names = []
