
from enthought.traits.api import implements

from openmdao.main.workflow import Workflow
from openmdao.main.interfaces import IComponent
from openmdao.main.exceptions import RunStopped
from openmdao.main.mp_support import has_interface
from openmdao.main.configinfo import ConfigInfo

__all__ = ['SequentialWorkflow']

class SequentialWorkflow(Workflow):
    """A Workflow that is a simple sequence of components."""
    
    def __init__(self, parent=None, scope=None, members=None):
        """ Create an empty flow. """
        self._names = []
        super(SequentialWorkflow, self).__init__(parent, scope, members)
        
    def __iter__(self):
        """Returns an iterator over the components in the workflow."""
        return self.get_components().__iter__()
    
    def __len__(self):
        return len(self._names)
    
    def __contains__(self, comp):
        return comp in self._names
    
    def get_configinfo(self, pathname):
        """Return a ConfigInfo object for this instance.  The
        ConfigInfo object should also contain ConfigInfo objects
        for children of this object.
        """
        info = ConfigInfo(self, pathname)
        for name in self._names:
            info.cmds.append("%s.add('%s')" % (pathname, name))
        return info
        
    def __eq__(self, other):
        return type(self) is type(other) and self._names == other._names
    
    def __ne__(self, other):
        return not self.__eq__(other)

    def get_names(self):
        """Return a list of component names in this workflow."""
        return self._names[:]
    
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
                self._names.append(node)
            else:
                raise TypeError("Components must be added by name to a workflow.")
        
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
