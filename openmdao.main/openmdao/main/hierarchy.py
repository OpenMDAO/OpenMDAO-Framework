
#public symbols
__all__ = ["HierarchyMember"]

__version__ = "0.1"


class HierarchyMember(object):
    """Base class for all objects living in the framework accessible
    hierarchy of named objects.

    """
    def __init__(self, name, parent=None):
        self.name = name
        self._parent = parent


    def get(self, path):
        """Return the framework-accessible object specified by the given 
        path, which may contain '.' characters.
        
        """
        raise NotImplementedError('get() is not implemented') 
    
    
    def get_pathname(self):
        """ Return full path name to this container. """
        if self._parent is not None:
            return '.'.join([self._parent.get_pathname(), self.name])
        else:
            return self.name

