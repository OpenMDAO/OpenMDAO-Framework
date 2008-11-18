
#public symbols
__all__ = ["HierarchyMember"]

__version__ = "0.1"


import weakref

class HierarchyMember(object):
    """Base class for all objects living in the framework accessible
    hierarchy of named objects.

    """
    def __init__(self, name, parent=None, desc=None):
        self.name = name
        self._parent = parent
        if desc is not None:
            self.__doc__ = desc

    def get(self, path):
        """Return the framework-accessible object specified by the given 
        path, which may contain '.' characters.
        
        """
        raise NotImplementedError('get() is not implemented') 
    
    
    def get_pathname(self):
        """ Return full path name to this container. """
        if self._parent is None:
            return self.name
        else:
            return '.'.join([self._parent.get_pathname(), self.name])

    
    def _get_parent(self):
        if self.__parent is None:
            return None
        else:
            return self.__parent() # need parens because self.__parent is a weakref
        
    def _set_parent(self, parent):
        if parent is None:
            self.__parent = None
        else:
            self.__parent = weakref.ref(parent)
           
    _parent = property(_get_parent,_set_parent)
    
    
