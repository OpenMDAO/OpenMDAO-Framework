#public symbols
__all__ = ["HierarchyMember"]

__version__ = "0.1"


import weakref
from openmdao.main.log import logger

class HierarchyMember(object):
    """Base class for all objects living in the framework accessible
    hierarchy of named objects.

    """
    def __init__(self, name, parent=None, desc=None):
        self.name = name
        if parent is None:
            self._parent = None
        else:
            self._parent = weakref.ref(parent)
        if desc is not None:
            self.__doc__ = desc

    def get(self, path, index=None):
        """Get a child object using a dotted path name. 
        (not implemented)
        """
        raise NotImplementedError('get') 
    
    def set(self, path, value, index=None):
        """Set the value of a child specified using a dotted path name.
        (not implemented)
        """
        raise NotImplementedError('set') 
    
    def get_pathname(self):
        """ Return full path name to this container. """
        if self.parent is None:
            return self.name
        else:
            try:
                path = self.parent.get_pathname()
            except AttributeError:
                return self.name
            else:
                return '.'.join([path, self.name])

    def _get_parent(self):
        if self._parent is None:
            return None
        else:
            return self._parent() # need parens since self.parent is a weakref
        
    def _set_parent(self, parent):
        if parent is None:
            self._parent = None
        else:
            self._parent = weakref.ref(parent)
           
    parent = property(_get_parent, _set_parent)
    
    def __getstate__(self):
        """ Return dict representing this container's state. """
        state = self.__dict__.copy()
        if state['_parent'] is not None:
            # remove weakref to parent because it won't pickle
            state['_parent'] = self._parent() 
        return state
    
    def __setstate__(self, state):
        """ Restore this component's state. """
        if state['_parent'] is not None:
            state['_parent'] = weakref.ref(state['_parent'])
        self.__dict__ = state
    
    # error reporting stuff
    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception"""
        full_msg = self.get_pathname()+': '+msg
#        logger.error(full_msg)
        raise exception_class(full_msg)
    
    def error(self, msg, *args, **kwargs):
        """Record an error message"""
        logger.error(self.get_pathname()+': '+msg, *args, **kwargs)
        
    def warning(self, msg, *args, **kwargs):
        """Record a warning message"""
        logger.warn(self.get_pathname()+': '+msg, *args, **kwargs)
        
    def info(self, msg, *args, **kwargs):
        """Record an informational message"""
        logger.info(self.get_pathname()+': '+msg, *args, **kwargs)
        
    def debug(self, msg, *args, **kwargs):
        """Record a debug message"""
        logger.debug(self.get_pathname()+': '+msg, *args, **kwargs)

