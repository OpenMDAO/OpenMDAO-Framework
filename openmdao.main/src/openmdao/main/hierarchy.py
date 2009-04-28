#public symbols
__all__ = ["HierarchyMember"]

__version__ = "0.1"


import traceback
import weakref
from openmdao.main.log import Logger, LOG_DEBUG

class HierarchyMember(object):
    """Base class for all objects living in the framework accessible
    hierarchy of named objects.

    """
    def __init__(self, name, parent=None, doc=None):
        self.name = name
        if parent is None:
            self._parent = None
        else:
            self._parent = weakref.ref(parent)
        
        if doc is not None:
            self.__doc__ = doc
        
        # Replace pathname to keep loggers from interfering with each other.
        self._logger = Logger(self.get_pathname().replace('.', ','))
        self.log_level = LOG_DEBUG

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
    
    def get_pathname(self, rel_to_scope=None):
        """ Return full path name to this container, relative to scope
        rel_to_scope. If rel_to_scope is None, return the full pathname.
        """
        path = []
        obj = self
        while obj is not None and obj != rel_to_scope and obj.name is not None:
            path.append(obj.name)
            obj = obj.parent
        return '.'.join(path[::-1])
    
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
    def _get_log_level(self):
        """ Return logging message level. """
        return self._logger.level

    def _set_log_level(self, level):
        """ Set logging message level. """
        self._logger.level = level

    log_level = property(_get_log_level, _set_log_level,
                         doc='Logging message level.')

    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception"""
        full_msg = '%s: %s' % (self.get_pathname(), msg)
#        self._logger.error(msg)
        raise exception_class(full_msg)
    
    def exception(self, msg, *args, **kwargs):
        """Log traceback from within exception handler."""
        self._logger.critical(msg, *args, **kwargs)
        self._logger.critical(traceback.format_exc())

    def error(self, msg, *args, **kwargs):
        """Record an error message"""
        self._logger.error(msg, *args, **kwargs)
        
    def warning(self, msg, *args, **kwargs):
        """Record a warning message"""
        self._logger.warning(msg, *args, **kwargs)
        
    def info(self, msg, *args, **kwargs):
        """Record an informational message"""
        self._logger.info(msg, *args, **kwargs)
        
    def debug(self, msg, *args, **kwargs):
        """Record a debug message"""
        self._logger.debug(msg, *args, **kwargs)

