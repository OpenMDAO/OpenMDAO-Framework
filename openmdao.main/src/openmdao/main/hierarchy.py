#public symbols
__all__ = ["HierarchyMember"]

__version__ = "0.1"

import traceback
import weakref
import re

from openmdao.main.log import Logger, LOG_DEBUG

# regex to check for valid names.  Added '.' as allowed because
# npsscomponent uses it...
_namecheck_rgx = re.compile('([_a-zA-Z][_a-zA-Z0-9]*)+(\.[_a-zA-Z][_a-zA-Z0-9]*)*')


class IMHolder(object):
    """Holds an instancemethod object in a pickleable form."""

    def __init__(self, obj):
        self.name = obj.__name__
        self.im_self = obj.im_self
        if obj.im_self:
            self.im_class = None  # Avoid possible __main__ issues.
        else:
            # TODO: handle __main__ for im_class.__module__.
            self.im_class = obj.im_class

    def method(self):
        """Return instancemethod corresponding to saved state."""
        if self.im_self:
            return getattr(self.im_self, self.name)
        else:
            return getattr(self.im_class, self.name)


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

        if name is not None:
            m = _namecheck_rgx.search(name)
            if m is None or m.group() != name:
                self.raise_exception("name '%s' contains illegal characters" %
                                     name, NameError)
            
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
        """Return dict representing this container's state."""
        state = self.__dict__.copy()
        if state['_parent'] is not None:
            # remove weakref to parent because it won't pickle
            state['_parent'] = self._parent() 

        # Put instancemethod objects in a pickleable form (only one level deep).
        for key, val in state.items():
            if val.__class__.__name__ == 'instancemethod':
                state[key] = IMHolder(val)
            elif isinstance(val, dict):
                for key2, obj in val.items():
                    if obj.__class__.__name__ == 'instancemethod':
                        state[key][key2] = IMHolder(obj)
            else:
                if isinstance(val, list) or isinstance(val, set):
                    for i, obj in enumerate(val):
                        if obj.__class__.__name__ == 'instancemethod':
                            state[key][i] = IMHolder(obj)
        return state
    
    def __setstate__(self, state):
        """Restore this component's state."""
        self.__dict__ = state
        if self._parent is not None:
            self._parent = weakref.ref(self._parent)

        # Restore instancemethod objects (only one level deep).
        for key, val in self.__dict__.items():
            if isinstance(val, IMHolder):
                self.__dict__[key] = val.method()
            elif isinstance(val, dict):
                for key2, obj in val.items():
                    if isinstance(obj, IMHolder):
                        self.__dict__[key][key2] = obj.method()
            else:
                if isinstance(val, list) or isinstance(val, set):
                    for i, obj in enumerate(val):
                        if isinstance(obj, IMHolder):
                            self.__dict__[key][i] = obj.method()

    # error reporting stuff
    def _get_log_level(self):
        """Return logging message level."""
        return self._logger.level

    def _set_log_level(self, level):
        """Set logging message level."""
        self._logger.level = level

    log_level = property(_get_log_level, _set_log_level,
                         doc='Logging message level.')

    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        full_msg = '%s: %s' % (self.get_pathname(), msg)
#        self._logger.error(msg)
        raise exception_class(full_msg)
    
    def exception(self, msg, *args, **kwargs):
        """Log traceback from within exception handler."""
        self._logger.critical(msg, *args, **kwargs)
        self._logger.critical(traceback.format_exc())

    def error(self, msg, *args, **kwargs):
        """Record an error message."""
        self._logger.error(msg, *args, **kwargs)
        
    def warning(self, msg, *args, **kwargs):
        """Record a warning message."""
        self._logger.warning(msg, *args, **kwargs)
        
    def info(self, msg, *args, **kwargs):
        """Record an informational message."""
        self._logger.info(msg, *args, **kwargs)
        
    def debug(self, msg, *args, **kwargs):
        """Record a debug message."""
        self._logger.debug(msg, *args, **kwargs)

