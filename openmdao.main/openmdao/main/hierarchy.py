
#public symbols
__all__ = ["HierarchyMember"]

__version__ = "0.1"


import weakref

from openmdao.main.logger import logger

class HierarchyMember(object):
    """Base class for all objects living in the framework accessible
    hierarchy of named objects.

    """
    def __init__(self, name, parent=None, desc=None):
        self.name = name
        if parent is None:
            self.__parent = None
        else:
            self.__parent = weakref.ref(parent)
        if desc is not None:
            self.__doc__ = desc

    def get(self, path, index=None):
        raise NotImplementedError('get') 
    
    def set(self, path, value, index=None):
        raise NotImplementedError('set') 
    
    
    def get_pathname(self):
        """ Return full path name to this container. """
        if self.parent is None:
            return self.name
        else:
            return '.'.join([self.parent.get_pathname(), self.name])

    
    def _get_parent(self):
        if self.__parent is None:
            return None
        else:
            return self.__parent() # need parens because self.parent is a weakref
        
    def _set_parent(self, parent):
        if parent is None:
            self.__parent = None
        else:
            self.__parent = weakref.ref(parent)
           
    parent = property(_get_parent,_set_parent)
    
    # error reporting stuff
    def raise_exception(self, msg, exception_class=Exception):
        full_msg = self.get_pathname()+': '+msg
        logger.error(full_msg)
        raise exception_class(full_msg)
    
    def error(self, msg):
        logger.error(self.get_pathname()+': '+msg)
        
    def warning(self, msg):
        logger.warn(self.get_pathname()+': '+msg)
        
    def info(self, msg):
        logger.info(self.get_pathname()+': '+msg)
        
    def debug(self, msg):
        logger.debug(self.get_pathname()+': '+msg)
        
