"""
An object that knows how to create a particular component --NEW COMMENT
"""

#public symbols
#__all__ = []

__version__ = "0.1"

    
class Factory(object):
    """Base class for objects that know how to create other objects
    based on a type argument and several optional arguments (version,
    server id, resource description, and name.
    """
    def __init__(self):
        pass
        
    def create(self, typname, name=None, version=None, server=None, 
               res_desc=None):
        """Return the object created by calling self.ctor. If necessary,
        first activate the distribution and load the entry point, and
        check for conflicting version dependencies before loading.
        """
        raise NotImplementedError('create')
                
                
                    
