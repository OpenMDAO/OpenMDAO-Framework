"""
An object that knows how to create a particular component
"""

#public symbols
#__all__ = []

__version__ = "0.1"

    
class Factory(object):
    def __init__(self):
        pass
        
    def create(self, typname, name=None, version=None, server=None, 
               res_desc=None):
        """Return the object created by calling self.ctor. If necessary,
        first activate the distribution and load the entry point, and
        check for conflicting version dependencies before loading.
        """
        raise NotImplementedError('create')
                
                
                    