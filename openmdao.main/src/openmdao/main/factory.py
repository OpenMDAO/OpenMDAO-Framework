
#public symbols
__all__ = ["Factory"]



    
class Factory(object):
    """Base class for objects that know how to create other objects
    based on a type argument and several optional arguments (version,
    server id, resource description, and name).
    """
    def __init__(self):
        pass
        
    def create(self, typname, version=None, server=None, 
               res_desc=None, **ctor_args):
        """Return an object of type *typename,* using the specified
        package version, server location, and resource description.
        
        """
        raise NotImplementedError('create')
    
    def get_available_types(self, groups=None):
        """Return a set of tuples of the form (typename, dist_version), one
        for each available plugin type in the given entry point groups.
        If groups is *None,* return the set for all openmdao entry point groups.
        """
        raise NotImplementedError('get_available_types')
