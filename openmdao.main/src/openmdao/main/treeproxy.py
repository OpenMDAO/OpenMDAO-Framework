
import weakref

class TreeProxy(object):
    """Acts as a proxy object for objects within an opaque, hierarchically
    structured local object so that normal python attribute access will work
    to access objects inside of the opaque object's tree.
    
    The opaque object must implement the following interface which is checked
    only via duck typing:
    
    ::
    
        def get(pathname, index=None)
        def set(pathname, value, index=None)
        def call(pathname, *args, **kwargs)
        def __contains__(pathname)
    
    where pathname is a dot separated name, and index is a list of element
    indices or attribute names, e.g., [2,1], ['mykey'] or [2,1,['attrname']].
    Attribute names are buried inside of a nested list to avoid confusion
    with strings used to index into a container.  Also, nested tuples are not
    used to avoid confusion for the same reason, i.e. they are hashable and
    therefore could be used to index into a container.
    """
    def __init__(self, root, path):
        object.__setattr__(self, '_root', weakref.ref(root))
        if path:
            object.__setattr__(self, '_path', path+'.')  # from root, the pathname of the object this proxy refers to
        else:
            object.__setattr__(self, '_path', '')
        object.__setattr__(self, '_internal', set())  # set of attributes that are part of the proxy itself
        self._internal.update(self.__dict__.keys())
        
    def __getattr__(self, name):
        """If getattr fails, this function is called."""
        path = self._path + name
        try:
            return self._root().get(path)
        except (AttributeError, KeyError):
            if path in self._root():
                proxy = TreeProxy(self._root(), path)
                object.__setattr__(self, name, proxy)
                return proxy
            else:
                raise AttributeError("'%s' not found" % path)
    
    def __setattr__(self, name, val):
        """This is always called whenever someone tries to set an attribute on this
        proxy.
        """
        if name in self._internal:
            object.__setattr__(self, name, val)
        else:
            self._root().set(self._path + name, val)

    def __contains__(self, name):
        return self._root().__contains__(self._path + name)
    
    def __getitem__(self, key):
        return self._root().get(self._path, index=(key,))
    
    def __setitem__(self, key, value):
        return self._root().set(self._path, value, index=(key,))
    
    def __call__(self, *args, **kwargs):
        return self._root().call(self._path[:-1], *args, **kwargs)


def all_tree_names(pathnames):
    """Returns the set of all names, including intermediate names,
    given a list of pathnames. For example, given the pathname 'a.b.c',
    it would return set(['a', 'a.b', 'a.b.c'])
    """
    allnames = set()
    for key in pathnames:
        parts = key.split('.')
        allnames.update(['.'.join(parts[:i+1]) for i in range(len(parts))])
    return allnames
