
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
    
    where pathname is a dot separated name, and index is a tuple of element
    indices, e.g., (2,1) or ('mykey',).
    """
    def __init__(self, root, path):
        object.__setattr__(self, '_root', weakref.ref(root))
        if path:
            object.__setattr__(self, '_path', path+'.')  # from root, the pathname of the object this proxy refers to
        else:
            object.__setattr__(self, '_path', '')
        object.__setattr__(self, '_subproxies', {})  # reuse proxies that refer to the same objects
        object.__setattr__(self, '_internal', set())  # set of attributes that are part of the proxy itself
        self._internal.update(self.__dict__.keys())
        
    def __getattr__(self, name):
        """If getattr fails, this function is called."""
        path = self._path + name
        proxy = self._subproxies.get(name)
        if proxy:
            return proxy
        try:
            return self._root().get(path)
        except (AttributeError, KeyError):
            if path in self._root():
                proxy = TreeProxy(self._root(), path)
                self._subproxies[name] = proxy
                return proxy
            else:
                raise AttributeError("'%s' not found" % path)
    
    def __setattr__(self, name, val):
        """This is always called whenever someone tries to set an attribute on this
        proxy.
        """
        try:
            self._root().set(self._path + name, val)
        except AttributeError as err:
            if name in self._internal:
                object.__setattr__(self, name, val)
            else:
                raise err

    def __contains__(self, name):
        return self._root().__contains__(self._path + name)
    
    def __getitem__(self, key):
        return self._root().get(self._path, index=(key,))
    
    def __call__(self, *args, **kwargs):
        return self._root().call(self._path[:-1], *args, **kwargs)
    
    def set(self, name, value, index=None, src=None, force=False):
        self._root.set(self._path + name, value)


def all_tree_names(pathnames):
    """Returns the set of all names, including intermediate names,
    given a list of pathnames. For example, given the pathname 'a.b.c',
    it would return set(['a', 'a.b', 'a.b.c'])
    """
    allnames = set()
    for key in pathnames:
        parts = key.split('.')
        for i in range(len(parts)):
            path = '.'.join(parts[:i+1])
            allnames.add(path)
    return allnames
