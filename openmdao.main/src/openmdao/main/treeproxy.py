
import weakref

class TreeProxy(object):
    """Acts as a proxy object for objects within an opaque, hierarchically
    structured local object so that normal python attribute access will work
    to access objects inside of the opaque object's tree.
    
    The opaque object must implement the IOpaqueTree interface.
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
        return self._root().contains(self._path + name)
    
    def __getitem__(self, key):
        raise NotImplemented('__getitem__')
    
    def __call__(self, *args, **kwargs):
        raise NotImplemented('__call__')
    
    def set(self, name, value, index=None, src=None, force=False):
        self._root.set(self._path + name, value)

