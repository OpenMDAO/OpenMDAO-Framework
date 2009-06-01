#public symbols
__all__ = ['Socket']
__version__ = "0.1"


class Socket(object):
    """A descriptor that checks the interface of a given child. Filling of a 
    Socket can be required or optional.
    """
    def __init__(self, iface, doc='', required=True):
        self.name = ''
        self.iface = iface
        self.doc = doc
        self.required = required
    
    def __get__(self, instance, klass):
        if instance is None:
            return self  # allow direct access to Socket object throught owner class
        else:
            try:
                return instance._socket_objs[self.name][0]
            except KeyError:
                instance.raise_exception("socket '%s' is empty" % self.name,
                                         RuntimeError)
    
    def __set__(self, instance, plugin):
        if plugin is None:
            del instance._socket_objs[self.name]
        else:
            if self.iface is not None:
                if not self.iface.providedBy(plugin):
                    instance.raise_exception(
                        "Socket '%s' requires interface '%s'" % \
                        (self.name, self.iface.__name__), ValueError)
            instance._socket_objs[self.name] = (plugin, self)
    
