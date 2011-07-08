"""
Proxies for AnalysisServer components and variables.
"""

import os.path
import socket

from enthought.traits.api import TraitError

from openmdao.main.api import Component, Container, FileRef
from openmdao.main.mp_support import is_instance
from openmdao.lib.datatypes.api import Array, Bool, Enum, File, Float, Int, Str

from client import Client
from units  import get_translation

def _float2str(val):
    """ Return accurate string value for float. """
    return '%.16g' % val


class ComponentProxy(Component):
    """
    Proxy for a component running under an AnalysisServer.
    `typname` and `objname` are the component type and instance names on
    the server located at `host` and `port`.

    The proxy will populate itself with proxies for all variables exposed
    by the remote component, in an identical hierarchy.

    .. note::
        While variable attributes like 'units' are read from the remote
        component, attempts to set attributes other than 'value' from the local
        side will have no effect on the remote side.
    """

    def __init__(self, typname, host, port):
        self._typname = typname
        self._objname = 'the_obj'
        self._host = host
        self._port = port

        self._client = Client(host, port)
        self._client.start(typname, self._objname)
        super(ComponentProxy, self).__init__()
        self._populate(self, self._objname)

    def _populate(self, container, path):
        """ Populate a container from a remote object. """
# TODO: local/remote name collision detection/resolution?
        info = self._client.list_properties(path)
        for prop, typ, iotype in info:
            rpath = '.'.join([path, prop])
            if typ == 'PHXDouble' or typ == 'PHXLong' or typ == 'PHXString':
                enum_valstrs = self._client.get(rpath+'.enumValues')
                if enum_valstrs:
                    container.add_trait(prop,
                                        EnumProxy(iotype, self._client, rpath,
                                                  typ, enum_valstrs))
                    continue

            if typ == 'PHXBoolean':
                container.add_trait(prop,
                                    BoolProxy(iotype, self._client, rpath))
            elif typ == 'PHXDouble':
                container.add_trait(prop,
                                    FloatProxy(iotype, self._client, rpath))
            elif typ == 'PHXLong':
                container.add_trait(prop,
                                    IntProxy(iotype, self._client, rpath))
            elif typ == 'PHXRawFile':
                container.add_trait(prop,
                                    FileProxy(iotype, self._client, rpath,
                                              self))
            elif typ == 'PHXString':
                container.add_trait(prop,
                                    StrProxy(iotype, self._client, rpath))
            elif typ == 'PHXGroup':
                group = container.add(prop, Container())
                self._populate(group, rpath)  # Recurse.

            elif typ.startswith('double['):
                container.add_trait(prop,
                                    ArrayProxy(iotype, self._client, rpath,
                                               float))
            elif typ.startswith('long['):
                container.add_trait(prop,
                                    ArrayProxy(iotype, self._client, rpath,
                                               int))
            elif typ.startswith('java.lang.String['):
                container.add_trait(prop,
                                    ArrayProxy(iotype, self._client, rpath,
                                               str))
            else:
                raise NotImplementedError('%r type %r' % (prop, typ))

    def __getstate__(self):
        """ Return dict representing this component's local state. """
        state = super(ComponentProxy, self).__getstate__()
        del state['_client']
        return state

    def __setstate__(self, state):
        """ Restore this component's local state. """
        state['_client'] = None
        super(ComponentProxy, self).__setstate__(state)

    def post_load(self):
        """
        Restore client connection and remote state after loading local state.
        """
        super(ComponentProxy, self).post_load()
        self._client = Client(self._host, self._port)
        self._client.start(self._typname, self._objname)
        self._restore(self)

    def _restore(self, container):
        """ Restore remote state (variables don't have a post_load()). """
        # Using _alltraits() here because at this point items()
        # considers the ProxyMixin traits as 'Missing'.
        for name, trait in container._alltraits().items():
            typ = trait.trait_type
            if isinstance(typ, ProxyMixin):
                typ.restore(self._client)

        for name, obj in container.items():
            if is_instance(obj, Container):
                self._restore(obj)  # Recurse.

    def pre_delete(self):
        """ Unload remote instance before we get deleted. """
        super(ComponentProxy, self).pre_delete()
        if self._client is not None:
            self._client.end(self._objname)
            self._client = None

    def __del__(self):
        """ Cleanup client if it hasn't been already. """
        if self._client is not None:
            try:
                self._client.end(self._objname)
            except (EOFError, socket.error):  # pragma no cover
                pass
            self._client = None

    def execute(self):
        """ Execute remote component. """
        self._client.execute(self._objname)


class ProxyMixin(object):
    """
    Common methods for variable proxies.
    The remote variable is accessed via `client` and `rpath`.
    """

    def __init__(self, client, rpath):
        self._client = client
        self._rpath = rpath
        self._valstr = client.get(rpath)  # Needed for later restore.

    def __getstate__(self):
        """ Return dict representing this proxy's state. """
        state = self.__dict__.copy()
        del state['_client']
        return state

    def __setstate__(self, state):
        """ Restore this proxy's state. """
        state['_client'] = None
        self.__dict__ = state

    def restore(self, client):
        """ Restore remote state. """
        self._client = client
        if self.iotype == 'in':
            self._client.set(self._rpath, self._valstr)

    def rget(self):
        """ Get remote value as a string. """
        return self._client.get(self._rpath)

    def rset(self, valstr):
        """ Set remote value from `valstr`. """
        if self.iotype == 'out':
            raise TraitError("Can't set an output")
        self._client.set(self._rpath, valstr)
        self._valstr = valstr


class ArrayProxy(ProxyMixin, Array):
    """ Proxy for a remote double, long, or String array. """

    def __init__(self, iotype, client, rpath, typ):
        ProxyMixin.__init__(self, client, rpath)
        self._type = typ

        default = [typ(val.strip(' "')) for val in self._valstr.split(',')]
        desc = client.get(rpath+'.description')

        if typ == float:
            as_units = client.get(rpath+'.units')
            if as_units:
                om_units = get_translation(as_units)
            else:
                om_units = None

        if typ != str:
            if client.get(rpath+'.hasUpperBound') == 'true':
                high = typ(client.get(rpath+'.upperBound'))
            else:
                high = None
            if client.get(rpath+'.hasLowerBound') == 'true':
                low = typ(client.get(rpath+'.lowerBound'))
            else:
                low = None

        if typ == float:
            Array.__init__(self, dtype=typ, iotype=iotype, desc=desc,
                           default_value=default, low=low, high=high,
                           units=om_units)
        elif typ == int:
            Array.__init__(self, dtype=typ, iotype=iotype, desc=desc,
                           default_value=default, low=low, high=high)
        else:
            Array.__init__(self, dtype=typ, iotype=iotype, desc=desc,
                           default_value=default)

    def get(self, obj, name):
        """ Get remote value. """
        return [self._type(val.strip(' "')) for val in self.rget().split(',')]

    def set(self, obj, name, value):
        """ Set remote value. """
        if self._type == float:
            valstr = ', '.join([_float2str(val) for val in value])
        else:
            valstr = ', '.join([str(val) for val in value])
        self.rset(valstr)


class BoolProxy(ProxyMixin, Bool):
    """ Proxy for a remote ``PHXBoolean`` at `rpath` accessed via `client`. """

    def __init__(self, iotype, client, rpath):
        ProxyMixin.__init__(self, client, rpath)

        default = self._valstr == 'true'
        desc = client.get(rpath+'.description')

        Bool.__init__(self, default_value=default, iotype=iotype, desc=desc)

    def get(self, obj, name):
        """ Get remote value. """
        return self.rget() == 'true'

    def set(self, obj, name, value):
        """ Set remote value. """
        self.rset('true' if value else 'false')


class EnumProxy(ProxyMixin, Enum):
    """
    Proxy for a remote ``PHXDouble``, ``PHXLong``, or ``PHXString`` which has
    a non-empty ``enumValues`` property.
    The remote variable is at `rpath` accessed via `client`.
    """

    def __init__(self, iotype, client, rpath, typ, valstrs):
        ProxyMixin.__init__(self, client, rpath)

        om_units = None
        if typ == 'PHXDouble':
            self._from_string = float
            self._to_string = _float2str
            as_units = client.get(rpath+'.units')
            if as_units:
                om_units = get_translation(as_units)
        elif typ == 'PHXLong':
            self._from_string = int
            self._to_string = str
        elif typ == 'PHXString':
            self._from_string = self._null
            self._to_string = self._null
        else:
            raise NotImplementedError('EnumProxy for %r' % typ)

        default = self._from_string(self._valstr)
        desc = client.get(rpath+'.description')

        enum_values = []
        for valstr in valstrs.split(','):
            enum_values.append(self._from_string(valstr.strip(' "')))

        enum_aliases = []
        aliases = self._client.get(rpath+'.enumAliases')
        if aliases:
            for alias in aliases.split(','):
                enum_aliases.append(alias.strip(' "'))
        if enum_aliases:
            if len(enum_aliases) != len(enum_values):
                raise ValueError("Aliases %r don't match values %r"
                                 % (enum_aliases, enum_values))

        Enum.__init__(self, default_value=default, iotype=iotype, desc=desc,
                      values=enum_values, aliases=enum_aliases, units=om_units)

    def get(self, obj, name):
        """ Get remote value. """
        return self._from_string(self.rget())

    def set(self, obj, name, value):
        """ Set remote value. """
        self.rset(self._to_string(value))

    def _null(self, val):
        """ Just returns `val` unmodified. """
        return val


class FileProxy(ProxyMixin, File):
    """ Proxy for a remote ``PHXRawFile`` at `rpath` accessed via `client`. """

    def __init__(self, iotype, client, rpath, component):
        ProxyMixin.__init__(self, client, rpath)
        self._component = component
        self._path = 'AS-%s.dat' % rpath

        desc = client.get(rpath+'.description')
        metadata = {}
        if iotype == 'out':
            metadata['path'] = self._path

        File.__init__(self, default_value=None, iotype=iotype, desc=desc,
                      **metadata)

    def get(self, obj, name):
        """ Get remote value and write to local file. """
        if self._component._call_tree_rooted:
            return None  # Not initialized.

        binary = self._client.get(self._rpath+'.isBinary') == 'true'
        valstr = self.rget()
        mode = 'wb' if binary else 'w'
        with self._component.dir_context:
            with open(self._path, mode) as out:
                out.write(valstr)
        return FileRef(self._path, self._component, binary=binary)

    def set(self, obj, name, value):
        """ Set remote value. """
        # `value` is a FileRef.
        with value.open() as inp:
            valstr = inp.read()
#        binary = 'true' if value.binary else 'false'
#        self._client.set(self._rpath+'.isBinary', binary)
        self.rset(valstr)


class FloatProxy(ProxyMixin, Float):
    """ Proxy for a remote ``PHXDouble`` at `rpath` accessed via `client`. """

    def __init__(self, iotype, client, rpath):
        ProxyMixin.__init__(self, client, rpath)

        default = float(self._valstr)
        desc = client.get(rpath+'.description')
        as_units = client.get(rpath+'.units')
        if as_units:
            om_units = get_translation(as_units)
        else:
            om_units = None
        if client.get(rpath+'.hasUpperBound') == 'true':
            high = float(client.get(rpath+'.upperBound'))
        else:
            high = None
        if client.get(rpath+'.hasLowerBound') == 'true':
            low = float(client.get(rpath+'.lowerBound'))
        else:
            low = None

        Float.__init__(self, default_value=default, iotype=iotype, desc=desc,
                       low=low, high=high, units=om_units)

    def get(self, obj, name):
        """ Get remote value. """
        return float(self.rget())

    def set(self, obj, name, value):
        """ Set remote value. """
        self.rset(_float2str(value))


class IntProxy(ProxyMixin, Int):
    """ Proxy for a remote ``PHXLong`` at `rpath` accessed via `client`. """

    def __init__(self, iotype, client, rpath):
        ProxyMixin.__init__(self, client, rpath)

        default = int(self._valstr)
        desc = client.get(rpath+'.description')
        if client.get(rpath+'.hasUpperBound') == 'true':
            high = int(client.get(rpath+'.upperBound'))
        else:
            high = None
        if client.get(rpath+'.hasLowerBound') == 'true':
            low = int(client.get(rpath+'.lowerBound'))
        else:
            low = None

        Int.__init__(self, default_value=default, iotype=iotype, desc=desc,
                     low=low, high=high)

    def get(self, obj, name):
        """ Get remote value. """
        return int(self.rget())

    def set(self, obj, name, value):
        """ Set remote value. """
        self.rset(str(value))


class StrProxy(ProxyMixin, Str):
    """ Proxy for a remote 'PHXString'. """

    def __init__(self, iotype, client, rpath):
        ProxyMixin.__init__(self, client, rpath)

        default = self._valstr
        desc = client.get(rpath+'.description')

        Str.__init__(self, default_value=default, iotype=iotype, desc=desc)

    def get(self, obj, name):
        """ Get remote value. """
        return self.rget()

    def set(self, obj, name, value):
        """ Set remote value. """
        self.rset(value)

