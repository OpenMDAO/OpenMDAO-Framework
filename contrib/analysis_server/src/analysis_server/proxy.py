"""
Proxies for AnalysisServer components and variables.
Component proxies are returned by :class:`factory.Factory`.
Variable proxies are members of their associated component and created when
the component proxy is built.
"""

import numpy
import socket
import types

from enthought.traits.api import TraitError

from openmdao.main.api import Component, Container, FileRef, VariableTree
from openmdao.main.mp_support import is_instance
from openmdao.main.datatypes.api import Array, Bool, Enum, File, Float, Int, \
                                        List, Str

from analysis_server.client import Client
from analysis_server.objxml import get_as_xml, set_from_xml, populate_from_xml
from analysis_server.units  import get_translation

# Map from server host,port to object class dictionary.
_OBJECT_CLASSES = {}

def _float2str(val):
    """ Return accurate string value for float. """
    return '%.16g' % val


class ComponentProxy(Component):
    """
    Proxy for a component running under an AnalysisServer.
    The proxy will populate itself with proxies for all variables exposed
    by the remote component, in an identical hierarchy.

    .. note::
        While variable attributes like 'units' are read from the remote
        component, attempts to set attributes other than 'value' from the local
        side will have no effect on the remote side.

    typname: string
        Component type.

    host: string
        Hostname of server.

    port: int
        Port on `host` used by server.
    """

    def __init__(self, typname, host, port):
        self._typname = typname
        self._objname = 'the_obj'
        self._host = host
        self._port = port

        self._client = Client(host, port)
        self._client.start(typname, self._objname)
        super(ComponentProxy, self).__init__()

        # Add properties (variables).
        self._populate(self, self._objname)

        # Add methods.
        self._methods = self._client.list_methods(self._objname)
        self._add_methods()

    def _populate(self, container, path):
        """
        Populate a container from a remote object.

        container: Container
            The local container object to be populated.

        path: string
            Path on server corresponding to `container`.
        """
        info = self._client.list_properties(path)
        for prop, typ, iotype in info:
            if hasattr(container, prop):
                container.raise_exception('Name %r already bound to %r'
                                          % (prop, getattr(container, prop)),
                                          AttributeError)

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
                                    ListProxy(iotype, self._client, rpath,
                                              str))
            elif typ == 'PHXScriptObject':
                container.add(prop,
                              ObjProxy(iotype, self._client, rpath))
            else:
                raise NotImplementedError('%r type %r' % (prop, typ))

    def _add_methods(self):
        """ Add methods to invoke remote methods. """
        for name in self._methods:
            if hasattr(self, name):
                self.raise_exception('Name %r already bound to %r'
                                     % (name, getattr(self, name)),
                                     AttributeError)
            dct = {}
            exec """
def %s(self):
    return self._invoke_method(%r)
""" % (name, name) in dct
            setattr(self, name,
                    types.MethodType(dct[name], self, self.__class__))

    def __getstate__(self):
        """ Return dict representing this component's local state. """
        state = super(ComponentProxy, self).__getstate__()
        del state['_client']
        for name in self._methods:
            del state[name]
        return state

    def __setstate__(self, state):
        """ Restore this component's local state. """
        state['_client'] = None
        super(ComponentProxy, self).__setstate__(state)
        self._add_methods()

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
                if isinstance(obj, VarTreeMixin):
                    obj.restore(self._client)
                else:
                    self._restore(obj)  # Recurse.

    def pre_delete(self):
        """ Unload remote instance before we get deleted. """
        super(ComponentProxy, self).pre_delete()
        if self._client is not None:
            self._client.end(self._objname)
            self._client.quit()
            self._client = None

    def execute(self):
        """ Execute remote component. """
        self._flush_proxies()
        self._client.execute(self._objname)
        self._update_proxies()

    def _invoke_method(self, name):
        """ Invoke remote method. """
        self._flush_proxies()
        result = self._client.invoke('%s.%s' % (self._objname, name))
        self._update_proxies()
        return result

    def _flush_proxies(self):
        """ Flush all 'in' object proxies. """
        for name, obj in self.items():
            if isinstance(obj, VariableTree):
                if obj.iotype == 'in':
                    obj.flush()

    def _update_proxies(self):
        """ Update all 'out' object proxies. """
        for name, obj in self.items():
            if isinstance(obj, VariableTree):
                if obj.iotype == 'out':
                    obj.update()


class ProxyMixin(object):
    """
    Common methods for variable proxies.
    The remote variable is accessed via `client` and `rpath`.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.
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
        """
        Restore remote state.

        client: :class:`client.Client`
            The client to use to access the remote variable.
        """
        self._client = client
        if self.iotype == 'in':
            self._client.set(self._rpath, self._valstr)

    def rget(self):
        """ Get remote value as a string. """
        if self._client is None:  # Happens during component.__setstate__
            return self._valstr
        else:
            return self._client.get(self._rpath)

    def rset(self, valstr):
        """
        Set remote value from `valstr`.

        valstr: string
            Value to be set, in string form.
        """
        if self.iotype == 'out':
            raise TraitError("Can't set an output")
        self._client.set(self._rpath, valstr)
        self._valstr = valstr


class ArrayProxy(ProxyMixin, Array):
    """
    Array-style proxy for a remote double, long, or String array.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.

    typ: Python type
        Type for each element.
    """

    def __init__(self, iotype, client, rpath, typ):
        ProxyMixin.__init__(self, client, rpath)
        self._type = typ

        default = self._parse(self._valstr)
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
# FIXME: This will pickle, but not unpickle.
#        Probabably don't want fixed max-length string storage anyway.
            Array.__init__(self, dtype=typ, iotype=iotype, desc=desc,
                           default_value=default)

    def get(self, obj, name):
        """
        Get remote value.

        obj: object
            Containing object, ignored.

        name: string
            Name in `obj`, ignored.
        """
        return self._parse(self.rget())

    def _parse(self, valstr):
        """
        Return parsed `valstr` as :class:`numpy.ndarray`.
        """
        if valstr.startswith('bounds['):
            dims, rbrack, rest = valstr[7:].partition(']')
            dims = [int(val.strip(' "')) for val in dims.split(',')]
            junk, lbrace, rest = rest.partition('{')
            data, rbrace, rest = rest.partition('}')
            return numpy.array([self._type(val.strip(' "'))
                                for val in data.split(',')]).reshape(dims)
        elif valstr:
            return numpy.array([self._type(val.strip(' "'))
                                for val in valstr.split(',')])
        else:
            return []

    def set(self, obj, name, value):
        """
        Set remote value after validation.

        obj: object
            Containing object.

        name: string
            Name in `obj`.

        value:
            Value to be set.
        """
        value = self.validate(obj, name, value)
        if self._type == float:
            valstr = ', '.join([_float2str(val) for val in value.flat])
        elif self._type == int:
            valstr = ', '.join([str(val) for val in value.flat])
        else:
            valstr = ', '.join(['"%s"' % val.encode('string_escape')
                                for val in value.flat])
        if len(value.shape) > 1:
            valstr = 'bounds[%s] {%s}' \
                     % (', '.join(['%d' % dim for dim in value.shape]), valstr)
        self.rset(valstr)


class ListProxy(ProxyMixin, List):
    """
    List-style proxy for a remote 1D double, long, or String array.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.

    typ: Python type
        Type for each element.
    """

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
            List.__init__(self, trait=Float, iotype=iotype, desc=desc,
                           value=default, low=low, high=high,
                           units=om_units)
        elif typ == int:
            List.__init__(self, trait=Int, iotype=iotype, desc=desc,
                           value=default, low=low, high=high)
        else:
            List.__init__(self, trait=Str, iotype=iotype, desc=desc,
                          value=default)

    def get(self, obj, name):
        """
        Get remote value.

        obj: object
            Containing object, ignored.

        name: string
            Name in `obj`, ignored.
        """
        return [self._type(val.strip(' "')) for val in self.rget().split(',')]

    def set(self, obj, name, value):
        """
        Set remote value after validation.

        obj: object
            Containing object.

        name: string
            Name in `obj`.

        value:
            Value to be set.
        """
        value = self.validate(obj, name, value)
        if self._type == float:
            valstr = ', '.join([_float2str(val) for val in value])
        elif self._type == int:
            valstr = ', '.join([str(val) for val in value])
        else:
            valstr = ', '.join(['"%s"' % val.encode('string_escape')
                                for val in value])
        self.rset(valstr)


class BoolProxy(ProxyMixin, Bool):
    """
    Proxy for a remote ``PHXBoolean`` at `rpath` accessed via `client`.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.
    """

    def __init__(self, iotype, client, rpath):
        ProxyMixin.__init__(self, client, rpath)

        default = self._valstr == 'true'
        desc = client.get(rpath+'.description')

        Bool.__init__(self, default_value=default, iotype=iotype, desc=desc)

    def get(self, obj, name):
        """
        Get remote value.

        obj: object
            Containing object, ignored.

        name: string
            Name in `obj`, ignored.
        """
        return self.rget() == 'true'

    def set(self, obj, name, value):
        """
        Set remote value after validation.

        obj: object
            Containing object.

        name: string
            Name in `obj`.

        value: bool
            Value to be set.
        """
        self.rset('true' if self.validate(obj, name, value) else 'false')


class EnumProxy(ProxyMixin, Enum):
    """
    Proxy for a remote ``PHXDouble``, ``PHXLong``, or ``PHXString`` which has
    a non-empty ``enumValues`` property.
    The remote variable is at `rpath` accessed via `client`.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.

    typ: string
        AnalysisServer type string.

    valstrs: list[string]
        Enumeration values as strings.
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
        """
        Get remote value.

        obj: object
            Containing object, ignored.

        name: string
            Name in `obj`, ignored.
        """
        return self._from_string(self.rget())

    def set(self, obj, name, value):
        """
        Set remote value after validation.

        obj: object
            Containing object.

        name: string
            Name in `obj`.

        value:
            Value to be set.
        """
        self.rset(self._to_string(self.validate(obj, name, value)))

    def _null(self, val):
        """
        Just returns `val` unmodified.
        """
        return val


class FileProxy(ProxyMixin, File):
    """
    Proxy for a remote ``PHXRawFile`` at `rpath` accessed via `client`.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.

    component: :class:`ComponentProxy`
        Parent component of remote variable.
    """

    def __init__(self, iotype, client, rpath, component):
        ProxyMixin.__init__(self, client, rpath)
        self._component = component
        self._path = 'AS-%s.dat' % rpath  # Local filename for remote data.

        desc = client.get(rpath+'.description')
        metadata = {}
        if iotype == 'out':
            metadata['path'] = self._path

        File.__init__(self, default_value=None, iotype=iotype, desc=desc,
                      **metadata)

    def get(self, obj, name):
        """
        Get remote value and write to local file.

        obj: object
            Containing object, ignored.

        name: string
            Name in `obj`, ignored.
        """
        if self._component._call_cpath_updated:
            return None  # Not initialized.

        if self._client is None:  # Happens during component.__setstate__
            return None

        binary = self._client.get(self._rpath+'.isBinary') == 'true'
        valstr = self.rget()
        mode = 'wb' if binary else 'w'
        with self._component.dir_context:
            with open(self._path, mode) as out:
                out.write(valstr)
        return FileRef(self._path, self._component, binary=binary)

    def set(self, obj, name, value):
        """
        Set remote value after validation.

        obj: object
            Containing object.

        name: string
            Name in `obj`.

        value: FileRef
            Value to be set.
        """
        value = self.validate(obj, name, value)
        with value.open() as inp:
            valstr = inp.read()
#        binary = 'true' if value.binary else 'false'
#        self._client.set(self._rpath+'.isBinary', binary)
        self.rset('"%s"' % valstr.encode('string_escape'))


class FloatProxy(ProxyMixin, Float):
    """
    Proxy for a remote ``PHXDouble`` at `rpath` accessed via `client`.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.
    """

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
        """
        Get remote value.

        obj: object
            Containing object, ignored.

        name: string
            Name in `obj`, ignored.
        """
        return float(self.rget())

    def set(self, obj, name, value):
        """
        Set remote value after validation.

        obj: object
            Containing object.

        name: string
            Name in `obj`.

        value: float
            Value to be set.
        """
        self.rset(_float2str(self.validate(obj, name, value)))


class IntProxy(ProxyMixin, Int):
    """
    Proxy for a remote ``PHXLong`` at `rpath` accessed via `client`.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.
    """

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
        """
        Get remote value.

        obj: object
            Containing object, ignored.

        name: string
            Name in `obj`, ignored.
        """
        return int(self.rget())

    def set(self, obj, name, value):
        """
        Set remote value after validation.

        obj: object
            Containing object.

        name: string
            Name in `obj`.

        value: int
            Value to be set.
        """
        self.rset(str(self.validate(obj, name, value)))


class StrProxy(ProxyMixin, Str):
    """
    Proxy for a remote 'PHXString'.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.
    """

    def __init__(self, iotype, client, rpath):
        ProxyMixin.__init__(self, client, rpath)

        default = self._valstr
        desc = client.get(rpath+'.description')

        Str.__init__(self, default_value=default, iotype=iotype, desc=desc)

    def get(self, obj, name):
        """
        Get remote value.

        obj: object
            Containing object, ignored.

        name: string
            Name in `obj`, ignored.
        """
        return self.rget()

    def set(self, obj, name, value):
        """
        Set remote value after validation.

        obj: object
            Containing object.

        name: string
            Name in `obj`.

        value: string
            Value to be set.
        """
        self.rset('"%s"' % \
                  self.validate(obj, name, value).encode('string_escape'))


class VarTreeMixin(object):
    """
    Add and override some VariableTree methods to support proxying
    a remote 'PHXScriptObject'.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.
    """

    def __init__(self, iotype, client, rpath):
        self._iotype = iotype
        self._client = client
        self._rpath = rpath
        self._valstr = client.get(rpath)  # Needed for later restore.
        self._dirty = False  # Set True after something is modified,

    def __getstate__(self):
        """ Return state of object. """
        state = VariableTree.__getstate__(self)
        state['_client'] = None
        return state

    def __setstate__(self, state):
        """ Set state of object from `state`. """
        VariableTree.__setstate__(self, state)

    def add(self, name, obj):
        """ Overrides VariableTree to manipulate `_dirty` flag. """
        retval = VariableTree.add(self, name, obj)
        if self._iotype == 'in':
            self.on_trait_change(self._trait_modified, name)
            if isinstance(obj, VariableTree):
                self._register(name, obj)
        return retval

    def _register(self, path, vartree):
        """ Helper method for :meth:`add`. """
        for name, obj in vartree.items():
            name = '.'.join((path, name))
            self.on_trait_change(self._trait_modified, name)
            if isinstance(obj, VariableTree):
                self._register(name, obj)
        
    def _trait_modified(self, obj, name, old, new):
        """ Record that local is now different than remote. """
        self._dirty = True

    def flush(self):
        """ If we've been modified, then update remote from local. """
        if self._dirty:
            xml = get_as_xml(self)
            self._client.set(self._rpath, xml)
            self._valstr = xml
            self._dirty = False

    def restore(self, client):
        """
        Restore remote state.

        client: :class:`client.Client`
            The client to use to access the remote variable.
        """
        self._client = client
        if self.iotype == 'in':
            self._client.set(self._rpath, self._valstr)

    def update(self):
        """ Update local from remote. """
        xml = self._client.get(self._rpath)
        set_from_xml(self, xml)


class ObjProxy(VarTreeMixin, VariableTree):
    """
    Proxy for a remote 'PHXScriptObject'.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.
    """

    def __init__(self, iotype, client, rpath):
        VarTreeMixin.__init__(self, iotype, client, rpath)
        desc = client.get(rpath+'.description')
        VariableTree.__init__(self, doc=desc, iotype=iotype)
        xml = client.get(rpath)
        populate_from_xml(self, xml)
        self._dirty = False


# Currently unused. While the code works, there are issues regarding how
# other components can access the generated definition, as well as how the
# definition could be accessed while loading from a pickled state.
'''
def make_object_proxy(host, port, iotype, client, rpath):
    """
    Create proxy for a remote 'PHXScriptObject'.
    This will create a class definition if necessary via
    :meth:`make_proxy_class` and then create an instance of that.

    host: string
        server hostname

    port: int
        server port on `host`.

    iotype: string
        'in' or 'out'.

    client: :class:`client.Client`
        The client to use to access the remote variable.

    rpath: string
        Path to the remote variable.
    """
    server_id = '%s:%s' % (host, port)
    url = client.get(rpath+'.classURL')
    try:
        classes = _OBJECT_CLASSES[server_id]
    except KeyError:
        _OBJECT_CLASSES[server_id] = {}
        classes = _OBJECT_CLASSES[server_id]
    try:
        cls = classes[url]
    except KeyError:
        # Create a proxy class based on the object at `rpath` on `client`.
        xml = client.get(rpath)
        name, definition = generate_from_xml(xml)
        exec definition in globals()
        classes[url] = globals()[name]

    return cls(iotype, client, rpath)
'''

