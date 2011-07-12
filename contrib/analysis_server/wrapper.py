"""
Wrappers for OpenMDAO components and variables.
"""

import os
import sys
import time
import logging
import xml.etree.cElementTree as ElementTree
from xml.sax.saxutils import escape, quoteattr

try:
    import resource
except ImportError:  # pragma no cover
    pass  # Not available on Windows.

from openmdao.main.api import Container, FileRef
from openmdao.main.assembly import PassthroughTrait
from openmdao.main.container import find_trait_and_value
from openmdao.main.mp_support import is_instance

from openmdao.lib.datatypes.api import Array, Bool, Enum, File, Float, Int, \
                                       List, Str
from monitor import FileMonitor

class WrapperError(Exception):
    """ Denotes wrapper-specific errors. """
    pass

# Mapping from OpenMDAO variable type to wrapper type.
TYPE_MAP = {}

def _float2str(val):
    """ Return accurate string value for float. """
    return '%.16g' % val


class ComponentWrapper(object):
    """
    Component wrapper providing a ModelCenter AnalysisServer interface,
    based on the protocol described in:
    http://www.phoenix-int.com/~AnalysisServer/commands/index.html

    Wraps component `comp`, named `name`, with configuraton `cfg` on `server`.
    `send_reply` and `send_exc` are used to communicate back to the client.
    """

    def __init__(self, name, comp, cfg, server, send_reply, send_exc):
        self._name = name
        self._comp = comp
        self._cfg = cfg
        self._server = server
        self._send_reply = send_reply
        self._send_exc = send_exc
        self._monitors = {}  # Maps from monitor_id to monitor.
        self._wrappers = {}  # Maps from internal var path to var wrapper.
        self._path_map = {}  # Maps from external path to (var wrapper, attr).
        self._start = None
        self._rusage = None  # For ps() on UNIX.

    def _get_var_wrapper(self, ext_path):
        """ Return '(wrapper, attr)' for `ext_path`. """
        try:
            return self._path_map[ext_path]
        except KeyError:
            # Determine internal path to variable.
            ext_attr = None
            if ext_path in self._cfg.properties:
                int_path = self._cfg.properties[ext_path]
                epath = ext_path
            else:
                epath, dot, ext_attr = ext_path.rpartition('.')
                if epath in self._cfg.properties:
                    int_path = self._cfg.properties[epath]
                else:
                    raise WrapperError('no such property <%s>.' % ext_path)
            try:
                wrapper = self._wrappers[int_path]
            except KeyError:
                # Find variable.
                obj = self._comp
                rel_path = int_path
                while is_instance(obj, Container):
                    container = obj
                    rpath = rel_path
                    name, dot, rel_path = rel_path.partition('.')
                    try:
                        obj = getattr(obj, name)
                    except Exception:
                        break  # Unserializable => not Container.
                iotype = 'in' if epath in self._cfg.inputs else 'out'
                trait = container.get_dyn_trait(rpath, iotype=iotype)
                if trait is None:
                    raise WrapperError('no such trait %r %r.'
                                       % (container.get_pathname(), rpath))
                # Determine variable type.
                typ = trait.trait_type or trait.trait
                if isinstance(typ, PassthroughTrait):
                    connections = container._depgraph.connections_to(name)
                    if iotype == 'in':
                        real_name = connections[0][1]
                    else:
                        real_name = connections[0][0]
                    typ = container.get_dyn_trait(real_name)
                key = type(typ)
                if key not in TYPE_MAP:
                    for base in key.__bases__:
                        if base in TYPE_MAP:
                            key = base
                            break
                    else:
                        raise WrapperError('%s: unsupported variable type %r.'
                                           % (ext_path, key))
                # Wrap it.
                wrapper_class = TYPE_MAP[key]
                wrapper = wrapper_class(container, rpath, epath)
                if wrapper_class is FileWrapper:
                    wrapper.set_server(self._server)
                self._wrappers[int_path] = wrapper

            attr = ext_attr or 'value'
            map_value = (wrapper, attr)
            self._path_map[ext_path] = map_value
            return map_value

    def pre_delete(self):
        """ Prepare for deletion. """
        for monitor in self._monitors.values():
            monitor.stop()
        self._comp.pre_delete()

    def execute(self, req_id):
        """ Runs a component instance. """
        try:
            if sys.platform != 'win32':
                self._rusage = resource.getrusage(resource.RUSAGE_SELF)

            self._start = time.time()
            try:
                self._comp.run()
            except Exception as exc:
                raise WrapperError('%s' % exc)
            else:
                self._send_reply('%s completed.' % self._name, req_id)
            finally:
                self._start = None
        except Exception as exc:
            self._send_exc(exc, req_id)

    def get(self, path, req_id):
        """ Returns the value of a variable. """
        try:
            wrapper, attr = self._get_var_wrapper(path)
            self._send_reply(wrapper.get(attr, path), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def get_hierarchy(self, req_id):
        """ Return all inputs & outputs as XML. """
        try:
            group = ''
            lines = []
            lines.append("<?xml version='1.0' encoding='utf-8'?>")
            lines.append('<Group>')
            for path in sorted(self._cfg.properties.keys()):
                wrapper, attr = self._get_var_wrapper(path)
                path, dot, name = path.rpartition('.')
                if path != group:
                    while not path.startswith(group):  # Exit subgroups.
                        lines.append('</Group>')
                        group, dot, name = group.rpartition('.')
                    name, dot, rest = path.partition('.')
                    if name:
                        lines.append('<Group name="%s">' % name)
                    while rest:  # Enter subgroups.
                        name, dot, rest = rest.partition('.')
                        lines.append('<Group name="%s">' % name)
                    group = path
                lines.append(wrapper.get_as_xml())
            lines.append('</Group>')
            self._send_reply('\n'.join(lines), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def invoke(self, method, req_id):
        """ Invokes a method on a component instance. """
        try:
            try:
                attr = self._cfg.methods[method]
            except KeyError:
                raise WrapperError('no such method <%s>.' % method)
            meth = getattr(self._comp, attr)
            result = meth()
# FIXME: MC 8 doesn't understand return value (uses 'full' option)
            if result is None:
                reply = ''
            elif type(result) == float:
                reply = _float2str(result)
            else:
                reply = str(result)
            self._send_reply(reply, req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def list_array_values(self, path, req_id):
        """ Lists all the values of an array variable. """
        try:
            raise NotImplementedError('listArrayValues')
        except Exception as exc:
            self._send_exc(exc, req_id)

    def list_methods(self, full, req_id):
        """ Lists all methods available on a component instance. """
        try:
            lines = ['']
            for name in sorted(self._cfg.methods.keys()):
                line = '%s()' % name
                if full:
                    line += ' fullName="%s"' % name
                lines.append(line)

            lines[0] = '%d methods found:' % (len(lines)-1)
            self._send_reply('\n'.join(lines), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def list_monitors(self, req_id):
        """ Lists all available monitorable items on a component instance. """
        try:
            root = self._comp.get_abs_directory()
            if self._server is None:  # Used when testing.
                paths = os.listdir(root)
                paths = [path for path in paths
                              if not os.path.isdir(os.path.join(root, path))]
            else:  # pragma no cover
                paths = self._server.listdir(root)
                paths = [path for path in paths
                              if not self._server.isdir(os.path.join(root, path))]
            paths = [path for path in paths if not path.startswith('.')]
            text_files = []
            for path in paths:  # List only text files.
                if path.startswith('.'):
                    continue
                if self._server is None:  # Used when testing.
                    inp = open(os.path.join(root, path), 'rb')
                else:  # pragma no cover
                    inp = self._server.open(os.path.join(root, path), 'rb')
                try:
                    data = inp.read(1 << 12)  # 4KB
                    if '\x00' not in data:
                        text_files.append(path)
                finally:
                    inp.close()
            lines = ['%d monitors:' % len(text_files)]
            lines.extend(sorted(text_files))
            self._send_reply('\n'.join(lines), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def list_properties(self, path, req_id):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable.
        """
        try:
            self._send_reply(self._list_properties(path), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def _list_properties(self, path):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable.
        """
        lines = ['']
        try:
            wrapper, attr = self._get_var_wrapper(path)
        except WrapperError:
            # Must be a container.
            if path:
                path += '.'
            length = len(path)
            groups = set()
            for ext_path in sorted(self._cfg.properties.keys()):
                if path and not ext_path.startswith(path):
                    continue
                rest = ext_path[length:]
                name, dot, rest = rest.partition('.')
                if rest:
                    if name in groups:
                        continue
                    groups.add(name)
                    typ = 'com.phoenix_int.aserver.PHXGroup'
                    access = 'sg'
                else:
                    wrapper, attr = self._get_var_wrapper(ext_path)
                    typ = wrapper.phx_type
                    access = wrapper.phx_access
                lines.append('%s (type=%s) (access=%s)' % (name, typ, access))
        else:
            lines.extend(wrapper.list_properties())
        lines[0] = '%d properties found:' % (len(lines)-1)
        return '\n'.join(lines)

    def list_values(self, path, req_id):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable.
        """
# TODO: this is different than listValuesURL for file variables and DFT.
        self.list_values_url(path, req_id)

    def list_values_url(self, path, req_id):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable.
        """
# TODO: this is different than listValues for file variables and DFT.
        try:
            lines = []
            # Get list of properties.
            props = self._list_properties(path).split('\n')
            lines.append(props[0])
            if path:
                path += '.'
            # Collect detailed property information.
            for line in props[1:]:
                name, typ, access = line.split()
                if typ == '(type=com.phoenix_int.aserver.PHXGroup)':
                    val = 'Group: %s' % name
                    lines.append('%s %s %s  vLen=%d  val=%s'
                                 % (name, typ, access, len(val), val))
                else:
                    ext_path = path + name
                    wrapper, attr = self._get_var_wrapper(ext_path)
                    val = wrapper.get('value', ext_path)
                    lines.append('%s %s %s  vLen=%d  val=%s'
                                 % (name, typ, access, len(val), val))
                    sub_props = self._list_properties(ext_path).split('\n')
                    sub_props = sub_props[1:]
                    lines.append('   %d SubProps found:' % len(sub_props))
                    for line in sub_props:
                        name, typ, access = line.split()
                        if typ == '(type=com.phoenix_int.aserver.PHXGroup)':
                            val = 'Group: %s' % name
                            lines.append('%s %s %s  vLen=%d  val=%s'
                                         % (name, typ, access, len(val), val))
                        else:
                            val = wrapper.get(name, ext_path)
                            lines.append('%s %s %s  vLen=%d  val=%s'
                                         % (name, typ, access, len(val), val))
            self._send_reply('\n'.join(lines), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def start_monitor(self, path, req_id):
        """ Starts a monitor on a raw output file or available monitor. """
        try:
            path = os.path.join(self._comp.get_abs_directory(), path)
            monitor = FileMonitor(self._server, path, 'r',
                                  req_id, self._send_reply)
            monitor.start()
            self._monitors[str(req_id)] = monitor  # Monitor id is request id.
        except Exception as exc:
            self._send_exc(exc, req_id)

    def stop_monitor(self, monitor_id, req_id):
        """ Stops a monitor on a raw output file or available monitor. """
        try:
            monitor = self._monitors.pop(monitor_id)
        except KeyError:
            raise WrapperError('No registered monitor for %r' % monitor_id)
        else:
            monitor.stop()
            self._send_reply('', req_id)

    def ps(self, req_id):
        """ Lists all running processes for a component instance. """
        try:
            pid = os.getpid()
            command = os.path.basename(sys.executable)

            if self._start is None:  # Component not running.
                # Forcing PID to zero helps with testing.
                reply = """\
<Processes length='1'>
 <Process pid='0'>
  <ParentPID>0</ParentPID>
  <PercentCPU>0.0</PercentCPU>
  <Memory>0</Memory>
  <Time>0</Time>
  <WallTime>0</WallTime>
  <Command>%s</Command>
 </Process>
</Processes>""" % escape(command)

            else:
                now = time.time()
                walltime = now - self._start

                if sys.platform == 'win32':  # pragma no cover
                    reply = """\
<Processes length='1'>
 <Process pid='%d'>
  <ParentPID>0</ParentPID>
  <PercentCPU>0.0</PercentCPU>
  <Memory>0</Memory>
  <Time>0</Time>
  <WallTime>%.1f</WallTime>
  <Command>%s</Command>
 </Process>
</Processes>""" % (pid, walltime, escape(command))

                else:
                    rusage = resource.getrusage(resource.RUSAGE_SELF)
                    cputime = (rusage.ru_utime + rusage.ru_stime) \
                            - (self._rusage.ru_utime + self._rusage.ru_stime)
                    if walltime > 0:
                        percent_cpu = cputime / walltime
                    else:
                        percent_cpu = 0.
                    memory = rusage.maxrss * resource.getpagesize()

                    reply = """\
<Processes length='1'>
 <Process pid='%d'>
  <ParentPID>%d</ParentPID>
  <PercentCPU>%.1f</PercentCPU>
  <Memory>%d</Memory>
  <Time>%.1f</Time>
  <WallTime>%.1f</WallTime>
  <Command>%s</Command>
 </Process>
</Processes>""" % (pid, os.getppid(), percent_cpu, memory, cputime, walltime,
                   escape(command))

            self._send_reply(reply, req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def set(self, path, valstr, req_id):
        """ Sets the value of `path` to `valstr`. """
        try:
            self._set(path, valstr)
            self._send_reply('value set for <%s>' % path, req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def _set(self, path, valstr):
        """ Sets the value of `path` to `valstr`. """
        wrapper, attr = self._get_var_wrapper(path)
        wrapper.set(attr, path, valstr)

    def set_hierarchy(self, xml, req_id):
        """ Set hiearchy of variable values from `xml`. """
        try:
            header, newline, xml = xml.partition('\n')
            root = ElementTree.fromstring(xml)
            for var in root.findall('Variable'):
                valstr = '' if var.text is None else var.text
                valstr = valstr.decode('string_escape')
                self._set(var.attrib['name'], valstr)
            self._send_reply('values set', req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)


class BaseWrapper(object):
    """ Base class for variable wrappers. """

    def __init__(self, container, name, ext_path):
        self._container = container
        self._name = name
        self._ext_path = ext_path
        self._ext_name = ext_path.rpartition('.')[2]
        trait = container.get_dyn_trait(name)
        self._trait = trait
        self._access = 'sg' if trait.iotype == 'in' else 'g'
        self._io = 'input' if trait.iotype == 'in' else 'output'

    @property
    def phx_access(self):
        """ Return AnalysisServer access string. """
        return self._access

    def get(self, attr, path):
        """ Return attribute corresponding to `attr`. """
        if attr == 'description':
            return self._trait.desc
        else:
            raise WrapperError('no such property <%s>.' % path)


class ArrayBase(BaseWrapper):
    """
    Base for wrappers providing double[], long[], or String[] interface.
    """

    def __init__(self, container, name, ext_path, typ):
        super(ArrayBase, self).__init__(container, name, ext_path)
        self.typ = typ
        if typ is float:
            self._typstr = 'double'
        elif typ is int:
            self._typstr = 'long'
        elif typ is str:
            self._typstr = 'string'

    @property
    def phx_type(self):
        """ Return AnalysisServer type string for value. """
        value = self._container.get(self._name)
        if self.typ == float:
            return 'double[%d]' % len(value)
        elif self.typ == int:
            return 'long[%d]' % len(value)
        else:
            return 'java.lang.String[%d]' % len(value)

    def get(self, attr, path):
        """ Return attribute corresponding to `attr`. """
        if attr == 'value':
            value = self._container.get(self._name)
            if self.typ == float:
                fmt = '%.16g'
            elif self.typ == int:
                fmt = '%d'
            else:
                fmt = '"%s"'
            return ', '.join([fmt % val for val in value])
        elif attr == 'componentType':
            return self._typstr
        elif attr == 'dimensions':
            value = self._container.get(self._name)
            return '"%d"' % len(value)
        elif attr == 'enumAliases':
            return ''
        elif attr == 'enumValues':
            return ''
        elif attr == 'first':
            value = self._container.get(self._name)
            if len(value):
                return '%s' % value[0]
            else:
                return ''
        elif attr == 'format':
            return 'null'
        elif attr == 'hasUpperBound' and self._trait.dtype != str:
            return 'false' if self._trait.high is None else 'true'
        elif attr == 'upperBound' and self._trait.dtype != str:
            return '0' if self._trait.high is None else str(self._trait.high)
        elif attr == 'hasLowerBound' and self._trait.dtype != str:
            return 'false' if self._trait.low is None else 'true'
        elif attr == 'lowerBound' and self._trait.dtype != str:
            return '0' if self._trait.low is None else str(self._trait.low)
        elif attr == 'length':
            value = self._container.get(self._name)
            return '%d' % len(value)
        elif attr == 'lockResize':
            return 'false'
        elif attr == 'numDimensions':
            return '1'
        elif attr == 'units':
            if self.typ == float:
                return '' if self._trait.units is None else self._trait.units
            else:
                return ''
        else:
            return super(ArrayBase, self).get(attr, path)

    def get_as_xml(self):
        """ Return info in XML form. """
        return '<Variable name="%s" type="%s[]" io="%s" format=""' \
               ' description=%s units="%s">%s</Variable>' \
               % (self._ext_name, self._typstr, self._io,
                  quoteattr(self._trait.desc),
                  self.get('units', self._ext_path),
                  self.get('value', self._ext_path))

    def set(self, attr, path, valstr):
        """ Set attribute corresponding to `attr` to `valstr`. """
        if attr == 'value':
            self._container.set(self._name,
                                [self.typ(val.strip(' "'))
                                 for val in valstr.split(',')])
        elif attr in ('componentType', 'description', 'dimensions',
                      'enumAliases', 'enumValues', 'first', 'format',
                      'hasLowerBound', 'lowerBound',
                      'hasUpperBound', 'upperBound',
                      'length', 'lockResize', 'numDimensions', 'units'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        if self.typ == float:
            typstr = 'double'
        elif self.typ == int:
            typstr = 'long'
        else:
            typstr = 'java.lang.String'

        lines = ['componentType (type=java.lang.Class) (access=g)',
                 'description (type=java.lang.String) (access=sg)',
                 'dimensions (type=int[1]) (access=sg)',
                 'enumAliases (type=java.lang.String[0]) (access=sg)',
                 'enumValues (type=%s[0]) (access=sg)' % typstr,
                 'first (type=java.lang.Object) (access=sg)',
                 'length (type=int) (access=sg)',
                 'lockResize (type=boolean) (access=sg)',
                 'numDimensions (type=int) (access=g)',
                 'units (type=java.lang.String) (access=sg)']

        if self.typ != str:
            lines.extend(['format (type=java.lang.String) (access=g)',
                          'hasLowerBound (type=boolean) (access=sg)',
                          'hasUpperBound (type=boolean) (access=sg)',
                          'lowerBound (type=%s) (access=sg)' % typstr,
                          'upperBound (type=%s) (access=sg)' % typstr])

        return sorted(lines)


class ArrayWrapper(ArrayBase):
    """
    Wrapper for `Array` providing double[], long[], or String[] interface.
    """

    # Map from numpy dtype.kind to scalar converter.
    _converters = {'f':float, 'i':int, 'S':str}

    def __init__(self, container, name, ext_path):
        value = container.get(name)
        kind = value.dtype.kind
        try:
            typ = self._converters[kind]
        except KeyError:
            raise WrapperError('Unsupported dtype for %s.%s: %r (%r)'
                               % (container.get_pathname(), name,
                                  value.dtype, kind))

        super(ArrayWrapper, self).__init__(container, name, ext_path, typ)

TYPE_MAP[Array] = ArrayWrapper


class ListWrapper(ArrayBase):
    """
    Wrapper for `List` providing double[], long[], or String[] interface.
    """

    def __init__(self, container, name, ext_path):
        value = container.get(name)
# HACK!
        typ = str
        super(ListWrapper, self).__init__(container, name, ext_path, typ)

TYPE_MAP[List] = ListWrapper


class BoolWrapper(BaseWrapper):
    """ Wrapper for `Bool` providing ``PHXBoolean`` interface. """

    @property
    def phx_type(self):
        """ Return AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXBoolean'

    def get(self, attr, path):
        """ Return attribute corresponding to `attr`. """
        if attr == 'value' or attr == 'valueStr':
            return 'true' if self._container.get(self._name) else 'false'
        else:
            return super(BoolWrapper, self).get(attr, path)

    def get_as_xml(self):
        """ Return info in XML form. """
        return '<Variable name="%s" type="boolean" io="%s" format=""' \
               ' description=%s>%s</Variable>' \
               % (self._ext_name, self._io, quoteattr(self._trait.desc),
                  self.get('value', self._ext_path))

    def set(self, attr, path, valstr):
        """ Set attribute corresponding to `attr` to `valstr`. """
        valstr = valstr.strip('"')
        if attr == 'value':
            if valstr == 'true':
                self._container.set(self._name, True)
            elif valstr == 'false':
                self._container.set(self._name, False)
            else:
                raise WrapperError('invalid boolean value %r for <%s>'
                                   % (valstr, path))
        elif attr in ('valueStr', 'description'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        return ['description (type=java.lang.String) (access=g)',
                'value (type=boolean) (access=%s)' % self._access,
                'valueStr (type=boolean) (access=g)']

TYPE_MAP[Bool] = BoolWrapper


class EnumWrapper(BaseWrapper):
    """
    Wrapper for `Enum` providing ``PHXDouble``, ``PHXLong``, or ``PHXString``
    interface.
    """

    def __init__(self, container, name, ext_path):
        super(EnumWrapper, self).__init__(container, name, ext_path)
        typ = type(self._trait.values[0])
        for val in self._trait.values:
            if type(val) != typ:
                raise WrapperError('inconsistent value types for %s.%s'
                                   % (container.get_pathname(), name))
        if typ not in (float, int, str):
            raise WrapperError('unexpected value type for %s.%s: %r'
                               % (container.get_pathname(), name, typ))
        self._py_type = typ
        if typ == float:
            self._phx_type = 'com.phoenix_int.aserver.types.PHXDouble'
            self._val_type = 'double'
        elif typ == int:
            self._phx_type = 'com.phoenix_int.aserver.types.PHXLong'
            self._val_type = 'long'
        else:
            self._phx_type = 'com.phoenix_int.aserver.types.PHXString'
            self._val_type = 'java.lang.String'

    @property
    def phx_type(self):
        """ Return AnalysisServer type string for value. """
        return self._phx_type

    def get(self, attr, path):
        """ Return attribute corresponding to `attr`. """
        if attr == 'value' or attr == 'valueStr':
            if self._py_type == float:
                return _float2str(self._container.get(self._name))
            else:
                return str(self._container.get(self._name))
        elif attr == 'enumAliases':
            if self._trait.aliases:
                return ', '.join(['"%s"' % alias
                                  for alias in self._trait.aliases])
            else:
                return ''
        elif attr.startswith('enumAliases['):
            i = int(attr[12:-1])
            return self._trait.aliases[i]
        elif attr == 'enumValues':
            if self._py_type == float:
                return ', '.join([_float2str(value) for value in self._trait.values])
            elif self._py_type == int:
                return ', '.join(['%s' % value for value in self._trait.values])
            else:
                return ', '.join(['"%s"' % value for value in self._trait.values])
        elif attr.startswith('enumValues['):
            i = int(attr[11:-1])
            if self._py_type == float:
                return _float2str(self._trait.values[i])
            else:
                return str(self._trait.values[i])
        elif attr == 'format':
            return 'null'
        elif attr == 'hasLowerBound':
            return 'false'
        elif attr == 'lowerBound':
            return ''
        elif attr == 'hasUpperBound':
            return 'false'
        elif attr == 'upperBound':
            return ''
        elif attr == 'units':
            if self._py_type == float:
                units = self._trait.units
                return '' if units is None else units
            else:
                return ''
        else:
            return super(EnumWrapper, self).get(attr, path)

    def get_as_xml(self):
        """ Return info in XML form. """
        if self._py_type == float:
            typstr = 'double'
        elif self._py_type == int:
            typstr = 'long'
        else:
            typstr = 'string'
        return '<Variable name="%s" type="%s" io="%s" format=""' \
               ' description=%s units="%s">%s</Variable>' \
               % (self._ext_name, typstr, self._io, quoteattr(self._trait.desc),
                  self.get('units', self._ext_path),
                  escape(self.get('value', self._ext_path)))

    def set(self, attr, path, valstr):
        """ Set attribute corresponding to `attr` to `valstr`. """
        valstr = valstr.strip('"')
        if attr == 'value':
            try:
                i = self._trait.aliases.index(valstr)
            except (AttributeError, ValueError):
                self._container.set(self._name, self._py_type(valstr))
            else:
                self._container.set(self._name, self._trait.values[i])
        elif attr in ('valueStr', 'description', 'enumAliases', 'enumValues'
                      'format', 'hasLowerBound', 'lowerBound', 'hasUpperBound',
                      'upperBound', 'units'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        n_vals = len(self._trait.values)
        n_alias = len(self._trait.aliases) if self._trait.aliases else 0

        lines = ['description (type=java.lang.String) (access=g)',
                 'enumAliases (type=java.lang.String[%d]) (access=g)' % n_alias,
                 'enumValues (type=%s[%d]) (access=g)' % (self._val_type, n_vals)]
        if self._py_type == float:
            lines.append('format (type=java.lang.String) (access=g)')
        if self._py_type == float or self._py_type == int:
            lines.extend(['hasLowerBound (type=boolean) (access=g)',
                          'hasUpperBound (type=boolean) (access=g)',
                          'lowerBound (type=%s) (access=g)' % self._val_type,
                          'units (type=java.lang.String) (access=g)',
                          'upperBound (type=%s) (access=g)' % self._val_type])
        lines.extend(['value (type=%s) (access=%s)' \
                      % (self._val_type, self._access),
                      'valueStr (type=java.lang.String) (access=g)'])
        return lines

TYPE_MAP[Enum] = EnumWrapper


class FileWrapper(BaseWrapper):
    """ Wrapper for `File` providing ``PHXRawFile`` interface. """

    def __init__(self, container, name, ext_path):
        super(FileWrapper, self).__init__(container, name, ext_path)
        self._server = None
        self._owner = None

    @property
    def phx_type(self):
        """ Return AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXRawFile'

    def set_server(self, server):
        """ Set container's server to `server` for file operations. """
        self._server = server
        owner = self._container
        while owner is not None:
            if hasattr(owner, 'get_abs_directory'):
                self._owner = owner
                break
            if hasattr(owner, 'parent'):
                owner = owner.parent

    def get(self, attr, path):
        """ Return attribute corresponding to `attr`. """
        if attr == 'value':
            file_ref = self._container.get(self._name)
            if file_ref is None:
                return ''
            try:
                with file_ref.open() as inp:
                    return inp.read()
            except IOError as exc:
                logging.warning('get %s.value: %r', path, exc)
                return ''
        elif attr == 'isBinary':
            file_ref = self._container.get(self._name)
            if file_ref is None:
                return 'false'
            else:
                return 'true' if file_ref.binary else 'false'
        elif attr == 'mimeType':
            file_ref = self._container.get(self._name)
            if file_ref is None:
                return ''
            elif file_ref.binary:
                return 'application/octet-stream'
            else:
                return 'text/plain'
        elif attr == 'name':
            return ''
        elif attr == 'nameCoded':
            return ''
        elif attr == 'url':
            return ''
        else:
            return super(FileWrapper, self).get(attr, path)

    def get_as_xml(self):
        """ Return info in XML form. """
        return '<Variable name="%s" type="file" io="%s" description=%s' \
               ' isBinary="%s" fileName="">%s</Variable>' \
               % (self._ext_name, self._io, quoteattr(self._trait.desc),
                  self.get('isBinary', self._ext_path),
                  escape(self.get('value', self._ext_path)))

    def set(self, attr, path, valstr):
        """ Set attribute corresponding to `attr` to `valstr`. """
        valstr = valstr.strip('"')
        if attr == 'value':
            if self._trait.iotype != 'in':
                raise WrapperError('cannot set output <%s>.' % path)
            left, dot, name = self._ext_path.rpartition('.')
            filename = os.path.join(self._owner.get_abs_directory(),
                                    '%s.dat' % name)
            mode = 'wb'
            if self._server is None:  # Used during testing.
                out = open(filename, mode)
            else:  # pragma no cover
                out = self._server.open(filename, mode)
            try:
                out.write(valstr)
            finally:
                out.close()
            file_ref = FileRef(path=filename, owner=self._owner)
            self._container.set(self._name, file_ref)
        elif attr in ('description', 'isBinary', 'mimeType',
                      'name', 'nameCoded', 'url'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        return ['description (type=java.lang.String) (access=g)',
                'isBinary (type=boolean) (access=g)',
                'mimeType (type=java.lang.String) (access=g)',
                'name (type=java.lang.String) (access=g)',
                'nameCoded (type=java.lang.String) (access=g)',
                'url (type=java.lang.String) (access=g)']

TYPE_MAP[File] = FileWrapper


class FloatWrapper(BaseWrapper):
    """ Wrapper for `Float` providing ``PHXDouble`` interface. """

    @property
    def phx_type(self):
        """ Return AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXDouble'

    def get(self, attr, path):
        """ Return attribute corresponding to `attr`. """
        if attr == 'value' or attr == 'valueStr':
            return _float2str(self._container.get(self._name))
        elif attr == 'enumAliases':
            return ''
        elif attr == 'enumValues':
            return ''
        elif attr == 'format':
            return 'null'
        elif attr == 'hasLowerBound':
            return 'false' if self._trait.low is None else 'true'
        elif attr == 'lowerBound':
            return '0.0' if self._trait.low is None else str(self._trait.low)
        elif attr == 'hasUpperBound':
            return 'false' if self._trait.high is None else 'true'
        elif attr == 'upperBound':
            return '0.0' if self._trait.high is None else str(self._trait.high)
        elif attr == 'units':
            try:
                units = self._trait.units
            except AttributeError:
                return ''
            return '' if units is None else units
        else:
            return super(FloatWrapper, self).get(attr, path)

    def get_as_xml(self):
        """ Return info in XML form. """
        return '<Variable name="%s" type="double" io="%s" format=""' \
               ' description=%s units="%s">%s</Variable>' \
               % (self._ext_name, self._io, quoteattr(self._trait.desc),
                  self.get('units', self._ext_path),
                  self.get('value', self._ext_path))

    def set(self, attr, path, valstr):
        """ Set attribute corresponding to `attr` to `valstr`. """
        valstr = valstr.strip('"')
        if attr == 'value':
            self._container.set(self._name, float(valstr))
        elif attr in ('valueStr', 'description', 'enumAliases', 'enumValues'
                      'format', 'hasLowerBound', 'lowerBound',
                      'hasUpperBound', 'upperBound', 'units'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        return ['description (type=java.lang.String) (access=g)',
                'enumAliases (type=java.lang.String[0]) (access=g)',
                'enumValues (type=double[0]) (access=g)',
                'format (type=java.lang.String) (access=g)',
                'hasLowerBound (type=boolean) (access=g)',
                'hasUpperBound (type=boolean) (access=g)',
                'lowerBound (type=double) (access=g)',
                'units (type=java.lang.String) (access=g)',
                'upperBound (type=double) (access=g)',
                'value (type=double) (access=%s)' % self._access,
                'valueStr (type=java.lang.String) (access=g)']

TYPE_MAP[Float] = FloatWrapper


class IntWrapper(BaseWrapper):
    """ Wrapper for `Int` providing ``PHXLong`` interface. """

    @property
    def phx_type(self):
        """ Return AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXLong'

    def get(self, attr, path):
        """ Return attribute corresponding to `attr`. """
        if attr == 'value' or attr == 'valueStr':
            return str(self._container.get(self._name))
        elif attr == 'enumAliases':
            return ''
        elif attr == 'enumValues':
            return ''
        elif attr == 'hasUpperBound':
            return 'false' if self._trait.high is None else 'true'
        elif attr == 'upperBound':
            return '0' if self._trait.high is None else str(self._trait.high)
        elif attr == 'hasLowerBound':
            return 'false' if self._trait.low is None else 'true'
        elif attr == 'lowerBound':
            return '0' if self._trait.low is None else str(self._trait.low)
        elif attr == 'units':
            return ''
        else:
            return super(IntWrapper, self).get(attr, path)

    def get_as_xml(self):
        """ Return info in XML form. """
        return '<Variable name="%s" type="long" io="%s" format=""' \
               ' description=%s>%s</Variable>' \
               % (self._ext_name, self._io, quoteattr(self._trait.desc),
                  self.get('value', self._ext_path))

    def set(self, attr, path, valstr):
        """ Set attribute corresponding to `attr` to `valstr`. """
        valstr = valstr.strip('"')
        if attr == 'value':
            self._container.set(self._name, int(valstr))
        elif attr in ('valueStr', 'description', 'enumAliases', 'enumValues'
                      'hasLowerBound', 'lowerBound',
                      'hasUpperBound', 'upperBound', 'units'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        return ['description (type=java.lang.String) (access=g)',
                'enumAliases (type=java.lang.String[0]) (access=g)',
                'enumValues (type=long[0]) (access=g)',
                'hasLowerBound (type=boolean) (access=g)',
                'hasUpperBound (type=boolean) (access=g)',
                'lowerBound (type=long) (access=g)',
                'units (type=java.lang.String) (access=g)',
                'upperBound (type=long) (access=g)',
                'value (type=long) (access=%s)' % self._access,
                'valueStr (type=java.lang.String) (access=g)']

TYPE_MAP[Int] = IntWrapper


class StrWrapper(BaseWrapper):
    """ Wrapper for `Str` providing ``PHXString`` interface. """

    @property
    def phx_type(self):
        """ Return AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXString'

    def get(self, attr, path):
        """ Return attribute corresponding to `attr`. """
        if attr == 'value' or attr == 'valueStr':
            return self._container.get(self._name)
        elif attr == 'enumValues':
            return ''
        elif attr == 'enumAliases':
            return ''
        else:
            return super(StrWrapper, self).get(attr, path)

    def get_as_xml(self):
        """ Return info in XML form. """
        return '<Variable name="%s" type="string" io="%s" format=""' \
               ' description=%s>%s</Variable>' \
               % (self._ext_name, self._io, quoteattr(self._trait.desc),
                  escape(self.get('value', self._ext_path)))

    def set(self, attr, path, valstr):
        """ Set attribute corresponding to `attr` to `valstr`. """
        valstr = valstr.strip('"')
        if attr == 'value':
            self._container.set(self._name, valstr)
        elif attr in ('valueStr', 'description', 'enumAliases', 'enumValues'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        return ['description (type=java.lang.String) (access=g)',
                'enumAliases (type=java.lang.String[0]) (access=g)',
                'enumValues (type=java.lang.String[0]) (access=g)',
                'value (type=java.lang.String) (access=%s)' % self._access,
                'valueStr (type=java.lang.String) (access=g)']

TYPE_MAP[Str] = StrWrapper

