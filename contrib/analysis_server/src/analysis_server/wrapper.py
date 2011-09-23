"""
Wrappers for OpenMDAO components and variables.
Component wrappers are created by the ``start`` command after the associated
component's server has been started.
Variable wrappers are created on demand when a wrapped component's variable
is referenced.
"""

import base64
import cStringIO
import gzip
import numpy
import os
import sys
import time
import xml.etree.cElementTree as ElementTree
from xml.sax.saxutils import escape, quoteattr

try:
    import resource
except ImportError:  # pragma no cover
    pass  # Not available on Windows.

from openmdao.main.api import Container, FileRef
from openmdao.main.attrwrapper import AttrWrapper
from openmdao.main.mp_support import is_instance

from openmdao.lib.datatypes.api import Array, Bool, Enum, File, Float, Int, \
                                       List, Str

from analysis_server.monitor import FileMonitor
from analysis_server.objxml import get_as_xml, set_from_xml

class WrapperError(Exception):
    """ Denotes wrapper-specific errors. """
    pass


class _GzipFile(gzip.GzipFile):
    """ Class to patch _read_eof() for Python prior to 2.7. """

    def _read_eof(self):
        """ Handle zero padding at end of file for Python prior to 2.7. """
        gzip.GzipFile._read_eof(self)
        if float(sys.version[:3]) < 2.7:
            # Gzip files can be padded with zeroes and still have archives.
            # Consume all zero bytes and set the file position to the first
            # non-zero byte. See http://www.gzip.org/#faq8
            c = "\x00"
            while c == "\x00":
                c = self.fileobj.read(1)
            if c:
                self.fileobj.seek(-1, 1)


# Mapping from OpenMDAO variable type to wrapper type.
_TYPE_MAP = {}

def _register(typ, wrapper):
    """
    Register `wrapper` for `typ`.

    typ: Python type
        Type to be registered.

    wrapper: Python type
        Wrapper class to associate with `typ`.
    """
    typename = '%s.%s' % (typ.__module__, typ.__name__)
    _TYPE_MAP[typename] = wrapper

def lookup(typenames):
    """
    Return wrapper for first supported type in `typenames`.

    typenames: list[string]
        Python type names to be checked.
    """
    for name in typenames:
        if name in _TYPE_MAP:
            return _TYPE_MAP[name]
    return None


def _float2str(val):
    """
    Return accurate string value for float.

    val: float
        Value to format.
    """
    return '%.16g' % val


class ComponentWrapper(object):
    """
    Component wrapper providing a ModelCenter AnalysisServer interface,
    based on the protocol described in:
    http://www.phoenix-int.com/~AnalysisServer/commands/index.html

    Wraps component `comp`, named `name`, with configuraton `cfg` on `server`.
    `send_reply` and `send_exc` are used to communicate back to the client.

    name: string
        Instance name.

    comp: proxy
        Proxy to remote component.

    cfg: :class:`server._WrapperConfig`
        Component configuration data.

    server: proxy
        Proxy to remote server hosting remote component.

    send_reply: callable
        Used to send a reply message back to client.

    send_exc: callable
        Used to send an exception message back to client.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    def __init__(self, name, comp, cfg, server, send_reply, send_exc, logger):
        self._name = name
        self._comp = comp
        self._cfg = cfg
        self._server = server
        self._send_reply = send_reply
        self._send_exc = send_exc
        self._logger = logger
        self._monitors = {}  # Maps from monitor_id to monitor.
        self._wrappers = {}  # Maps from internal var path to var wrapper.
        self._path_map = {}  # Maps from external path to (var wrapper, attr).
        self._start = None
        self._rusage = None  # For ps() on UNIX.

    def _get_var_wrapper(self, ext_path):
        """
        Return '(wrapper, attr)' for `ext_path`.

        ext_path: string
            External reference for variable.
        """
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
                    if not dot:
                        break
                    try:
                        obj = getattr(obj, name)
                    except Exception:
                        break  # Unserializable => not Container.
                iotype = 'in' if epath in self._cfg.inputs else 'out'
                # Determine variable type.
                typenames = container.get_trait_typenames(rpath, iotype)
                if not typenames:
                    raise WrapperError('no such trait %r %r.'
                                       % (container.get_pathname(), rpath))
                wrapper_class = lookup(typenames)
                if wrapper_class is None:
                    raise WrapperError('%s: unsupported variable type %r.'
                                       % (ext_path, typenames[0]))
                # Wrap it.
                wrapper = wrapper_class(container, rpath, epath, self._logger)
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
        """
        Runs a component instance.

        req_id: string
            'Raw' mode request identifier.
        """
        try:
            if sys.platform != 'win32':
                self._rusage = resource.getrusage(resource.RUSAGE_SELF)

            self._start = time.time()
            try:
                self._comp.run()
            except Exception as exc:
                self._logger.exception('run() failed:')
                raise WrapperError('%s' % exc)
            else:
                self._send_reply('%s completed.' % self._name, req_id)
            finally:
                self._start = None
        except Exception as exc:
            self._send_exc(exc, req_id)

    def get(self, path, req_id):
        """
        Returns the value of a variable.

        path: string
            External variable reference.

        req_id: string
            'Raw' mode request identifier.
        """
        try:
            wrapper, attr = self._get_var_wrapper(path)
            self._send_reply(wrapper.get(attr, path), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def get_hierarchy(self, req_id, gzipped):
        """
        Return all inputs & outputs as XML.

        req_id: string
            'Raw' mode request identifier.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        try:
            group = ''
            lines = []
            lines.append("<?xml version='1.0' encoding='utf-8'?>")
            lines.append('<Group>')
            for path in sorted(self._cfg.properties.keys()):
                wrapper, attr = self._get_var_wrapper(path)
                prefix, dot, name = path.rpartition('.')
                if prefix != group:
                    while not prefix.startswith(group):  # Exit subgroups.
                        lines.append('</Group>')
                        group, dot, name = group.rpartition('.')
                    name, dot, rest = prefix.partition('.')
                    if name:
                        lines.append('<Group name="%s">' % name)
                    while rest:  # Enter subgroups.
                        name, dot, rest = rest.partition('.')
                        lines.append('<Group name="%s">' % name)
                    group = prefix
                try:
                    lines.append(wrapper.get_as_xml(gzipped))
                except Exception as exc:
                    raise type(exc)("Can't get %r: %s" % (path, exc))
            lines.append('</Group>')
            self._send_reply('\n'.join(lines), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def invoke(self, method, full, req_id):
        """
        Invokes a method on a component instance.

        method: string
            External method reference.

        full: bool
            If True, return result as XML.

        req_id: string
            'Raw' mode request identifier.
        """
        try:
            try:
                attr = self._cfg.methods[method]
            except KeyError:
                raise WrapperError('no such method <%s>.' % method)

            meth = getattr(self._comp, attr)
            result = meth()
            if result is None:
                reply = ''
            elif isinstance(result, float):
                reply = _float2str(result)
            elif isinstance(result, basestring):
                reply = result.encode('string_escape')
            else:
                reply = str(result)

            # Setting 'download' True since we have no idea about side-effects.
            if full:
                reply = """\
<?xml version="1.0" encoding="UTF-8" standalone="no" ?>\
<response>\
<version>100.0</version>\
<download>true</download>\
<string>%s</string>\
</response>""" % escape(reply)

            self._send_reply(reply, req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def list_array_values(self, path, req_id):
        """
        Lists all the values of an array variable.

        path: string
            External reference to array.

        req_id: string
            'Raw' mode request identifier.
        """
        try:
            raise NotImplementedError('listArrayValues')
        except Exception as exc:
            self._send_exc(exc, req_id)

    def list_methods(self, full, req_id):
        """
        Lists all methods available on a component instance.

        full: bool
            If True, include 'full/long' name.

        req_id: string
            'Raw' mode request identifier.
        """
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
        """
        Lists all available monitorable items on a component instance.

        req_id: string
            'Raw' mode request identifier.
        """
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

        path: string
            External reference.

        req_id: string
            'Raw' mode request identifier.
        """
        try:
            self._send_reply(self._list_properties(path), req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def _list_properties(self, path):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable.

        path: string
            External reference.
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

        path: string
            External reference.

        req_id: string
            'Raw' mode request identifier.
        """
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
                    if path:
                        continue  # No sub_props.

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

    def list_values_url(self, path, req_id):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable. This version supplies a URL for file data
        if DirectFileTransfer is supported.

        path: string
            External reference.

        req_id: string
            'Raw' mode request identifier.
        """
        self.list_values(path, req_id)

    def start_monitor(self, path, req_id):
        """
        Starts a monitor on a raw output file or available monitor.

        path: string
            Monitor reference.

        req_id: string
            'Raw' mode request identifier.
        """
        try:
            path = os.path.join(self._comp.get_abs_directory(), path)
            monitor = FileMonitor(self._server, path, 'r',
                                  req_id, self._send_reply)
            monitor.start()
            self._monitors[str(req_id)] = monitor  # Monitor id is request id.
        except Exception as exc:
            self._send_exc(exc, req_id)

    def stop_monitor(self, monitor_id, req_id):
        """
        Stops a monitor on a raw output file or available monitor.

        monitor_id: string
            Monitor identifier.

        req_id: string
            'Raw' mode request identifier.
        """
        try:
            monitor = self._monitors.pop(monitor_id)
        except KeyError:
            raise WrapperError('No registered monitor for %r' % monitor_id)
        else:
            monitor.stop()
            self._send_reply('', req_id)

    def ps(self, req_id):
        """
        Lists all running processes for a component instance.

        req_id: string
            'Raw' mode request identifier.
        """
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
        """
        Sets the value of `path` to `valstr`.

        path: string
            External reference to variable.

        valstr: string
            Value to set.

        req_id: string
            'Raw' mode request identifier.
        """
        # Quotes around the value are semi-optional.
        if valstr.startswith('"') and valstr.endswith('"'):
            valstr = valstr[1:-1]
        try:
            self._set(path, valstr)
            self._send_reply('value set for <%s>' % path, req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)

    def _set(self, path, valstr, gzipped=False):
        """
        Sets the value of `path` to `valstr`.

        path: string
            External reference to variable.

        valstr: string
            Value to set.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        wrapper, attr = self._get_var_wrapper(path)
        wrapper.set(attr, path, valstr, gzipped)

    def set_hierarchy(self, xml, req_id):
        """
        Set hierarchy of variable values from `xml`.

        xml: string
            XML describing values to be set.

        req_id: string
            'Raw' mode request identifier.
        """
        try:
            header, newline, xml = xml.partition('\n')
            root = ElementTree.fromstring(xml)
            for var in root.findall('Variable'):
                valstr = var.text or ''
                if var.get('gzipped', 'false') == 'true':
                    gzipped = True
                else:
                    gzipped = False
                try:
                    self._set(var.attrib['name'], valstr, gzipped)
                except Exception as exc:
                    self._logger.exception("Can't set %r", var.attrib['name'])
                    raise type(exc)("Can't set %r from %r: %s" 
                                    % (var.attrib['name'], valstr[:1000], exc))
            self._send_reply('values set', req_id)
        except Exception as exc:
            self._send_exc(exc, req_id)


class BaseWrapper(object):
    """
    Base class for variable wrappers.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    def __init__(self, container, name, ext_path, logger):
        self._container = container
        self._name = name
        self._ext_path = ext_path
        self._ext_name = ext_path.rpartition('.')[2]
        self._logger = logger
        trait = container.get_dyn_trait(name)
        self._trait = trait
        self._access = 'sg' if trait.iotype == 'in' else 'g'
        self._io = 'input' if trait.iotype == 'in' else 'output'

    @property
    def phx_access(self):
        """ AnalysisServer access string. """
        return self._access

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
        if attr == 'description':
            valstr = self._trait.desc or ''
            return valstr.encode('string_escape')
        else:
            raise WrapperError('no such property <%s>.' % path)


class ArrayBase(BaseWrapper):
    """
    Base for wrappers providing double[], long[], or String[] interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.

    typ: Python type
        Element type.

    is_array: bool
        If True, numpy ndarray, else List.
    """

    def __init__(self, container, name, ext_path, logger, typ, is_array):
        super(ArrayBase, self).__init__(container, name, ext_path, logger)
        self.typ = typ
        if typ is float:
            self._typstr = 'double'
        elif typ is int:
            self._typstr = 'long'
        elif typ is str:
            self._typstr = 'string'
        self._is_array = is_array

    @property
    def phx_type(self):
        """ AnalysisServer type string for value. """
        value = self._container.get(self._name)
        if self._is_array:
            dims = '[%s]' % ']['.join(['%d' % dim for dim in value.shape])
            if self.typ == float:
                return 'double%s' % dims
            elif self.typ == int:
                return 'long%s' % dims
            else:
                return 'java.lang.String%s' % dims
        else:
            if self.typ == float:
                return 'double[%d]' % len(value)
            elif self.typ == int:
                return 'long[%d]' % len(value)
            else:
                return 'java.lang.String[%d]' % len(value)

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
        if attr == 'value':
            value = self._container.get(self._name)
            if self.typ == float:
                fmt = '%.16g'
            elif self.typ == int:
                fmt = '%d'
            else:
                fmt = '"%s"'
            if self._is_array and len(value.shape) > 1:
                valstr = 'bounds[%s] {%s}' % (
                         ', '.join(['%d' % dim for dim in value.shape]),
                         ', '.join([fmt % val for val in value.flat]))
            else:
                valstr = ', '.join([fmt % val for val in value])
            if self.typ == str:
                valstr = valstr.encode('string_escape')
            return valstr
        elif attr == 'componentType':
            return self._typstr
        elif attr == 'dimensions':
            value = self._container.get(self._name)
            if self._is_array:
                return ', '.join(['"%d"' % dim for dim in value.shape])
            else:
                return '"%d"' % len(value)
        elif attr == 'enumAliases':
            return ''
        elif attr == 'enumValues':
            return ''
        elif attr == 'first':
            value = self._container.get(self._name)
            if len(value):
                if self._is_array and len(value.shape) > 1:
                    first = '%s' % value.flat[0]
                else:
                    first = '%s' % value[0]
                if self.typ == str:
                    first = first.encode('string_escape')
            else:
                first = ''
            return first
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
            if self._is_array:
                return '%d' % value.shape[0]
            else:
                return '%d' % len(value)
        elif attr == 'lockResize':
            return 'true' if self._is_array else 'false'
        elif attr == 'numDimensions':
            if self._is_array:
                value = self._container.get(self._name)
                return '%d' % len(value.shape)
            else:
                return '1'
        elif attr == 'units':
            if self.typ == float:
                return '' if self._trait.units is None else self._trait.units
            else:
                return ''
        else:
            return super(ArrayBase, self).get(attr, path)

    def get_as_xml(self, gzipped):
        """
        Return info in XML form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        return '<Variable name="%s" type="%s[]" io="%s" format=""' \
               ' description=%s units="%s">%s</Variable>' \
               % (self._ext_name, self._typstr, self._io,
                  quoteattr(self.get('description', self._ext_path)),
                  self.get('units', self._ext_path),
                  escape(self.get('value', self._ext_path)))

    def set(self, attr, path, valstr, gzipped):
        """
        Set attribute corresponding to `attr` to `valstr`.

        attr: string
            Name of property.

        path: string
            External reference to property.

        valstr: string
            Value to be set, in string form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        if attr == 'value':
            if self.typ == str:
                valstr = valstr.decode('string_escape')
            if self._is_array:
                if valstr.startswith('bounds['):
                    dims, rbrack, rest = valstr[7:].partition(']')
                    dims = [int(val.strip(' "')) for val in dims.split(',')]
                    junk, lbrace, rest = rest.partition('{')
                    data, rbrace, rest = rest.partition('}')
                    value = numpy.array([self.typ(val.strip(' "'))
                                         for val in data.split(',')]).reshape(dims)
                else:
                    value = numpy.array([self.typ(val.strip(' "'))
                                         for val in valstr.split(',')])
            else:
                value = [self.typ(val.strip(' "')) for val in valstr.split(',')]
            self._container.set(self._name, value)
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
                 'description (type=java.lang.String) (access=g)',
                 'dimensions (type=int[1]) (access=g)',
                 'enumAliases (type=java.lang.String[0]) (access=g)',
                 'enumValues (type=%s[0]) (access=g)' % typstr,
                 'first (type=java.lang.Object) (access=g)',
                 'length (type=int) (access=g)',
                 'lockResize (type=boolean) (access=g)',
                 'numDimensions (type=int) (access=g)',
                 'units (type=java.lang.String) (access=g)']

        if self.typ != str:
            lines.extend(['format (type=java.lang.String) (access=g)',
                          'hasLowerBound (type=boolean) (access=g)',
                          'hasUpperBound (type=boolean) (access=g)',
                          'lowerBound (type=%s) (access=g)' % typstr,
                          'upperBound (type=%s) (access=g)' % typstr])

        return sorted(lines)


class ArrayWrapper(ArrayBase):
    """
    Wrapper for `Array` providing double[], long[], or String[] interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    # Map from numpy dtype.kind to scalar converter.
    _converters = {'f':float, 'i':int, 'S':str}

    def __init__(self, container, name, ext_path, logger):
        value = container.get(name)
        kind = value.dtype.kind
        try:
            typ = self._converters[kind]
        except KeyError:
            raise WrapperError('Unsupported dtype for %s.%s: %r (%r)'
                               % (container.get_pathname(), name,
                                  value.dtype, kind))

        super(ArrayWrapper, self).__init__(container, name, ext_path, logger,
                                           typ, is_array=True)

_register(Array, ArrayWrapper)


class ListWrapper(ArrayBase):
    """
    Wrapper for `List` providing double[], long[], or String[] interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    def __init__(self, container, name, ext_path, logger):
        value = container.get(name)
        if value:
            typ = type(value[0])
        else:
            typ = str  # HACK!
        super(ListWrapper, self).__init__(container, name, ext_path, logger,
                                          typ, is_array=False)

_register(List, ListWrapper)


class BoolWrapper(BaseWrapper):
    """
    Wrapper for `Bool` providing ``PHXBoolean`` interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    @property
    def phx_type(self):
        """ AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXBoolean'

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
        if attr == 'value' or attr == 'valueStr':
            return 'true' if self._container.get(self._name) else 'false'
        else:
            return super(BoolWrapper, self).get(attr, path)

    def get_as_xml(self, gzipped):
        """
        Return info in XML form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        return '<Variable name="%s" type="boolean" io="%s" format=""' \
               ' description=%s>%s</Variable>' \
               % (self._ext_name, self._io,
                  quoteattr(self.get('description', self._ext_path)),
                  self.get('value', self._ext_path))

    def set(self, attr, path, valstr, gzipped):
        """
        Set attribute corresponding to `attr` to `valstr`.

        attr: string
            Name of property.

        path: string
            External reference to property.

        valstr: string
            Value to be set, in string form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
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
        return ('description (type=java.lang.String) (access=g)',
                'value (type=boolean) (access=%s)' % self._access,
                'valueStr (type=boolean) (access=g)')

_register(Bool, BoolWrapper)


class EnumWrapper(BaseWrapper):
    """
    Wrapper for `Enum` providing ``PHXDouble``, ``PHXLong``, or ``PHXString``
    interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    def __init__(self, container, name, ext_path, logger):
        super(EnumWrapper, self).__init__(container, name, ext_path, logger)
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
        """ AnalysisServer type string for value. """
        return self._phx_type

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
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
                return ', '.join([_float2str(value)
                                  for value in self._trait.values])
            elif self._py_type == int:
                return ', '.join(['%s' % value
                                  for value in self._trait.values])
            else:
                return ', '.join(['"%s"' % value
                                  for value in self._trait.values])
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
                return self._trait.units or ''
            else:
                return ''
        else:
            return super(EnumWrapper, self).get(attr, path)

    def get_as_xml(self, gzipped):
        """
        Return info in XML form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        if self._py_type == float:
            typstr = 'double'
        elif self._py_type == int:
            typstr = 'long'
        else:
            typstr = 'string'
        return '<Variable name="%s" type="%s" io="%s" format=""' \
               ' description=%s units="%s">%s</Variable>' \
               % (self._ext_name, typstr, self._io,
                  quoteattr(self.get('description', self._ext_path)),
                  self.get('units', self._ext_path),
                  escape(self.get('value', self._ext_path)))

    def set(self, attr, path, valstr, gzipped):
        """
        Set attribute corresponding to `attr` to `valstr`.

        attr: string
            Name of property.

        path: string
            External reference to property.

        valstr: string
            Value to be set, in string form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
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

_register(Enum, EnumWrapper)


class FileWrapper(BaseWrapper):
    """
    Wrapper for `File` providing ``PHXRawFile`` interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    def __init__(self, container, name, ext_path, logger):
        super(FileWrapper, self).__init__(container, name, ext_path, logger)
        self._server = None
        self._owner = None

    @property
    def phx_type(self):
        """ AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXRawFile'

    def set_server(self, server):
        """
        Set container's server to `server` for file operations.

        server: proxy
            Proxy to the server hosting this file.
        """
        self._server = server
        owner = self._container
        while owner is not None:
            if hasattr(owner, 'get_abs_directory'):
                self._owner = owner
                break
            if hasattr(owner, 'parent'):
                owner = owner.parent

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
        if attr == 'value':
            file_ref = self._container.get(self._name)
            if file_ref is None:
                return ''
            try:
                with file_ref.open() as inp:
                    data = inp.read()
            except IOError as exc:
                self._logger.warning('get %s.value: %r', path, exc)
                return ''
            else:
                return data.encode('string_escape')
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

    def get_as_xml(self, gzipped):
        """
        Return info in XML form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        if gzipped:
            file_ref = self._container.get(self._name)
            if file_ref is None:
                data = ''
            else:
                data = cStringIO.StringIO()
                try:
                    with file_ref.open() as inp:
                        gz_file = _GzipFile(mode='wb', fileobj=data)
                        gz_file.writelines(inp)
                        gz_file.close()
                except IOError as exc:
                    self._logger.warning('get %s.value: %r', path, exc)
                    data = ''
                else:
                    data = base64.b64encode(data.getvalue())
            zipped=' gzipped="true"'
        else:
            data = escape(self.get('value', self._ext_path))
            zipped=''

        return '<Variable name="%s" type="file" io="%s" description=%s' \
               ' isBinary="%s" fileName=""%s>%s</Variable>' \
               % (self._ext_name, self._io,
                  quoteattr(self.get('description', self._ext_path)),
                  self.get('isBinary', self._ext_path),
                  zipped, data)

    def set(self, attr, path, valstr, gzipped):
        """
        Set attribute corresponding to `attr` to `valstr`.

        attr: string
            Name of property.

        path: string
            External reference to property.

        valstr: string
            Value to be set, in string form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        if attr == 'value':
            if self._trait.iotype != 'in':
                raise WrapperError('cannot set output <%s>.' % path)
            if self._trait.local_path:
                filename = self._trait.local_path
            else:
                left, dot, name = self._ext_path.rpartition('.')
                filename = '%s.dat' % name
            if not os.path.isabs(filename):
                filename = os.path.join(self._owner.get_abs_directory(),
                                        filename)
            if gzipped:
                data = cStringIO.StringIO(self._decode(valstr))
                gz_file = _GzipFile(mode='rb', fileobj=data)
                valstr = gz_file.read()
            else:
                valstr = valstr.strip('"').decode('string_escape')

            mode = 'wb'
            if self._server is None:  # Used during testing.
                with open(filename, mode) as out:
                    out.write(valstr)
            else:  # pragma no cover
                with self._server.open(filename, mode) as out:
                    out.write(valstr)
            file_ref = FileRef(path=filename, owner=self._owner)
            self._container.set(self._name, file_ref)
        elif attr in ('description', 'isBinary', 'mimeType',
                      'name', 'nameCoded', 'url'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    @staticmethod
    def _decode(data):
        """
        At times we receive data with incorrect padding. This code will
        keep truncating the data until it decodes. We hope the (un)gzip
        process will catch any erroneous result.

        data: string
            Data to be decoded.
        """
        while data:
            try:
                data = base64.b64decode(data)
            except TypeError:
                data = data[:-1]
            else:
                break
        return data

    def list_properties(self):
        """ Return lines listing properties. """
        return ('description (type=java.lang.String) (access=g)',
                'isBinary (type=boolean) (access=g)',
                'mimeType (type=java.lang.String) (access=g)',
                'name (type=java.lang.String) (access=g)',
                'nameCoded (type=java.lang.String) (access=g)',
                'url (type=java.lang.String) (access=g)')

_register(File, FileWrapper)


class FloatWrapper(BaseWrapper):
    """
    Wrapper for `Float` providing ``PHXDouble`` interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    @property
    def phx_type(self):
        """ AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXDouble'

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
        if attr == 'value' or attr == 'valueStr':
            val = self._container.get(self._name)
            if isinstance(val, AttrWrapper):
                val = val.value
            return _float2str(val)
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
                return self._trait.units or ''
            except AttributeError:
                return ''
        else:
            return super(FloatWrapper, self).get(attr, path)

    def get_as_xml(self, gzipped):
        """
        Return info in XML form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        return '<Variable name="%s" type="double" io="%s" format=""' \
               ' description=%s units="%s">%s</Variable>' \
               % (self._ext_name, self._io,
                  quoteattr(self.get('description', self._ext_path)),
                  self.get('units', self._ext_path),
                  self.get('value', self._ext_path))

    def set(self, attr, path, valstr, gzipped):
        """
        Set attribute corresponding to `attr` to `valstr`.

        attr: string
            Name of property.

        path: string
            External reference to property.

        valstr: string
            Value to be set, in string form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
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
        return ('description (type=java.lang.String) (access=g)',
                'enumAliases (type=java.lang.String[0]) (access=g)',
                'enumValues (type=double[0]) (access=g)',
                'format (type=java.lang.String) (access=g)',
                'hasLowerBound (type=boolean) (access=g)',
                'hasUpperBound (type=boolean) (access=g)',
                'lowerBound (type=double) (access=g)',
                'units (type=java.lang.String) (access=g)',
                'upperBound (type=double) (access=g)',
                'value (type=double) (access=%s)' % self._access,
                'valueStr (type=java.lang.String) (access=g)')

_register(Float, FloatWrapper)


class IntWrapper(BaseWrapper):
    """
    Wrapper for `Int` providing ``PHXLong`` interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    @property
    def phx_type(self):
        """ AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXLong'

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
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

    def get_as_xml(self, gzipped):
        """
        Return info in XML form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        return '<Variable name="%s" type="long" io="%s" format=""' \
               ' description=%s>%s</Variable>' \
               % (self._ext_name, self._io,
                  quoteattr(self.get('description', self._ext_path)),
                  self.get('value', self._ext_path))

    def set(self, attr, path, valstr, gzipped):
        """
        Set attribute corresponding to `attr` to `valstr`.

        attr: string
            Name of property.

        path: string
            External reference to property.

        valstr: string
            Value to be set, in string form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
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
        return ('description (type=java.lang.String) (access=g)',
                'enumAliases (type=java.lang.String[0]) (access=g)',
                'enumValues (type=long[0]) (access=g)',
                'hasLowerBound (type=boolean) (access=g)',
                'hasUpperBound (type=boolean) (access=g)',
                'lowerBound (type=long) (access=g)',
                'units (type=java.lang.String) (access=g)',
                'upperBound (type=long) (access=g)',
                'value (type=long) (access=%s)' % self._access,
                'valueStr (type=java.lang.String) (access=g)')

_register(Int, IntWrapper)


class StrWrapper(BaseWrapper):
    """
    Wrapper for `Str` providing ``PHXString`` interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    @property
    def phx_type(self):
        """ AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXString'

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
        if attr == 'value' or attr == 'valueStr':
            return self._container.get(self._name).encode('string_escape')
        elif attr == 'enumValues':
            return ''
        elif attr == 'enumAliases':
            return ''
        else:
            return super(StrWrapper, self).get(attr, path)

    def get_as_xml(self, gzipped):
        """
        Return info in XML form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        return '<Variable name="%s" type="string" io="%s" format=""' \
               ' description=%s>%s</Variable>' \
               % (self._ext_name, self._io,
                  quoteattr(self.get('description', self._ext_path)),
                  escape(self.get('value', self._ext_path)))

    def set(self, attr, path, valstr, gzipped):
        """
        Set attribute corresponding to `attr` to `valstr`.

        attr: string
            Name of property.

        path: string
            External reference to property.

        valstr: string
            Value to be set, in string form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        if attr == 'value':
            self._container.set(self._name,
                                valstr.decode('string_escape').strip('"'))
        elif attr in ('valueStr', 'description', 'enumAliases', 'enumValues'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        return ('description (type=java.lang.String) (access=g)',
                'enumAliases (type=java.lang.String[0]) (access=g)',
                'enumValues (type=java.lang.String[0]) (access=g)',
                'value (type=java.lang.String) (access=%s)' % self._access,
                'valueStr (type=java.lang.String) (access=g)')

_register(Str, StrWrapper)


class ObjWrapper(BaseWrapper):
    """
    Wrapper for a general object providing ``PHXScriptObject`` interface.

    container: proxy
        Proxy to remote parent container.

    name: string
        Name of variable.

    ext_path: string
        External reference to variable.

    logger: :class:`logging.Logger`
        Used for progress, errors, etc.
    """

    def __init__(self, container, name, ext_path, logger):
        super(ObjWrapper, self).__init__(container, name, ext_path, logger)
        obj = container.get(name)
        self._cls = type(obj)
        self._access = 'sg' if obj.iotype == 'in' else 'g'
        self._io = 'input' if obj.iotype == 'in' else 'output'

    @property
    def phx_type(self):
        """ AnalysisServer type string for value. """
        return 'com.phoenix_int.aserver.types.PHXScriptObject'

    def get(self, attr, path):
        """
        Return attribute corresponding to `attr`.

        attr: string
            Name of property.

        path: string
            External reference to property.
        """
        if attr == 'value':
            obj = self._container.get(self._name)
            xml = get_as_xml(obj, self._name)
            return xml
        elif attr == 'classURL':
            path = sys.modules[self._cls.__module__].__file__
            if path.endswith(('.pyc', '.pyo')):
                path = path[:-1]
            path = os.path.abspath(path)
            return '%s#%s' % (path, self._cls.__name__)
        else:
            return super(ObjWrapper, self).get(attr, path)

    def get_as_xml(self, gzipped):
        """
        Return info in XML form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        return '<Variable name="%s" type="object" io="%s"' \
               ' description=%s>%s</Variable>' \
               % (self._ext_name, self._io,
                  quoteattr(self.get('description', self._ext_path)),
                  escape(self.get('value', self._ext_path)))

    def set(self, attr, path, valstr, gzipped):
        """
        Set attribute corresponding to `attr` to `valstr`.

        attr: string
            Name of property.

        path: string
            External reference to property.

        valstr: string
            Value to be set, in string form.

        gzipped: bool
            If True, file data is gzipped and then base64 encoded.
        """
        if attr == 'value':
            obj = self._cls()
            set_from_xml(obj, valstr.decode('string_escape'))
            self._container.set(self._name, obj)
        elif attr in ('classURL', 'description'):
            raise WrapperError('cannot set <%s>.' % path)
        else:
            raise WrapperError('no such property <%s>.' % path)

    def list_properties(self):
        """ Return lines listing properties. """
        return ('classURL (type=java.lang.String) (access=g)',
                'description (type=java.lang.String) (access=g)')

_register(Container, ObjWrapper)

