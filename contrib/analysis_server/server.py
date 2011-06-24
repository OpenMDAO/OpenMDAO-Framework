"""
Server providing a ModelCenter AnalysisServer interface, based on the
protocol described in:
http://www.phoenix-int.com/~AnalysisServer/commands/index.html

Component types to be supported are described by ``<name>.cfg`` files parsed
by :class:`ConfigParser.SafeConfigParser`, for example:

.. parsed-literal::

    [Description]
    # Metadata describing the component.
    version: 0.1
    description: Component for testing AnalysisServer functionality.
    help_url: unknown
    keywords:
    requirements:

    [Python]
    # Information for creating an instance.
    filename: ASTestComp.py
    classname: TestComponent

    [Inputs]
    # Mapping from ModelCenter name to OpenMDAO name.
    # *: *                    To allow any valid input, using the same path.
    # <path>: *               To allow <path> as an input.
    # <ext_path>: <int_path>  To access <int_path> via <ext_path>
    *: *

    [Outputs]
    # Mapping from ModelCenter name to OpenMDAO name.
    # *: *                    To allow any valid output, using the same path.
    # <path>: *               To allow <path> as an output.
    # <ext_path>: <int_path>  To access <int_path> via <ext_path>
    *: *

    [Methods]
    # Methods which may be invoked by ModelCenter.
    # *: *                    To allow any valid method, using the same name.
    # <name>: *               To allow <name> to be invoked.
    # <ext_name>: <int_name>  To invoke <int_name> via <ext_name>
    *: *

"""

import ConfigParser
import getpass
import glob
import inspect
import logging
import optparse
import os.path
import pkg_resources
import platform
import re
import signal
import SocketServer
import socket
import string
import sys
import threading
import time
import traceback

if sys.platform != 'win32':
    import pwd

from openmdao.main.api import Component, Container, set_as_top
from openmdao.main.mp_util import read_allowed_hosts
from openmdao.main.rbac import get_credentials, set_credentials
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.main.resource import LocalAllocator

from openmdao.util.filexfer import filexfer
from openmdao.util.publickey import make_private, HAVE_PYWIN32
from openmdao.util.shellproc import ShellProc, STDOUT
from openmdao.util.wrkpool import WorkerPool

from monitor import Heartbeat
from stream  import Stream
from wrapper import ComponentWrapper, TYPE_MAP

DEFAULT_PORT = 1835
ERROR_PREFIX = 'ERROR: '

# Attributes to be ignored (everything in a 'vanilla' component).
_IGNORE_ATTR = set()

_COMMANDS = {}  # Maps from command string to command handler.

_DISABLE_HEARTBEAT = False  # If True, no heartbeat replies are sent.


class Server(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    """
    Server to process client requests. `host` is the host name or IP address
    of the host to connect to and `port` is the port to use. `allowed_hosts`
    is a list of allowed host or domain addresses (domain addresses must end
    in '.'). If None, '127.0.0.1' is used (only the local host is allowed
    access).
    """

    allow_reuse_address = True

    def __init__(self, host='localhost', port=DEFAULT_PORT, allowed_hosts=None):
        SocketServer.TCPServer.__init__(self, (host, port), _Handler)
        self._allowed_hosts = allowed_hosts or ['127.0.0.1']
        self._num_clients = 0
        self._components = {}  # Maps from category/component to (cls, config)
        self._eggs = {}        # Maps from cls to egg-info.
        self.handlers = {}     # Maps from client address to handler.
        self._credentials = get_credentials()  # For PublicKey servers.
        self._config_errors = 0
        self._read_configuration()

    @property
    def num_clients(self):
        """ Return number of clients. """
        return self._num_clients

    @property
    def components(self):
        """ Return component map. """
        return self._components

    @property
    def credentials(self):
        """ Return credentials. """
        return self._credentials

    @property
    def eggs(self):
        """ Return egg map. """
        return self._eggs

    @property
    def config_errors(self):
        """ Return number of configuration errors detected. """
        return self._config_errors

    def _read_configuration(self):
        """ Read component configuration files. """
        for dirpath, dirnames, filenames in os.walk('.'):
            for name in sorted(filenames):
                if name.endswith('.cfg'):
                    path = os.path.join(dirpath, name)
                    path = path.lstrip('.').lstrip(os.sep)
                    try:
                        self._read_config(path)
                    except Exception as exc:
                        logging.error(str(exc))
                        self._config_errors += 1

    def _read_config(self, path):
        """ Read component configuration file. """
        logging.info('Reading config file %r', path)
        config = ConfigParser.SafeConfigParser()
        config.optionxform = str  # Preserve case.
        try:
            config.read(path)
        except Exception as exc:
            raise RuntimeError("Can't read %r: %r" % (path, exc))

        orig = os.getcwd()
        directory = os.path.dirname(path)
        if directory:
            os.chdir(directory)
        try:
            self._process_config(config, path)
        finally:
            os.chdir(orig)

    def _process_config(self, config, path):
        """ Process data read into `config` from `path`. """
        for sect in ('Python', 'Description', 'Inputs', 'Outputs', 'Methods'):
            if not config.has_section(sect):
                raise RuntimeError("No %s section in %r" % (sect, path))

        if config.has_option('Python', 'egg'):
            # Create temporary instance from egg.
            egg = config.get('Python', 'egg')
            obj = Container.load_from_eggfile(egg)
        else:
            # Get Python class and create temporary instance.
            cwd = os.getcwd()
            filename = config.get('Python', 'filename')
            classname = config.get('Python', 'classname')
            dirname = os.path.dirname(filename)
            if dirname:
                dirname = os.path.join(cwd, dirname)
            else:
                dirname = cwd
            if not dirname in sys.path:
                logging.debug('    prepending %r to sys.path', dirname)
                sys.path.insert(0, dirname)
            modname = os.path.basename(filename)[:-3]  # drop '.py'
            try:
                __import__(modname)
            except ImportError as exc:
                raise RuntimeError("Can't import %r: %r" % (modname, exc))

            module = sys.modules[modname]
            try:
                cls = getattr(module, classname)
            except AttributeError as exc:
                raise RuntimeError("Can't get class %r in %r: %r" \
                                   % (classname, modname, exc))
            try:
                obj = cls()
            except Exception as exc:
                logging.error(traceback.format_exc())
                raise RuntimeError("Can't instantiate %s.%s: %r" \
                                   % (modname, classname, exc))

        # Create wrapper configuration object.
        set_as_top(obj)
        try:
            cfg = _WrapperConfig(config, obj,
                                 os.path.join(cwd, os.path.basename(path)))
        except Exception as exc:
            raise RuntimeError("Bad configuration in %r: %s" % (path, exc))

        # Register under path normalized to category/component form.
        path = path[:-4]  # Drop '.cfg'
        path = path.replace('\\', '/')  # Always use '/'.
        logging.debug('    registering %s: %s.%s', path, modname, classname)
        self._components[path] = (cls, cfg)

        obj.pre_delete()
        del obj

    def verify_request(self, request, client_address):
        """
        Returns True if the client at `client_address` is on a legal host.
        """
        host, port = client_address
        for pattern in self._allowed_hosts:
            if pattern[-1] == '.':  # Any host in domain.
                if host.startswith(pattern):
                    return True
            elif host == pattern:
                return True

        logging.warning('Rejecting connection from %s:%s', host, port)
        return False

    # This will be exercised by client side tests.
    def finish_request(self, request, client_address):  # pragma no cover
        """
        Overrides superclass to track active clients and cleanup
        upon client disconnect.
        """
        host, port = client_address
        logging.info('Connection from %s:%s', host, port)
        self._num_clients += 1
        try:
            SocketServer.TCPServer.finish_request(self, request, client_address)
        finally:
            logging.info('Disconnect %s:%s', host, port)
            self._num_clients -= 1
            try:  # It seems handler.finish() isn't called on disconnect...
                self.handlers[client_address].cleanup()
            except Exception, exc:
                logging.warning('Exception during handler cleanup: %r', exc)


class _Handler(SocketServer.BaseRequestHandler):
    """ Handles requests from a single client. """

    def setup(self):
        """ Initialize before :meth:`handle` is invoked. """
        self.server.handlers[self.client_address] = self
        self._stream = Stream(self.request)
        self._lock = threading.Lock()  # Synchronize access to reply stream.
        self._raw = False
        self._req = None
        self._req_id = None
        self._background = False
        self._heartbeat = None
        self._monitors = {}      # Maps from req_id to name.
        self._instance_map = {}  # Maps from name to (wrapper, worker).
        self._servers = {}       # Maps from wrapper to server.
        set_credentials(self.server.credentials)

        # Set False during some testing for coverage check.
        # Also avoids odd problems under nose suite test.
        self._server_per_obj = True

    def handle(self):
        """ Process any received requests. """
        self._send_reply("""\
Welcome to the OpenMDAO Analysis Server.
version: 0.1""")

        try:
            while self._req != 'quit':
                try:
                    if self._raw:
                        logging.debug('Waiting for raw-mode request...')
                        req, req_id, background = self._stream.recv_request()
                        logging.debug('Request from %s: %r (id %s bg %s)',
                                      self.client_address, req,
                                      req_id, background)
                        self._req_id = req_id
                        self._background = background
                    else:
                        logging.debug('Waiting for request...')
                        req = self._stream.recv_request()
                        logging.debug('Request from %s: %r',
                                      self.client_address, req)
                        self._req_id = None
                        self._background = False

                    # Just being defensive.
                    if not req:  # pragma no cover
                        continue

                    args = req.split()
                    self._req = req
                    try:
                        cmd = _COMMANDS[args[0]]
                    except KeyError:
                        self._send_error('command <%s> not recognized'
                                         % req.strip())
                        continue

                    try:
                        cmd(self, args[1:])
                    except Exception as exc:
                        self._send_exc(exc)

                except EOFError:
                    break
        finally:
            self.cleanup()

    def cleanup(self):
        """ 'end' all existing objects. """
        if self._heartbeat is not None:
            self._heartbeat.stop()
        for name in self._instance_map.keys():
            self.__end(name)

    def _get_component(self, typ):
        """ Return '(cls, cfg)' for `typ`. """
        typ = typ.strip('"').lstrip('/')
        try:
            return self.server.components[typ]
        except KeyError:
            if not '/' in typ:  # Just to match real AnalysisServer.
                typ = '/'+typ
            self._send_error('component <%s> does not match a known component'
                             % typ)
            return (None, None)

    def _get_wrapper(self, name, background=False):
        """
        Return (wrapper, worker) for component `name`.
        If `background` and the request is not backgrounded, wait for
        the normal worker to complete before returning the background
        worker. This currently only occurs in the rare case of
        ``execute comp &``.
        """
        try:
            wrapper, sync_worker = self._instance_map[name]
        except KeyError:
            self._send_error('no such object: <%s>' % name)
            return (None, None)

        if self._background:
            worker = WorkerPool.get(one_shot=True)
        else:
            worker = sync_worker
            if background:
                worker.join()
                worker = WorkerPool.get(one_shot=True)
        return (wrapper, worker)

    def _send_reply(self, reply, req_id=None):
        """ Send reply to client, with optional logging. """
        if self._raw:
            req_id = req_id or self._req_id
            logging.debug('(req_id %s)\n%s', req_id, reply)
        else:
            logging.debug('    %s', reply)
        with self._lock:
            self._stream.send_reply(reply, req_id)

    def _send_error(self, reply, req_id=None):
        """ Send error reply to client, with optional logging. """
        if self._raw:
            req_id = req_id or self._req_id
            logging.error('%s (id %s)', reply, req_id)
        else:
            logging.error('%s', reply)
        reply = ERROR_PREFIX+reply
        with self._lock:
            self._stream.send_reply(reply, req_id, 'error')

    def _send_exc(self, exc, req_id=None):
        """ Send exception reply to client, with optional logging. """
        self._send_error('Exception: %r' % exc, req_id)
        logging.error(traceback.format_exc())


    def _add_proxy_clients(self, args):
        """
        Adds one or more host IDs to the list of client hosts in the proxy
        chain between client and server. This supports access restrictions
        based on all hosts between client and server.
        """
        if len(args) < 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'addProxyClients <clientHost1>, ...')
            return

        # Currently no access restrictions to enforce.
        self._send_reply('Client hosts added.')

    _COMMANDS['addProxyClients'] = _add_proxy_clients


    def _describe(self, args):
        """ Describes a published component. """
        if len(args) < 1 or len(args) > 2:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'describe,d <category/component> [-xml]')
            return

        cls, cfg = self._get_component(args[0])
        if cfg is None:
            return

        if len(args) > 1 and args[1] == '-xml':
# FIXME: arbitrary strings could invalidate XML.
            self._send_reply("""\
<Description>
 <Version>%s</Version>
 <Author>%s</Author>
 <Description>%s</Description>
 <HelpURL>%s</HelpURL>
 <Keywords>%s</Keywords>
 <TimeStamp>%s</TimeStamp>
 <Checksum>%s</Checksum>
 <Requirements>%s</Requirements>
 <hasIcon>%s</hasIcon>
 <HasVersionInfo>%s</HasVersionInfo>
</Description>""" % (cfg.version, cfg.author, cfg.description, cfg.help_url,
                     ' '.join(cfg.keywords), cfg.timestamp, cfg.checksum,
                     ' '.join(cfg.requirements), str(cfg.has_icon).lower(),
                     str(cfg.has_version_info).lower()))
        else:
            self._send_reply("""\
Version: %s
Author: %s
hasIcon: %s
Description: %s
Help URL: %s
Keywords: %s
Driver: false
Time Stamp: %s
Requirements: %s
HasVersionInfo: %s
Checksum: %s""" % (cfg.version, cfg.author, str(cfg.has_icon).lower(),
                   cfg.description, cfg.help_url, ' '.join(cfg.keywords),
                   cfg.timestamp, ' '.join(cfg.requirements),
                   str(cfg.has_version_info).lower(), cfg.checksum))

    _COMMANDS['describe'] = _describe
    _COMMANDS['d'] = _describe


    def _end(self, args):
        """ Unloads a component instance. """
        if len(args) != 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'end <object>')
            return

        name = args[0]
        try:
            self.__end(name)
        except KeyError:
            self._send_error('no such object: <%s>' % name)
        else:
            self._send_reply("""\
%s completed.
Object %s ended.""" % (name, name))

    def __end(self, name):
        """ Delete component instance `name`. """
        wrapper, worker = self._instance_map.pop(name)
        wrapper.pre_delete()
        WorkerPool.release(worker)
        server = self._servers.pop(wrapper)
        if server is not None:  # pragma no cover
            RAM.release(server)

    _COMMANDS['end'] = _end


    def _execute(self, args):
        """ Runs a component instance. """
        if len(args) < 1 or len(args) > 2:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'execute,x <objectName>[&]')
            return

        name = args[0]
        if name.endswith('&'):
            background = True
            name = name[:-1]
        elif len(args) > 1 and args[1] == '&':
            background = True
        else:
            background = False

        wrapper, worker = self._get_wrapper(name, background)
        if wrapper is not None:
            worker.put((wrapper.execute, (self._req_id,), {}, None))

    _COMMANDS['execute'] = _execute
    _COMMANDS['x'] = _execute


    def _get(self, args):
        """ Gets the value of a variable. """
        if len(args) != 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'get <object.property>')
            return

        name, dot, path = args[0].partition('.')
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.get, (path, self._req_id), {}, None))

    _COMMANDS['get'] = _get


    def _get_branches(self, args):
        """ Handler for ``getBranchesAndTags``. """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'getBranchesAndTags')
            return

        self._send_reply('')

    _COMMANDS['getBranchesAndTags'] = _get_branches


    def _get_direct_transfer(self, args):
        """ Return 'true' if we support direct file transfers. """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'getDirectTransfer')
            return

        self._send_reply('false')

    _COMMANDS['getDirectTransfer'] = _get_direct_transfer


    def _get_hierarchy(self, args):
        """ Get hierarchy of values in component. """
        if len(args) != 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'getHierarchy <object>')
            return

        wrapper, worker = self._get_wrapper(args[0])
        if wrapper is not None:
            worker.put((wrapper.get_hierarchy, (self._req_id,), {}, None))

    _COMMANDS['getHierarchy'] = _get_hierarchy


    def _get_icon(self, args):
        """ Gets the icon data for the published component. """
        if len(args) != 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'getIcon <analysisComponent>')
            return

        cls, cfg = self._get_component(args[0])
        if cfg is None:
            return

        raise NotImplementedError('getIcon')

    _COMMANDS['getIcon'] = _get_icon


    def _get_license(self, args):
        """ Retrieves Analysis Server's license agreement. """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'getLicense')
            return

        self._send_reply('Use at your own risk!')

    _COMMANDS['getLicense'] = _get_license


    def _get_status(self, args):
        """ Lists the run status of all component instances. """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'getStatus')
            return

        lines = []
        for name in sorted(self._instance_map.keys()):
            lines.append('%s: ready' % name)
        self._send_reply('\n'.join(lines))

    _COMMANDS['getStatus'] = _get_status


    def _get_sys_info(self, args):
        """
        Retrieves information about the server and the system it is on.
        """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'getSysInfo')
            return

        self._send_reply("""\
version: 5.01
build: 331
num clients: %d
num components: %d
os name: %s
os arch: %s
os version: %s
python version: %s
user name: %s"""
             % (self.server.num_clients, len(self.server.components),
                platform.system(), platform.processor(),
                platform.release(), platform.python_version(),
                getpass.getuser()))

    _COMMANDS['getSysInfo'] = _get_sys_info


    def _get_version(self, args):
        """ Gets the version and build number for Analysis Server. """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'getVersion')
            return

        self._send_reply("""\
OpenMDAO Analysis Server 0.1
Use at your own risk!
Attempting to support Phoenix Integration, Inc.
version: 5.01, build: 331""")

    _COMMANDS['getVersion'] = _get_version


    def _heartbeat(self, args):
        """
        Starts up socket heartbeating in order to keep sockets alive through
        firewalls with timeouts.
        """
        if len(args) != 1 or args[0] not in ('start', 'stop'):
            self._send_error('invalid syntax. Proper syntax:\n'
                             'heartbeat,hb [start|stop]')
            return

        if args[0] == 'start':
            if not _DISABLE_HEARTBEAT:
                if self._heartbeat is not None:  # Ensure only one.
                    self._heartbeat.stop()
                self._heartbeat = Heartbeat(self._req_id, self._send_reply)
                self._heartbeat.start()
            self._send_reply('Heartbeating started')
        else:
            if self._heartbeat is not None:
                self._heartbeat.stop()
            self._send_reply('Heartbeating stopped')

    _COMMANDS['heartbeat'] = _heartbeat
    _COMMANDS['hb'] = _heartbeat


    def _help(self, args):
        """ Help on Analysis Server commands. """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'help,h')
            return

        # As listed by Analysis Server version: 5.01, build: 331.
        self._send_reply("""\
Available Commands:
   listComponents,lc [category]
   listCategories,la [category]
   describe,d <category/component> [-xml]
   start <category/component> <instanceName>
   end <object>
   execute,x <objectName>
   listProperties,list,ls,l [object]
   listGlobals,lg
   listValues,lv <object>
   listArrayValues,lav <object> (NOT IMPLEMENTED)
   get <object.property>
   set <object.property> = <value>
   move,rename,mv,rn <from> <to> (NOT IMPLEMENTED)
   getIcon <analysisComponent> (NOT IMPLEMENTED)
   getVersion
   getLicense
   getStatus
   help,h
   quit
   getSysInfo
   invoke <object.method()> [full]
   listMethods,lm <object> [full]
   addProxyClients <clientHost1>,<clientHost2>
   monitor start <object.property>, monitor stop <id>
   versions,v category/component (NOT IMPLEMENTED)
   ps <object> (NOT IMPLEMENTED)
   listMonitors,lo <objectName>
   heartbeat,hb [start|stop]
   listValuesURL,lvu <object>
   getDirectTransfer
   getByUrl <object.property> <url> (NOT IMPLEMENTED)
   setByUrl <object.property> = <url> (NOT IMPLEMENTED)
   setDictionary <xml dictionary string> (NOT IMPLEMENTED)
   getHierarchy <object.property>
   setHierarchy <object.property> <xml>
   deleteRunShare <key> (NOT IMPLEMENTED)
   getBranchesAndTags""")

    _COMMANDS['help'] = _help
    _COMMANDS['h'] = _help


    def _invoke(self, args):
        """ Invokes a method on a component instance. """
        if len(args) < 1 or len(args) > 2:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'invoke <object.method()> [full]')
            return
# TODO: what does 'full' mean?

        name, dot, method = args[0].partition('.')
        method = method[:-2]
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.invoke, (method, self._req_id), {}, None))

    _COMMANDS['invoke'] = _invoke


    def _list_array_values(self, args):
        """ Lists all the values of an array variable. """
        if len(args) != 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listArrayValues,lav <object>')
            return

        name, dot, path = args[0].partition('.')
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.list_array_values,
                        (path, self._req_id), {}, None))

    _COMMANDS['listArrayValues'] = _list_array_values
    _COMMANDS['lav'] = _list_array_values


    def _list_categories(self, args):
        """ Lists all the sub-categories available in a category. """
        if len(args) > 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listCategories,la [category]')
            return

        if args:
            category = args[0].strip('"').strip('/') + '/' # Ensure trailing '/'
            if category == '/':
                category = ''
        else:
            category = ''

        lines = ['']
        for name in sorted(self.server.components.keys()):
            if name.startswith(category):
                name = name[len(category):]
                slash = name.find('/')
                if slash > 0:
                    name = name[:slash]
                    if name not in lines:
                        lines.append(name)
        lines[0] = '%d categories found:' % (len(lines)-1)
        self._send_reply('\n'.join(lines))

    _COMMANDS['listCategories'] = _list_categories
    _COMMANDS['la'] = _list_categories


    def _list_components(self, args):
        """ Lists all the components available in a category. """
        if len(args) > 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listComponents,lc [category]')
            return

        if args:
            category = args[0].strip('"').strip('/') + '/' # Ensure trailing '/'
            if category == '/':
                category = ''
        else:
            category = ''

        lines = ['']
        for name in sorted(self.server.components.keys()):
            if name.startswith(category):
                name = name[len(category):]
                if '/' not in name:
                    lines.append(name)
        lines[0] = '%d components found:' % (len(lines)-1)
        self._send_reply('\n'.join(lines))

    _COMMANDS['listComponents'] = _list_components
    _COMMANDS['lc'] = _list_components


    def _list_globals(self, args):
        """ Lists all component instances in the global namespace. """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listGlobals,lg')
            return

        self._send_reply('0 global objects started:')

    _COMMANDS['listGlobals'] = _list_globals
    _COMMANDS['lg'] = _list_globals


    def _list_methods(self, args):
        """ Lists all methods available on a component instance. """
        if len(args) < 1 or len(args) > 2:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listMethods,lm <object> [full]')
            return

        name = args[0]
        full = len(args) == 2 and args[1] == 'full'
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.list_methods, (full, self._req_id), {}, None))

    _COMMANDS['listMethods'] = _list_methods
    _COMMANDS['lm'] = _list_methods


    def _list_monitors(self, args):
        """ Lists all available monitorable items on a component instance. """
        if len(args) != 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listMonitors,lo <objectName>')
            return

        name = args[0]
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.list_monitors, (self._req_id,), {}, None))

    _COMMANDS['listMonitors'] = _list_monitors
    _COMMANDS['lo'] = _list_monitors


    def _list_properties(self, args):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable.
        """
        if len(args) > 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listProperties,list,ls,l [object]')
            return

        if len(args) == 0:
            names = sorted(self._instance_map.keys())
            lines = ['%d objects started:' % len(names)]
            lines.extend(names)
            self._send_reply('\n'.join(lines))
        else:
            name, dot, path = args[0].partition('.')
            wrapper, worker = self._get_wrapper(name)
            if wrapper is not None:
                worker.put((wrapper.list_properties,
                            (path, self._req_id), {}, None))

    _COMMANDS['listProperties'] = _list_properties
    _COMMANDS['list'] = _list_properties
    _COMMANDS['ls'] = _list_properties
    _COMMANDS['l'] = _list_properties


    def _list_values(self, args):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable.
        """
        if len(args) > 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listValues,lv [object]')
            return

        name, dot, path = args[0].partition('.')
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.list_values, (path, self._req_id), {}, None))

    _COMMANDS['listValues'] = _list_values
    _COMMANDS['lv'] = _list_values


    def _list_values_url(self, args):
        """
        Lists all available variables and their sub-properties on a component
        instance or sub-variable.
        """
        if len(args) > 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'listValuesURL,lvu [object]')
            return

        name, dot, path = args[0].partition('.')
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.list_values_url,
                        (path, self._req_id), {}, None))

    _COMMANDS['listValuesURL'] = _list_values_url
    _COMMANDS['lvu'] = _list_values_url


    def _monitor(self, args):
        """
        Starts/stops a monitor on a raw output file or available monitor.
        """
        if len(args) != 2 or args[0] not in ('start', 'stop'):
            self._send_error('invalid syntax. Proper syntax:\n'
                             'monitor start <object.property>, '
                             'monitor stop <id>')
            return

        if args[0] == 'start':
            name, dot, path = args[1].partition('.')
            wrapper, worker = self._get_wrapper(name)
            if wrapper is not None:
                worker.put((wrapper.start_monitor,
                            (path, self._req_id), {}, None))
                self._monitors[str(self._req_id)] = name
        else:
            try:
                name = self._monitors.pop(args[1])
            except KeyError:
                self._send_error('No monitor registered for %r' % args[1])
            else:
                wrapper, worker = self._get_wrapper(name)
                worker.put((wrapper.stop_monitor,
                            (args[1], self._req_id), {}, None))

    _COMMANDS['monitor'] = _monitor


    def _move(self, args):
        """ Moves or renames a component instance. """
        if len(args) != 2:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'move,rename,mv,rn <from> <to>')
            return

        raise NotImplementedError('move')

    _COMMANDS['move'] = _move
    _COMMANDS['rename'] = _move
    _COMMANDS['mv'] = _move
    _COMMANDS['rn'] = _move


    def _ps(self, args):
        """ Lists all running processes for a component instance. """
        if len(args) != 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'ps <object>')
            return

        name = args[0].strip('"')
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.ps, (self._req_id,), {}, None))

    _COMMANDS['ps'] = _ps


    def _publish_egg(self, args):
        """
        Receive an egg file and publish it.
        This is an extension to the AnalysisServer protocol.
        """
        if len(args) < 4:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'publishEgg <path> <version> <author> <eggdata>')
            return

        logging.critical('publish_egg')
        path, space, rest = self._req.partition(' ')
        version, space, rest = rest.partition(' ')
        author, space, eggdata = rest.partition(' ')
        logging.critical('    %r %r %r', path, version, author)

        # Create directory.
        path = path.strip('/')
        directory, slash, name = path.rpartition('/')
        if directory and not os.path.exists(directory):
            os.makedirs(directory)

        # Write egg file.
        egg_filename = '%s.%s.egg' % (name, version)
        egg_path = os.path.join(directory, egg_filename)
        if os.path.exists(egg_path):
            self._send_error('Egg %r already exists' % egg_path)
            return
        with open(egg_path, 'wb') as out:
            out.write(eggdata)

        # Load egg to verify.
        component = Container.load_from_eggfile(egg_path)
        description = component.__doc__

        # Write config file.
        cfg_filename = '%s-%s.cfg' % (name, version)
        cfg_path = os.path.join(directory, cfg_filename)
        if os.path.exists(cfg_path):
            self._send_error('Config file %r already exists' % cfg_path)
            os.remove(egg_path)
            return
        with open(cfg_path, 'w') as out:
            out.write("""\
[Description]
version: %s
author: %s
description: %s

[Python]
egg: %s

[Inputs]
*: *

[Outputs]
*: *

[Methods]
*: *
""" % (version, author, description))

        component.pre_delete()
        try:
            self._read_config(cfg_path)
        except Exception as exc:
            logging.error("Can't publishEgg: %r", exc)
            self._send_error(str(exc))
#            os.remove(egg_path)
#            os.remove(cfg_path)
        else:
            self._send_reply('Egg published.')

    _COMMANDS['publishEgg'] = _publish_egg


    def _quit(self, args):
        """ Close the connection. """
        if len(args) != 0:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'quit')
            return

    _COMMANDS['quit'] = _quit


    def _set(self, args):
        """ Sets the value of a variable. """
        cmd, space, assignment = self._req.partition(' ')
        lhs, eqsign, rhs = assignment.partition('=')
        name, dot, path = lhs.strip().partition('.')
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.set,
                        (path, rhs.strip(), self._req_id), {}, None))

    _COMMANDS['set'] = _set


    def _set_hierarchy(self, args):
        """ Set hierarchy of variable values in component. """
        cmd, space, rest = self._req.partition(' ')
        name, space, xml = rest.partition(' ')
        wrapper, worker = self._get_wrapper(name)
        if wrapper is not None:
            worker.put((wrapper.set_hierarchy, (xml, self._req_id), {}, None))

    _COMMANDS['setHierarchy'] = _set_hierarchy


    def _set_mode(self, args):
        """ Sets the connection into 'raw' mode. """
        if len(args) != 1 or args[0] != 'raw':
            self._send_error('invalid syntax. Proper syntax:\n'
                             'setMode raw')
            return

        self._raw = True
        self._stream.raw = True

    _COMMANDS['setMode'] = _set_mode


    def _start(self, args):
        """ Creates a new component instance. """
        if len(args) != 2:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'start <category/component> <instanceName>')
            return

        cls, cfg = self._get_component(args[0])
        if cls is None:
            return

        name = args[1]
        if name in self._instance_map:
            self._send_error('Name already in use: "%s"' % name)
            return

        if cls not in self.server.eggs:
            # If only local host will be used, we can skip determining
            # distributions required by the egg.
            allocators = RAM.list_allocators()
            need_reqs = False
            for allocator in allocators:
                if not isinstance(allocator, LocalAllocator):
                    need_reqs = True
                    break

            table = string.maketrans('/-. ', '____')
            egg_name = args[0].strip('"').translate(table)
            obj = set_as_top(cls())
            py_dir = os.path.dirname(cfg.cfg_path)
            egg_info = obj.save_to_egg(egg_name, 'AS', py_dir=py_dir,
                                       need_requirements=need_reqs)
            self.server.eggs[cls] = egg_info
            obj.pre_delete()
            del obj

        egg_info = self.server.eggs[cls]
        egg_file = egg_info[0]
        resource_desc = {
            'required_distributions': egg_info[1],
            'orphan_modules': egg_info[2],
            'python_version': sys.version[:3]
        }

        if self._server_per_obj:  # pragma no cover
            # Allocate a server.
            server, server_info = RAM.allocate(resource_desc)
            if server is None:
                raise RuntimeError('Server allocation failed :-(')

            # Transfer egg to it and load.
            filexfer(None, egg_file, server, egg_file, 'b')
            obj = server.load_model(egg_file)
        else:
            server = None
            obj = set_as_top(cls())
        obj.name = name

        wrapper = ComponentWrapper(name, obj, cfg, server,
                                   self._send_reply, self._send_exc)
        self._instance_map[name] = (wrapper, WorkerPool.get())
        self._servers[wrapper] = server
        self._send_reply('Object %s started.' % name)

    _COMMANDS['start'] = _start


    def _versions(self, args):
        """ Lists the version history of a component. """
        if len(args) != 1:
            self._send_error('invalid syntax. Proper syntax:\n'
                             'versions,v category/component')
            return

        cls, cfg = self._get_component(args[0])
        if cfg is None:
            return

        raise NotImplementedError('versions')

    _COMMANDS['versions'] = _versions
    _COMMANDS['v'] = _versions


class _WrapperConfig(object):
    """
    Retains configuration data for a wrapped component class.
    `config` is a :class:`ConfigParser.SafeConfigParser` instance,
    `instance` is a temporary wrapped instance to interrogate,
    and `cfg_path` is the path to the configuration file.
    """

    def __init__(self, config, instance, cfg_path):
        self.cfg_path = cfg_path

        if not _IGNORE_ATTR:
            for attr in dir(Component()):
                _IGNORE_ATTR.add(attr)

        # Get description.
        defaults = {
            'version': '',
            'author': '',
            'description': '',
            'help_url': '',
            'keywords': '',
            'requirements': '',
        }
        for option in defaults:
            if not config.has_option('Description', option):
                config.set('Description', option, defaults[option])
            setattr(self, option, config.get('Description', option))

        stat_info = os.stat(cfg_path)
        self.timestamp = time.ctime(stat_info.st_mtime)
        self.checksum = 0
        self.has_icon = False
        self.has_version_info = False

        if not self.description:
            if instance.__doc__ is not None:
                self.description = instance.__doc__

        if not self.author and sys.platform != 'win32':
            self.author = pwd.getpwuid(stat_info.st_uid).pw_name

        # Get properties.
        self.inputs = self._setup_mapping(instance,
                                          config.items('Inputs'), 'in')
        self.outputs = self._setup_mapping(instance,
                                           config.items('Outputs'), 'out')
        self.properties = {}
        self.properties.update(self.inputs)
        self.properties.update(self.outputs)

        # Get methods.
        self.methods = {}
        for ext_name, int_name in config.items('Methods'):
            if ext_name == '*':
                if int_name != '*':
                    raise ValueError("internal name must be '*'"
                                     " if the external name is '*'")
                # Register all valid non-vanilla methods.
                for attr in dir(instance):
                    if attr in _IGNORE_ATTR or attr.startswith('_'):
                        continue
                    if self._valid_method(instance, attr):
                        logging.debug('    register %s()', attr)
                        self.methods[attr] = attr
            else:
                if int_name == '*':
                    int_name = ext_name
                if self._valid_method(instance, int_name):
                    logging.debug('    register %r => %s()', ext_name, int_name)
                    self.methods[ext_name] = int_name
                else:
                    raise ValueError('%r is not a valid method' % int_name)

    def _setup_mapping(self, instance, paths, iotype):
        """ Return dictionary mapping external paths to internal paths. """
        mapping = {}
        for ext_path, int_path in paths:
            if ext_path == '*':
                if int_path != '*':
                    raise ValueError("internal path must be '*'"
                                     " if the external path is '*'")
                # Register all valid non-vanilla paths in this component.
                containers = [instance]
                containers.extend([val for name, val in instance.items()
                                       if isinstance(val, Container)])
                for container in containers:
                    for name, val in container.items(iotype=iotype):
                        if name in _IGNORE_ATTR or name.startswith('_'):
                            continue
                        trait = container.get_trait(name)
                        typ = None if trait is None else trait.trait_type
                        if type(typ) in TYPE_MAP:
                            if container is instance:
                                path = name
                            else:
                                path = '%s.%s' % (container.get_pathname(), name)
                            logging.debug('    register %r %r', path, iotype)
                            mapping[path] = path
            else:
                if int_path == '*':
                    int_path = ext_path
                if self._valid_path(instance, int_path, iotype):
                    logging.debug('    register %r => %r %r',
                                  ext_path, int_path, iotype)
                    mapping[ext_path] = int_path
                else:
                    raise ValueError('%r is not a valid %r variable'
                                      % (int_path, iotype))
        return mapping

    @staticmethod
    def _valid_path(instance, path, iotype):
        """ Return True if `path` refers to an `io_type` variable. """
        meta = instance.get_metadata(path, 'iotype')
        return meta == iotype

    @staticmethod
    def _valid_method(instance, attr):
        """ Return True if `attr` is a valid method for `instance`. """
        try:
            obj = getattr(instance, attr)
        except AttributeError:
            return False
        if not inspect.ismethod(obj):
            return False
        args, varargs, keywords, defaults = inspect.getargspec(obj.im_func)
        return len(args) == 1  # Just 'self'.


def start_server(port=DEFAULT_PORT, allowed_hosts=None, ignore=False):
    """
    Start server process at `port` (use zero for a system-selected port).
    If `allowed_hosts` is None then ``['127.0.0.1', socket.gethostname()]``
    is used. If `ignore` is True, then the ``--ignore-config-errors`` option
    is used. Returns ``(proc, port)``.
    """
    if allowed_hosts is None:
        allowed_hosts = ['127.0.0.1', socket.gethostname()]
    with open('hosts.allow', 'w') as out:
        for pattern in allowed_hosts:
            out.write('%s\n' % pattern)
    if sys.platform != 'win32' or HAVE_PYWIN32:
        make_private('hosts.allow')

    server_path = \
        pkg_resources.resource_filename('analysis_server', 'server.py')

    server_out = 'as-%d.out' % port
    server_up = 'as-%d.up' % port
    if os.path.exists(server_up):
        os.remove(server_up)

    args = ['python', server_path, '--port', '%d' % port, '--up', server_up]
    if ignore:
        args.append('--ignore-config-errors')
    proc = ShellProc(args, stdout=server_out, stderr=STDOUT)

    # Wait for valid server_up file.
    timeout = 30  # Seconds.
    retry = 0
    while (not os.path.exists(server_up)) or \
          (os.path.getsize(server_up) == 0):
        return_code = proc.poll()
        if return_code:
            error_msg = proc.error_message(return_code)
            raise RuntimeError('Server startup failed %s' % error_msg)
        retry += 1
        if retry < 10*timeout:
            time.sleep(.1)
        # Hard to cause a startup timeout.
        else:  # pragma no cover
            proc.terminate(timeout)
            raise RuntimeError('Server startup timeout')

    with open(server_up, 'r') as inp:
        host = inp.readline().strip()
        port = int(inp.readline().strip())
        pid  = int(inp.readline().strip())

    os.remove(server_up)

    return (proc, port)


def stop_server(proc):
    """ Stop server process. """
    proc.terminate(timeout=10)


def main():  # pragma no cover
    """
    OpenMDAO AnalysisServer process.  Component types to be supported
    are described by ``name.cfg`` files in the current directory or
    subdirectories.  Subdirectory names are used for category names.

    Usage: python server.py [--hosts=filename][--address=address][--port=number][--up=filename]

    --hosts: string
        Filename for allowed hosts specification. Default ``hosts.allow``.
        The file should contain IPv4 host addresses, IPv4 domain addresses,
        or hostnames, one per line. Blank lines are ignored, and '#' marks the
        start of a comment which continues to the end of the line.

    --address: string
        IPv4 address or hostname. Default is the host's default IPv4 address.

    --port: int
        Server port (default 1835).
        Note that ports below 1024 typically require special privileges.

    --ignore-config-errors:
        Used to ignore component configuration errors. Helpful during testing.

    --no-heartbeat:
        Do not send heartbeat replies. Simplifies debugging.

    --up: string
        Filename written once server is initialized. Typically used for
        programmatic startup during testing.
    """
    parser = optparse.OptionParser()
    parser.add_option('--hosts', action='store', type='str',
                      default='hosts.allow', help='filename for allowed hosts')
    parser.add_option('--address', action='store', type='str',
                      help='network address to serve.')
    parser.add_option('--port', action='store', type='int',
                      default=DEFAULT_PORT, help='port to listen on')
    parser.add_option('--ignore-config-errors', action='store_true',
                      help='Ignore component configuration errors')
    parser.add_option('--no-heartbeat', action='store_true',
                      help='Do not send heartbeat replies')
    parser.add_option('--up', action='store', default='',
                      help="if non-null, file written when server is 'up'")

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    logging.getLogger().setLevel(logging.DEBUG)

    _DISABLE_HEARTBEAT = options.no_heartbeat

    # Get allowed_hosts.
    if os.path.exists(options.hosts):
        try:
            allowed_hosts = read_allowed_hosts(options.hosts)
        except Exception as exc:
            print "Can't read allowed hosts file %r: %s" % (options.hosts, exc)
            sys.exit(1)
        if not allowed_hosts:
            print 'No allowed hosts!?.'
            sys.exit(1)
    else:
        print 'Allowed hosts file %r does not exist.' % options.hosts
        sys.exit(1)

    # Create server.
    host = options.address or socket.gethostname()
    server = Server(host, options.port, allowed_hosts)
    if server.config_errors and not options.ignore_config_errors:
        print '%d component configuration errors detected.' \
              % server.config_errors
        sys.exit(1)

    # Report server address and PID.
    port = server.server_address[1]
    pid = os.getpid()
    msg = 'Server started on %s:%d, pid %d.' % (host, port, pid)
    print msg
    logging.info(msg)
    if options.up:
        with open(options.up, 'w') as out:
            out.write('%s\n' % host)
            out.write('%d\n' % port)
            out.write('%d\n' % pid)

    # And away we go...
    signal.signal(signal.SIGINT,  _sigterm_handler)
    signal.signal(signal.SIGTERM, _sigterm_handler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    _cleanup()
    sys.exit(0)


def _sigterm_handler(signum, frame):  #pragma no cover
    """ Try to go down gracefully. """
    logging.getLogger().info('sigterm_handler invoked')
    print 'sigterm_handler invoked'
    sys.stdout.flush()
    _cleanup()
    sys.exit(1)


def _cleanup():  #pragma no cover
    """ Clean up any generated egg files. """
    for path in glob.glob('*-AS.*.egg'):
        os.remove(path)


if __name__ == '__main__':  # pragma no cover
    main()

