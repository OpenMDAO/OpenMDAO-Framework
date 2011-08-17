import getpass
import optparse
import socket
import sys
import telnetlib
import threading
import xml.etree.cElementTree as ElementTree

from analysis_server import server, stream

# These are used to shorten list_properties() output.
_ASERVER_PREFIX = 'com.phoenix_int.aserver.'
_TYPE_PREFIX = _ASERVER_PREFIX+'types.'


class Client(object):
    """
    ModelCenter AnalysisServer client, providing a Python interface to the
    protocol described in:
    http://www.phoenix-int.com/~AnalysisServer/commands/index.html.

    `host` is the host name or IP address of the host to connect to and
    `port` is the port to use. If `debug` is True then request and reply
    messages are displayed on stdout.

    .. note::
        Instances are not pickleable. Save/restore must be handled at
        a higher level.

    .. note::
        Multi-thread accesses are allowed, but synchronized in a primitive
        fashion such that one thread will block others.
    """

    def __init__(self, host='localhost', port=server.DEFAULT_PORT, debug=False):
        self._conn = telnetlib.Telnet(host, port)
        self._stream = stream.Stream(self._conn.sock, debug, debug)
        self._lock = threading.Lock()  # Controls stream access.
        self._raw = False
        self._curr_id = 0
        self._welcome = self._stream.recv_reply()

    def interact(self):  # pragma no cover
        """ Interactive command-line access, useful for debugging. """
        print self._welcome
        self._conn.interact()

    def _send_recv(self, request, raw=False):
        """
        Send a request and wait for reply.

        request: string
            Request to be sent.

        raw: bool
            If True, the stream is transitioning to ``raw`` mode.

        Returns the reply string.
        """
        with self._lock:
            if self._raw:
                self._curr_id += 1
                self._stream.send_request(request, self._curr_id)
            else:
                self._stream.send_request(request)

            if raw:  # No reply if switching to raw mode
                self._raw = True
                self._stream.raw = True
            else:
                if self._raw:
                    while True:
                        info = self._stream.recv_reply()
                        if info[1] == self._curr_id:
                            break
# FIXME: handle heartbeats, monitors, etc.
                    reply = info[0]
                else:
                    reply = self._stream.recv_reply()
                    if reply.endswith('\n>'):
                        reply = reply[:-2]
                    elif reply == '>':
                        reply = ''

                if reply.startswith(server.ERROR_PREFIX):
                    raise RuntimeError(reply[len(server.ERROR_PREFIX):])
                else:
                    return reply

    def add_proxy_clients(self, client_host_1, client_host_2):
        """
        Adds host IP addresses to the list of client hosts in the proxy chain
        between client and server. Used for permission checking.

        client_host_1, client_host_2: string
            IP addreses to be added.
        """
        self._send_recv('addProxyClients %s %s' \
                        % (client_host_1, client_host_2))

    def describe(self, path):
        """
        Returns a dictionary of terms describing published component `path`.

        path: string
            Path to component.
        """
        reply = self._send_recv('describe %s' % path)
        dct = {}
        lines = reply.split('\n')
        prev = None
        while lines:
            line = lines.pop(0)
            name, colon, value = line.partition(':')
            if colon:
                prev = name.strip()
                dct[prev] = value.strip()
            else:  # Additional line for previous entry.
                dct[prev] += '\n%s' % name.strip()
        return dct

    def end(self, name):
        """
        Unloads component instance `name`.

        name: string
            Name of instance to unload.
        """
        self._send_recv('end %s' % name)

    def execute(self, name, background=False):
        """
        Runs component instance `name`. If `background` is True, then return
        immediately. Otherwise block until execution completes.

        name: string
            Name of instance to execute.

        background: bool
            If True, execute as concurrent task.
        """
        req = 'execute %s' % name
        if background:
            req += ' &'
        self._send_recv(req)

    def get(self, path):
        """
        Returns the value of variable `path` as a string.

        path: string
            Path to variable.
        """
        return self._send_recv('get %s' % path)

    def get_branches_and_tags(self):
        """ Returns reply from 'getBranchesAndTags' command. """
        reply = self._send_recv('getBranchesAndTags')
# TODO: Pythonic return value.
        return reply

    def get_direct_transfer(self):
        """ Return True if direct transfers are supported by the server. """
        reply = self._send_recv('getDirectTransfer')
        return reply == 'true'

    def get_hierarchy(self, path):
        """
        Return all interface variable information for `path` as XML.

        path: string
            Path to component instance.
        """
        reply = self._send_recv('getHierarchy %s' % path)
# TODO: Pythonic return value.
        return reply

    def get_icon(self, path):
        """
        Gets the icon data for published component `path`.

        path: string
            Path to component.
        """
# TODO: getIcon
        raise NotImplementedError('Client.get_icon()')

    def get_license(self):
        """ Retrieves the server's license agreement. """
        return self._send_recv('getLicense')

    def get_status(self):
        """ Returns the run status of all components as a dictionary. """
        reply = self._send_recv('getStatus')
        dct = {}
        for line in reply.split('\n'):
            name, value = line.split(':')
            dct[name.strip()] = value.strip()
        return dct

    def get_sys_info(self):
        """
        Returns information about the server and the system it is on as a
        dictionary.
        """
        reply = self._send_recv('getSysInfo')
        dct = {}
        for line in reply.split('\n'):
            name, colon, value = line.partition(':')
            dct[name.strip()] = value.strip()
        return dct

    def get_version(self):
        """ Gets the version and build number for the server. """
        return self._send_recv('getVersion')

    def heartbeat(self, start):
        """
        If `start` is True, starts up socket heartbeating in order to keep
        sockets alive through firewalls with timeouts. Otherwise stops
        socket heartbeating.

        start: bool
            If True, start heartbeat.
        """
        req = 'heartbeat '
        req += 'start' if start else 'stop'
        self._send_recv(req)

    def help(self):
        """ Help on Analysis Server commands as a list of strings. """
        reply = self._send_recv('help')
        return [line.strip() for line in reply.split('\n')]

    def invoke(self, path):
        """
        Invokes component instance method `path`.

        path: string
            Path to method.
        """
        return self._send_recv('invoke %s()' % path)

    def list_array_values(self, path):
        """
        Lists all the values of array variable `path`.

        path: string
            Path to array variable.
        """
        reply = self._send_recv('listArrayValues %s' % path)
        data = []
        for line in reply.split('\n')[1:]:
            name, equalsign, value = line
            data.append(value)
        return data

    def list_categories(self, category=None):
        """
        Lists all subcategories available in `category`.

        category: string
            Category to list.
        """
        req = 'listCategories'
        if category:
            req += ' %s' % category
        reply = self._send_recv(req)
        return [line.strip() for line in reply.split('\n')[1:]]

    def list_components(self, category=None):
        """
        Lists all components available in `category`.

        category: string
            Category to list.
        """
        req = 'listComponents'
        if category:
            req += ' %s' % category
        reply = self._send_recv(req)
        return [line.strip() for line in reply.split('\n')[1:]]

    def list_globals(self):
        """ Lists all component instances in the global namespace. """
        reply = self._send_recv('listGlobals')
        return [line.strip() for line in reply.split('\n')[1:]]

    def list_methods(self, name, full=False):
        """
        Lists all methods available on component instance `name`.
        If `full` is True, returns a list of ``(method, fullname)``.

        name: string
            Component instance name.

        full: bool
            If True, include full name.
        """
        req = 'listMethods %s' % name
        if full:
            req += ' full'
        reply = self._send_recv(req)
        if full:
            info = []
            for line in reply.split('\n')[1:]:
                meth, space, full_name = line.partition(' ')
                meth = meth[:-2]
                full_name = full_name.split('=')[1].strip('"')
                info.append((meth, full_name))
            return info
        else:
            return [line.strip()[:-2] for line in reply.split('\n')[1:]]

    def list_monitors(self, name):
        """
        Lists all monitorable items on component instance `name`.

        name: string
            Component instance name.
        """
        reply = self._send_recv('listMonitors %s' % name)
        return [line.strip() for line in reply.split('\n')[1:]]

    def list_properties(self, path=None):
        """
        If `path` is None, then a list of all started components is returned.
        Otherwise a list of component properties is returned in the form
        ``(name, type, access)`` where `name` is the name of the property,
        `type` is the AnalysisServer type (such as 'PHXDouble'), and
        `access` is 'in' or 'out'.

        path: string
            Path to property.
        """
        req = 'listProperties'
        if path:
            req += ' %s' % path
        reply = self._send_recv(req)
        lines = reply.split('\n')
        if path:
            lines = lines[1:]
            info = []
            for line in lines:
                name, typ, access = line.strip().split()
                typ = typ.split('=')[1][:-1]
                if typ.startswith(_TYPE_PREFIX):
                    typ = typ[len(_TYPE_PREFIX):]
                elif typ.startswith(_ASERVER_PREFIX):
                    typ = typ[len(_ASERVER_PREFIX):]
                access = access.split('=')[1][:-1]
                if access == 'sg':
                    access = 'in'
                else:
                    access = 'out'
                info.append((name, typ, access))
            return info
        else:
            return lines[1:]

    def start_monitor(self, path):
        """
        Starts monitor `path`. Returns ``(initial_value, monitor_id)``.

        path: string
            Path to monitor to be started.
        """
# TODO: callback argument to handle updates.
        reply = self._send_recv('monitor start %s' % path)
        monitor_id = self._curr_id if self._raw else None
        return (reply, monitor_id)

    def stop_monitor(self, monitor_id):
        """
        Stops monitor `monitor_id`.

        monitor_id: string
            Identifier of monitor to be stopped.
        """
        self._send_recv('monitor stop %s' % monitor_id)

    def move(self, old, new):
        """
        Moves or renames a component instance `old` to `new`.
        Note that the global namespace is referenced by starting a path
        with ``globals/``.

        old: string
            Current instance name.

        new: string
            New instance name.
        """
        self._send_recv('move %s %s' % (old, new))

    def ps(self, name):
        """
        Lists all running processes for component instance `name`.
        Note that not all information may be valid, based on host
        operating system. Returns a list of dictionaries, one per process.

        name: string
            Component instance name.
        """
        reply = self._send_recv('ps %s' % name)
        root = ElementTree.fromstring(reply)
        processes = []
        for process in root.findall('Process'):
            proc = {}
            proc['PID'] = int(process.attrib['pid'])
            proc['ParentPID'] = int(process.find('ParentPID').text)
            proc['PercentCPU'] = float(process.find('PercentCPU').text)
            proc['Memory'] = int(process.find('Memory').text)
            proc['Time'] = float(process.find('Time').text)
            proc['WallTime'] = float(process.find('WallTime').text)
            proc['Command'] = process.find('Command').text
            processes.append(proc)
        return processes

    def publish_egg(self, path, version, comment, eggfile):
        """
        Publish `eggfile` under `path` and `version` with `comment`.
        The 'author' field will be set from the current user and host.
        This is an extension to the AnalysisServer protocol.

        path: string
            Component path to be published.

        version: string
            Version to be published.

        comment: string
            Description of this version of this component.

        eggfile: string
            Filename of egg to be published.
        """
        if not self._raw:
            self.set_mode_raw()
        author = '%s@%s' % (getpass.getuser(), socket.gethostname())
        request = 'publishEgg %s %s "%s" "%s"\0' \
                  % (path, version, comment, author)
        with open(eggfile, 'rb') as inp:
            request += inp.read()
        self._send_recv(request)

    def quit(self):
        """ Close the connection. Implicitly ends all non-global instances. """
        try:
            self._send_recv('quit')
        except EOFError:
            pass

    def set(self, path, valstr):
        """
        Sets the value of variable `path` from `valstr`.

        path: string
            Path to variable.

        valstr: string
            Value to set variable to in string form.
        """
        self._send_recv('set %s = %s' % (path, valstr))

    def set_hierarchy(self, path, xml):
        """
        Set multiple variable values from `xml` data.

        path:
            Path to component instance.

        xml: string
            XML describing values to be set.
        """
        self._send_recv('setHierarchy %s %s' % (path, xml))

    def set_mode_raw(self):
        """ Sets the connection into 'raw' mode. """
        self._send_recv('setMode raw', raw=True)

    def start(self, path, name):
        """
        Creates a new component instance of type `path` as `name`.

        path: string
            Component path.

        name: string
            name for instance.
        """
        self._send_recv('start %s %s' % (path, name))

    def versions(self, path):
        """
        Returns a list of versions for `path`.

        path: string
            Component path.
        """
        reply = self._send_recv('versions %s' % path)
        root = ElementTree.fromstring(reply)
        versions = []
        for version in root.findall('Version'):
            versions.append(version.attrib['name'])
        return versions


def main():  # pragma no cover
    """
    Starts up an interactive session, useful for testing.

    Usage: python client.py [--host=address][--port=number][--debug]

    --host: string
        IPv4 address or hostname. Default is 'localhost'.

    --port: int
        Server port (default 1835).

    --debug:
        Displays request and reply messages.
    """
    parser = optparse.OptionParser()
    parser.add_option('--host', action='store', type='string',
                      default='localhost', help='host to connect to')
    parser.add_option('--port', action='store', type='int',
                      default=server.DEFAULT_PORT, help='port to connect to')
    parser.add_option('--debug', action='store_true',
                      help='display request and reply messages')

    options, arguments = parser.parse_args()
    if arguments:
        parser.print_help()
        sys.exit(1)

    try:
        client = Client(options.host, options.port, debug=options.debug)
    except Exception as exc:
        print "Can't connect to %s:%d: %s" % (options.host, options.port, exc)
        sys.exit(1)

    client.interact()


if __name__ == '__main__':  # pragma no cover
    main()

