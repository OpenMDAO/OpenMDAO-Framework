"""
'DMZ' remote procedure call protocol using an intermediary file server as the
communications channel acessed via 'ssh' and 'scp'.  Really.

Each server has its own directory (``RJE-<host>``). This contains a
``heartbeat`` file, updated periodically by the server to indicate it is still
running.

Each client allocator has its own directory within a server directory
(``<host>-<pid>-<name>``). Within that each allocated server gets its own
directory for the communication channel::

    DMZ-root/
        Server1-root/
            Client1-root/
                Sim-1/
                Sim-2/
            Client2-root/
                Sim-1/

Each communication direction has its own set of files within this directory
prefixed by ``C`` or ``S``:

    ``[CS]-request.<mid>``
        Pickled ``(method, args, kwargs)``.

    ``[CS]-request_ready.<mid>``
        Flag indicating that all of ``[CS]-request.<mid>`` has been written.

    ``[CS]-reply.<mid>``
        Picked reply data.

    ``[CS]-reply_ready.<mid>``
        Flag indicating that all of ``[CS]-reply.<mid>`` has been written.

where ``<mid>`` is a message sequence number. Files are created by the sender
and removed by the receiver.

The above hierarchy is conceptual.  The NAS DMZ file servers do not allow a
user filesystem hierarchy, so all file paths have to be flattened. This is done
by replacing ``/`` with ``=``, and dealing with this special style of filename
in commands such as ``ls``.

The protocol makes some assumptions regarding connectivity:

- The remote login name matches the local login name.
- 'plink' and 'pscp' are available on Windows, 'ssh' and 'scp' otherwise.
- No user interaction is required to connect to the remote DMZ server.

"""

import cPickle
import datetime
import getpass
import logging
import os.path
import socket
import subprocess
import sys
import tempfile
import time

from openmdao.main import __version__


class RemoteError(Exception):
    """ Contains original exception as well as a traceback. """

    def __init__(self, exc, traceback=''):
        Exception.__init__(self, exc, traceback)

    @property
    def exc(self):
        """ The original exception. """
        return self.args[0]

    @property
    def traceback(self):
        """ Traceback at point of exception. """
        return self.args[1]

    def __str__(self):
        return '%s(%r)' % (self.exc.__class__.__name__, str(self.exc))

    def __repr__(self):
        return '%s(%s(%r))' % (self.__class__.__name__,
                               self.exc.__class__.__name__, str(self.exc))


DEBUG2 = logging.DEBUG - 2  # Show polling.
DEBUG3 = logging.DEBUG - 3  # Show ssh/scp.

if logging.getLevelName(DEBUG2) == 'Level 8':
    logging.addLevelName(DEBUG2, 'D2')
if logging.getLevelName(DEBUG3) == 'Level 7':
    logging.addLevelName(DEBUG3, 'D3')

_CLIENTS = []  # Server client names.
_SEP = '='

def _map_path(path):
    """ Map path separators to '='. """
    path = path.replace('/', _SEP)
    if sys.platform == 'win32':  # pragma no cover
        path = path.replace('\\', _SEP)
    return path

def _map_dir(path):
    """ Map path separators to '=', and append '='. """
    return _map_path(path)+_SEP


# These get reconfigured & restored during testing.
_SSH = ['plink', '-batch', '-ssh'] if sys.platform == 'win32' else ['ssh']
_SCP = ['pscp', '-batch', '-q'] if sys.platform == 'win32' else ['scp']

def configure_ssh(cmdlist):
    """ Configure 'ssh' command', returns previous configuration. """
    logging.debug('configure_ssh %r', cmdlist)
    previous = list(_SSH)
    _SSH[:] = cmdlist
    return previous

def configure_scp(cmdlist):
    """ Configure 'scp' command', returns previous configuration. """
    logging.debug('configure_scp %r', cmdlist)
    previous = list(_SCP)
    _SCP[:] = cmdlist
    return previous


def _ssh(host, args, logger, retry=True):
    """
    Return lines from `host` executing `args`.

    args: list[string]
        Command and arguments to be executed.

    logger: :class:`Logger`
        Displays full command being executed.

    retry: boolean
        If True, the command will be attempted up to three times.
        Be sure the command is idempotent!.
    """
    cmd = []
    cmd.extend(_SSH)
    cmd.extend(('-l', getpass.getuser()))
    cmd.append(host)
    cmd.extend(args)

    logger.log(DEBUG3, '%s', cmd)
    stdin = 'nul:' if sys.platform == 'win32' else '/dev/null'
    stdin = open(stdin, 'r')

    attempts = 5 if retry else 1
    for i in range(attempts):
        try:
            try:
                proc = subprocess.Popen(cmd, stdin=stdin,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE,
                                        universal_newlines=True)
            except Exception as exc:
                raise RuntimeError('%s: %s' % (cmd, exc))

            stdout, stderr = proc.communicate()
            if proc.returncode:
                msg = '%s: returncode %s: stdout: %s stderr: %s' \
                      % (cmd, proc.returncode, stdout, stderr)
                raise RuntimeError(msg)

        except Exception as exc:
            if i+1 < attempts:
                logger.error('%s\nretrying...', exc)
                time.sleep(i)  # Back-off a bit.
            else:
                raise
        else:
            break

    if not stdout and sys.platform == 'win32':  # pragma no cover
        # plink doesn't always set an error code.
        if "The server's host key is not cached in the registry" in stderr:
            raise RuntimeError(stderr)

    return stdout.split()


def _scp_send(path, host, directory, name, logger):
    """
    Send `path` to `host`:`directory` as `name`.

    path: string
        Path to local file.

    host: string
        Host to send to.

    directory: string
        Directory containing remote copy.

    name: string
        Remote file name.

    logger: :class:`Logger`
        Displays full command being executed.
    """
    src = path
    dst = _map_path('%s:%s/%s' % (host, directory, name))
    _scp(src, dst, logger)


def _scp_recv(host, directory, name, path, logger):
    """
    Receive `name` in `host`:`directory` to `path`.

    host: string
        Host to receive from.

    directory: string
        Directory containing remote copy.

    name: string
        Remote file name.

    path: string
        Path to local file.

    logger: :class:`Logger`
        Displays full command being executed.
    """
    src = _map_path('%s:%s/%s' % (host, directory, name))
    dst = path
    _scp(src, dst, logger)


def _scp(src, dst, logger):
    """
    Use 'scp' to copy `src` to `dst`.
    The command will be attempted up to three times.

    src, dst: string
        Source and destination designations.

    logger: :class:`Logger`
        Displays full command being executed.
    """
    cmd = []
    cmd.extend(_SCP)
    if sys.platform == 'win32':  # pragma no cover
        cmd.extend(('-l', getpass.getuser()))
    cmd.extend((src, dst))

    logger.log(DEBUG3, '%s', cmd)
    stdin = 'nul:' if sys.platform == 'win32' else '/dev/null'
    stdin = open(stdin, 'r')

    attempts = 5
    for i in range(attempts):
        try:
            try:
                proc = subprocess.Popen(cmd, stdin=stdin,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE,
                                        universal_newlines=True)
            except Exception as exc:
                raise RuntimeError('%s: %s' % (cmd, exc))

            stdout, stderr = proc.communicate()
            if proc.returncode:
                msg = '%s: returncode %s: stdout: %s stderr: %s' \
                      % (cmd, proc.returncode, stdout, stderr)
                raise RuntimeError(msg)

        except Exception as exc:
            if i+1 < attempts:
                logger.error('%s\nretrying...', exc)
                time.sleep(i)  # Back-off a bit.
            else:
                raise
        else:
            break


def connect(dmz_host, server_host, path, logger):
    """
    Connect client to server. Returns :class:`Connection`.

    Top-level connections are initiated by the client creating communication
    directory in server's root directory.

    Lower-level connections are made by the client sending a request for a new
    resource, server creates communication directory, starts polling thread,
    and returns the name of the new directory.

    dmz_host: string
        Intermediary file server.

    server_host: string
        Remote server host.

    path: string
        Path to communications directory.

    logger: :class:`Logger`
        Displays progress messages, passed to created :class:`Connection`.
    """
    root = _server_root(server_host)
    logger.debug('connecting to %s at %s', root, dmz_host)

    lines = _ssh(dmz_host, ('ls', '-1'), logger)
    mapped_root = _map_dir(root)
    if mapped_root not in lines:
        raise RuntimeError('server root %r on %r not found'
                           % (mapped_root, dmz_host))

    poll_delay = check_server_heartbeat(dmz_host, server_host, logger)

    if '/' in path:  # Connect to existing communications directory.
        root = path
    else:            # Create communications directory.
        root = '%s/%s-%s-%s' % (root, socket.gethostname(), os.getpid(), path)

    if root != path:  # Create communications directory.
        mapped_root = _map_dir(root)
        if mapped_root in lines:  # Need a clean directory.
            raise RuntimeError('client root %r already exists', mapped_root)
        _ssh(dmz_host, ('date', '>', mapped_root), logger)

    return Connection(dmz_host, root, False, poll_delay, logger)


def _server_root(hostname=None):
    """
    Returns root directory for `hostname`
    (default :meth:`socket.gethostname()).

    hostname: string
        Host name to use rather than default.
    """
    hostname = hostname or socket.gethostname()
    return 'RJE-%s' % hostname


# Server-side.
def server_init(dmz_host, logger):  # pragma no cover
    """
    Server initialization.

    dmz_host: string
        Intermediary file server.

    logger: :class:`Logger`
        Displays progress messages.
    """
    root = _server_root()
    mapped_root = _map_dir(root)  # Ensure a clean remote tree.
    try:
        _ssh(dmz_host, ('rm', '-f', '%s*' % mapped_root), logger)
    except Exception as exc:  # pragma no cover
        logger.warning("Can't remove server root: %s", exc)
    _ssh(dmz_host, ('date', '>', mapped_root), logger)


# Server-side.
def server_accept(dmz_host, poll_delay, logger):  # pragma no cover
    """
    Look for new client.
    Returns ``((new-client-name, :class:`Connection`), closed-clients)``.

    dmz_host: string
        Intermediary file server.

    poll_delay: int
        Maximum delay between checking for client activity.

    logger: :class:`Logger`
        Displays progress messages, passed to created :class:`Connection`.
    """
    root = _server_root()
    mapped_root = _map_dir(root)
    lines = _ssh(dmz_host, ('ls', '-1'), logger)
    lines = [line[len(mapped_root):] for line in lines
                                              if line.startswith(mapped_root)]
    info = None
    removed = []
    for client in _CLIENTS:
        if _map_dir(client) not in lines:
            logger.info('Client %r closed', client)
            _CLIENTS.remove(client)
            removed.append(client)

    for line in lines:
        if not line or line == 'heartbeat':
            continue
        client, sep, rest = line.partition(_SEP)
        if client not in _CLIENTS:
            _CLIENTS.append(client)
            logger.info('New client %r', client)
            root = '%s/%s' % (root, client)
            logger = logging.getLogger(client)
            conn = Connection(dmz_host, root, True, poll_delay, logger)
            info = (client, conn)
    
    return (info, removed)


def server_heartbeat(dmz_host, poll_delay, logger):
    """
    Update top-level server heartbeat file.

    dmz_host: string
        Intermediary file server.

    poll_delay: int
        Reported polling rate (seconds).
    """
    logger.log(DEBUG2, 'heartbeat')
    root = _server_root()
    tstamp = datetime.datetime.utcnow()
    fd, path = tempfile.mkstemp()
    try:
        os.close(fd)
        with open(path, 'w') as out:
            out.write('%s\n%s\n%s\n' % (tstamp, poll_delay, __version__))
        _scp_send(path, dmz_host, root, 'heartbeat', logger)
    finally:
        try:
            os.remove(path)
        except Exception as exc:  # pragma no cover
            logger.warning("Can't remove temporary file: %s", exc)


def check_server_heartbeat(dmz_host, server_host, logger):
    """
    Check that the server heartbeat file is 'current'.
    Returns server polling rate.

    dmz_host: string
        Intermediary file server.

    server_host: string
        Remote server.
    """
    logger.log(DEBUG2, 'check_server_heartbeat')
    root = _server_root(server_host)
    fd, path = tempfile.mkstemp()
    try:
        os.close(fd)
        try:
            _scp_recv(dmz_host, root, 'heartbeat', path, logger)
        except Exception as exc:
            raise RuntimeError("can't retrieve server heartbeat: %s" % exc)
        with open(path, 'rU') as inp:
            tstamp = inp.readline().strip()
            poll_delay = inp.readline().strip()
            version = inp.readline().strip()
    finally:
        try:
            os.remove(path)
        except Exception as exc:  # pragma no cover
            logger.warning("Can't remove temporary file: %s", exc)

    if '.' in tstamp:
        tstamp = datetime.datetime.strptime(tstamp, '%Y-%m-%d %H:%M:%S.%f')
    else:
        tstamp = datetime.datetime.strptime(tstamp, '%Y-%m-%d %H:%M:%S')
    now = datetime.datetime.utcnow()
    poll_delay = int(poll_delay)
    delta = now - tstamp
    if delta > datetime.timedelta(0, 4 * poll_delay):
        if delta.days:  # pragma no cover
            plural = 's' if delta.days > 1 else ''
            msg = '%d day%s' % (delta.days, plural)
        else:
            seconds = delta.seconds
            hours = int(seconds / 3600)
            seconds -= hours * 3600
            minutes = int(seconds / 60)
            seconds -= minutes * 60
            msg = '%d:%02d:%02d' % (hours, minutes, seconds)
        raise RuntimeError("server heartbeat hasn't been updated in %s" % msg)

    return poll_delay


# Server-side.
def server_cleanup(dmz_host, logger):  # pragma no cover
    """
    Close connection, removing all communication files.

    dmz_host: string
        Intermediary file server.

    logger: :class:`Logger`
        Displays progress messages.
    """
    root = _server_root()
    mapped_root = _map_dir(root)
    try:
        _ssh(dmz_host, ('rm', '-f', '%s*' % mapped_root), logger)
    except Exception as exc:
        logger.warning("Can't cleanup server root: %s", exc)


class Connection(object):
    """
    One end of a file-based communication channel.
    Files are stored on `dmz_host` in the `root` directory.

    dmz_host: string
        Intermediary file server.

    root: string
        Path to root directory for remote files and local copies.

    server: bool
        Set True if this is the server end of the connection.

    poll_delay: int
        Seconds to wait between polls when no timeout is specified.

    logger: :class:`Logger`
        Displays progress messages.
    """

    def __init__(self, dmz_host, root, server, poll_delay, logger):
        logger.info('initializing')
        self.dmz_host = dmz_host
        self.root = root
        self.poll_delay = poll_delay
        self._logger = logger
        self._seqno = 0         # Outgoing increments at send.
        self._remote_seqno = 1  # Incoming assumes increment.
        if server:  # pragma no cover
            self._prefix = 'S-'
            self._remote_prefix = 'C-'
        else:
            self._prefix = 'C-'
            self._remote_prefix = 'S-'

    @property
    def logger(self):
        """ This connection's logger. """
        return self._logger

    def close(self):
        """ Close connection, removing all communication files. """
        self._logger.debug('close')
        mapped_root = _map_dir(self.root)
        try:
            _ssh(self.dmz_host, ('rm', '-f', '%s*' % mapped_root), self._logger)
        except Exception as exc:
            self._logger.error('Error during close: %s', exc)

    def invoke(self, method, args=None, kwargs=None, timeout=0):
        """
        Invoke `method` with `args` and `kwargs` and return the result.

        method: string
            Remote method to be invoked.

        args: tuple
            Positional arguments for `method`.

        kwargs: dictionary
            Keyword arguments for `method`.

        timeout: int
            Seconds before giving up on reply. Zero implies no timeout.
        """
        args = args or ()
        kwargs = kwargs or {}
        self._logger.debug('request %d: %r %r %r',
                           self._seqno+1, method, args, kwargs)
        self.send_request((method, args, kwargs))
        result = self.recv_reply(True, timeout)
        self._logger.debug('reply %d: %r', self._seqno, result)
        if isinstance(result, RemoteError):
            self._logger.debug(result.traceback)
            raise result
        elif isinstance(result, Exception):
            raise RemoteError(result, '')
        return result

    def send_request(self, data):
        """
        Send request `data`.

        data: string
            Request to be sent.
        """
        self._seqno += 1
        self._send('%srequest' % self._prefix, data, self._seqno)

    def poll_reply(self):
        """ Return True if reply is ready. """
        return self._poll('%sreply' % self._remote_prefix, self._seqno)

    def recv_reply(self, wait=True, timeout=0):
        """
        Return reply.

        wait: bool
            If True, poll for reply ready.

        timeout: int
            Seconds before giving up on reply. Zero implies no timeout.
        """
        return self._recv('%sreply' % self._remote_prefix, self._seqno,
                          wait, timeout)

    # Server-side.
    def poll_request(self):  # pragma no cover
        """ Return True if request is ready. """
        return self._poll('%srequest' % self._remote_prefix, self._remote_seqno)

    # Server-side.
    def recv_request(self, wait=True, timeout=0):  # pragma no cover
        """
        Return request.

        wait: bool
            If True, poll for request ready.

        timeout: int
            Seconds before giving up on reply. Zero implies no timeout.
        """
        return self._recv('%srequest' % self._remote_prefix, self._remote_seqno,
                          wait, timeout)

    # Server-side.
    def send_reply(self, data):  # pragma no cover
        """
        Send reply `data`.

        data: string
            Data to be sent.
        """
        self._logger.debug('reply %d: %s', self._remote_seqno, data)
        self._send('%sreply' % self._prefix, data, self._remote_seqno)
        self._remote_seqno += 1

    def _send(self, prefix, data, seqno):
        """
        Send `data` for `prefix` and `seqno`.

        prefix: string
            Used to form destination filename.

        seqno: int
            Used to form destination filename.
        """
        name = '%s.%s' % (prefix, seqno)
        fd, path = tempfile.mkstemp()
        try:
            os.close(fd)
            with open(path, 'wb') as out:
                cPickle.dump(data, out, -1)
            self.send_file(path, name)
            ready = '%s-ready.%s' % (prefix, seqno)
            self.touch_file(ready)
        finally:
            try:
                os.remove(path)
            except Exception as exc:  # pragma no cover
                self._logger.warning("Can't remove temporary file: %s", exc)

    def _poll(self, prefix, seqno):
        """
        Return True if `prefix` message `seqno` is ready.

        prefix: string
            Used to form receive filename.

        seqno: int
            Used to form receive filename.
        """
        ready = '%s-ready.%s' % (prefix, seqno)
        mapped_root = _map_dir(self.root)
        lines = _ssh(self.dmz_host, ('ls', '-1'), self._logger)
        lines = [line[len(mapped_root):] for line in lines
                                         if line.startswith(mapped_root)]
        self._logger.log(DEBUG2, 'poll %s %s', ready, lines)
        return ready in lines

    def _wait(self, prefix, seqno, timeout=0):
        """
        Return when `prefix` message `seqno` is ready.

        prefix: string
            Used to form receive filename.

        seqno: int
            Used to form receive filename.

        timeout: int
            Seconds before giving up on reply. Zero implies no timeout.
        """
        start = time.time()
        delay = 1
        time.sleep(delay)
        while not self._poll(prefix, seqno):
            delay = min(delay + 1, self.poll_delay)  # Back-off polling rate.
            if timeout > 0:
                now = time.time()
                if now - start > timeout:
                    raise RuntimeError('timeout')
                # Set delay no longer than timeout.
                delay = min(delay, (start + timeout) - now)
            time.sleep(delay)

    def _recv(self, prefix, seqno, wait=True, timeout=0):
        """
        Return data contained in `prefix` message `seqno`.

        wait: bool
            If True, poll for receive file ready.

        prefix: string
            Used to form receive filename.

        seqno: int
            Used to form receive filename.

        timeout: int
            Seconds before giving up on reply. Zero implies no timeout.
        """
        if wait:
            self._wait(prefix, seqno, timeout)
        name = '%s.%s' % (prefix, seqno)
        fd, path = tempfile.mkstemp()
        try:
            os.close(fd)
            self.recv_file(name, path)
            with open(path, 'rb') as inp:
                data = cPickle.load(inp)
            ready = '%s-ready.%s' % (prefix, seqno)
            self.remove_files((name, ready))
        finally:
            try:
                os.remove(path)
            except Exception as exc:  # pragma no cover
                self._logger.warning("Can't remove temporary file: %s", exc)
        return data

    def send_file(self, path, name):
        """
        Copy `path` to remote file server as `name`.

        path: string
            Path to local file.

        name: string
            Name of remote file.
        """
        _scp_send(path, self.dmz_host, self.root, name, self._logger)

    def recv_file(self, name, path):
        """
        Copy `name` from remote file server to `path`.

        name: string
            Name of remote file.

        path: string
            Path to local file.
        """
        _scp_recv(self.dmz_host, self.root, name, path, self._logger)

    def remove_files(self, names):
        """
        Remove `name` from remote file server.

        names: sequence(string)
            Names of files to remove.
        """
        cmd = ['rm', '-f']
        for name in names:
            cmd.append(_map_path('%s/%s' % (self.root, name)))
        try:
            _ssh(self.dmz_host, cmd, self._logger)
        except Exception as exc:  # pragma no cover
            self._logger.warning("Can't remove remote file(s): %s", exc)

    def touch_file(self, name):
        """
        Create empty file `name` for existence checks.

        name: string
            Name of remote file to create.
        """
        fullname = '%s/%s' % (self.root, name)
        _ssh(self.dmz_host, ('date', '>', _map_path(fullname)), self._logger)

