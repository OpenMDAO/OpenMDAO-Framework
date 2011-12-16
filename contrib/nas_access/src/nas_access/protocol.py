"""
'DMZ' remote procedure call protocol using an intermediary file server as the
communications channel acessed via 'ssh' and 'scp'.  Really.

Each server has its own directory (``RJE-<host>``). This contains a
``heartbeat`` file, updated periodically by the server to indicate it is still
running.

Each client allocator has its own directory within a server directory
(``<host>-<pid>-<name>``). Within that each allocated server gets its own
directory for the communication channel.

DMZ-root/
    Server1-root/
        Client1-root/
            Sim-1/
            Sim-2/
        Client2-root/
            Sim-1/

Each communication direction has its own set of files within this directory
prefixed by 'C' or 'S':

    ``[CS]-request.<mid>``
        Pickled ``(method, args, kwargs)``.

    ``[CS]-request_ready.<mid>``
        Flag indicating that all of ``[CS]-request.<mid>`` has been written.

    ``[CS]-reply.<mid>``
        Picked reply data.

    ``[CS]-reply_ready.<mid>``
        Flag indicating that all of ``[CS]-reply.<mid>`` has been written.

where <mid> is a message sequence number. Files are created by the sender and
removed by the receiver.
"""

import cPickle
import datetime
import logging
import os.path
import shutil
import socket
import subprocess
import time

DEBUG2 = logging.DEBUG - 2  # Show polling.
DEBUG3 = logging.DEBUG - 3  # Show ssh/scp.

if logging.getLevelName(DEBUG2) == 'Level 8':
    logging.addLevelName(DEBUG2, 'D2')
if logging.getLevelName(DEBUG3) == 'Level 7':
    logging.addLevelName(DEBUG3, 'D3')

_CLIENTS = []  # Server client names.


# These get reconfigured & restored during testing.
_SSH = ['ssh']
_SCP = ['scp']

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


def _ssh(host, args, logger):
    """
    Return lines from `host` executing `args`.

    args: list[string]
        Command and arguments to be executed.

    logger: :class:`Logger`
        Displays full command being executed.
    """
    cmd = []
    cmd.extend(_SSH)
    cmd.append(host)
    cmd.extend(args)

    logger.log(DEBUG3, '%s', cmd)
    try:
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, universal_newlines=True)
    except Exception as exc:
        raise RuntimeError('%s: %s' % (cmd, exc))

    stdout, stderr = proc.communicate()
    if proc.returncode:
        msg = '%s: returncode %s: stdout: %s stderr: %s' \
              % (cmd, proc.returncode, stdout, stderr)
        raise RuntimeError(msg)
    return stdout.split()


def _scp_send(host, directory, filename, logger):
    """
    Send `filename` in `directory` to `host`:`directory`.

    host: string
        Host to send to.

    directory: string
        Directory containing local and remote copies.

    filename: string
        File to transfer.

    logger: :class:`Logger`
        Displays full command being executed.
    """
    src = os.path.join(directory, filename)
    dst = '%s:%s/%s' % (host, directory, filename)
    _scp(src, dst, logger)


def _scp_recv(host, directory, filename, logger):
    """
    Receive `filename` in `directory` from `host`:`directory`.

    host: string
        Host to send to.

    directory: string
        Directory containing local and remote copies.

    filename: string
        File to transfer.

    logger: :class:`Logger`
        Displays full command being executed.
    """
    src = '%s:%s/%s' % (host, directory, filename)
    dst = os.path.join(directory, filename)
    _scp(src, dst, logger)


def _scp(src, dst, logger):
    """
    Use 'scp' to copy `src` to `dst`.

    src, dst: string
        Source and destination designations.

    filename: string
        File to transfer.

    logger: :class:`Logger`
        Displays full command being executed.
    """
    cmd = []
    cmd.extend(_SCP)
    cmd.extend((src, dst))

    logger.log(DEBUG3, '%s', cmd)
    try:
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, universal_newlines=True)
    except Exception as exc:
        raise RuntimeError('%s: %s' % (cmd, exc))

    stdout, stderr = proc.communicate()
    if proc.returncode:
        msg = '%s: returncode %s: stdout: %s stderr: %s' \
              % (cmd, proc.returncode, stdout, stderr)
        raise RuntimeError(msg)


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
    if not os.path.exists(root):  # Ensure we have a local tree.
        os.mkdir(root)

    lines = _ssh(dmz_host, ('ls', '-1'), logger)
    if root not in lines:
        raise RuntimeError('Server directory %r not found' % root)

    if '/' in path:  # Connect to existing communications directory.
        root = path
    else:            # Create communications directory.
        root = '%s/%s-%s-%s' % (root, socket.gethostname(), os.getpid(), path)
    if not os.path.exists(root):
        os.mkdir(root)

    if root != path:  # Create communications directory.
        parent, slash, child = root.rpartition('/')
        lines = _ssh(dmz_host, ('ls', '-1', parent), logger)
        if child in lines:  # Need a clean directory.
            raise RuntimeError('Client directory %r already exists', root)
        _ssh(dmz_host, ('mkdir', root), logger)

    return Connection(dmz_host, root, False, logger)


def _server_root(hostname=None):
    """
    Returns root directory for `hostname`
    (default :meth:`socket.gethostname()).

    hostname: string
        Host name to use rather than default.
    """
    hostname = hostname or socket.gethostname()
    return 'RJE-%s' % hostname


def server_init(dmz_host, logger):
    """
    Server initialization.

    dmz_host: string
        Intermediary file server.

    logger: :class:`Logger`
        Displays progress messages.
    """
    root = _server_root()
    if os.path.exists(root):  # Ensure a clean local tree.
        shutil.rmtree(root)
    os.mkdir(root)

    lines = _ssh(dmz_host, ('ls', '-1'), logger)
    if root in lines:  # Ensure a clean remote tree.
        _ssh(dmz_host, ('rm', '-r', root), logger)
    _ssh(dmz_host, ('mkdir',  root), logger)


def server_accept(dmz_host, logger):
    """
    Look for new client. Returns :class:`Connection` if found.

    dmz_host: string
        Intermediary file server.

    logger: :class:`Logger`
        Displays progress messages, passed to created :class:`Connection`.
    """
    root = _server_root()
    lines = _ssh(dmz_host, ('ls', '-1', root), logger)

    for client in _CLIENTS:
        if client not in lines:
            logger.info('Client %r closed', client)
            _CLIENTS.remove(client)

    for line in lines:
        if line == 'heartbeat':
            continue
        if line not in _CLIENTS:
            _CLIENTS.append(line)
            logger.info('New client %r', line)
            root = '%s/%s' % (root, line)
            logger = logging.getLogger(line)
            return Connection(dmz_host, root, True, logger)
    
    return None


def server_heartbeat(dmz_host, logger):
    """
    Update top-level server heartbeat file.

    dmz_host: string
        Intermediary file server.
    """
    logger.log(DEBUG2, 'heartbeat')
    root = _server_root()
    heartbeat = os.path.join(root, 'heartbeat')
    with open(heartbeat, 'w') as out:
        out.write(datetime.datetime.utcnow().isoformat(' '))
    _scp_send(dmz_host, root, os.path.basename(heartbeat), logger)
    os.remove(heartbeat)


def check_server_heartbeat(dmz_host, server_host, logger):
    """
    Check top-level server heartbeat file is 'current'.

    dmz_host: string
        Intermediary file server.

    server_host: string
        Remote server.
    """
    logger.log(DEBUG2, 'check_server_heartbeat')
    root = _server_root(server_host)
    heartbeat = os.path.join(root, 'heartbeat')
    _scp_recv(dmz_host, root, heartbeat, logger)
    with open(heartbeat, 'r') as inp:
        timestamp = inp.read()
    os.remove(heartbeat)


def server_cleanup(dmz_host, logger):
    """
    Close connection, removing all communication files.

    dmz_host: string
        Intermediary file server.

    logger: :class:`Logger`
        Displays progress messages.
    """
    root = _server_root()
    _ssh(dmz_host, ('rm', '-rf', root), logger)
    if os.path.exists(root):
        shutil.rmtree(root)


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

    logger: :class:`Logger`
        Displays progress messages.
    """

    def __init__(self, dmz_host, root, server, logger):
        self.dmz_host = dmz_host
        self.root = root
        self._logger = logger
        self._seqno = 0        # Outgoing increments at send.
        self._remote_seqno = 1 # Incoming assumes increment.
        if server:
            self._prefix = 'S-'
            self._remote_prefix = 'C-'
        else:
            self._prefix = 'C-'
            self._remote_prefix = 'S-'
        logger.info('initializing')
        if os.path.exists(root):
            shutil.rmtree(root)
        os.mkdir(root)
        parent, slash, child = root.partition('/')
        lines = _ssh(self.dmz_host, ('ls', '-1', parent), logger)
        if server:
            if not child in lines:
                _ssh(self.dmz_host, ('mkdir', root), logger)

    @property
    def logger(self):
        """ This connection's logger. """
        return self._logger

    def close(self):
        """ Close connection, removing all communication files. """
        self._logger.debug('close')
        _ssh(self.dmz_host, ('rm', '-rf', self.root), self._logger)
        shutil.rmtree(self.root)

    def invoke(self, method, args=None, kwargs=None, timeout=0, poll_delay=0):
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

        poll_delay: int
            Seconds between polls. Zero implies an internal default.
        """
        args = args or ()
        kwargs = kwargs or {}
        self._logger.debug('request: %r %r %r', method, args, kwargs)
        self.send_request((method, args, kwargs))
        result = self.recv_reply(True, timeout, poll_delay)
        self._logger.debug('reply: %s', result)
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

    def recv_reply(self, wait=True, timeout=0, poll_delay=0):
        """
        Return reply.

        wait: bool
            If True, poll for reply ready.

        timeout: int
            Seconds before giving up on reply. Zero implies no timeout.

        poll_delay: int
            Seconds between polls. Zero implies an internal default.
        """
        return self._recv('%sreply' % self._remote_prefix, self._seqno,
                          wait, timeout, poll_delay)

    def poll_request(self):
        """ Return True if request is ready. """
        return self._poll('%srequest' % self._remote_prefix, self._remote_seqno)

    def recv_request(self, wait=True, timeout=0, poll_delay=0):
        """
        Return request.

        wait: bool
            If True, poll for request ready.

        timeout: int
            Seconds before giving up on reply. Zero implies no timeout.

        poll_delay: int
            Seconds between polls. Zero implies an internal default.
        """
        return self._recv('%srequest' % self._remote_prefix, self._remote_seqno,
                          wait, timeout, poll_delay)

    def send_reply(self, data):
        """
        Send reply `data`.

        data: string
            Data to be sent.
        """
        self._logger.debug('reply: %s', data)
        self._send('%sreply' % self._prefix, data, self._remote_seqno)
        self._remote_seqno += 1

    def send_exception(self, exc):
        """
        Send exception `exc`.

        exc: :class:`Exception`
            Exception to be sent.
        """
        self._logger.debug('exception: %s', exc)
        self._send('%sreply' % self._prefix, exc, self._remote_seqno)
        self._remote_seqno += 1

    def _heartbeat(self):
        """ Update heartbeat file. """
        self._logger.log(DEBUG2, 'heartbeat')
        heartbeat = os.path.join(self.root, '%sheartbeat' % self._prefix)
        with open(heartbeat, 'w') as out:
            out.write(datetime.datetime.utcnow().isoformat(' '))
        self.send_file(os.path.basename(heartbeat))
        os.remove(heartbeat)

    def _check_heartbeat(self):
        """" Check that other end's heartbeat file is 'current'. """
        self._logger.log(DEBUG2, 'check_heartbeat')
        heartbeat = '%sheartbeat' % self._remote_prefix
        self.recv_file(heartbeat)
        heartbeat = os.path.join(self.root, heartbeat)
        with open(heartbeat, 'r') as inp:
            timestamp = inp.read()
        os.remove(heartbeat)

    def _send(self, prefix, data, seqno):
        """
        Send `data` for `prefix` and `seqno`.

        prefix: string
            Used to form destination filename.

        seqno: int
            Used to form destination filename.
        """
        name = os.path.join(self.root, '%s.%s' % (prefix, seqno))
        with open(name, 'wb') as out:
            out.write(cPickle.dumps(data))
        self.send_file(os.path.basename(name))
        ready = '%s-ready.%s' % (prefix, seqno)
        self.touch_file(ready)
        os.remove(name)

    def _poll(self, prefix, seqno):
        """
        Return True if `prefix` message `seqno` is ready.

        prefix: string
            Used to form receive filename.

        seqno: int
            Used to form receive filename.
        """
        ready = '%s-ready.%s' % (prefix, seqno)
        lines = _ssh(self.dmz_host, ('ls', '-1', self.root), self._logger)
        self._logger.log(DEBUG2, 'poll %s %s', ready, lines)
        return ready in lines

    def _wait(self, prefix, seqno, timeout=0, poll_delay=0):
        """
        Return when `prefix` message `seqno` is ready.

        prefix: string
            Used to form receive filename.

        seqno: int
            Used to form receive filename.

        timeout: int
            Seconds before giving up on reply. Zero implies no timeout.

        poll_delay: int
            Seconds between polls. Zero implies an internal default.
        """
        if poll_delay <= 0:
            if timeout <= 0:
                delay = 1
            else:
                delay = timeout / 10.
        else:
            delay = poll_delay

        start = time.time()
        while not self._poll(prefix, seqno):
            if timeout > 0:
                now = time.time()
                if now - start > timeout:
                    raise RuntimeError('timeout')
            time.sleep(delay)

    def _recv(self, prefix, seqno, wait=True, timeout=0, poll_delay=0):
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

        poll_delay: int
            Seconds between polls. Zero implies an internal default.
        """
        if wait:
            self._wait(prefix, seqno, timeout, poll_delay)
        name = '%s.%s' % (prefix, seqno)
        fullname = os.path.join(self.root, name)
        self.recv_file(name)
        with open(fullname, 'rb') as inp:
            data = cPickle.loads(inp.read())
        os.remove(fullname)
        self.remove_file(name)
        ready = '%s-ready.%s' % (prefix, seqno)
        self.remove_file(ready)
        return data

    def send_file(self, name):
        """
        Copy `name` to remote file server.

        name: string
            Name of file to send.
        """
        _scp_send(self.dmz_host, self.root, name, self._logger)

    def recv_file(self, name):
        """
        Copy `name` from remote file server.

        name: string
            Name of file to receive.
        """
        _scp_recv(self.dmz_host, self.root, name, self._logger)

    def remove_file(self, name):
        """
        Remove `name` from remote file server.

        name: string
            Name of file to remove.
        """
        _ssh(self.dmz_host, ('rm', '%s/%s' % (self.root, name)), self._logger)

    def touch_file(self, name):
        """
        Create empty file `name` for existence checks.

        name: string
            Name of file to create.
        """
        # Actually putting something in the file for better fault detection.
        fullname = os.path.join(self.root, name)
        with open(fullname, 'w') as out:
            out.write('empty\n')
        self.send_file(name)
        os.remove(fullname)

