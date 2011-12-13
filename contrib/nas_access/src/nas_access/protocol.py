"""
Remote procedure call protocol using an intermediary file server as the
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

_CLIENTS = []

# These get reconfigured & restored during testing.
_SSH = ['ssh']
_SCP = ['scp']

def configure_ssh(cmdlist):
    """ Configure 'ssh' command' """
    while _SSH:
        _SSH.pop()
    _SSH.extend(cmdlist)

def configure_scp(cmdlist):
    """ Configure 'scp' command' """
    while _SCP:
        _SCP.pop()
    _SCP.extend(cmdlist)


def ssh(host, args, directory=''):
    """
    Return lines from `host` executing `command` with output in `directory`.
    """
    cmd = []
    cmd.extend(_SSH)
    cmd.append(host)
    cmd.extend(args)

    stdout = os.path.join(directory, 'ssh.stdout')
    stderr = os.path.join(directory, 'ssh.stderr')
    out = open(stdout, 'w')
    err = open(stderr, 'w')

    try:
        retcode = subprocess.call(cmd, stdout=out, stderr=err)
    except Exception as exc:
        out.close()
        err.close()
        with open(stdout, 'rU') as inp:
            outlines = inp.read()
        with open(stderr, 'rU') as inp:
            errlines = inp.read()
        msg = '%s: %s: stdout: %s stderr: %s' \
              % (cmd, exc, outlines, errlines)
        raise RuntimeError(msg)
    else:
        out.close()
        err.close()
        with open(stdout, 'rU') as inp:
            outlines = inp.read()
        with open(stderr, 'rU') as inp:
            errlines = inp.read()
        if retcode:
            msg = '%s: retcode %s: stdout: %s stderr: %s' \
                  % (cmd, retcode, outlines, errlines)
            raise RuntimeError(msg)
        return outlines.split()
    finally:
        os.remove(stdout)
        os.remove(stderr)


def scp_send(host, directory, filename):
    """ Send `filename` in `directory` to `host`:`directory`. """
    src = os.path.join(directory, filename)
    dst = '%s:%s/%s' % (host, directory, filename)
    _scp(src, dst, directory)

def scp_recv(host, directory, filename):
    """ Receive `filename` in `directory` from `host`:`directory`. """
    src = '%s:%s/%s' % (host, directory, filename)
    dst = os.path.join(directory, filename)
    _scp(src, dst, directory)

def _scp(src, dst, directory):
    """ Use 'scp' to copy `src` to `dst` with output in `directory`. """
    cmd = []
    cmd.extend(_SCP)
    cmd.extend((src, dst))

    stdout = os.path.join(directory, 'scp.stdout')
    out = open(stdout, 'w')

    try:
        retcode = subprocess.call(cmd, stdout=out, stderr=subprocess.STDOUT)
    except Exception as exc:
        out.close()
        with open(stdout, 'rU') as inp:
            lines = inp.read()
        msg = '%s: %s: %s' % (cmd, exc, lines)
        raise RuntimeError(msg)
    else:
        out.close()
        if retcode:
            with open(stdout, 'rU') as inp:
                lines = inp.read()
            msg = '%s: retcode %s: %s' % (cmd, retcode, lines)
            raise RuntimeError(msg)
    finally:
        os.remove(stdout)


def connect(dmz_host, server_host, path, logger):
    """
    Connect client to server. Returns :class:`Connection`.

    Top-level connections are initiated by client creating communication
    directory in server's root directory.

    Lower-level connections are made by client sending a request for a new
    resource, server creates communication directory, starts polling thread,
    and returns the name of the new directory.

    dmz_host: string
        Intermediary file server.

    server_host: string
        Remote server host.

    path: string
        Path to communications directory.
    """
    root = server_root(server_host)
    if not os.path.exists(root):  # Ensure we have a local tree.
        os.mkdir(root)

    lines = ssh(dmz_host, ('ls', '-1'), root)
    if root not in lines:
        raise RuntimeError('Server directory %r not found' % root)

    if '/' in path:  # Connect to existing communications directory.
        root = path
    else:            # Create communications directory.
        root = '%s/%s' % (root, path)
    if not os.path.exists(root):
        os.mkdir(root)

    if root != path:  # Create communications directory.
        parent, slash, child = root.rpartition('/')
        lines = ssh(dmz_host, ('ls', '-1', parent), root)
        if child in lines:  # Need a clean directory.
            raise RuntimeError('Communications directory %r already exists',
                               root)
        ssh(dmz_host, ('mkdir', root))

    return Connection(dmz_host, root, False, logger)


def server_root(hostname=None):
    """
    Returns root directory for `hostname`
    (default :meth:`socket.gethostname()).
    """
    return 'RJE-%s' % socket.gethostname()

def server_init(dmz_host):
    """
    Server initialization.

    dmz_host: string
        Intermediary file server.
    """
    root = server_root()
    if os.path.exists(root):  # Ensure a clean local tree.
        shutil.rmtree(root)
    os.mkdir(root)

    lines = ssh(dmz_host, ('ls', '-1'), root)
    if root in lines:  # Ensure a clean remote tree.
        ssh(dmz_host, ('rm', '-r', root))
    ssh(dmz_host, ('mkdir',  root))

def server_accept(dmz_host):
    """
    Look for new client. Returns :class:`Connection` if found.

    dmz_host: string
        Intermediary file server.
    """
    root = server_root()
    lines = ssh(dmz_host, ('ls', '-1', root))

    for client in _CLIENTS:
        if client not in lines:
            print 'Client %r closed' % client
            _CLIENTS.remove(client)

    for line in lines:
        if line == 'heartbeat':
            continue
        if line not in _CLIENTS:
            _CLIENTS.append(line)
            root = '%s/%s' % (root, line)
            logging.debug('server_accept: new connection at %s:%s',
                          dmz_host, root)
            print 'server_accept: new connection at %s:%s' % (dmz_host, root)
            logger = logging.getLogger('%s_handler' % line)
            return Connection(dmz_host, root, True, logger)
    
    return None

def server_heartbeat(dmz_host):
    """ Update top-level server heartbeat file. """
    root = server_root()
    heartbeat = os.path.join(root, 'heartbeat')
    with open(heartbeat, 'w') as out:
        out.write(datetime.datetime.utcnow().isoformat(' '))
    scp_send(dmz_host, root, os.path.basename(heartbeat))
    os.remove(heartbeat)

def check_server_heartbeat(dmz_host, server_host):
    """" Check top-level server heartbeat file is 'current'. """
    root = server_root()
    heartbeat = os.path.join(root, 'heartbeat')
    scp_recv(dmz_host, heartbeat)
    with open(heartbeat, 'r') as inp:
        timestamp = inp.read()
    os.remove(heartbeat)

def server_cleanup(dmz_host):
    """ Close connection, removing all communication files. """
    root = server_root()
    ssh(dmz_host, ('rm', '-rf', root))
    shutil.rmtree(root)


class Connection(object):
    """
    One end of a file-based communication channel.
    Files are stored on `dmz_host` in the `root` directory.
    """

    def __init__(self, dmz_host, root, server, logger):
        self.dmz_host = dmz_host
        self.root = root
        self.logger = logger
        self.seqno = 0        # Outgoing increments at send.
        self.remote_seqno = 1 # Incoming assumes increment.
        if server:
            self.prefix = 'S-'
            self.remote_prefix = 'C-'
        else:
            self.prefix = 'C-'
            self.remote_prefix = 'S-'
        if os.path.exists(root):
            shutil.rmtree(root)
        os.mkdir(root)
        parent, slash, child = root.partition('/')
        lines = ssh(self.dmz_host, ('ls', '-1', parent), root)
        if server:
            if not child in lines:
                ssh(self.dmz_host, ('mkdir', root), root)

    def close(self):
        """ Close connection, removing all communication files. """
        print 'closing %r' % self.root
        self.logger.debug('close')
        ssh(self.dmz_host, ('rm', '-rf', self.root), self.root)
        shutil.rmtree(self.root)

    def invoke(self, method, args=None, kwargs=None, timeout=0, poll_delay=0):
        """ Invoke `method` with `args` and `kwargs` and return the result. """
        self.logger.debug('invoke %r', method)
        args = args or ()
        kwargs = kwargs or {}
        self.send_request(method, args, kwargs)
        return self.recv_reply(True, timeout, poll_delay)

    def send_request(self, method, args=None, kwargs=None):
        """ Send request to execute `method` with `args` and `kwargs`. """
        self.logger.debug('send_request %r', method)
        args = args or ()
        kwargs = kwargs or {}
        self.seqno += 1
        self.send('%srequest' % self.prefix, (method, args, kwargs), self.seqno)

    def poll_reply(self):
        """ Return True if reply is ready. """
        return self.poll('%sreply' % self.remote_prefix, self.seqno)

    def recv_reply(self, wait=True, timeout=0, poll_delay=0):
        """ Return reply. """
        return self.recv('%sreply' % self.remote_prefix, self.seqno,
                         wait, timeout, poll_delay)

    def poll_request(self):
        """ Return True if request is ready. """
        return self.poll('%srequest' % self.remote_prefix, self.remote_seqno)

    def recv_request(self, wait=True, timeout=0, poll_delay=0):
        """ Return request. """
        return self.recv('%srequest' % self.remote_prefix, self.remote_seqno,
                         wait, timeout, poll_delay)

    def send_reply(self, data):
        """ Send reply `data`. """
        self.logger.debug('send_reply')
        self.send('%sreply' % self.prefix, data, self.remote_seqno)
        self.remote_seqno += 1

    def send_exception(self, exc):
        """ Send exception `exc`. """
        self.send('%sreply' % self.prefix, exc, self.remote_seqno)
        self.remote_seqno += 1

    def heartbeat(self):
        """ Update heartbeat file. """
        heartbeat = os.path.join(self.root, '%sheartbeat' % self.prefix)
        with open(heartbeat, 'w') as out:
            out.write(datetime.datetime.utcnow().isoformat(' '))
        self.send_file(os.path.basename(heartbeat))
        os.remove(heartbeat)

    def check_heartbeat(self):
        """" Check that other end's heartbeat file is 'current'. """
        heartbeat = '%sheartbeat' % self.remote_prefix
        self.recv_file(heartbeat)
        heartbeat = os.path.join(self.root, heartbeat)
        with open(heartbeat, 'r') as inp:
            timestamp = inp.read()
        os.remove(heartbeat)

    def send(self, prefix, data, seqno):
        """ Send `data` for `prefix` and `seqno`. """
        name = os.path.join(self.root, '%s.%s' % (prefix, seqno))
        with open(name, 'wb') as out:
            out.write(cPickle.dumps(data))
        self.send_file(os.path.basename(name))
        ready = '%s-ready.%s' % (prefix, seqno)
        self.touch_file(ready)
        os.remove(name)

    def poll(self, prefix, seqno):
        """ Return True if `prefix` message `seqno` is ready. """
        ready = '%s-ready.%s' % (prefix, seqno)
        lines = ssh(self.dmz_host, ('ls', '-1', self.root), self.root)
        print 'poll %s %s' % (ready, lines)
        return ready in lines

    def wait(self, prefix, seqno, timeout=0, poll_delay=0):
        """ Return when `prefix` message `seqno` is ready. """
        if poll_delay <= 0:
            if timeout <= 0:
                delay = 1
            else:
                delay = timeout / 10.
        else:
            delay = poll_delay

        start = time.time()
        while not self.poll(prefix, seqno):
            if timeout > 0:
                now = time.time()
                if now - start > timeout:
                    raise RuntimeError('timeout')
            time.sleep(delay)

    def recv(self, prefix, seqno, wait=True, timeout=0, poll_delay=0):
        """ Return data contained in `prefix` message `seqno`. """
        if wait:
            self.wait(prefix, seqno, timeout, poll_delay)
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
        """ Copy `name` to remote file server. """
        scp_send(self.dmz_host, self.root, name)

    def recv_file(self, name):
        """ Copy `name` from remote file server. """
        scp_recv(self.dmz_host, self.root, name)

    def remove_file(self, name):
        """ Remove `name` from remote file server. """
        ssh(self.dmz_host, ('rm', '%s/%s' % (self.root, name)), self.root)

    def touch_file(self, name):
        """ Create empty file `name` for existence checks. """
        # Actually putting something in the file for better fault detection.
        fullname = os.path.join(self.root, name)
        with open(fullname, 'w') as out:
            out.write('empty\n')
        self.send_file(name)
        os.remove(fullname)

