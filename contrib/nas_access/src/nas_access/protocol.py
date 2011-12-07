"""
Remote procedure call protocol using an intermediary file server as the
communications channel acessed via 'ssh' and 'scp'.  Really.

Each server has its own directory (``RJE-<host>``). This contains a
``heartbeat`` file, updated periodically by the server to indicate it is still
running.

Each client allocator has its own directory within a server directory
(``<host>-<pid>-<name>``). Within that each allocated server gets its own
directory for the communication channel.

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
import os
import socket
import subprocess
import time

_CLIENTS = []
_SSH = 'ssh'
_SCP = 'scp'


def ssh(host, args):
    """ Return lines from `host` executing `command`. """
    cmd = [_SSH, host]
    cmd.extend(args)
    stdout = 'ssh.stdout'
    out = open(stdout, 'w')
    try:
        retcode = subprocess.call(cmd, stdout=out, stderr=subprocess.STDOUT)
    except Exception as exc:
        out.close()
        with open(stdout, 'rU') as inp:
            lines = inp.read()
        os.remove(stdout)
        msg = '%s: %s: %s' % (cmd, exc, lines)
        raise RuntimeError(msg)

    out.close()
    with open(stdout, 'rU') as inp:
        lines = inp.read()
    os.remove(stdout)
    if retcode:
        msg = '%s: retcode %s: %s' % (cmd, retcode, lines)
        raise RuntimeError(msg)

    return lines


def scp_send(host, directory, filename):
    """ Send `filename` to `host`:`directory`. """
    src = filename
    dst = '%s:%s/%s' % (host, directory, filename)
    _scp(src, dst)

def scp_recv(host, directory, filename):
    """ Receive `filename` from `host`:`directory`. """
    src = '%s:%s/%s' % (host, directory, filename)
    dst = filename
    _scp(src, dst)

def _scp(src, dst):
    """ Use 'scp' to copy `src` to `dst`. """
    cmd = [_SCP, src, dst]
    stdout = 'scp.stdout'
    out = open(stdout, 'w')
    try:
        retcode = subprocess.call(cmd)
    except Exception as exc:
        out.close()
        with open(stdout, 'rU') as inp:
            lines = inp.read()
        os.remove(out)
        msg = '%s: %s: %s' % (cmd, exc, lines)
        raise RuntimeError(msg)

    out.close()
    if retcode:
        with open(stdout, 'rU') as inp:
            lines = inp.read()
        os.remove(out)
        msg = '%s: retcode %s: %s' % (cmd, retcode, lines)
        raise RuntimeError(msg)
    else:
        os.remove(out)


def connect(dmz_host, server_host, path):
    """
    Client setup. Returns :class:`Connection`.

    dmz_host: string
        Intermediary file server.

    server_host: string
        Remote server host.

    path: string
        Path to communications directory.
    """
    root = 'RJE-%s' % server_host
    lines = ssh(dmz_host, ('ls', '-1', 'RJE-*'))
    for line in lines:
        if root in line:
            break
    else:
        raise RuntimeError('Server directory %r not found' % root)
    root = '%s/%s' % (root, path)
    ssh(dmz_host, ('mkdir', root))
    return Connection(dmz_host, root, False)


def server_init(dmz_host):
    """
    Server initialization.

    dmz_host: string
        Intermediary file server.
    """
    ssh(dmz_host, ('mkdir',  socket.gethostname()))

def server_accept(dmz_host):
    """
    Look for new client. Returns :class:`Connection` if found.

    dmz_host: string
        Intermediary file server.
    """
    root = 'RJE-%s' % socket.gethostname()
    lines = ssh(dmz_host, ('ls', '-1', root))
    for line in lines:
        if line not in _CLIENTS:
            _CLIENTS.append(line)
            root = '%s/%s' % (root, line)
            return Connection(dmz_host, root, True)
    return None

def server_heartbeat(dmz_host):
    """ Update top-level server heartbeat file. """
    root = 'RJE-%s' % socket.gethostname()
    heartbeat = 'heartbeat'
    with open(heartbeat, 'w') as out:
        out.write(datetime.datetime.utcnow().isoformat(' '))
    scp_send(dmz_host, root, heartbeat)
    os.remove(heartbeat)

def check_server_heartbeat(dmz_host, server_host):
    """" Check top-level server heartbeat file is 'current'. """
    root = 'RJE-%s' % server_host
    heartbeat = 'heartbeat'
    scp_recv(dmz_host, root, heartbeat)
    with open(heartbeat, 'r') as inp:
        timestamp = inp.read()
    os.remove(heartbeat)

def server_cleanup(dmz_host):
    """ Close connection, removing all communication files. """
    root = 'RJE-%s' % socket.gethostname()
    ssh(dmz_host, ('rm', '-rf', root))


class Connection(object):
    """
    One end of a file-based communication channel.
    Files are stored on `dmz_host` in the `root` directory.
    """

    def __init__(self, dmz_host, root, server):
        self.dmz_host = dmz_host
        self.root = root
        self.seqno = 0        # Outgoing increments at send.
        self.remote_seqno = 1 # Incoming assumes increment.
        if server:
            self.prefix = 'S-'
            self.remote_prefix = 'C-'
        else:
            self.prefix = 'C-'
            self.remote_prefix = 'S-'

    def close(self):
        """ Close connection, removing all communication files. """
        ssh(self.dmz_host, ('rm', '-rf', self.root))

    def invoke(self, method, args=None, kwargs=None, timeout=0, poll_delay=0):
        """ Invoke `method` with `args` and `kwargs` and return the result. """
        self.send_request(method, args, kwargs)
        return self.recv_reply(timeout, poll_delay)

    def send_request(self, method, args=None, kwargs=None):
        """ Send request to execute `method` with `args` and `kwargs`. """
        args = args or ()
        kwargs = kwargs or {}
        self.seqno += 1
        self.send('%srequest' % self.prefix,
                  (method, args, kwargs), self.seqno)

    def poll_reply(self):
        """ Return True if reply is ready. """
        return self.poll('%sreply' % self.remote_prefix, self.seqno)

    def recv_reply(self, timeout=0, poll_delay=0):
        """ Return reply. """
        return self.recv('%sreply' % self.remote_prefix, self.seqno,
                         timeout, poll_delay)

    def poll_request(self):
        """ Return True if request is ready. """
        return self.poll('%srequest' % self.remote_prefix, self.remote_seqno)

    def recv_request(self, timeout=0, poll_delay=0):
        """ Return request. """
        return self.recv('%srequest' % self.remote_prefix, self.remote_seqno,
                         timeout, poll_delay)

    def send_reply(self, data):
        """ Send reply `data`. """
        self.send('%sreply' % self.prefix, data, self.remote_seqno)
        self.remote_seqno += 1

    def send_exception(self, exc):
        """ Send exception `exc`. """
        self.send('%sreply' % self.prefix, exc, self.remote_seqno)
        self.remote_seqno += 1

    def heartbeat(self):
        """ Update heartbeat file. """
        heartbeat = '%sheartbeat' % self.prefix
        with open(heartbeat, 'w') as out:
            out.write(datetime.datetime.utcnow().isoformat(' '))
        self.send_file(heartbeat)
        os.remove(heartbeat)

    def check_heartbeat(self):
        """" Check that other end's heartbeat file is 'current'. """
        heartbeat = '%sheartbeat' % self.remote_prefix
        self.recv_file(heartbeat)
        with open(heartbeat, 'r') as inp:
            timestamp = inp.read()
        os.remove(heartbeat)

    def send(self, prefix, data, seqno):
        """ Send `data` for `prefix` and `seqno`. """
        name = '%s.%s' % (prefix, seqno)
        ready = '%s-ready.%s' % (prefix, seqno)
        with open(name, 'wb') as out:
            out.write(cPickle.dumps(data))
        self.send_file(name)
        self.touch_file(ready)
        os.remove(name)

    def poll(self, prefix, seqno):
        """ Return True if `prefix` message `seqno` is ready. """
        ready = '%s-ready.%s' % (prefix, seqno)
        if self.recv_file(ready):
            return True
        return False

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

    def recv(self, prefix, seqno, timeout=0, poll_delay=0):
        """ Return data contained in `prefix` message `seqno`. """
        self.wait(prefix, seqno, timeout, poll_delay)
        name = '%s.%s' % (prefix, seqno)
        self.recv_file(name)
        with open(name, 'rb') as inp:
            data = cPickle.loads(inp.read())
        os.remove(name)
        self.remove_file(name)
        ready = '%s-ready.%s' % (prefix, seqno)
        os.remove(ready)
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
        ssh(self.dmz_host, ('rm', '%s/%s' % (self.root, name)))

    def touch_file(self, name):
        """ Create empty file for existence checks. """
        # Actually putting something in the file for better fault detection.
        with open(name, 'w') as out:
            out.write('empty\n')
        self.send_file(name)
        os.remove(name)

