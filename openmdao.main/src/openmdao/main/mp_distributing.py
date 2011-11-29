"""
This module is based on the *distributing.py* file example which was
(temporarily) posted with the multiprocessing module documentation.

This code assumes `ssh` has been set-up on all hosts such that no user
intervention for passwords or passphrases is required.
"""

import copy
import cPickle
import getpass
import logging
import os
import Queue
import shutil
import socket
import subprocess
import sys
import tarfile
import threading
import time
import traceback

from multiprocessing import current_process, managers
from multiprocessing import util, connection, forking

from openmdao.main.mp_support import OpenMDAO_Manager, OpenMDAO_Server, \
                                     register, decode_public_key, keytype
from openmdao.main.rbac import get_credentials, set_credentials

from openmdao.util.wrkpool import WorkerPool


# SSH command to be used to access remote hosts.
if sys.platform == 'win32':  #pragma no cover
    _SSH = r'C:\Putty\%s%s' % (getpass.getuser(), '.ppk')
    _SSH = [r'C:\Putty\plink.exe', '-i', _SSH]
else:
    _SSH = ['ssh']

# Logging.
_LOGGER = logging.getLogger('mp_distributing')


# This is used by Cluster, which also isn't covered.
# Cluster allocation requires ssh configuration and multiple hosts.
class HostManager(OpenMDAO_Manager):  #pragma no cover
    """
    Manager used for spawning processes on a remote host.

    address: (ip_addr, port) or string referring to pipe.
        Address to use to connect back to parent.

    authkey: string
        Authorization key, passed to :class:`OpenMDAO_Manager`.
    """

    def __init__(self, address, authkey):
        super(HostManager, self).__init__(address, authkey)
        self._name = 'Host-unknown'

    @classmethod
    def from_address(cls, address, authkey):
        """
        Return manager given an address.

        address: (ip_addr, port) or string referring to pipe.
            Address to connect to.

        authkey: string
            Authorization key.
        """
        manager = cls(address, authkey)
        conn = connection.Client(address, authkey=authkey)
        try:
            managers.dispatch(conn, None, 'dummy')
        finally:
            conn.close()
        manager._state.value = managers.State.STARTED
        manager._name = 'Host-%s:%s' % manager.address
        manager.shutdown = util.Finalize(
            manager, HostManager._finalize_host,
            args=(manager._address, manager._authkey, manager._name),
            exitpriority=-10
            )
        return manager

    @staticmethod
    def _finalize_host(address, authkey, name):
        """ Sends a shutdown message. """
        conn = connection.Client(address, authkey=authkey)
        try:
            return managers.dispatch(conn, None, 'shutdown')
        finally:
            conn.close()

    def __repr__(self):
        return '<Host(%s)>' % self._name


# Cluster allocation requires ssh configuration and multiple hosts.
class Cluster(OpenMDAO_Manager):  #pragma no cover
    """
    Represents a collection of hosts.

    hostlist: list(:class:`Host`)
        Hosts which are to be members of the cluster.

    modules: list(string)
        Names of modules to be sent to each host.

    authkey: string
        Authorization key, passed to :class:`OpenMDAO_Manager`.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Use with caution!
    """

    def __init__(self, hostlist, modules=None, authkey=None, allow_shell=False):
        super(Cluster, self).__init__(authkey=authkey)
        self._hostlist = hostlist
        self._allow_shell = allow_shell
        modules = modules or []
        if __name__ not in modules:
            modules.append(__name__)
        files = [sys.modules[name].__file__ for name in modules]
        for i, filename in enumerate(files):
            if filename.endswith(('.pyc', '.pyo')):
                files[i] = filename[:-1]
        self._files = [os.path.abspath(filename) for filename in files]
        self._reply_q = Queue.Queue()
        self._up = []

    def __getitem__(self, i):
        return self._up[i]

    def __iter__(self):
        return iter(self._up)

    def __len__(self):
        return len(self._up)

    def start(self):
        """ Start this manager and all remote managers. """
        super(Cluster, self).start()
        hostname = socket.getfqdn()
        listener = connection.Listener(address=(hostname, 0),
                                       authkey=self._authkey,
                                       backlog=5)  # Default is 1.
# TODO: support multiple addresses if multiple networks are attached.

        # Start managers in separate thread to avoid losing connections.
        starter = threading.Thread(target=self._start_hosts,
                                   args=(listener.address, get_credentials()))
        starter.daemon = True
        starter.start()

        # Accept callback connections from started managers.
        waiting = ['']
        retry = 0
        while waiting:
            host_processed = False
            for host in self._hostlist:
                host.poll()
                if host.state == 'started':
                    # Accept conection from *any* host.
                    _LOGGER.debug('waiting for a connection, host %s',
                                  host.hostname)
                    # This will hang if server doesn't receive our address.
                    conn = listener.accept()
                    i, address, pubkey_text = conn.recv()
                    conn.close()
                    other_host = self._hostlist[i]
                    if address is None:
                        _LOGGER.error('Host %s died: %s', other_host.hostname,
                                      pubkey_text)  # Exception text.
                        continue

                    other_host.manager = HostManager.from_address(address,
                                                                  self._authkey)
                    other_host.state = 'up'
                    if pubkey_text:
                        other_host.manager._pubkey = \
                            decode_public_key(pubkey_text)
                    host_processed = True
                    _LOGGER.debug('Host %s is now up', other_host.hostname)
                    self._up.append(other_host)

            # See if there are still hosts to wait for.
            waiting = []
            for host in self._hostlist:
                host.poll()
                if host.state == 'init' or host.state == 'started':
                    waiting.append(host)
            if waiting:
                if not host_processed:
                    retry += 1
                    if retry < 600:  # ~60 seconds.
                        time.sleep(0.1)
                    else:
                        _LOGGER.warning('Cluster startup timeout,'
                                        ' hosts not started:')
                        for host in waiting:
                            _LOGGER.warning('    %s (%s) in dir %s',
                                            host.hostname, host.state,
                                            host.tempdir)
                        break
            else:
                break

        self._up = sorted(self._up, key=lambda host: host.hostname)

        self._base_shutdown = self.shutdown
        del self.shutdown

    def _start_hosts(self, address, credentials):
        """
        Start host managers. Sequence for each host is:
        1. Check connectivity via simple 'ssh' call.
        2. Send startup files.
        3. Invoke remote Python process. (state 'started')
        4. Receive remote connection information. (state 'up')
        """
        # Start first set of hosts.
        todo = []
        max_workers = 5  # Somewhat related to listener backlog.
        for i, host in enumerate(self._hostlist):
            if i < max_workers:
                worker_q = WorkerPool.get()
                _LOGGER.info('Starting host %s...', host.hostname)
                worker_q.put((self._start_manager,
                             (host, i, address, credentials), {},
                              self._reply_q))
            else:
                todo.append(host)

        # Wait for worker, start next host.
        for i in range(len(self._hostlist)):
            worker_q, host, exc, trace = self._reply_q.get()
            if exc:
                _LOGGER.error(trace)
                raise exc

            _LOGGER.debug('Host %r state %s', host.hostname, host.state)
            try:
                next_host = todo.pop(0)
            except IndexError:
                WorkerPool.release(worker_q)
            else:
                _LOGGER.info('Starting host %s...', next_host.hostname)
                worker_q.put((self._start_manager,
                              (next_host, i+max_workers, address, credentials),
                               {}, self._reply_q))

    def _start_manager(self, host, i, address, credentials):
        """ Start one host manager. """
        set_credentials(credentials)
        try:
            host.start_manager(i, self._authkey, address, self._files,
                               self._allow_shell)
        except Exception as exc:
            msg = '%s\n%s' % (exc, traceback.format_exc())
            _LOGGER.error('starter for %s caught exception %s',
                          host.hostname, msg)
        return host

    def shutdown(self):
        """ Shut down all remote managers and then this one. """
        for host in self._hostlist:
            if host.state == 'up':
                host.state = 'shutdown'
                host.manager.shutdown()
        self._base_shutdown()


# Requires ssh configuration.
class Host(object):  #pragma no cover
    """
    Represents a host to use as a node in a cluster.

    hostname: string
        Name of the host. `ssh` is used to log into the host. To log in as a
        different user use a host name of the form: "username@somewhere.org".

    python: string
        Path the the Python command to be used on `hostname`.
    """

    def __init__(self, hostname, python=None):
        self.hostname = hostname
        # Putty/Plink.exe wants user@host always.
        parts = hostname.split('@')
        if len(parts) == 1:
            user = getpass.getuser()
            self.hostname = '%s@%s' % (user, hostname)
        self.python = python or 'python'
        self.registry = {}
        self.state = 'init'
        self.proc = None
        self.tempdir = None

    def register(self, cls):
        """
        Register proxy info to be sent to remote server.

        cls: class
            Class to be registered.
        """
        name = cls.__name__
        module = cls.__module__
        self.registry[name] = module

    def start_manager(self, index, authkey, address, files, allow_shell=False):
        """
        Launch remote manager process via `ssh`.
        The environment variable ``OPENMDAO_KEEPDIRS`` can be used to avoid
        removal of the temporary directory used on the host.

        index: int
            Index in parent cluster.

        authkey: string
            Authorization key used to connect to host server.

        address: (ip_addr, port) or string referring to pipe.
            Address to use to connect back to parent.

        files: list(string)
            Files to be sent to support server startup.

        allow_shell: bool
            If True, :meth:`execute_command` and :meth:`load_model` are allowed
            in created servers. Use with caution!
        """
        try:
            _check_ssh(self.hostname)
        except Exception:
            self.state = 'failed'
            return

        self.tempdir = _copy_to_remote(self.hostname, files, self.python)
        _LOGGER.debug('startup files copied to %s:%s',
                      self.hostname, self.tempdir)
        cmd = copy.copy(_SSH)
        cmd.extend([self.hostname, self.python, '-c',
                   '"import sys;'
                   ' sys.path.append(\'.\');'
                   ' import os;'
                   ' os.chdir(\'%s\');'
                   ' from mp_distributing import main;'
                   ' main()"' % self.tempdir])
        self.proc = subprocess.Popen(cmd, stdin=subprocess.PIPE,
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE)

        credentials = get_credentials()
        allowed_users = {credentials.user: credentials.public_key}

        data = dict(
            name='BoostrappingHost', index=index,
            # Avoid lots of SUBDEBUG messages.
            dist_log_level=max(_LOGGER.getEffectiveLevel(), logging.DEBUG),
            dir=self.tempdir, authkey=str(authkey),
            allowed_users=allowed_users, allow_shell=allow_shell,
            parent_address=address, registry=self.registry,
            keep_dirs=os.environ.get('OPENMDAO_KEEPDIRS', '0')
            )
        cPickle.dump(data, self.proc.stdin, cPickle.HIGHEST_PROTOCOL)
        self.proc.stdin.close()
# TODO: put timeout in accept() to avoid this hack.
        time.sleep(1)  # Give the proc time to register startup problems.
        self.poll()
        if self.state != 'failed':
            self.state = 'started'

    def poll(self):
        """ Poll for process status. """
        if self.proc is not None and self.state != 'failed':
            self.proc.poll()
            if self.proc.returncode is not None:
                _LOGGER.error('Host %r in dir %s exited, returncode %s',
                              self.hostname, self.tempdir, self.proc.returncode)
                for line in self.proc.stdout:
                    _LOGGER.error('>    %s', line.rstrip())
                for line in self.proc.stderr:
                    _LOGGER.error('>>    %s', line.rstrip())
                self.state = 'failed'


# Requires ssh configuration.
def _check_ssh(hostname):  #pragma no cover
    """ Check basic communication with `hostname`. """
    cmd = copy.copy(_SSH)
    cmd.extend([hostname, 'date'])
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)
    for retry in range(100):  # ~10 seconds based on sleep() below.
        proc.poll()
        if proc.returncode is None:
            time.sleep(0.1)
        elif proc.returncode == 0:
            return
        else:
            msg = "ssh to %r failed, returncode %s" \
                  % (hostname, proc.returncode)
            _LOGGER.error(msg)
            for line in proc.stdout:
                _LOGGER.error('   >%s', line.rstrip())
            raise RuntimeError(msg)

    # Timeout.  Kill process and log error.
    proc.kill()
    for retry in range(5):
        proc.poll()
        if proc.returncode is None:
            time.sleep(1)
        msg = "ssh to %r timed-out, returncode %s" \
              % (hostname, proc.returncode)
        _LOGGER.error(msg)
        for line in proc.stdout:
            _LOGGER.error('   >%s', line.rstrip())
        raise RuntimeError(msg)

    # Total zombie...
    msg = "ssh to %r is a zombie, PID %s" % (hostname, proc.pid)
    _LOGGER.error(msg)
    for line in proc.stdout:
        _LOGGER.error('   >%s', line.rstrip())
    raise RuntimeError(msg)


_UNZIP_CODE = '''"import tempfile, os, sys, tarfile
tempdir = tempfile.mkdtemp(prefix='omdao-')
os.chdir(tempdir)
tf = tarfile.open(fileobj=sys.stdin, mode='r|gz')
tf.extractall()
print tempdir"'''

# Requires ssh configuration.
def _copy_to_remote(hostname, files, python):  #pragma no cover
    """ Copy files to remote directory, returning directory path. """
    cmd = copy.copy(_SSH)
    cmd.extend([hostname, python, '-c', _UNZIP_CODE.replace('\n', ';')])
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    archive = tarfile.open(fileobj=proc.stdin, mode='w|gz')
    for name in files:
        archive.add(name, os.path.basename(name))
    archive.close()
    proc.stdin.close()
    return proc.stdout.read().rstrip()


# Runs on the remote host.
def main():  #pragma no cover
    """
    Code which runs a host manager.
    Expects configuration data from parent on `stdin`.
    Replies with address and optionally public key.
    The environment variable ``OPENMDAO_KEEPDIRS`` can be used to avoid
    removal of the temporary directory used here.
    """
    sys.stdout = open('stdout', 'w')
    sys.stderr = open('stderr', 'w')

#    util.log_to_stderr(logging.DEBUG)
    # Avoid root possibly masking us.
    logging.getLogger().setLevel(logging.DEBUG)

    import platform
    hostname = platform.node()
    pid = os.getpid()
    ident = '(%s:%d)' % (hostname, pid)
    print '%s main startup' % ident
    sys.stdout.flush()

    # Get data from parent over stdin.
    data = cPickle.load(sys.stdin)
    sys.stdin.close()
    print '%s data received' % ident

    authkey = data['authkey']
    allow_shell = data['allow_shell']
    allowed_users = data['allowed_users']
    print '%s using %s authentication' % (ident, keytype(authkey))
    if allowed_users is None:
        print '%s allowed_users: ANY' % ident
    else:
        print '%s allowed_users: %s' % (ident, sorted(allowed_users.keys()))
    if allow_shell:
        print '%s ALLOWING SHELL ACCESS' % ident
    sys.stdout.flush()
    log_level = data['dist_log_level']
    os.environ['OPENMDAO_KEEPDIRS'] = data['keep_dirs']

    exc = None
    server = None
    try:
        # Update HostManager registry.
        dct = data['registry']
        print '%s registry:' % ident
        for name in dct.keys():
            module = dct[name]
            print'    %s: %s' % (name, module)
            mod = __import__(module, fromlist=name)
            cls = getattr(mod, name)
            register(cls, HostManager)

        # Set some stuff.
        print '%s preparing to fork, log level %d' % (ident, log_level)
        sys.stdout.flush()
        util.get_logger().setLevel(log_level)
        forking.prepare(data)

        # Create Server for a HostManager object.
        name = '%d[%d]' % (data['index'], pid)
        logging.getLogger(name).setLevel(log_level)
        server = OpenMDAO_Server(HostManager._registry, (hostname, 0),
                                 authkey, 'pickle', name=name,
                                 allowed_users=allowed_users,
                                 allowed_hosts=[data['parent_address'][0]])
    except Exception as exc:
        print '%s caught exception: %s' % (ident, exc)

    # Report server address and public key back to parent.
    print '%s connecting to parent at %s' % (ident, data['parent_address'])
    sys.stdout.flush()
    conn = connection.Client(data['parent_address'], authkey=authkey)
    if exc:
        conn.send((data['index'], None, str(exc)))
    else:
        conn.send((data['index'], server.address, server.public_key_text))
    conn.close()

    if exc:
        print '%s exiting' % ident
        sys.exit(1)

    # Set name etc.
    current_process()._server = server
    current_process()._name = 'Host-%s:%s' % server.address
    current_process().authkey = authkey
    logging.getLogger(current_process()._name).setLevel(log_level)
    util._run_after_forkers()

    # Register a cleanup function.
    def cleanup(directory):
        keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
        if not keep_dirs and os.path.exists(directory):
            print '%s removing directory %s' % (ident, directory)
            shutil.rmtree(directory)
        print '%s shutting down host manager' % ident
    util.Finalize(None, cleanup, args=[data['dir']], exitpriority=0)

    # Start host manager.
    print '%s remote host manager starting in %s' % (ident, data['dir'])
    sys.stdout.flush()
    server.serve_forever()

