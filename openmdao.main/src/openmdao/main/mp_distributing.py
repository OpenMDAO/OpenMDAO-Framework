"""
This module is based on the *distributing.py* file example which was
(temporarily) posted with the multiprocessing module documentation.

This code uses ssh (or plink on Windows) to access remote hosts and assumes
authorization has been set up on all hosts in such a way that no user intervention for
passwords or passphrases is required.

Also, it's assumed that connections to remote hosts will access a UNIX-like
shell.  Remote Windows hosts will need to be configured for cygwin or
something similar.
"""

import base64
import copy
import cPickle
import errno
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
                                     OpenMDAO_Proxy, register, \
                                     decode_public_key, keytype
from openmdao.main.mp_util import setup_tunnel, setup_reverse_tunnel
from openmdao.main.rbac import get_credentials, set_credentials

from openmdao.util.wrkpool import WorkerPool
from openmdao.util.fileutil import onerror


# SSH command to be used to access remote hosts.
if sys.platform == 'win32':  #pragma no cover
    _SSH = ['plink', '-batch', '-ssh']
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
    def from_address(cls, address, authkey, host):
        """
        Return manager given an address.

        address: (ip_addr, port) or string referring to pipe.
            Address to connect to.

        authkey: string
            Authorization key.

        host: :class:`Host`
            Host we're managing.
        """
        if host.tunnel_outgoing:
            _LOGGER.debug('Client setting up tunnel for %s:%s',
                          host.hostname, address[1])
            address, cleanup = setup_tunnel(host.hostname, address[1],
                                            identity=host.identity_filename)
        else:
            cleanup = None

        manager = cls(address, authkey)
        _LOGGER.debug('Client connecting to server at %s' % (address,))
        conn = connection.Client(address, authkey=authkey)
        try:
            managers.dispatch(conn, None, 'dummy')
        finally:
            conn.close()
        manager._state.value = managers.State.STARTED
        manager._name = 'Host-%s:%s' % manager.address
        manager.shutdown = util.Finalize(
            manager, HostManager._finalize_host,
            args=(manager._address, manager._authkey,
                  cleanup, host.reverse_cleanup),
            exitpriority=-10)
        return manager

    @staticmethod
    def _finalize_host(address, authkey, fcleanup, rcleanup):
        """ Sends a shutdown message and cleans up tunnels. """
        mgr_ok = OpenMDAO_Proxy.manager_is_alive(address)
        if mgr_ok:
            conn = connection.Client(address, authkey=authkey)
            try:
                managers.dispatch(conn, None, 'shutdown')
            except EOFError:
                pass
            finally:
                conn.close()
        if fcleanup is not None:
            fcleanup[0](*fcleanup[1:], **dict(keep_log=not mgr_ok))
        if rcleanup is not None:
            rcleanup[0](*rcleanup[1:], **dict(keep_log=not mgr_ok))

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
        Authorization key; passed to :class:`OpenMDAO_Manager`.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Use with caution!

    hostname: string
        Fully qualified domain name for this host. Normally :meth:socket.getfqdn
        is used, but on some systems it doesn't report an externally usable
        address. This is the address remote hosts will use to connect back to.

    Once started, available hosts may be accessed via sequence operations.
    """

    def __init__(self, hostlist, modules=None, authkey=None, allow_shell=False,
                 hostname=None):
        super(Cluster, self).__init__(authkey=authkey)
        self._hostlist = hostlist
        self._allow_shell = allow_shell
        self._hostname = hostname or socket.getfqdn()
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
        """
        Start this manager and all remote managers. If some managers fail to
        start, errors are logged and the corresponding host's state is set to
        ``failed``. You can use ``len(cluster)`` to determine how many remote
        managers are available.

        A :class:`RuntimeError` will be raised if no managers were successfully
        started.
        """
        super(Cluster, self).start()
        listener = connection.Listener(address=(self._hostname, 0),
                                       authkey=self._authkey,
                                       backlog=5)  # Default is 1.

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
                    # Normal accept() can hang.
                    retval = []
                    accepter = threading.Thread(target=self._accept,
                                                args=(listener, retval),
                                                name='ClusterAccepter')
                    accepter.daemon = True
                    accepter.start()
                    accepter.join(30)
                    if accepter.is_alive():
                        msg = 'timeout waiting for reply from %s' \
                              % [host.hostname for host in self._hostlist
                                               if host.state == 'started']
                        _LOGGER.error(msg)
                        for host in self._hostlist:
                            if host.state == 'started':
                                if host.proc is not None:
                                    host.proc.terminate()
                                if host.reverse_cleanup is not None:
                                    host.reverse_cleanup[0](*host.reverse_cleanup[1:])
                                host.state = 'failed'
                        continue

                    conn = retval[0]
                    i, address, pubkey_text = conn.recv()
                    conn.close()

                    other_host = self._hostlist[i]
                    if address is None:
                        _LOGGER.error('Host %s died: %s', other_host.hostname,
                                      pubkey_text)  # Exception text.
                        other_host.state = 'failed'
                        continue
                    try:
                        other_host.manager = \
                            HostManager.from_address(address, self._authkey,
                                                     other_host)
                    except Exception as exc:
                        _LOGGER.error("Can't start manager for %s: %s",
                                      other_host.hostname, str(exc) or repr(exc))
                        if other_host.proc is not None:
                            other_host.proc.terminate()
                        other_host.state = 'failed'
                        continue
                    else:
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
                    if retry < 300:  # ~60 seconds.
                        time.sleep(0.2)
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

        # So our class defined shutdown() is called before the superclass
        # installed shutdown().
        self._base_shutdown = self.shutdown
        del self.shutdown

        if len(self._up) < 1:
            raise RuntimeError('No hosts successfully started')

    def _accept(self, listener, retval):
        """
        Accept connection on `listener`. `retval` is an empty list that
        will have the accepted connection appended to it.
        """
        retval.append(listener.accept())
        # Comment-out the line above and use this equivalent
        # to debug connectivity issues.
        #retval.append(listener._listener.accept())
        #_LOGGER.critical('connection attempt from %s',
        #                 listener.last_accepted)
        #if listener._authkey:
        #    connection.deliver_challenge(conn, listener._authkey)
        #    connection.answer_challenge(conn, listener._authkey)

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
            _LOGGER.error('starter for %s caught exception: %s',
                          host.hostname, msg)
            host.state = 'failed'
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
        different user, use a host name of the form: "username@somewhere.org".

    python: string
        Path to the Python command to be used on `hostname`.

    tunnel_incoming: bool
        True if we need to set up a tunnel for `hostname` to connect to us.
        This is the case when a local firewall blocks connections.

    tunnel_outgoing: bool
        True if we need to set up a tunnel to connect to `hostname`.
        This is the case when a remote firewall blocks connections.

    identity_filename: string
        Path to optional identity file to pass to ssh.
    """

    def __init__(self, hostname, python=None, tunnel_incoming=False,
                 tunnel_outgoing=False, identity_filename=None):
        if '@' not in hostname:
            self.hostname = '%s@%s' % (getpass.getuser(), hostname)
        else:
            self.hostname = hostname
        self.python = python or 'python'
        self.tunnel_incoming = tunnel_incoming
        self.tunnel_outgoing = tunnel_outgoing
        self.identity_filename = identity_filename

        self.registry = {}
        self.state = 'init'
        self.proc = None
        self.tempdir = None
        self.reverse_cleanup = None

    def _ssh_cmd(self):
        """ Return leading part of ssh command as a list. """
        cmd = copy.copy(_SSH)
        if self.identity_filename:
            cmd.extend(['-i', self.identity_filename])
        cmd.extend(['-x', '-T'])
        return cmd

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
            self._check_ssh()
        except RuntimeError:
            self.state = 'failed'
            return

        self.tempdir = self._copy_to_remote(files)
        if not self.tempdir:
            self.state = 'failed'
            return
        _LOGGER.debug('startup files copied to %s:%s',
                      self.hostname, self.tempdir)

        if self.tunnel_incoming:
            _LOGGER.debug('setup reverse tunnel from %s to %s:%s',
                          self.hostname, address[0], address[1])
            address, cleanup = \
                setup_reverse_tunnel(self.hostname, address[0], address[1], 
                                     identity=self.identity_filename)
            self.reverse_cleanup = cleanup

        cmd = self._ssh_cmd()
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

        # Tell the server what name to bind to
        # (in case it has multiple interfaces).
        user, remote_name = self.hostname.split('@')
        
        data = dict(
            name='BoostrappingHost', index=index, hostname=remote_name,
            # Avoid lots of SUBDEBUG messages.
            dist_log_level=max(_LOGGER.getEffectiveLevel(), logging.DEBUG),
            dir=self.tempdir, authkey=str(authkey), allowed_users=allowed_users,
            allow_shell=allow_shell, allow_tunneling=self.tunnel_outgoing,
            parent_address=address, registry=self.registry,
            keep_dirs=os.environ.get('OPENMDAO_KEEPDIRS', '0'))

        # Windows can't handle binary on stdin.
        dump = cPickle.dumps(data, cPickle.HIGHEST_PROTOCOL)
        dump = base64.b64encode(dump)
        _LOGGER.debug('sending %s config info (%s)', self.hostname, len(dump))
        self.proc.stdin.write(dump)
        self.proc.stdin.close()
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

    def _check_ssh(self):
        """ Check basic communication with `hostname`. """
        cmd = self._ssh_cmd()
        cmd.extend([self.hostname, 'echo', 'Hello', 'World'])
        try:
            proc = subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT)
        except Exception as exc:
            msg = "Can't start %r: %s" % (' '.join(cmd), str(exc) or repr(exc))
            _LOGGER.error(msg)
            raise RuntimeError(msg)
 
        for retry in range(150):  # ~15 seconds based on sleep() below.
            proc.poll()
            if proc.returncode is None:
                time.sleep(0.1)
            else:
                output = ''.join(proc.stdout)
                if proc.returncode == 0 and 'Hello World' in output:
                    return
                else:
                    msg = "ssh to %r failed, returncode %s" \
                          % (self.hostname, proc.returncode)
                    _LOGGER.error('%s:\n%s', msg, output)
                    raise RuntimeError(msg)

        # Timeout.  Kill process and log error.
        proc.kill()
        for retry in range(5):
            proc.poll()
            if proc.returncode is None:
                time.sleep(1)
            else:
                output = ''.join(proc.stdout)
                msg = "ssh to %r timed-out, returncode %s" \
                      % (self.hostname, proc.returncode)
                _LOGGER.error('%s:\n%s', msg, output)
                raise RuntimeError(msg)

        # Total zombie...
        output = ''.join(proc.stdout)
        msg = "ssh to %r is a zombie, PID %s" % (self.hostname, proc.pid)
        _LOGGER.error('%s:\n%s', msg, output)
        raise RuntimeError(msg)

    def _copy_to_remote(self, files):
        """ Copy files to remote directory, returning directory path. """
        cmd = self._ssh_cmd()
        cmd.extend([self.hostname, self.python,
                    '-c', _UNZIP_CODE.replace('\n', ';')])
        proc = subprocess.Popen(cmd, universal_newlines=True,
                                stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        # Can't 'w|gz'/'r|gz', Windows can't handle binary on stdin.
        archive = tarfile.open(fileobj=proc.stdin, mode='w|')
        for name in files:
            archive.add(name, os.path.basename(name))
        archive.close()
        proc.stdin.close()
        output = proc.stdout.read()
        for line in output.split('\n'):
            if line.startswith('tempdir'):
                tempdir = line.split()[1]
                return tempdir.replace('\\', '/')
        else:
            _LOGGER.error("Couldn't copy files to remote %s:\n"
                          "[stdout]\n%s\n[stderr]\n%s",
                          self.hostname, output, proc.stderr.read())
            return None

# FIXME: this currently won't work for Windows if ssh doesn't connect to a
# UNIX-like shell (cygwin, etc.)
_UNZIP_CODE = '''"import tempfile, os, sys, tarfile
tempdir = tempfile.mkdtemp(prefix='omdao-')
os.chdir(tempdir)
tf = tarfile.open(fileobj=sys.stdin, mode='r|')
tf.extractall()
tf.close()
print 'tempdir', tempdir"'''


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

    pid = os.getpid()
    ident = '(%s:%d)' % (socket.gethostname(), pid)
    print '%s main startup' % ident
    sys.stdout.flush()

    # Get data from parent over stdin.
    dump = sys.stdin.read()
    sys.stdin.close()
    print '%s data received (%s)' % (ident, len(dump))
    data = cPickle.loads(base64.b64decode(dump))

    hostname = data['hostname']
    print '%s using hostname %s' % (ident, hostname)

    authkey = data['authkey']
    print '%s using %s authentication' % (ident, keytype(authkey))

    allowed_users = data['allowed_users']
    if allowed_users is None:
        print '%s allowed_users: ANY' % ident
    else:
        print '%s allowed_users: %s' % (ident, sorted(allowed_users.keys()))

    allow_shell = data['allow_shell']
    if allow_shell:
        print '%s ALLOWING SHELL ACCESS' % ident

    allow_tunneling = data['allow_tunneling']
    print '%s allow_tunneling: %s' % (ident, allow_tunneling)
    if allow_tunneling:
        hostname = 'localhost'

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
                                 allowed_hosts=[data['parent_address'][0]],
                                 allow_tunneling=allow_tunneling)
        print '%s server listening at %s' % (ident, server.address)
    except Exception as exc:
        print '%s caught exception: %s' % (ident, exc)

    # Report server address and public key back to parent.
    print '%s connecting to parent at %s' % (ident, data['parent_address'])
    sys.stdout.flush()
    for retry in range(10):
        try:
            conn = connection.Client(data['parent_address'], authkey=authkey)
        except socket.error as sock_exc:
            print '%s %s' % (ident, sock_exc)
            if retry < 9 and (sock_exc.args[0] == errno.ECONNREFUSED or \
                              sock_exc.args[0] == errno.ENOENT):
                print '%s retrying...' % ident
                time.sleep(1)
            else:
                print '%s exiting' % ident
                sys.exit(1)
        else:
            break
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
        """ Removes our directory unless OPENMDAO_KEEPDIRS set. """
        keep_dirs = int(os.environ.get('OPENMDAO_KEEPDIRS', '0'))
        if not keep_dirs and os.path.exists(directory):
            print '%s removing directory %s' % (ident, directory)
            try:
                shutil.rmtree(directory, onerror=onerror)
            except WindowsError as exc:
                print '%s %s' % (ident, exc)
        print '%s shutting down host manager' % ident
    util.Finalize(None, cleanup, args=[data['dir']], exitpriority=0)

    # Start host manager.
    print '%s remote host manager starting in %s' % (ident, data['dir'])
    sys.stdout.flush()
    server.serve_forever()

