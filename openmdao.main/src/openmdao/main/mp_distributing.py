"""
This module is based on the *distributing.py* file example which was
(temporarily) posted with the multiprocessing module documentation.
"""

#
# Module to allow spawning of processes on foreign host
#
# Depends on `multiprocessing` package -- tested with `processing-0.60`
#
# Copyright (c) 2006-2008, R Oudkerk
# All rights reserved.
#

import copy
import cPickle
import getpass
import itertools
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

from multiprocessing import current_process, managers #, Process
from multiprocessing import util, connection, forking

from openmdao.main.mp_support import OpenMDAO_Manager, OpenMDAO_Server, \
                                     register, decode_public_key
from openmdao.main.rbac import Credentials, get_credentials, set_credentials

from openmdao.util.wrkpool import WorkerPool


# SSH command to be used to access remote hosts.
if sys.platform == 'win32':  #pragma no cover
    _SSH = r'C:\Putty\%s%s' % (getpass.getuser(), '.ppk')
    _SSH = [r'C:\Putty\plink.exe', '-i', _SSH]
else:
    _SSH = ['ssh']

# Logging.
#util.log_to_stderr(logging.DEBUG)
#_LOGGER = util.get_logger()
_LOGGER = logging.getLogger('mp_distributing')


# This is used by Cluster, which also isn't covered.
# Cluster allocation requires ssh configuration and multiple hosts.
class HostManager(OpenMDAO_Manager):  #pragma no cover
    """ Manager used for spawning processes on a remote host. """

    def __init__(self, address, authkey):
        super(HostManager, self).__init__(address, authkey)
        self._name = 'Host-unknown'

    @classmethod
    def from_address(cls, address, authkey):
        """ Return manager given an address. """
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
        conn = connection.Client(address, authkey=authkey)
        try:
            return managers.dispatch(conn, None, 'shutdown')
        finally:
            conn.close()

    def __repr__(self):
        return '<Host(%s)>' % self._name


# Cluster allocation requires ssh configuration and multiple hosts.
class Cluster(OpenMDAO_Manager):  #pragma no cover
    """ Represents a collection of hosts. """

    def __init__(self, hostlist, modules, authkey=None):
        super(Cluster, self).__init__(authkey=authkey)
        self._hostlist = hostlist
        self._modules = modules
        if __name__ not in modules:
            modules.append(__name__)
        files = [sys.modules[name].__file__ for name in modules]
        for i, filename in enumerate(files):
            if filename.endswith(('.pyc', '.pyo')):
                files[i] = filename[:-1]
        self._files = [os.path.abspath(filename) for filename in files]
        self._reply_q = Queue.Queue()

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
                    conn = listener.accept()
                    i, address, pubkey_text = conn.recv()
                    conn.close()
                    other_host = self._hostlist[i]
                    other_host.manager = HostManager.from_address(address,
                                                                  self._authkey)
#                    other_host.Process = other_host.manager.Process
                    other_host.state = 'up'
                    if pubkey_text:
                        other_host.manager._pubkey = decode_public_key(pubkey_text)
                    host_processed = True
                    _LOGGER.debug('Host %s is now up', other_host.hostname)

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

        self._slotlist = [_Slot(host) for host in self._hostlist
                                              if host.state == 'up']
        self._slot_iterator = itertools.cycle(self._slotlist)
        self._base_shutdown = self.shutdown
        del self.shutdown

    def _start_hosts(self, address, credentials):
        """ Start host managers. """
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

            _LOGGER.debug('Host %s state %s', host.hostname, host.state)
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
            host.start_manager(i, self._authkey, address, self._files)
        except Exception, exc:
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

#    def Process(self, group=None, target=None, name=None,
#                args=None, kwargs=None):
#        """ Return a :class:`Process` object associated with a host.  """
#        args = args or ()
#        kwargs = kwargs or {}
#        slot = self._slot_iterator.next()
#        return slot.Process(
#            group=group, target=target, name=name, args=args, kwargs=kwargs
#            )

    def __getitem__(self, i):
        return self._slotlist[i]

    def __len__(self):
        return len(self._slotlist)

    def __iter__(self):
        return iter(self._slotlist)


# Used by Cluster, which isn't covered.
class _Slot(object):  #pragma no cover
    """ Class representing a notional cpu in the cluster. """

    def __init__(self, host):
        self.host = host
#        self.Process = host.Process


# Requires ssh configuration.
class Host(object):  #pragma no cover
    """
    Represents a host to use as a node in a cluster.
    `hostname` gives the name of the host.
    `python` is the path the the Python command to be used on `hostname`.
    ssh is used to log in to the host. To log in as a different user use
    a host name of the form: "username@somewhere.org".
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
        """ Register proxy info to be sent to remote process. """
        name = cls.__name__
        module = cls.__module__
        self.registry[name] = module

    def start_manager(self, index, authkey, address, files):
        """ Launch remote manager process. """
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
        data = dict(
            name='BoostrappingHost', index=index,
            dist_log_level=_LOGGER.getEffectiveLevel(),
            dir=self.tempdir, authkey=str(authkey), parent_address=address,
            registry=self.registry
            )
        cPickle.dump(data, self.proc.stdin, pickle.HIGHEST_PROTOCOL)
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
                _LOGGER.error('Host %s in dir %s exited, returncode %s',
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
    for retry in range(150):  # ~15 seconds based on sleep() below.
        proc.poll()
        if proc.returncode is None:
            time.sleep(0.1)
        elif proc.returncode == 0:
            return
        else:
            msg = "ssh to '%s' failed, returncode %s" \
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
        msg = "ssh to '%s' timed-out, returncode %s" \
              % (hostname, proc.returncode)
        _LOGGER.error(msg)
        for line in proc.stdout:
            _LOGGER.error('   >%s', line.rstrip())
        raise RuntimeError(msg)

    # Total zombie...
    msg = "ssh to '%s' is a zombie, PID %s" % (hostname, proc.pid)
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
    """ Copy files to remote directory, returning name of directory. """
    cmd = copy.copy(_SSH)
    cmd.extend([hostname, python, '-c', _UNZIP_CODE.replace("\n", ';')])
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
    """ Code which runs a host manager. """
    sys.stdout = open('stdout', 'w')
    sys.stderr = open('stderr', 'w')
    _LOGGER.setLevel(logging.DEBUG)

    out = open('debug.out', 'w')
    import platform
    hostname = platform.node()
    pid = os.getpid()
    ident = '(%s:%d)' % (hostname, pid)
    _LOGGER.debug('%s main startup', ident)
    out.write('%s main startup\n'% ident)
    out.flush()

    # Get data from parent over stdin.
    data = cPickle.load(sys.stdin)
    sys.stdin.close()
    _LOGGER.debug('%s data received', ident)
    out.write('%s data received\n' % ident)
    out.flush()

    if data['authkey'] == 'PublicKey':
        _LOGGER.debug('%s using PublicKey authentication', ident)
        out.write('%s using PublicKey authentication\n' % ident)
        out.flush()

    # Update HostManager registry.
    dct = data['registry']
    out.write('%s registry:' % ident)
    for name in dct.keys():
        module = dct[name]
        out.write('    %s: %s\n' % (name, module))
        out.flush()
        mod = __import__(module, fromlist=name)
        cls = getattr(mod, name)
        register(cls, HostManager)

    # Set some stuff.
    _LOGGER.setLevel(data['dist_log_level'])
    forking.prepare(data)

    # Create Server for a `HostManager` object.
    set_credentials(Credentials())
    server = OpenMDAO_Server(HostManager._registry, (hostname, 0),
                             data['authkey'], 'pickle')
    current_process()._server = server

    # Report server address and number of cpus back to parent.
    _LOGGER.debug('%s connecting to parent at %s',
                  ident, data['parent_address'])
    out.write('%s connecting to parent at %s\n'
              % (ident, data['parent_address']))
    out.flush()
    conn = connection.Client(data['parent_address'], authkey=data['authkey'])
    conn.send((data['index'], server.address, server.public_key_text))
    conn.close()

    # Set name etc.
    current_process()._name = 'Host-%s:%s' % server.address
    util._run_after_forkers()

    # Register a cleanup function.
    def cleanup(directory):
        _LOGGER.debug('%s removing directory %s', ident, directory)
        shutil.rmtree(directory)
        _LOGGER.debug('%s shutting down host manager', ident)
    util.Finalize(None, cleanup, args=[data['dir']], exitpriority=0)

    # Start host manager.
    _LOGGER.debug('%s remote host manager starting in %s', ident, data['dir'])
    out.write('%s remote host manager starting in %s\n' % (ident, data['dir']))
    out.flush()
    server.serve_forever()

