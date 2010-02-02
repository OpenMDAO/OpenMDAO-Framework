"""
Based on the 'distributing.py' file example which was (temporarily) posted
with the multiprocessing module documentation.
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
import sys
import os
import tarfile
import traceback
import shutil
import subprocess
import logging
import itertools
import Queue

try:
    import cPickle as pickle
except ImportError:
    import pickle

from multiprocessing import Process, current_process, cpu_count
from multiprocessing import util, connection, forking, pool
#from multiprocessing import managers
import openmdao.main.mp_managers as managers

__all__ = ['Cluster', 'Host', 'current_process']


# SSH command.
if sys.platform == 'win32':
    _SSH = [r'C:\Putty\plink.exe', '-i', r'C:\Users\setowns1\id-rsa.ppk']
else:
    _SSH = ['ssh']

# Logging.
util.log_to_stderr(logging.WARNING)  # Keep debug out of testing output.
_logger = util.get_logger()
info = _logger.info
debug = _logger.debug

# Get number of cpus.
try:
    slot_count = cpu_count()
except NotImplemented:
    slot_count = 1


class HostManager(managers.SyncManager):
    """
    Manager type used for spawning processes on a (presumably) foreign host.
    """

    def __init__(self, address, authkey):
        managers.SyncManager.__init__(self, address, authkey)
        self._name = 'Host-unknown'

    def Process(self, group=None, target=None, name=None,
                args=None, kwargs=None):
        args = args or ()
        kwargs = kwargs = {}
        if hasattr(sys.modules['__main__'], '__file__'):
            main_path = os.path.basename(sys.modules['__main__'].__file__)
        else:
            main_path = None
        data = pickle.dumps((target, args, kwargs))
        proc = self._RemoteProcess(data, main_path)
        if name is None:
            temp = self._name.split('Host-')[-1] + '/Process-%s'
            name = temp % ':'.join(map(str, proc.get_identity()))
        proc.name = name
        return proc

    @classmethod
    def from_address(cls, address, authkey):
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


class RemoteProcess(Process):
    """ Represents a process started on a remote host. """

    def __init__(self, data, main_path):
        assert not main_path or os.path.basename(main_path) == main_path
        Process.__init__(self)
        self._data = data
        self._main_path = main_path

    def start(self):
        try:
            Process.start(self)
        except Exception:
            traceback.print_exc()
            raise

    def _bootstrap(self):
        forking.prepare({'main_path': self._main_path})
        self._target, self._args, self._kwargs = pickle.loads(self._data)
        return Process._bootstrap(self)

    def get_identity(self):
        return self._identity

HostManager.register('_RemoteProcess', RemoteProcess)


class DistributedPool(pool.Pool):
    """ A Pool class that uses a cluster. """

    def __init__(self, cluster, processes=None, initializer=None, initargs=()):
        self._cluster = cluster
        self.Process = cluster.Process
        pool.Pool.__init__(self, processes or len(cluster),
                           initializer, initargs)

    def _setup_queues(self):
        self._inqueue = self._cluster._SettableQueue()
        self._outqueue = self._cluster._SettableQueue()
        self._quick_put = self._inqueue.put
        self._quick_get = self._outqueue.get

    @staticmethod
    def _help_stuff_finish(inqueue, task_handler, size):
        inqueue.set_contents([None] * size)


#
# Manager type which starts host managers on other machines
#

def LocalProcess(**kwds):
    p = Process(**kwds)
    p.name = 'localhost/' + p.name
    return p


class Cluster(managers.SyncManager):
    """
    Represents collection of slots running on various hosts.

    `Cluster` is a subclass of `SyncManager` so it allows creation of
    various types of shared objects.
    """

    def __init__(self, hostlist, modules):
        managers.SyncManager.__init__(self, address=('localhost', 0))
        self._hostlist = hostlist
        self._modules = modules
        if __name__ not in modules:
            modules.append(__name__)
        files = [sys.modules[name].__file__ for name in modules]
        for i, filename in enumerate(files):
            if filename.endswith(('.pyc', '.pyo')):
                files[i] = filename[:-1]
        self._files = [os.path.abspath(filename) for filename in files]

    def start(self):
        managers.SyncManager.start(self)
#        l = connection.Listener(family='AF_INET', authkey=self._authkey)
        import socket
        hostname = socket.getfqdn()
        l = connection.Listener(address=(hostname, 0), authkey=self._authkey)
# TODO: support multiple addresses if multiple networks attached.

        for i, host in enumerate(self._hostlist):
            host._start_manager(i, self._authkey, l.address, self._files)

        for host in self._hostlist:
            if host.hostname != 'localhost':
                conn = l.accept()
                i, address, cpus = conn.recv()
                conn.close()
                other_host = self._hostlist[i]
                other_host.manager = HostManager.from_address(address,
                                                              self._authkey)
                other_host.slots = other_host.slots or cpus
                other_host.Process = other_host.manager.Process
            else:
                host.slots = host.slots or slot_count
                host.Process = LocalProcess

        self._slotlist = [
            Slot(host) for host in self._hostlist for i in range(host.slots)
            ]
        self._slot_iterator = itertools.cycle(self._slotlist)
        self._slot_index = 0
        self._base_shutdown = self.shutdown
        del self.shutdown

    def shutdown(self):
        for host in self._hostlist:
            if host.hostname != 'localhost':
                host.manager.shutdown()
        self._base_shutdown()

    def Process(self, group=None, target=None, name=None,
                args=None, kwargs=None):
        args = args or ()
        kwargs = kwargs or {}
        slot = self._slot_iterator.next()
        return slot.Process(
            group=group, target=target, name=name, args=args, kwargs=kwargs
            )

    def get_host_manager(self):
        """ Return a HostManager. """
        slot = self._slot_iterator.next()
        if slot.host.hostname != 'localhost':
            return slot.host.manager
        else:
            return self

    def Pool(self, processes=None, initializer=None, initargs=()):
        return DistributedPool(self, processes, initializer, initargs)

    def __getitem__(self, i):
        return self._slotlist[i]

    def __len__(self):
        return len(self._slotlist)

    def __iter__(self):
        return iter(self._slotlist)


class SettableQueue(Queue.Queue):
    """ Queue subclass used by distributed pool. """

    def empty(self):
        return not self.queue

    def full(self):
        return self.maxsize > 0 and len(self.queue) == self.maxsize

    def set_contents(self, contents):
        """
        Length of contents must be at least as large as the number of
        threads which have potentially called get().
        """
        self.not_empty.acquire()
        try:
            self.queue.clear()
            self.queue.extend(contents)
            self.not_empty.notifyAll()
        finally:
            self.not_empty.release()

Cluster.register('_SettableQueue', SettableQueue)


class Slot(object):
    """ Class representing a notional cpu in the cluster. """

    def __init__(self, host):
        self.host = host
        self.Process = host.Process


class Host(object):
    """
    Represents a host to use as a node in a cluster.

    `hostname` gives the name of the host.  If hostname is not
    "localhost" then ssh is used to log in to the host.  To log in as
    a different user use a host name of the form
    "username@somewhere.org"

    `slots` is used to specify the number of slots for processes on
    the host.  This affects how often processes will be allocated to
    this host.  Normally this should be equal to the number of cpus on
    that host.
    """

    def __init__(self, hostname, slots=None, python=None):
        self.hostname = hostname
        if hostname != 'localhost':
            # Plink.exe wants user@host always.
            parts = hostname.split('@')
            if len(parts) == 1:
                if sys.platform == 'win32':
                    user = os.environ['USERNAME']
                else:
                    user = os.environ['USER']
                self.hostname = '%s@%s' % (user, hostname)
        self.slots = slots
        self.python = python or 'python'
        self.registry = {}

    def register(self, typeid, callable=None, method_to_typeid=None):
        """ Register proxy info to be sent to remote process. """
        if callable is None:
            name = None
            module = None
        else:
            name = callable.__name__
            module = callable.__module__
        self.registry[typeid] = (name, module, method_to_typeid)

    def _start_manager(self, index, authkey, address, files):
        if self.hostname != 'localhost':
            tempdir = copy_to_remote_temporary_directory(self.hostname, files)
            debug('startup files copied to %s:%s', self.hostname, tempdir)
            cmd = copy.copy(_SSH)
            cmd.extend([self.hostname, self.python, '-c',
                 '"import sys; sys.path.append(\'.\'); import os; os.chdir(\'%s\'); from mp_distributing import main; main()"' % tempdir])
            p = subprocess.Popen(cmd, stdin=subprocess.PIPE,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            data = dict(
                name='BoostrappingHost', index=index,
                dist_log_level=_logger.getEffectiveLevel(),
                dir=tempdir, authkey=str(authkey), parent_address=address,
                registry=self.registry
                )
            pickle.dump(data, p.stdin, pickle.HIGHEST_PROTOCOL)
            p.stdin.close()


_UNZIP_CODE = '''"import tempfile, os, sys, tarfile
tempdir = tempfile.mkdtemp(prefix='distrib-')
os.chdir(tempdir)
tf = tarfile.open(fileobj=sys.stdin, mode='r|gz')
tf.extractall()
print tempdir"'''

def copy_to_remote_temporary_directory(hostname, files):
    """ Copy files to remote directory, returning name of directory. """
    cmd = copy.copy(_SSH)
    cmd.extend([hostname, 'python', '-c', _UNZIP_CODE.replace("\n", ';')])
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stdin=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    archive = tarfile.open(fileobj=proc.stdin, mode='w|gz')
    for name in files:
        archive.add(name, os.path.basename(name))
    archive.close()
    proc.stdin.close()
    return proc.stdout.read().rstrip()


def main():
    """ Code which runs a host manager. """
    sys.stdout = open('stdout', 'w')
    sys.stderr = open('stderr', 'w')
    _logger.setLevel(logging.DEBUG)

    out = open('debug.out', 'w')
    import platform
    hostname = platform.node()
    pid = os.getpid()
    ident = '(%s:%d)' % (hostname, pid)
    debug('%s main startup', ident)
    out.write('%s main startup\n'% ident)
    out.flush()

    # Get data from parent over stdin.
    data = pickle.load(sys.stdin)
    sys.stdin.close()
    debug('%s data received', ident)
    out.write('%s data received\n' % ident)
    out.flush()

    # Update HostManager registry.
    dct = data['registry']
    for key in dct.keys():
        name, module, method_to_typeid = dct[key]
        out.write('%s: %s %s %s\n' % (key, name, module, method_to_typeid))
        out.flush
        if name:
            mod = __import__(module, fromlist=name)
            callable = getattr(mod, name)
        else:
            callable = None
        HostManager.register(key, callable, method_to_typeid=method_to_typeid)

    # Set some stuff.
    _logger.setLevel(data['dist_log_level'])
    forking.prepare(data)

    # Create Server for a `HostManager` object.
    server = managers.Server(HostManager._registry, (hostname, 0),
                             data['authkey'], "pickle")
    current_process()._server = server

    # Report server address and number of cpus back to parent.
    debug('%s connecting to parent at %s', ident, data['parent_address'])
    out.write('%s connecting to parent at %s\n'
              % (ident, data['parent_address']))
    out.flush()
    conn = connection.Client(data['parent_address'], authkey=data['authkey'])
    conn.send((data['index'], server.address, slot_count))
    conn.close()

    # Set name etc.
    current_process()._name = 'Host-%s:%s' % server.address
    util._run_after_forkers()

    # Register a cleanup function.
    def cleanup(directory):
        debug('%s removing directory %s', ident, directory)
        shutil.rmtree(directory)
        debug('%s shutting down host manager', ident)
    util.Finalize(None, cleanup, args=[data['dir']], exitpriority=0)

    # Start host manager.
    debug('%s remote host manager starting in %s', ident, data['dir'])
    out.write('%s remote host manager starting in %s\n' % (ident, data['dir']))
    out.flush()
    server.serve_forever()

