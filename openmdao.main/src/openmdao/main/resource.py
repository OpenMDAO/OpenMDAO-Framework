"""
Allocate servers from one or more resources (i.e. the local host, a cluster
of remote hosts, etc.)
"""

import logging
import platform
import sys
import threading
import traceback

from openmdao.main import mp_distributing
from openmdao.main.objserverfactory import ObjServerFactory, ObjServer


class ResourceAllocationManager(object):
    """ Primitive allocation manager. """

    _lock = threading.Lock()
    _RAM = None  # Singleton.

    def __init__(self):
        self._allocations = 0
        self._allocators = []
        self._alloc_index = 0

        self._allocators.append(LocalAllocator())
        if sys.platform != 'win32':
            # Storm needs firewall changes.
            self._allocators.append(RemoteAllocator())

    @staticmethod
    def get_instance():
        """ Return singleton instance. """
        with ResourceAllocationManager._lock:
            if ResourceAllocationManager._RAM is None:
                ResourceAllocationManager._RAM = ResourceAllocationManager()
            return ResourceAllocationManager._RAM

    @staticmethod
    def allocate(resource_desc, transient):
        """ Allocate a server. """
        for handler in logging._handlerList:
            handler.flush()

        ram = ResourceAllocationManager.get_instance()

        # Simple round-robin allocator selection.
        with ResourceAllocationManager._lock:
            ram._allocations += 1
            name = 'Sim-%d' % ram._allocations
            allocator = ram._allocators[ram._alloc_index]
            ram._alloc_index = (ram._alloc_index + 1) % len(ram._allocators)

        server = allocator.allocate(name, resource_desc, transient)
        server_info = {'name':server.get_name(), 'pid':server.get_pid(),
                       'host':server.get_host()}
        logging.debug('RAM: allocated %s pid %d on %s', server_info['name'],
                      server_info['pid'], server_info['host'])
        return (server, server_info)

    @staticmethod
    def release(server):
        """ Release a server. """
        name = server.get_name()
        try:
            server.cleanup()
        except Exception:
            tb = traceback.format_exc()
            try:
                logging.warning('Caught exception during cleanup of %s: %s',
                                name, tb)
            except Exception:
                print >>sys.stderr, \
                      'Caught exception logging cleanup of %s: %s', name, tb
        del server


class LocalAllocator(ObjServerFactory):
    """ Purely local resource allocator. """

    def __init__(self):
        super(LocalAllocator, self).__init__()

    def allocate(self, name, resource_desc, transient):
        """ Allocate a server. """
        return self.create(typname='', name=name)

    @staticmethod
    def register(manager):
        """ Register LocalAllocator proxy info with `manager`. """
        name = 'LocalAllocator'
        ObjServer.register(manager)
        method_to_typeid = {
            'allocate': 'ObjServer',
        }
        manager.register(name, LocalAllocator,
                         method_to_typeid=method_to_typeid)

LocalAllocator.register(mp_distributing.Cluster)
LocalAllocator.register(mp_distributing.HostManager)


class RemoteAllocator(object):
    """ Cluster-based resource allocator. """

    def __init__(self):
        self._lock = threading.Lock()
        hosts = []

        node = platform.node()
        if node == 'gxterm3':
            python = '/gx/u/setowns1/OpenMDAO-0.1-py2.6/buildout/bin/python'
            for i in range(1, 6):
                hosts.append(mp_distributing.Host('gx%02d' % i, slots=1,
                                                  python=python))
        elif node == 'torpedo.grc.nasa.gov':
            python = '/OpenMDAO/dev/setowns1/T0047/buildout/bin/python'
            hosts.append(mp_distributing.Host('torpedo', slots=1,
                                              python=python))
        elif node == 'viper.grc.nasa.gov':
            python = '/Users/setowns1/T0047/buildout/bin/python'
            hosts.append(mp_distributing.Host('viper', slots=1,
                                              python=python))
        for host in hosts:
            LocalAllocator.register(host)

        self.cluster = mp_distributing.Cluster(hosts, [])
        self.cluster.start()
        self.local_allocators = {}
        logging.debug('RemoteAllocator: cluster server listening on %s',
                      self.cluster.address)

    def allocate(self, name, resource_desc, transient):
        """ Allocate a server. """
        manager = self.cluster.get_host_manager()
        try:
            host = manager._name
        except AttributeError:
            host = 'localhost'
            host_ip = '127.0.0.1'
        else:
            # 'host' is 'Host-<ipaddr>:<port>
            dash = host.index('-')
            colon = host.index(':')
            host_ip = host[dash+1:colon]

        with self._lock:
            if host_ip not in self.local_allocators:
                self.local_allocators[host_ip] = manager.LocalAllocator()
                logging.debug('LocalAllocator for %s %s', host,
                              self.local_allocators[host_ip])

        return self.local_allocators[host_ip].allocate(name, resource_desc,
                                                       transient)

