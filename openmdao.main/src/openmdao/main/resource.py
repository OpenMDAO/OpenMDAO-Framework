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

    @staticmethod
    def get_instance():
        """ Return singleton instance. """
        with ResourceAllocationManager._lock:
            if ResourceAllocationManager._RAM is None:
                ResourceAllocationManager._RAM = ResourceAllocationManager()
            return ResourceAllocationManager._RAM

    @staticmethod
    def add_allocator(allocator):
        """ Add an allocator to the list of resource allocators. """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            ram._allocators.append(allocator)

    @staticmethod
    def insert_allocator(self, index, allocator):
        """ Insert an allocator into the list of resource allocators. """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            ram._allocators.insert(index, allocator)

    @staticmethod
    def allocate(resource_desc, transient):
        """ Allocate a server. """
        for handler in logging._handlerList:
            handler.flush()  # Try to keep log messages sane.

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


class ClusterAllocator(object):
    """ Cluster-based resource allocator. """

    def __init__(self, machines):
        self._lock = threading.Lock()
        self.machines = machines
        self.local_allocators = {}

        hosts = []
        for machine in machines:
            logging.debug('ClusterAllocator: %s', machine)
            host = mp_distributing.Host(machine['hostname'], slots=1,
                                        python=machine['python'])
            LocalAllocator.register(host)
            hosts.append(host)

        self.cluster = mp_distributing.Cluster(hosts, [])
        self.cluster.start()
        logging.debug('ClusterAllocator: server listening on %s',
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

