"""
Allocate servers from one or more resources (i.e. the local host, a cluster
of remote hosts, etc.)
"""

import atexit
import logging
import os
import platform
import Queue
import sys
import threading
import time
import traceback

from openmdao.main import mp_distributing
from openmdao.main.objserverfactory import ObjServerFactory, ObjServer
from openmdao.util.eggloader import check_requirements


class ResourceAllocationManager(object):
    """
    The allocation manager maintains a list of allocators which are used
    to select the 'best fit' for a particular resource request.  The manager
    is initialized with a :class:`LocalAllocator` for the local host.
    """

    _lock = threading.Lock()
    _RAM = None  # Singleton.

    def __init__(self):
        self._logger = logging.getLogger('RAM')
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
    def insert_allocator(index, allocator):
        """ Insert an allocator into the list of resource allocators. """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            ram._allocators.insert(index, allocator)

    @staticmethod
    def allocate(resource_desc):
        """
        Determine best resource for `resource_desc` and deploy.
        In the case of a tie, the first allocator in the allocators list wins.
        Returns (proxy-object, server-dict).
        """
        for handler in logging._handlerList:
            handler.flush()  # Try to keep log messages sane.

        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            return ram._allocate(resource_desc)

    def _allocate(self, resource_desc):
        """ Do the allocation. """
        deployment_retries = 0
        best_score = -1
        while best_score == -1:
            best_score, best_criteria, best_allocator = \
                self._get_scores(resource_desc)
            if best_score >= 0:
                self._allocations += 1
                name = 'Sim-%d' % self._allocations
                self._logger.debug('deploying on %s', best_allocator.name)
                server = best_allocator.deploy(name, resource_desc,
                                               best_criteria)
                if server is not None:
                    server_info = {
                        'name':server.get_name(),
                        'pid':server.get_pid(),
                        'host':server.get_host()
                    }
                    self._logger.debug('allocated %s pid %d on %s',
                                       server_info['name'], server_info['pid'],
                                       server_info['host'])
                    return (server, server_info)
                else:
                    deployment_retries += 1
                    if deployment_retries > 10:
                        self._logger.error('deployment failed too many times.')
                        return (None, None)
                    self._logger.warning('deployment failed, retrying.')
                    best_score = -1
            elif best_score != -1:
                return (None, None)
            else:
                time.sleep(1)

    def _get_scores(self, resource_desc):
        """ Return best (score, criteria, allocator). """
        best_score = -2
        best_criteria = None
        best_allocator = None

        for allocator in self._allocators:
            score, criteria = allocator.rate_resource(resource_desc)
            self._logger.debug('allocator %s returned %g',
                               allocator.name, score)
            if (best_score == -2 and score >= -1) or \
               (best_score == 0  and score >  0) or \
               (best_score >  0  and score < best_score):
                best_score = score
                best_criteria = criteria
                best_allocator = allocator

        return (best_score, best_criteria, best_allocator)

    @staticmethod
    def release(server):
        """ Release a server (proxy). """
        name = server.get_name()
        try:
            server.cleanup()
        except Exception:
            trace = traceback.format_exc()
            ram = ResourceAllocationManager.get_instance()
            try:
                ram._logger.warning('caught exception during cleanup of %s: %s',
                                    name, trace)
            except Exception:
                print >>sys.stderr, \
                      'RAM: caught exception logging cleanup of %s: %s', \
                      name, trace
        del server


class ResourceAllocator(ObjServerFactory):
    """ Estimates suitability of a resource and can deploy on that resource. """

    def __init__(self, name):
        super(ResourceAllocator, self).__init__()
        self.name = name
        self._logger = logging.getLogger(name)

    def get_name(self):
        """ Return :attr:`name`. """
        return self.name

    def rate_resource(self, resource_desc):
        """
        Return a score indicating how well this resource allocator can satisfy
        the `resource_desc` request.

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        """
        raise NotImplementedError

    def check_required_distributions(self, resource_value):
        """
        Returns True if this allocator can support the specified required
        distributions.
        """
        required = []
        for dist in resource_value:
            required.append(dist.as_requirement())
        not_avail = check_requirements(sorted(required), logger=self._logger)
        if not_avail:  # Distribution not found or version conflict.
            return (-2, {'required_distributions' : not_avail})
        return (0, None)

    def check_orphan_modules(self, resource_value):
        """
        Returns True if this allocator can support the specified 'orphan'
        modules.
        """
#FIXME: shouldn't pollute the environment like this does.
        not_found = []
        for module in sorted(resource_value):
            self._logger.debug("checking for 'orphan' module: %s", module)
            try:
                __import__(module)
            except ImportError:
                self._logger.info('    not found')
                not_found.append(module)
        if len(not_found) > 0:  # Can't import module(s).
            return (-2, {'orphan_modules' : not_found})
        return (0, None)

    def deploy(self, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        `criteria` is the dictionary returned by :meth:`rate_resource`.
        Returns a proxy to the deployed server.
        """
        raise NotImplementedError


class LocalAllocator(ResourceAllocator):
    """ Purely local resource allocator. """

    def __init__(self, name='LocalAllocator', total_cpus=1, max_load=2):
        super(LocalAllocator, self).__init__(name)
        self.total_cpus = total_cpus
        self.max_load = max_load

    def rate_resource(self, resource_desc):
        """
        Return a score indicating how well this resource allocator can satisfy
        the `resource_desc` request.

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        """
        for key, value in resource_desc.items():
            if key == 'localhost':
                if not value:
                    return (-2, {key : value})  # Specifically not localhost.

            elif key == 'n_cpus':
                if value > self.total_cpus:
                    return (-2, {key : value})  # Too many cpus.

            elif key == 'required_distributions':
                score, info = self.check_required_distributions(value)
                if score < 0:
                    return (score, info)  # Not found or version conflict.

            elif key == 'orphan_modules':
                score, info = self.check_orphan_modules(value)
                if score < 0:
                    return (score, info)  # Can't import module(s).

            elif key == 'python_version':
                if sys.version[:3] != value:
                    return (-2, {key : value})  # Version mismatch.

            else:
                return (-2, {key : value})  # Unrecognized => unsupported.

        # Check system load.
        try:
            loadavgs = os.getloadavg()
        except AttributeError:
            return (0, {})
        self._logger.debug('loadavgs %.2f, %.2f, %.2f, max_load %d',
                           loadavgs[0], loadavgs[1], loadavgs[2], self.max_load)
        if loadavgs[0] < self.max_load:
            return (0, {'loadavgs' : loadavgs, 'max_load' : self.max_load})
        else:
            return (-1, {'loadavgs' : loadavgs, 'max_load' : self.max_load})

    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Returns a proxy to the deployed server.
        """
        return self.create(typname='', name=name)

    @staticmethod
    def register(manager):
        """ Register :class:`LocalAllocator` proxy info with `manager`. """
        name = 'LocalAllocator'
        ObjServer.register(manager)
        method_to_typeid = {
            'deploy': 'ObjServer',
        }
        manager.register(name, LocalAllocator,
                         method_to_typeid=method_to_typeid)

LocalAllocator.register(mp_distributing.Cluster)
LocalAllocator.register(mp_distributing.HostManager)


class ClusterAllocator(object):
    """
    Cluster-based resource allocator.  This allocator manages a collection
    of :class:`LocalAllocator`, one for each machine in the cluster.
    `machines` is a list of dictionaries providing configuration data for each
    machine in the cluster.  At a minimum, each dictionary must specify a host
    address in 'hostname' and the path to the OpenMDAO python command in
    'python'.

    We assume that machines in the cluster are similar enough that ranking
    by load average is reasonable.
    """

    def __init__(self, name, machines):
        self.name = name
        self._allocators = {}
        self._last_deployed = None
        self._logger = logging.getLogger(name)
        self._score_q = Queue.Queue()
        self._pool = WorkerPool(self._service_loop) # Pool of worker threads.

        hosts = []
        for machine in machines:
            self._logger.debug('initializing %s', machine)
            host = mp_distributing.Host(machine['hostname'],
                                        python=machine['python'])
            LocalAllocator.register(host)
            hosts.append(host)

        self.cluster = mp_distributing.Cluster(hosts, [])
        self.cluster.start()
        self._logger.debug('server listening on %s', self.cluster.address)

        for slot in self.cluster:
            if slot.host.state != 'up':
                self._logger.error('Host %s state is %s', slot.host.hostname,
                                   slot.host.state)
                continue

            manager = slot.host.manager
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

            if host_ip not in self._allocators:
                self._allocators[host_ip] = manager.LocalAllocator(host)
                self._logger.debug('LocalAllocator for %s %s', host,
                                   self._allocators[host_ip])

    def __len__(self):
        """ Length of cluster is the number of allocators. """
        return len(self._allocators)

    def rate_resource(self, resource_desc):
        """
        Return a score indicating how well this resource allocator can satisfy
        the `resource_desc` request.

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        This allocator polls each :class:`LocalAllocator` in the cluster
        to find the best match and returns that.
        """
        best_score = -2
        best_criteria = None
        best_allocator = None
 
        # Prefer not to repeat use of just-used allocator.
        prev_score = -2
        prev_criteria = None
        prev_allocator = self._last_deployed
        self._last_deployed = None

        # Drain _score_q.
        while True:
            try:
                self._score_q.get_nowait()
            except Queue.Empty:
                break

        # Get scores via worker threads.
        todo = []
        max_workers = 10
        for i, allocator in enumerate(self._allocators.values()):
            if i < max_workers:
                worker_q = self._pool.get()
                worker_q.put((allocator, resource_desc))
            else:
                todo.append(allocator)

        # Process scores.
        for i in range(len(self._allocators)):
            worker_q, allocator, msg, score, criteria = self._score_q.get()
            try:
                next_allocator = todo.pop()
            except IndexError:
                self._pool.release(worker_q)
            else:
                worker_q.put((next_allocator, resource_desc))

            if msg:
                continue

            if allocator is prev_allocator:
                prev_score = score
                prev_criteria = criteria
            elif (best_score <= 0 and score > best_score) or \
                 (best_score >  0 and score < best_score):
                best_score = score
                best_criteria = criteria
                best_allocator = allocator
            elif (best_score == 0 and score == 0):
                best_load = best_criteria['loadavgs'][0]
                load = criteria['loadavgs'][0]
                if load < best_load:
                    best_score = score
                    best_criteria = criteria
                    best_allocator = allocator

        # If no alternative, repeat use of previous allocator.
        if best_score < 0 and prev_score >= 0:
            best_score = prev_score
            best_criteria = prev_criteria
            best_allocator = prev_allocator

        # Save best allocator in criteria in case we're asked to deploy.
        if best_criteria is not None:
            best_criteria['allocator'] = best_allocator
        return (best_score, best_criteria)

    def _service_loop(self, request_q):
        """ Get score from an allocator and queue results. """
        while True:
            allocator, resource_desc = request_q.get()
            if allocator is None:
                request_q.task_done()
                return  # Shutdown.

            msg = None
            try:
                score, criteria = allocator.rate_resource(resource_desc)
                if score == 0:
                    self._logger.debug('allocator %s returned %g (%g)',
                                       allocator.get_name(), score,
                                       criteria['loadavgs'][0])
                else:
                    self._logger.debug('allocator %s returned %g',
                                       allocator.get_name(), score)
            except Exception, exc:
                msg = '%s\n%s' % (exc, traceback.format_exc())
                self._logger.error('allocator %s caught exception %s',
                                   allocator.get_name(), msg)
                score = None
                criteria = None

            request_q.task_done()
            self._score_q.put((request_q, allocator, msg, score, criteria))

    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Returns a proxy to the deployed server.
        """
        allocator = criteria['allocator']
        self._last_deployed = allocator
        return allocator.deploy(name, resource_desc, criteria)

    def shutdown(self):
        """ Shutdown, releasing resources. """
        self.cluster.shutdown()


class WorkerPool(object):
    """ Pool of worker threads, grows as necessary. """

    def __init__(self, target):
        self._target = target
        self._idle = []     # Queues of idle workers.
        self._workers = {}  # Maps queue to worker.
        atexit.register(self.cleanup)

    def cleanup(self):
        """ Cleanup resources (worker threads). """
        for queue in self._workers:
            queue.put((None, None))
            self._workers[queue].join(1)
            if self._workers[queue].is_alive():
                print 'Worker join timed-out.'
            try:
                self._idle.remove(queue)
            except ValueError:
                pass  # Never released due to some other issue...
        self._workers.clear()

    def get(self):
        """ Get a worker queue from the pool. """
        try:
            return self._idle.pop()
        except IndexError:
            queue = Queue.Queue()
            worker = threading.Thread(target=self._target, args=(queue,))
            worker.daemon = True
            worker.start()
            self._workers[queue] = worker
            return queue

    def release(self, queue):
        """ Release a worker queue back to the pool. """
        self._idle.append(queue)

