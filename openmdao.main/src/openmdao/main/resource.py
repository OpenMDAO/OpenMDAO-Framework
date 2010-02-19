"""
Support for allocation of servers from one or more resources
(i.e. the local host, a cluster of remote hosts, etc.)
"""

import logging
import multiprocessing
import os
import Queue
import sys
import threading
import time
import traceback

from openmdao.main import mp_distributing
from openmdao.main.objserverfactory import ObjServerFactory, ObjServer
from openmdao.util.eggloader import check_requirements
from openmdao.util.wrkpool import WorkerPool


class ResourceAllocationManager(object):
    """
    The allocation manager maintains a list of allocators which are used
    to select the 'best fit' for a particular resource request.  The manager
    is initialized with a :class:`LocalAllocator` for the local host.
    Additional allocators can be added and the manager will look for the
    best fit across all the allocators.
    """

    _lock = threading.Lock()
    _RAM = None  # Singleton.

    def __init__(self):
        self._logger = logging.getLogger('RAM')
        self._allocations = 0
        self._allocators = []
        self._alloc_index = 0
        self._allocators.append(LocalAllocator())

    def __getitem__(self, i):
        return self._allocators[i]

    def __iter__(self):
        return iter(self._allocators)

    def __len__(self):
        return len(self._allocators)

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
    def get_allocator(index):
        """ Insert an allocator into the list of resource allocators. """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            return ram._allocators[index]

    @staticmethod
    def max_servers(resource_desc):
        """
        Returns the maximum number of servers compatible with 'resource_desc`.
        This shouyld be considered an upper limit on the number of concurrent
        allocations attempted.
        """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            return ram._max_servers(resource_desc)

    def _max_servers(self, resource_desc):
        """ Return total of each allocator's max servers. """
        total = 0
        for allocator in self._allocators:
            count = allocator.max_servers(resource_desc)
            self._logger.debug('allocator %s returned %d',
                               allocator.name, count)
            total += count
        return total

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
                print >> sys.stderr, \
                      'RAM: caught exception logging cleanup of %s: %s', \
                      name, trace
        del server


class ResourceAllocator(ObjServerFactory):
    """
    Base class for allocators. Allocators estimate the suitability of a
    resource and can deploy on that resource.
    """

    def __init__(self, name):
        super(ResourceAllocator, self).__init__()
        self.name = name
        self._logger = logging.getLogger(name)

    def get_name(self):
        """ Return this allocator's name. """
        return self.name

    def max_servers(self, resource_desc):
        """
        Return the maximum number of servers which could be deployed for
        `resource_desc`.  The value needn't be exact, but performance may
        suffer if it overestimates.  The value is used to limit the number
        of concurrent evaluations.
        """
        raise NotImplementedError

    def rate_resource(self, resource_desc):
        """
        Return a score indicating how well this resource allocator can satisfy
        the `resource_desc` request.  The score will be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the score, such as load averages, unsupported resources, etc.
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

    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        `criteria` is the dictionary returned by :meth:`rate_resource`.
        Returns a proxy to the deployed server.
        """
        raise NotImplementedError


class LocalAllocator(ResourceAllocator):
    """
    Purely local resource allocator. If `total_cpus` is >0, then that is
    taken as the number of cpus/cores available.  Otherwise the number is
    taken from :meth:`multiprocessing.cpu_count`.  The `max_load`
    parameter specifies the maximum cpu-adjusted load allowed when determining
    if another server may be started in :meth:`rate_resource`.
    """

    def __init__(self, name='LocalAllocator', total_cpus=0, max_load=1.0):
        super(LocalAllocator, self).__init__(name)
        if total_cpus > 0:
            self.total_cpus = total_cpus
        else:
            try:
                self.total_cpus = multiprocessing.cpu_count()
            except NotImplementedError:
                self.total_cpus = 1
        self.max_load = max(max_load, 0.5)  # Ensure > 0!

    def max_servers(self, resource_desc):
        """
        Return the maximum number of servers which could be deployed for
        `resource_desc`.  The value needn't be exact, but performance may
        suffer if it overestimates.  The value is used to limit the number
        of concurrent evaluations.
        """
        score, criteria = self._check_compatibility(resource_desc, False)
        if score < 0:
            return 0  # Incompatible with resource_desc.
        return max(int(self.total_cpus * self.max_load), 1)

    def rate_resource(self, resource_desc):
        """
        Return (score, criteria) indicating how well this allocator can satisfy
        the `resource_desc` request.  The score will be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the score, such as load averages, unsupported resources, etc.
        """
        score, criteria = self._check_compatibility(resource_desc, True)
        if score < 0:
            return (score, criteria)

        # Check system load.
        try:
            loadavgs = os.getloadavg()
        except AttributeError:
            return (0, {})
        self._logger.debug('loadavgs %.2f, %.2f, %.2f, max_load %d',
                           loadavgs[0], loadavgs[1], loadavgs[2], self.max_load)
        criteria = {
            'loadavgs'   : loadavgs,
            'total_cpus' : self.total_cpus,
            'max_load'   : self.max_load
        }
        if (loadavgs[0] / self.total_cpus) < self.max_load:
            return (0, criteria)
        else:
            return (-1, criteria)  # Try again later.

    def _check_compatibility(self, resource_desc, log_failure):
        """
        Check compatibility against `resource_desc`.
        Returns (score, criteria), where `score` >= 0 implies compatibility.
        """
        for key, value in resource_desc.items():
            if key == 'localhost':
                if not value:
                    if log_failure:
                        self._logger.debug('Rating failed:' \
                                           ' specifically not localhost.')
                    return (-2, {key : value})

            elif key == 'n_cpus':
                if value > self.total_cpus:
                    if log_failure:
                        self._logger.debug('Rating failed: too many cpus.')
                    return (-2, {key : value})

            elif key == 'required_distributions':
                score, info = self.check_required_distributions(value)
                if score < 0:
                    if log_failure:
                        self._logger.debug('Rating failed:' \
                                           ' not found or version conflict.')
                    return (score, info)

            elif key == 'orphan_modules':
                score, info = self.check_orphan_modules(value)
                if score < 0:
                    if log_failure:
                        self._logger.debug("Rating failed:" \
                                           " can't import module(s).")
                    return (score, info)

            elif key == 'python_version':
                if sys.version[:3] != value:
                    if log_failure:
                        self._logger.debug('Rating failed: version mismatch.')
                    return (-2, {key : value})

            else:
                if log_failure:
                    self._logger.debug('Rating failed:' \
                                       ' unrecognized => unsupported.')
                return (-2, {key : value})
        return (0, {})

    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Returns a proxy to the deployed server.
        """
        return self.create(typname='', name=name)

    @staticmethod
    def register(manager):
        """
        Register :class:`LocalAllocator` proxy info with `manager`.
        Not typically called by user code.
        """
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
        self._lock = threading.Lock()
        self._allocators = {}
        self._last_deployed = None
        self._logger = logging.getLogger(name)
        self._reply_q = Queue.Queue()

        hosts = []
        for machine in machines:
            host = mp_distributing.Host(machine['hostname'],
                                        python=machine['python'])
            LocalAllocator.register(host)
            hosts.append(host)

        self.cluster = mp_distributing.Cluster(hosts, [])
        self.cluster.start()
        self._logger.debug('server listening on %s', self.cluster.address)

        for slot in self.cluster:
            manager = slot.host.manager
            try:
                host = manager._name
            except AttributeError:
                host = 'localhost'
                host_ip = '127.0.0.1'
                host_id = host_ip
            else:
                # 'host' is 'Host-<ipaddr>:<port>
                dash = host.index('-')
                colon = host.index(':')
                host_ip = host[dash+1:colon]
                host_id = host[dash+1:]

            if host_ip not in self._allocators:
                self._allocators[host_ip] = manager.LocalAllocator(host)
                self._logger.debug('LocalAllocator for %s at %s',
                                   slot.host.hostname, host_id)

    def __getitem__(self, i):
        return self._allocators[i]

    def __iter__(self):
        return iter(self._allocators)

    def __len__(self):
        return len(self._allocators)

    def max_servers(self, resource_desc):
        """
        Return the maximum number of servers which could be deployed for
        `resource_desc`.  The value needn't be exact, but performance may
        suffer if it overestimates.  The value is used to limit the number
        of concurrent evaluations.
        """
        with self._lock:
            # Drain _reply_q.
            while True:
                try:
                    self._reply_q.get_nowait()
                except Queue.Empty:
                    break

            # Get counts via worker threads.
            todo = []
            max_workers = 10
            for i, allocator in enumerate(self._allocators.values()):
                if i < max_workers:
                    worker_q = WorkerPool.get()
                    worker_q.put((self._get_count, (allocator, resource_desc),
                                  {}, self._reply_q))
                else:
                    todo.append(allocator)

            # Process counts.
            total = 0
            for i in range(len(self._allocators)):
                worker_q, retval, exc, trace = self._reply_q.get()
                if exc:
                    self._logger.error(trace)
                    raise exc

                try:
                    next_allocator = todo.pop(0)
                except IndexError:
                    WorkerPool.release(worker_q)
                else:
                    worker_q.put((self._get_count,
                                  (next_allocator, resource_desc),
                                  {}, self._reply_q))
                count = retval
                if count:
                    total += count
            return total

    def _get_count(self, allocator, resource_desc):
        """ Get `max_servers` from an allocator. """
        count = 0
        try:
            count = allocator.max_servers(resource_desc)
        except Exception, exc:
            msg = '%s\n%s' % (exc, traceback.format_exc())
            self._logger.error('allocator %s caught exception %s',
                               allocator.get_name(), msg)
        return count

    def rate_resource(self, resource_desc):
        """
        Return a score indicating how well this resource allocator can satisfy
        the `resource_desc` request.  The score will be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the score, such as load averages, unsupported resources, etc.

        This allocator polls each :class:`LocalAllocator` in the cluster
        to find the best match and returns that.  The best allocator is saved
        in the returned criteria for a subsequent :meth:`deploy`.
        """
        with self._lock:
            best_score = -2
            best_criteria = None
            best_allocator = None
 
            # Prefer not to repeat use of just-used allocator.
            prev_score = -2
            prev_criteria = None
            prev_allocator = self._last_deployed
            self._last_deployed = None

            # Drain _reply_q.
            while True:
                try:
                    self._reply_q.get_nowait()
                except Queue.Empty:
                    break

            # Get scores via worker threads.
            todo = []
            max_workers = 10
            for i, allocator in enumerate(self._allocators.values()):
                if i < max_workers:
                    worker_q = WorkerPool.get()
                    worker_q.put((self._get_rating, (allocator, resource_desc),
                                  {}, self._reply_q))
                else:
                    todo.append(allocator)

            # Process scores.
            for i in range(len(self._allocators)):
                worker_q, retval, exc, trace = self._reply_q.get()
                if exc:
                    self._logger.error(trace)
                    raise exc

                try:
                    next_allocator = todo.pop(0)
                except IndexError:
                    WorkerPool.release(worker_q)
                else:
                    worker_q.put((self._get_rating,
                                  (next_allocator, resource_desc),
                                  {}, self._reply_q))

                allocator, score, criteria = retval
                if score is None:
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

    def _get_rating(self, allocator, resource_desc):
        """ Get (score, criteria) from an allocator. """
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

        return (allocator, score, criteria)

    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Uses allocator saved in `criteria`.
        Returns a proxy to the deployed server.
        """
        with self._lock:
            allocator = criteria['allocator']
            self._last_deployed = allocator
        return allocator.deploy(name, resource_desc, criteria)

    def shutdown(self):
        """ Shutdown, releasing resources. """
        self.cluster.shutdown()

