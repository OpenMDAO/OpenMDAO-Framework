"""
Support for allocation of servers from one or more resources
(i.e., the local host, a cluster of remote hosts, etc.)
"""

import logging
import multiprocessing
import os
import pkg_resources
import Queue
import socket
import sys
import threading
import time
import traceback

from openmdao.main import mp_distributing
from openmdao.main.mp_support import register
from openmdao.main.objserverfactory import ObjServerFactory
from openmdao.main.rbac import get_credentials, set_credentials, rbac

from openmdao.util.eggloader import check_requirements
from openmdao.util.wrkpool import WorkerPool


class ResourceAllocationManager(object):
    """
    The allocation manager maintains a list of :class:`ResourceAllocator`
    which are used to select the "best fit" for a particular resource request.
    The manager is initialized with a :class:`LocalAllocator` for the local
    host, using `authkey` of 'PublicKey', and allowing 'shell' access.
    Additional allocators can be added and the manager will look for the best
    fit across all the allocators.
    """

    _lock = threading.Lock()
    _RAM = None  # Singleton.

    def __init__(self):
        self._logger = logging.getLogger('RAM')
        self._allocations = 0
        self._allocators = []
        self._allocators.append(LocalAllocator('LocalHost',
                                               authkey='PublicKey',
                                               allow_shell=True))
        self._deployed_servers = {}

    @staticmethod
    def get_instance():
        """ Return singleton instance. """
        with ResourceAllocationManager._lock:
            if ResourceAllocationManager._RAM is None:
                ResourceAllocationManager._RAM = ResourceAllocationManager()
            return ResourceAllocationManager._RAM

    @staticmethod
    def add_allocator(allocator):
        """
        Add an allocator to the list of resource allocators.

        allocator: ResourceAllocator
            The allocator to be added.
        """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            ram._allocators.append(allocator)

    @staticmethod
    def insert_allocator(index, allocator):
        """
        Insert an allocator into the list of resource allocators.

        index: int
            List index for the insertion point.

        allocator: ResourceAllocator
            The allocator to be inserted.
        """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            ram._allocators.insert(index, allocator)

    @staticmethod
    def get_allocator(index):
        """
        Return allocator at `index`.

        index: int
            List index for allocator to be returned.
        """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            return ram._allocators[index]

    @staticmethod
    def list_allocators():
        """ Return list of allocators. """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            return ram._allocators

    @staticmethod
    def max_servers(resource_desc):
        """
        Returns the maximum number of servers compatible with 'resource_desc`.
        This should be considered an upper limit on the number of concurrent
        allocations attempted.

        resource_desc: dict
            Description of required resources.
        """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            return ram._max_servers(resource_desc)

    def _max_servers(self, resource_desc):
        """ Return total of each allocator's max servers. """
        total = 0
        for allocator in self._allocators:
            count = allocator.max_servers(resource_desc)
            self._logger.debug('%r returned %d', allocator._name, count)
            total += count
        return total

    @staticmethod
    def allocate(resource_desc):
        """
        Determine best resource for `resource_desc` and deploy.
        In the case of a tie, the first allocator in the allocators list wins.
        Returns ``(proxy-object, server-dict)``.

        resource_desc: dict
            Description of required resources.
        """
        for handler in logging._handlerList:
            handler.flush()  # Try to keep log messages sane.

        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            return ram._allocate(resource_desc)

    def _allocate(self, resource_desc):
        """ Do the allocation. """
        deployment_retries = 0
        best_estimate = -1
        while best_estimate == -1:
            best_estimate, best_criteria, best_allocator = \
                self._get_estimates(resource_desc)
            if best_estimate >= 0:
                self._allocations += 1
                name = 'Sim-%d' % self._allocations
                self._logger.debug('deploying on %r', best_allocator._name)
                server = best_allocator.deploy(name, resource_desc,
                                               best_criteria)
                if server is not None:
                    server_info = {
                        'name': name,
                        'pid':  server.pid,
                        'host': server.host
                    }
                    self._logger.info('allocated %r pid %d on %s',
                                      name, server_info['pid'],
                                      server_info['host'])
                    self._deployed_servers[id(server)] = \
                        (best_allocator, server, server_info)
                    return (server, server_info)
                # Difficult to generate deployable request that won't deploy...
                else:  #pragma no cover
                    deployment_retries += 1
                    if deployment_retries > 10:
                        self._logger.error('deployment failed too many times.')
                        return (None, None)
                    self._logger.warning('deployment failed, retrying.')
                    best_estimate = -1
            elif best_estimate != -1:
                return (None, None)
            # Difficult to generate deployable request that won't deploy...
            else:  #pragma no cover
                time.sleep(1)  # Wait a bit between retries.

    @staticmethod
    def get_hostnames(resource_desc):
        """
        Determine best resource for `resource_desc` and return hostnames.
        In the case of a tie, the first allocator in the allocators list wins.
        Typically used by parallel code wrappers which have MPI or something
        similar for process deployment.

        resource_desc: dict
            Description of required resources.
        """
        ram = ResourceAllocationManager.get_instance()
        with ResourceAllocationManager._lock:
            return ram._get_hostnames(resource_desc)

    def _get_hostnames(self, resource_desc):
        """ Get the hostnames. """
        best_score = -1
        while best_score == -1:
            best_score, best_criteria, best_allocator = \
                self._get_estimates(resource_desc, need_hostnames=True)
            if best_score >= 0:
                self._logger.debug('using %s', best_criteria['hostnames'])
                return best_criteria['hostnames']
            elif best_score != -1:
                return None
            # Difficult to generate deployable request that won't deploy...
            else:  #pragma no cover
                time.sleep(1)  # Wait a bit between retries.

    def _get_estimates(self, resource_desc, need_hostnames=False):
        """ Return best (estimate, criteria, allocator). """
        best_estimate = -2
        best_criteria = None
        best_allocator = None

        for allocator in self._allocators:
            estimate, criteria = allocator.time_estimate(resource_desc)
            self._logger.debug('%r returned %g', allocator._name, estimate)
            if (best_estimate == -2 and estimate >= -1) or \
               (best_estimate == 0  and estimate >  0) or \
               (best_estimate >  0  and estimate < best_estimate):
                # All current allocators support 'hostnames'.
                if need_hostnames and not 'hostnames' in criteria:  #pragma no cover
                    self._logger.debug("%r is missing 'hostnames'",
                                       allocator._name)
                else:
                    best_estimate = estimate
                    best_criteria = criteria
                    best_allocator = allocator

        return (best_estimate, best_criteria, best_allocator)

    @staticmethod
    def release(server):
        """
        Release a server (proxy).

        server: :class:`OpenMDAO_Proxy`
            Server to be released.
        """
        ram = ResourceAllocationManager.get_instance()
        return ram._release(server)

    def _release(self, server):
        """ Release a server (proxy). """
        with ResourceAllocationManager._lock:
            try:
                allocator, server, server_info = \
                    self._deployed_servers[id(server)]
            # Just being defensive.
            except KeyError:  #pragma no cover
                self._logger.error('server %r not found', server)
                return
            del self._deployed_servers[id(server)]

        self._logger.info('release %r pid %d on %s', server_info['name'],
                          server_info['pid'], server_info['host'])
        try:
            allocator.release(server)
        # Just being defensive.
        except Exception as exc:  #pragma no cover
            self._logger.error("Can't release %r: %r", server_info['name'], exc)
        server._close.cancel()


class ResourceAllocator(ObjServerFactory):
    """
    Base class for allocators. Allocators estimate the suitability of a
    resource and can deploy on that resource.

    name: string
        Name of allocator, used in log messages, etc.

    authkey: string
        Authorization key for this allocator and any deployed servers.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Use with caution!
    """

    def __init__(self, name, authkey=None, allow_shell=False):
        if authkey is None:
            authkey = multiprocessing.current_process().authkey
            if authkey is None:
                authkey = 'PublicKey'
                multiprocessing.current_process().authkey = authkey
        super(ResourceAllocator, self).__init__(name, authkey, allow_shell)
        self.name = name

    # To be implemented by real allocator.
    def max_servers(self, resource_desc):  #pragma no cover
        """
        Return the maximum number of servers which could be deployed for
        `resource_desc`.  The value needn't be exact, but performance may
        suffer if it overestimates.  The value is used to limit the number
        of concurrent evaluations.

        resource_desc: dict
            Description of required resources.
        """
        raise NotImplementedError

    # To be implemented by real allocator.
    def time_estimate(self, resource_desc):  #pragma no cover
        """
        Return ``(estimate, criteria)`` indicating how well this resource
        allocator can satisfy the `resource_desc` request.  The estimate will
        be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the estimate, such as hostnames, load averages, unsupported
        resources, etc.

        resource_desc: dict
            Description of required resources.
        """
        raise NotImplementedError

    def check_required_distributions(self, resource_value):
        """
        Returns True if this allocator can support the specified required
        distributions.

        resource_value: list
            List of Distributions or Requirements.
        """
        required = []
        for item in resource_value:
            if isinstance(item, pkg_resources.Distribution):
                required.append(item.as_requirement())
            else:
                required.append(item)
        not_avail = check_requirements(sorted(required)) #, logger=self._logger)
        if not_avail:  # Distribution not found or version conflict.
            return (-2, {'required_distributions' : not_avail})
        return (0, None)

    def check_orphan_modules(self, resource_value):
        """
        Returns True if this allocator can support the specified 'orphan'
        modules.

        resource_value: list
            List of 'orphan' module names.
        """
#FIXME: shouldn't pollute the environment like this does.
        not_found = []
        for module in sorted(resource_value):
#            self._logger.debug("checking for 'orphan' module: %s", module)
            try:
                __import__(module)
            except ImportError:
#                self._logger.info('    not found')
                not_found.append(module)
        if len(not_found) > 0:  # Can't import module(s).
            return (-2, {'orphan_modules' : not_found})
        return (0, None)

    # To be implemented by real allocator.
    def deploy(self, name, resource_desc, criteria):  #pragma no cover
        """
        Deploy a server suitable for `resource_desc`.
        Returns a proxy to the deployed server.

        name: string
            Name for server.

        resource_desc: dict
            Description of required resources.

        criteria: dict
            The dictionary returned by :meth:`time_estimate`.
        """
        raise NotImplementedError


class LocalAllocator(ResourceAllocator):
    """
    Purely local resource allocator.

    name: string
        Name of allocator, used in log messages, etc.

    total_cpus: int
        If >0, then that is taken as the number of cpus/cores available.
        Otherwise the number is taken from :meth:`multiprocessing.cpu_count`.

    max_load: float
        Specifies the maximum cpu-adjusted load (obtained from
        :meth:`os.getloadavg`) allowed when reporting :meth:`max_servers` and
        when determining if another server may be started in
        :meth:`time_estimate`.

    authkey: string
        Authorization key for this allocator and any deployed servers.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Use with caution!
    """

    def __init__(self, name='LocalAllocator', total_cpus=0, max_load=1.0,
                 authkey=None, allow_shell=False):
        super(LocalAllocator, self).__init__(name, authkey, allow_shell)
        self._name = name  # To allow looking like a proxy.
        self.pid = os.getpid()  # We may be a process on a remote host.
        if total_cpus > 0:
            self.total_cpus = total_cpus
        else:
            try:
                self.total_cpus = multiprocessing.cpu_count()
            # Just being defensive (according to docs this could happen).
            except NotImplementedError:  # pragma no cover
                self.total_cpus = 1
        self.max_load = max(max_load, 0.5)  # Ensure > 0!

    @rbac('*')
    def max_servers(self, resource_desc):
        """
        Returns `total_cpus` * `max_load` if `resource_desc` is supported,
        otherwise zero.

        resource_desc: dict
            Description of required resources.
        """
        estimate, criteria = self._check_compatibility(resource_desc, False)
        if estimate < 0:
            return 0  # Incompatible with resource_desc.
        return max(int(self.total_cpus * self.max_load), 1)

    @rbac('*')
    def time_estimate(self, resource_desc):
        """
        Returns ``(estimate, criteria)`` indicating how well this allocator can
        satisfy the `resource_desc` request.  The estimate will be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the estimate, such as hostnames, load averages, unsupported
        resources, etc.

        resource_desc: dict
            Description of required resources.
        """
        estimate, criteria = self._check_compatibility(resource_desc, True)
        if estimate < 0:
            return (estimate, criteria)

        # Check system load.
        try:
            loadavgs = os.getloadavg()
        # Not available on Windows.
        except AttributeError:  #pragma no cover
            criteria = {
                'hostnames'  : [socket.gethostname()],
                'total_cpus' : self.total_cpus,
            }
            return (0, criteria)

        self._logger.debug('loadavgs %.2f, %.2f, %.2f, max_load %d',
                           loadavgs[0], loadavgs[1], loadavgs[2], self.max_load)
        criteria = {
            'hostnames'  : [socket.gethostname()],
            'loadavgs'   : loadavgs,
            'total_cpus' : self.total_cpus,
            'max_load'   : self.max_load
        }
        if (loadavgs[0] / self.total_cpus) < self.max_load:
            return (0, criteria)
        # Tests force max_load high to avoid other issues.
        else:  #pragma no cover
            return (-1, criteria)  # Try again later.

    def _check_compatibility(self, resource_desc, log_failure):
        """
        Check compatibility against `resource_desc`.
        Returns ``(estimate, criteria)``, where `estimate` >= 0 implies
        compatibility.
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
                estimate, info = self.check_required_distributions(value)
                if estimate < 0:
                    if log_failure:
                        self._logger.debug('Rating failed:' \
                                           ' not found or version conflict.')
                    return (estimate, info)

            elif key == 'orphan_modules':
                estimate, info = self.check_orphan_modules(value)
                if estimate < 0:
                    if log_failure:
                        self._logger.debug("Rating failed:" \
                                           " can't import module(s).")
                    return (estimate, info)

            elif key == 'python_version':
                if sys.version[:3] != value:
                    if log_failure:
                        self._logger.debug('Rating failed: version mismatch.')
                    return (-2, {key : value})

            elif key == 'exclude':
                 if socket.gethostname() in value:
                    if log_failure:
                        self._logger.debug('Rating failed: excluded host.')
                    return (-2, {key : value})

            elif key == 'allocator':
                 if self.name != value:
                    if log_failure:
                        self._logger.debug('Rating failed: wrong allocator.')
                    return (-2, {key : value})

            else:
                if log_failure:
                    self._logger.debug('Rating failed:' \
                                       ' unrecognized => unsupported.')
                return (-2, {key : value})

        return (0, {})

    @rbac('*')
    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Returns a proxy to the deployed server.

        name: string
            Name for server.

        resource_desc: dict
            Description of required resources.

        criteria: dict
            The dictionary returned by :meth:`time_estimate`.
        """
        credentials = get_credentials()
        allowed_users = {credentials.user: credentials.public_key}
        try:
            return self.create(typname='', allowed_users=allowed_users,
                               name=name)
        # Shouldn't happen...
        except Exception as exc:  #pragma no cover
            self._logger.error('create failed: %r', exc)
            return None

register(LocalAllocator, mp_distributing.Cluster)
register(LocalAllocator, mp_distributing.HostManager)


# Cluster allocation requires ssh configuration and multiple hosts.
class ClusterAllocator(object):  #pragma no cover
    """
    Cluster-based resource allocator.  This allocator manages a collection
    of :class:`LocalAllocator`, one for each machine in the cluster.

    name: string
        Name of allocator, used in log messages, etc.

    machines: list(dict)
        Dictionaries providing configuration data for each machine in the
        cluster.  At a minimum, each dictionary must specify a host
        address in 'hostname' and the path to the OpenMDAO Python command in
        'python'.

    authkey: string
        Authorization key to be passed-on to remote servers.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Use with caution!

    We assume that machines in the cluster are similar enough that ranking
    by load average is reasonable.
    """

    def __init__(self, name, machines, authkey=None, allow_shell=False):
        if authkey is None:
            authkey = multiprocessing.current_process().authkey
            if authkey is None:
                authkey = 'PublicKey'
                multiprocessing.current_process().authkey = authkey

        self.name = name   # Duplication to look like both a server and proxy.
        self._name = name
        self._lock = threading.Lock()
        self._allocators = {}
        self._last_deployed = None
        self._logger = logging.getLogger(name)
        self._reply_q = Queue.Queue()
        self._deployed_servers = {}

        hosts = []
        for machine in machines:
            host = mp_distributing.Host(machine['hostname'],
                                        python=machine['python'])
            host.register(LocalAllocator)
            hosts.append(host)

        self.cluster = mp_distributing.Cluster(hosts, authkey=authkey,
                                               allow_shell=allow_shell)
        self.cluster.start()
        self._logger.debug('server listening on %r', (self.cluster.address,))

        for host in self.cluster:
            manager = host.manager
            try:
                name = manager._name
            except AttributeError:
                name = 'localhost'
                host_ip = '127.0.0.1'
            else:
                # 'host' is 'Host-<ipaddr>:<port>
                dash = name.index('-')
                colon = name.index(':')
                host_ip = name[dash+1:colon]

            if host_ip not in self._allocators:
                allocator = \
                    manager.openmdao_main_resource_LocalAllocator(name=name,
                                                        allow_shell=allow_shell)
                allocator._name = allocator.name
                self._allocators[host_ip] = allocator
                self._logger.debug('%s allocator %r pid %s', host.hostname,
                                   allocator._name, allocator.pid)

    def __getitem__(self, i):
        return self._allocators[i]

    def __iter__(self):
        return iter(self._allocators)

    def __len__(self):
        return len(self._allocators)

    def max_servers(self, resource_desc):
        """
        Returns the total of :meth:`max_servers` across all
        :class:`LocalAllocator` in the cluster.

        resource_desc: dict
            Description of required resources.
        """
        credentials = get_credentials()

        key = 'allocator'
        value = resource_desc.get(key, '')
        if value:
            if self.name != value:
                return 0
            else:
                # Any host in our cluster is OK.
                resource_desc = resource_desc.copy()
                del resource_desc[key]

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
                    worker_q.put((self._get_count,
                                  (allocator, resource_desc, credentials),
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
                                  (next_allocator, resource_desc, credentials),
                                  {}, self._reply_q))
                count = retval
                if count:
                    total += count
            return total

    def _get_count(self, allocator, resource_desc, credentials):
        """ Get `max_servers` from an allocator. """
        set_credentials(credentials)
        count = 0
        try:
            count = allocator.max_servers(resource_desc)
        except Exception:
            msg = traceback.format_exc()
            self._logger.error('%r max_servers() caught exception %s',
                               allocator._name, msg)
        return count

    def time_estimate(self, resource_desc):
        """
        Returns ``(estimate, criteria)`` indicating how well this allocator
        can satisfy the `resource_desc` request.  The estimate will be:

        - >0 for an estimate of walltime (seconds).
        -  0 for no estimate.
        - -1 for no resource at this time.
        - -2 for no support for `resource_desc`.

        The returned criteria is a dictionary containing information related
        to the estimate, such as hostnames, load averages, unsupported
        resources, etc.

        This allocator polls each :class:`LocalAllocator` in the cluster
        to find the best match and returns that.  The best allocator is saved
        in the returned criteria for a subsequent :meth:`deploy`.

        resource_desc: dict
            Description of required resources.
        """
        credentials = get_credentials()

        key = 'allocator'
        value = resource_desc.get(key, '')
        if value:
            if self.name != value:
                return (-2, {key: value})
            else:
                # Any host in our cluster is OK.
                resource_desc = resource_desc.copy()
                del resource_desc[key]

        n_cpus = resource_desc.get('n_cpus', 0)
        if n_cpus:
            # Spread across LocalAllocators.
            resource_desc = resource_desc.copy()
            resource_desc['n_cpus'] = 1

        with self._lock:
            best_estimate = -2
            best_criteria = None
            best_allocator = None
 
            # Prefer not to repeat use of just-used allocator.
            prev_estimate = -2
            prev_criteria = None
            prev_allocator = self._last_deployed
            self._last_deployed = None

            # Drain _reply_q.
            while True:
                try:
                    self._reply_q.get_nowait()
                except Queue.Empty:
                    break

            # Get estimates via worker threads.
            todo = []
            max_workers = 10
            for i, allocator in enumerate(self._allocators.values()):
                if i < max_workers:
                    worker_q = WorkerPool.get()
                    worker_q.put((self._get_estimate,
                                  (allocator, resource_desc, credentials),
                                  {}, self._reply_q))
                else:
                    todo.append(allocator)

            # Process estimates.
            host_loads = []  # Sorted list of (hostname, load)
            for i in range(len(self._allocators)):
                worker_q, retval, exc, trace = self._reply_q.get()
                if exc:
                    self._logger.error(trace)
                    retval = None

                try:
                    next_allocator = todo.pop(0)
                except IndexError:
                    WorkerPool.release(worker_q)
                else:
                    worker_q.put((self._get_estimate,
                                  (next_allocator, resource_desc, credentials),
                                  {}, self._reply_q))

                if retval is None:
                    continue
                allocator, estimate, criteria = retval
                if estimate is None:
                    continue

                # Update loads.
                if estimate >= 0 and n_cpus:
                    load = criteria['loadavgs'][0]
                    new_info = (criteria['hostnames'][0], load)
                    if host_loads:
                        for i, info in enumerate(host_loads):
                            if load < info[1]:
                                host_loads.insert(i, new_info)
                                break
                        else:
                            host_loads.append(new_info)
                    else:
                        host_loads.append(new_info)

                # Update best estimate.
                if allocator is prev_allocator:
                    prev_estimate = estimate
                    prev_criteria = criteria
                elif (best_estimate <= 0 and estimate > best_estimate) or \
                     (best_estimate >  0 and estimate < best_estimate):
                    best_estimate = estimate
                    best_criteria = criteria
                    best_allocator = allocator
                elif (best_estimate == 0 and estimate == 0):
                    best_load = best_criteria['loadavgs'][0]
                    load = criteria['loadavgs'][0]
                    if load < best_load:
                        best_estimate = estimate
                        best_criteria = criteria
                        best_allocator = allocator

            # If no alternative, repeat use of previous allocator.
            if best_estimate < 0 and prev_estimate >= 0:
                best_estimate = prev_estimate
                best_criteria = prev_criteria
                best_allocator = prev_allocator

            # Save best allocator in criteria in case we're asked to deploy.
            if best_criteria is not None:
                best_criteria['allocator'] = best_allocator

                # Save n_cpus hostnames in criteria.
                best_criteria['hostnames'] = \
                    [host_loads[i][0] \
                     for i in range(min(n_cpus, len(host_loads)))]

            return (best_estimate, best_criteria)

    def _get_estimate(self, allocator, resource_desc, credentials):
        """ Get (estimate, criteria) from an allocator. """
        set_credentials(credentials)
        try:
            estimate, criteria = allocator.time_estimate(resource_desc)
        except Exception:
            msg = traceback.format_exc()
            self._logger.error('%r time_estimate() caught exception %s',
                               allocator._name, msg)
            estimate = None
            criteria = None
        else:
            if estimate == 0:
                self._logger.debug('%r returned %g (%g)', allocator._name,
                                   estimate, criteria['loadavgs'][0])
            else:
                self._logger.debug('%r returned %g', allocator._name, estimate)

        return (allocator, estimate, criteria)

    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Uses the allocator saved in `criteria`.
        Returns a proxy to the deployed server.

        name: string
            Name for server.

        resource_desc: dict
            Description of required resources.

        criteria: dict
            The dictionary returned by :meth:`time_estimate`.
        """
        with self._lock:
            allocator = criteria['allocator']
            self._last_deployed = allocator
            del criteria['allocator']  # Don't pass a proxy without a server!
        try:
            server = allocator.deploy(name, resource_desc, criteria)
        except Exception as exc:
            self._logger.error('%r deploy() failed for %s: %r',
                               allocator._name, name, exc)
            return None

        if server is None:
            self._logger.error('%r deployment failed for %s',
                               allocator._name, name)
        else:
            self._deployed_servers[id(server)] = (allocator, server)
        return server

    def release(self, server):
        """
        Release a server (proxy).

        server: :class:`OpenMDAO_Proxy`
            Server to be released.
        """
        with self._lock:
            try:
                allocator = self._deployed_servers[id(server)][0]
            except KeyError:
                self._logger.error('server %r not found', server)
                return
            del self._deployed_servers[id(server)]

        try:
            allocator.release(server)
        except Exception as exc:
            self._logger.error("Can't release %r: %r", server, exc)
        server._close.cancel()

    def shutdown(self):
        """ Shutdown, releasing resources. """
        self.cluster.shutdown()

