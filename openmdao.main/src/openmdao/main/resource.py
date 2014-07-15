"""
.. _`resource.py`:

Support for allocation of servers from one or more resources
(i.e., the local host, a cluster of remote hosts, etc.).
"""

import ConfigParser
import datetime
import logging
import multiprocessing
import os.path
import pkg_resources
import Queue
import re
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

# DRMAA JobTemplate derived keys.
QUEUING_SYSTEM_KEYS = set((
    'remote_command',
    'args',
    'submit_as_hold',
    'rerunnable',
    'job_environment',
    'working_directory',
    'job_category',
    'email',
    'email_on_started',
    'email_on_terminated',
    'job_name',
    'input_path',
    'output_path',
    'error_path',
    'join_files',
    'reservation_id',
    'queue_name',
    'priority',
    'start_time',
    'deadline_time',
    'resource_limits',
    'accounting_id',

    # 'escape' mechanism kept from earlier version.
    'native_specification',
))

# DRMAA derived job categories.
JOB_CATEGORIES = set((
    'MPI',
    'GridMPI',
    'LAM-MPI',
    'MPICH1',
    'MPICH2',
    'OpenMPI',
    'PVM',
    'OpenMP',
    'OpenCL',
    'Java',
))

# DRMAA derived resource limits.
RESOURCE_LIMITS = set((
    'core_file_size',
    'data_seg_size',
    'file_size',
    'open_files',
    'stack_size',
    'virtual_memory',
    'cpu_time',
    'wallclock_time',
))

# DRMAA derived constants.
HOME_DIRECTORY = '$drmaa_hd_ph$'
WORKING_DIRECTORY = '$drmaa_wd_ph$'

# Legal allocator name pattern.
_LEGAL_NAME = re.compile(r'^[a-zA-Z][_a-zA-Z0-9]*$')

# Checks for IPv4 address.
_IPV4_HOST = re.compile(r'[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$')


class ResourceAllocationManager(object):
    """
    The allocation manager maintains a list of :class:`ResourceAllocator`
    which are used to select the "best fit" for a particular resource request.
    The manager is initialized with a :class:`LocalAllocator` for the local
    host, using `authkey` of 'PublicKey', and allowing 'shell' access.

    By default ``~/.openmdao/resources.cfg`` will be used for additional
    configuration information. To avoid this, call :meth:`configure` before
    any other allocation routines, or set the ``OPENMDAO_RAMFILE`` environment
    variable to the path to be used (a null path is legal and avoids any
    additional configuration).
    """

    _lock = threading.Lock()
    _lock_pid = os.getpid()  # For detecting copy from fork.
    _RAM = None              # Singleton.

    def __init__(self, config_filename=None):
        self._logger = logging.getLogger('RAM')
        self._pid = os.getpid()  # For detecting copy from fork.
        self._allocations = 0
        self._allocators = []
        self._deployed_servers = {}
        self._allocators.append(LocalAllocator('LocalHost',
                                               authkey='PublicKey',
                                               allow_shell=True))

        # If OPENMDAO_RAMFILE is specified, use that.
        # Set to null during testing for better coverage analysis.
        if config_filename is None:
            config_filename = os.environ.get('OPENMDAO_RAMFILE')
            if config_filename is not None:
                if config_filename and not os.path.exists(config_filename):
                    self._logger.error('OPENMDAO_RAMFILE %r not found',
                                       config_filename)

        if config_filename is None:
            config_filename = os.path.join('~', '.openmdao', 'resources.cfg')
            config_filename = os.path.expanduser(config_filename)
            if not os.path.exists(config_filename):
                return

        if config_filename:
            self._configure(config_filename)

    @staticmethod
    def configure(config_filename):
        """
        Configure allocators. This *must* be called before any other accesses
        if you want to avoid getting the default configuration as specified
        by ``~/.openmdao/resources.cfg``.

        config_filename: string
            Name of configuration file.
            If null, no additional configuration is performed.
        """
        orig = ResourceAllocationManager._RAM
        ram = ResourceAllocationManager._get_instance(config_filename)
        if ram is orig:  # Not configured.
            with ResourceAllocationManager._lock:
                ram._configure(config_filename)

    def _configure(self, config_filename):
        """ Configure manager instance. """
        self._logger.debug('Configuring from %r', config_filename)
        with open(config_filename, 'r') as inp:
            cfg = ConfigParser.ConfigParser()
            cfg.readfp(inp)
            for name in cfg.sections():
                self._logger.debug('  name: %s', name)
                for allocator in self._allocators:
                    if allocator.name == name:
                        self._logger.debug('        existing allocator')
                        allocator.configure(cfg)
                        break
                else:
                    if not cfg.has_option(name, 'classname'):
                        self._logger.debug('        skipping %s', name)
                        continue

                    classname = cfg.get(name, 'classname')
                    self._logger.debug('    classname: %s', classname)
                    mod_name, _, cls_name = classname.rpartition('.')
                    try:
                        __import__(mod_name)
                    except ImportError as exc:
                        raise RuntimeError("RAM configure %s: can't import %r: %s"
                                           % (name, mod_name, exc))
                    module = sys.modules[mod_name]
                    if not hasattr(module, cls_name):
                        raise RuntimeError('RAM configure %s: no class %r in %s'
                                           % (name, cls_name, mod_name))
                    cls = getattr(module, cls_name)
                    allocator = cls(name)
                    allocator.configure(cfg)
                    self._allocators.append(allocator)

    @staticmethod
    def _get_instance(config_filename=None):
        """ Return singleton instance. """
        # May need to release forked lock.
        if ResourceAllocationManager._lock_pid != os.getpid(): # pragma no cover
            try:
                ResourceAllocationManager._lock.release()
            except threading.ThreadError:
                pass  # Not locked.
            else:
                logging.warning('RAM: released forked lock.')
            ResourceAllocationManager._lock_pid = os.getpid()

        with ResourceAllocationManager._lock:
            ram = ResourceAllocationManager._RAM
            if ram is None:
                ram = ResourceAllocationManager._RAM = \
                          ResourceAllocationManager(config_filename)
            elif ram._pid != os.getpid():  # pragma no cover
                # We're a copy from a fork.
                for allocator in ram._allocators:
                    allocator.invalidate()
                ram = ResourceAllocationManager._RAM = \
                          ResourceAllocationManager(config_filename)
            return ram

    @staticmethod
    def add_allocator(allocator):
        """
        Add an allocator to the list of resource allocators.

        allocator: ResourceAllocator
            The allocator to be added.
        """
        ram = ResourceAllocationManager._get_instance()
        with ResourceAllocationManager._lock:
            name = allocator.name
            for alloc in ram._allocators:
                if alloc.name == name:
                    raise RuntimeError('allocator named %r already exists' % name)
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
        ram = ResourceAllocationManager._get_instance()
        with ResourceAllocationManager._lock:
            name = allocator.name
            for alloc in ram._allocators:
                if alloc.name == name:
                    raise RuntimeError('allocator named %r already exists' % name)
            ram._allocators.insert(index, allocator)

    @staticmethod
    def get_allocator(selector):
        """
        Return allocator at `selector` or whose name is `selector`.

        selector: int or string
            List index or name of allocator to be returned.
        """
        ram = ResourceAllocationManager._get_instance()
        with ResourceAllocationManager._lock:
            if isinstance(selector, basestring):
                for allocator in ram._allocators:
                    if allocator.name == selector:
                        return allocator
                raise ValueError('allocator %r not found' % selector)
            else:
                return ram._allocators[selector]

    @staticmethod
    def remove_allocator(selector):
        """
        Remove allocator at `selector` or whose name is `selector`.

        selector: int or string
            List index or name of allocator to be removed.
        """
        ram = ResourceAllocationManager._get_instance()
        with ResourceAllocationManager._lock:
            if isinstance(selector, basestring):
                for i, allocator in enumerate(ram._allocators):
                    if allocator.name == selector:
                        return ram._allocators.pop(i)
                raise ValueError('allocator %r not found' % selector)
            else:
                return ram._allocators.pop(selector)

    @staticmethod
    def list_allocators():
        """ Return list of allocators. """
        ram = ResourceAllocationManager._get_instance()
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
        ResourceAllocationManager.validate_resources(resource_desc)
        ram = ResourceAllocationManager._get_instance()
        with ResourceAllocationManager._lock:
            return ram._max_servers(resource_desc)

    def _max_servers(self, resource_desc):
        """ Return total of each allocator's max servers. """
        total = 0
        for allocator in self._allocators:
            count, criteria = allocator.max_servers(resource_desc)
            if count <= 0:
                keys = criteria.keys()
                if keys:
                    info = ': key %r: %s' % (keys[0], criteria[keys[0]])
                else:
                    info = ''  # Don't die on an empty dictionary.
                self._logger.debug('%r incompatible%s', allocator.name, info)
            else:
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
        ResourceAllocationManager.validate_resources(resource_desc)
        ram = ResourceAllocationManager._get_instance()
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
        ResourceAllocationManager.validate_resources(resource_desc)
        ram = ResourceAllocationManager._get_instance()
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
            if estimate == -2:
                key = criteria.keys()[0]
                info = criteria[key]
                self._logger.debug('%r incompatible: key %r: %s',
                                   allocator.name, key, info)
            else:
                msg = 'OK' if estimate == 0 else 'returned %g' % estimate
                self._logger.debug('%r %s', allocator.name, msg)

            if (best_estimate == -2 and estimate >= -1) or \
               (best_estimate == 0  and estimate >  0) or \
               (best_estimate >  0  and estimate < best_estimate):
                # All current allocators support 'hostnames'.
                if estimate >= 0 and need_hostnames \
                   and not 'hostnames' in criteria:  #pragma no cover
                    self._logger.debug("%r is missing 'hostnames'",
                                       allocator.name)
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
        ram = ResourceAllocationManager._get_instance()
        # Lock in _release() so we don't keep the lock unnecessarily.
        return ram._release(server)

    def _release(self, server):
        """ Release a server (proxy). """
        with ResourceAllocationManager._lock:
            try:
                allocator, server, server_info = self._deployed_servers[id(server)]
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

    @staticmethod
    def add_remotes(server, prefix=''):
        """
        Add allocators from a remote server to the list of resource allocators.

        server: proxy for a remote server
            The server whose allocators are to be added.
            It must support :meth:`get_ram`, which should return the server's
            `ResourceAllocationManager` and a `host` attribute.

        prefix: string
            Prefix for the local names of the remote allocators.
            The default is the remote hostname.
        """
        remote_ram = server.get_ram()
        total = remote_ram.get_total_allocators()
        if not prefix:
            prefix = ResourceAllocationManager._make_prefix(server.host)
        proxies = []
        for i in range(total):
            allocator = remote_ram.get_allocator_proxy(i)
            proxy = RemoteAllocator('%s_%s' % (prefix, allocator.name),
                                    allocator)
            proxies.append(proxy)
        ram = ResourceAllocationManager._get_instance()
        with ResourceAllocationManager._lock:
            ram._allocators.extend(proxies)

    @staticmethod
    def _make_prefix(hostid):
        """ Return legal prefix based on `hostid`. """
        if _IPV4_HOST.match(hostid):  # Use all digits to be unique.
            prefix = hostid.replace('.', '')
        else:  # IP hostname (letters, digits, and hyphen are legal).
            prefix, _, rest = hostid.partition('.')
            prefix = prefix.replace('-', '')
        return prefix

    @rbac('*')
    def get_total_allocators(self):
        """ Return number of allocators for remote use. """
        return len(self._allocators)

    @rbac('*', proxy_types=[object])
    def get_allocator_proxy(self, index):
        """
        Return allocator for remote use.

        index: int
            Index of the allocator to return.
        """
        return self._allocators[index]

    @staticmethod
    def max_request(assembly):
        """
        Determine the maximum resources requested by `assembly`.

        Resource descriptions are assumed to be attributes named `resources`.
        Scans the assembly's components for resources and determines a 'peak'
        value of requested CPUs and resource limits. This can be used to
        ensure an allocated server can support the maximum of any resources
        requested by an assembly's components.

        Returns a resource description for the maximum.

        assembly: :class:`Assembly`
            Assembly containing components requesting resources.
        """
        req = {}
        for path, obj in assembly.items(recurse=True):
            prefix, _, name = path.rpartition('.')
            if name == 'resources' and isinstance(obj, dict):
                req = ResourceAllocationManager._max_request(req, obj)
        return req

    @staticmethod
    def _max_request(base, new):
        """ Helper for :meth:`max_request`. """
        req = base.copy()
        for item in ('min_cpus', 'max_cpus', 'min_phys_memory'):
            if item in new:
                req[item] = max(req.get(item, 0), new[item])
        if req.get('min_cpus', 0) > req.get('max_cpus', 0):
            req['max_cpus'] = req['min_cpus']

        if 'resource_limits' in new:
            new_limits = new['resource_limits']
            req_limits = base.get('resource_limits', {}).copy()
            for item in ('core_file_size', 'data_seg_size', 'file_size',
                         'open_files', 'stack_size', 'virtual_memory',
                         'cpu_time', 'wallclock_time'):
                if item in new_limits:
                    req_limits[item] = max(req_limits.get(item, 0),
                                           new_limits[item])
            req['resource_limits'] = req_limits
        return req

    @staticmethod
    def total_request(assembly):
        """
        Determine the total resources requested by `assembly`.

        Resource descriptions are assumed to be attributes named ``resources``.
        Scans the assembly's components for resources and determines a total
        value of requested run time.  The descriptions are first processed by
        :meth:`max_request` to set peak values.  Then the descriptions are
        also checked for queuing compatibility (accounting id, job category,
        etc).  This can be used to obtain a resource description for running
        `assembly` as a batch job.

        Returns a resource description for the total.

        assembly: :class:`Assembly`
            Assembly containing components requesting resources.
        """
        req = {}
        for path, obj in assembly.items(recurse=True):
            prefix, _, name = path.rpartition('.')
            if name == 'resources' and isinstance(obj, dict):
                req = ResourceAllocationManager._total_request(req, obj)
        return req

    @staticmethod
    def _total_request(base, new):
        """ Helper for :meth:`total_request`. """
        req = ResourceAllocationManager._max_request(base, new)

        # Rerunnable only if all are rerunnable.
        if 'rerunnable' in new:
            req['rerunnable'] = base.get('rerunnable', True) and new['rerunnable']

        # If specified, these should match.
        for item in ('accounting_id', 'queue_name', 'job_category', 'localhost'):
            if item in new:
                if item in base:
                    if new[item] != base[item]:
                        raise ValueError('Incompatible settings for %r: %r vs. %r'
                                         % (item, new[item], base[item]))
                else:
                    req[item] = new[item]

        # Total time requested.
        if 'resource_limits' in new:
            req_limits = req['resource_limits']
            new_limits = new['resource_limits']
            base_limits = base.get('resource_limits', {})
            for item in ('cpu_time', 'wallclock_time'):
                if item in new_limits:
                    req_limits[item] = base_limits.get(item, 0) + new_limits[item]
            req['resource_limits'] = req_limits

        return req

    @staticmethod
    def validate_resources(resource_desc):
        """
        Validate that `resource_desc` is legal.

        resource_desc: dict
            Description of required resources.
        """
        for key, value in resource_desc.items():
            try:
                if not _VALIDATORS[key](value):
                    raise ValueError('Invalid resource value for %r: %r'
                                     % (key, value))
            except KeyError:
                raise KeyError('Invalid resource key %r' % key)

        if 'max_cpus' in resource_desc:
            if 'min_cpus' not in resource_desc:
                raise KeyError('min_cpus required if max_cpus specified')
            min_cpus = resource_desc['min_cpus']
            max_cpus = resource_desc['max_cpus']
            if max_cpus < min_cpus:
                raise ValueError('max_cpus %d < min_cpus %d'
                                 % (max_cpus, min_cpus))

def _true(value):
    """ Just returns True -- these registered keys need more work. """
    return True

def _bool(value):
    """ Validate bool key value. """
    return isinstance(value, bool)

def _datetime(value):
    """ Validate datetime key value. """
    return isinstance(value, datetime.datetime)

def _int(value):
    """ Validate int key value. """
    return isinstance(value, int)

def _positive(value):
    """ Validate positive key value. """
    return isinstance(value, int) and value > 0

def _string(value):
    """ Validate string key value. """
    return isinstance(value, basestring)

def _no_whitespace(value):
    """ Validate no_whitespace key value. """
    return isinstance(value, basestring) and len(value.split(' /t/n')) == 1

def _stringlist(value):
    """ Validate sequence of strings value. """
    if not isinstance(value, (list, tuple)):
        return False
    for item in value:
        if not isinstance(item, basestring):
            return False
    return True

def _allocator(value):
    """ Validate 'allocator' key value. """
    for allocator in ResourceAllocationManager.list_allocators():
        if allocator.name == value:
            return True
    return False

def _job_environment(value):
    """ Validate 'job_environment' key value. """
    if not isinstance(value, dict):
        return False
    for key, val in value.items():
        if not isinstance(key, basestring) or len(key.split()) > 1:
            return False
        if not isinstance(val, basestring):
            return False
    return True

def _job_category(value):
    """ Validate 'job_category' key value. """
    return value in JOB_CATEGORIES

def _resource_limits(value):
    """ Validate 'resource_limits' key value. """
    if not isinstance(value, dict):
        return False
    for key, val in value.items():
        if key not in RESOURCE_LIMITS:
            return False
        if not isinstance(val, int):
            return False
        if val < 0:
            return False
    return True

# Registry of resource validators.
_VALIDATORS = {'allocator': _allocator,
               'localhost': _bool,
               'exclude': _stringlist,
               'required_distributions': _true,
               'orphan_modules': _stringlist,
               'python_version': _true,
               'python_platform': _true,

               'min_cpus': _positive,
               'max_cpus': _positive,
               'min_phys_memory': _positive,

               'remote_command': _no_whitespace,
               'args': _stringlist,
               'submit_as_hold': _bool,
               'rerunnable': _bool,
               'job_environment': _job_environment,
               'working_directory': _string,
               'job_category': _job_category,
               'email': _stringlist,
               'email_on_started': _bool,
               'email_on_terminated': _bool,
               'job_name': _string,
               'input_path': _string,
               'output_path': _string,
               'error_path': _string,
               'join_files': _bool,
               'reservation_id': _no_whitespace,
               'queue_name': _no_whitespace,
               'priority': _int,
               'start_time': _datetime,
               'deadline_time': _datetime,
               'resource_limits': _resource_limits,
               'accounting_id': _no_whitespace,
               'native_specification': _stringlist}


class ResourceAllocator(object):
    """
    Base class for allocators. Allocators estimate the suitability of a
    resource and can deploy on that resource.

    name: string
        Name of allocator; used in log messages, etc.
        Must be alphanumeric (underscore also allowed).
    """

    def __init__(self, name):
        match = _LEGAL_NAME.match(name)
        if match is None:
            raise NameError('name %r is not alphanumeric' % name)
        self._name = name
        self._logger = logging.getLogger(name)

    @property
    def name(self):
        """ This allocator's name. """
        return self._name

    def invalidate(self):
        """
        Invalidate this allocator. This will be called by the manager when
        it detects that its allocators are copies due to a process fork.
        The default implementation does nothing.
        """
        return

    # To be implemented by real allocator.
    def configure(self, cfg):  #pragma no cover
        """
        Configure allocator from :class:`ConfigParser` instance.
        Normally only called during manager initialization.

        cfg: :class:`ConfigParser`
            Configuration data is located under the section matching
            this allocator's `name`.

        The default implementation does nothing.
        """
        return

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
        raise NotImplementedError('max_servers')

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
        raise NotImplementedError('time_estimate')

    def check_compatibility(self, resource_desc):
        """
        Check compatibility with common resource attributes.

        resource_desc: dict
            Description of required resources.

        Returns ``(retcode, info)``.  If `retcode` is zero, then `info`
        is a list of keys in `recource_desc` that have not been processed.
        Otherwise, `retcode` will be -2 and `info` will be a single-entry
        dictionary whose key is the incompatible key in `resource_desc`
        and whose value provides data regarding the incompatibility.
        """
        keys = []
        for key, value in resource_desc.items():
            if key in QUEUING_SYSTEM_KEYS:
                pass
            elif key == 'required_distributions':
                missing = self.check_required_distributions(value)
                if missing:
                    return (-2, {key: 'missing %s' % missing})
            elif key == 'orphan_modules':
                missing = self.check_orphan_modules(value)
                if missing:
                    return (-2, {key: 'missing %s' % missing})
            elif key == 'python_version':
                # Require major match, minor request <= system.
                req_ver = float(value)
                sys_ver = float(sys.version[:3])
                if int(sys_ver) != int(req_ver) or \
                   sys_ver < req_ver:
                    return (-2, {key : 'want %s, have %s' % (req_ver, sys_ver)})
            elif key == 'exclude':
                if socket.gethostname() in value:
                    return (-2, {key : 'excluded host %s' % socket.gethostname()})
            elif key == 'allocator':
                if self.name != value:
                    return (-2, {key : 'wrong allocator'})
            else:
                keys.append(key)
        return (0, keys)

    def check_required_distributions(self, resource_value):
        """
        Returns a list of distributions that are not available.

        resource_value: list
            List of Distributions or Requirements.
        """
        required = []
        for item in resource_value:
            if isinstance(item, pkg_resources.Distribution):
                required.append(item.as_requirement())
            else:
                required.append(item)
        return check_requirements(sorted(required))

    def check_orphan_modules(self, resource_value):
        """
        Returns a list of 'orphan' modules that are not available.

        resource_value: list
            List of 'orphan' module names.
        """
#FIXME: shouldn't pollute the environment like this does.
        not_found = []
        for module in sorted(resource_value):
            if module:
                try:
                    __import__(module)
                except ImportError:
                    not_found.append(module)
        return not_found

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
        raise NotImplementedError('deploy')

    # To be implemented by real allocator.
    def release(self, server):  #pragma no cover
        """
        Shut-down `server`.

        .. note::

            Unlike other methods which are protected from multithreaded
            access by the manager, :meth:`release` must be multithread-safe.

        server: :class:`ObjServer`
            Server to be shut down.
        """
        raise NotImplementedError('release')


class FactoryAllocator(ResourceAllocator):
    """
    Base class for allocators using :class:`ObjServerFactory`.

    name: string
        Name of allocator; used in log messages, etc.

    authkey: string
        Authorization key for this allocator and any deployed servers.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Use with caution!
    """
    def __init__(self, name, authkey=None, allow_shell=False):
        super(FactoryAllocator, self).__init__(name)
        self._deployed_servers = []

        if authkey is None:
            authkey = multiprocessing.current_process().authkey
            if authkey is None:
                authkey = 'PublicKey'
                multiprocessing.current_process().authkey = authkey
        self.factory = ObjServerFactory(name, authkey, allow_shell)

    def configure(self, cfg):
        """
        Configure allocator from :class:`ConfigParser` instance.
        Normally only called during manager initialization.

        cfg: :class:`ConfigParser`
            Configuration data is located under the section matching
            this allocator's `name`.

        Allows modifying `auth_key` and `allow_shell`.
        """
        if cfg.has_option(self.name, 'authkey'):
            value = cfg.get(self.name, 'authkey')
            self._logger.debug('    authkey: %s', value)
            self.factory._authkey = value

        if cfg.has_option(self.name, 'allow_shell'):
            value = cfg.getboolean(self.name, 'allow_shell')
            self._logger.debug('    allow_shell: %s', value)
            self.factory._allow_shell = value

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
            server = self.factory.create(typname='', name=name,
                                         res_desc=resource_desc,
                                         allowed_users=allowed_users)
        # Shouldn't happen...
        except Exception:  #pragma no cover
            self._logger.exception('create failed:')
            return None

        self._deployed_servers.append(server)
        return server

    @rbac(('owner', 'user'))
    def release(self, server):
        """
        Release `server`.

        server: typically :class:`ObjServer`
            Previously deployed server to be shut down.
        """
        self.factory.release(server)
        self._deployed_servers.remove(server)


class LocalAllocator(FactoryAllocator):
    """
    Purely local resource allocator.

    name: string
        Name of allocator; used in log messages, etc.

    total_cpus: int
        If >0, then that is taken as the number of cpus/cores available.
        Otherwise, the number is taken from :meth:`multiprocessing.cpu_count`.

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

    server_limit: int
        If 0, limit :meth:`max_servers` result to `total_cpus`.
        If >0, limit to `server_limit`, if <0, no limit to normal calculation.

    The `max_load` argument is typically used to work around limitations
    on the number of servers based on system load.  The `server_limit`
    argument can be used to avoid overcommiting memory, especially if
    `max_load` has been used to reduce the effect of system load on server
    count.

    Resource configuration file entry equivalent to the default
    `LocalHost` allocator::

        [LocalHost]
        classname: openmdao.main.resource.LocalAllocator
        total_cpus: 1
        max_load: 1.0
        server_limit: 0
        authkey: PublicKey
        allow_shell: True

    """

    def __init__(self, name='LocalAllocator', total_cpus=0, max_load=1.0,
                 authkey=None, allow_shell=False, server_limit=0):
        super(LocalAllocator, self).__init__(name, authkey, allow_shell)
        if total_cpus > 0:
            self.total_cpus = total_cpus
        else:
            try:
                self.total_cpus = multiprocessing.cpu_count()
            # Just being defensive (according to docs this could happen).
            except NotImplementedError:  # pragma no cover
                self.total_cpus = 1
        if max_load > 0.:
            self.max_load = max_load
        else:
            raise ValueError('%s: max_load must be > 0, got %g'
                             % (name, max_load))
        self.server_limit = server_limit

    @property
    def host(self):
        """ Allocator hostname. """
        return self.factory.host

    @property
    def pid(self):
        """ Allocator process ID. """
        return self.factory.pid

    def configure(self, cfg):
        """
        Configure allocator from :class:`ConfigParser` instance.
        Normally only called during manager initialization.

        cfg: :class:`ConfigParser`
            Configuration data is located under the section matching
            this allocator's `name`.

        Allows modifying factory options, `total_cpus`, and `max_load`.
        """
        super(LocalAllocator, self).configure(cfg)

        if cfg.has_option(self.name, 'total_cpus'):
            value = cfg.getint(self.name, 'total_cpus')
            self._logger.debug('    total_cpus: %s', value)
            if value > 0:
                self.total_cpus = value
            else:
                raise ValueError('%s: total_cpus must be > 0, got %d'
                                 % (self.name, value))

        if cfg.has_option(self.name, 'max_load'):
            value = cfg.getfloat(self.name, 'max_load')
            self._logger.debug('    max_load: %s', value)
            if value > 0.:
                self.max_load = value
            else:
                raise ValueError('%s: max_load must be > 0, got %g'
                                 % (self.name, value))

        if cfg.has_option(self.name, 'server_limit'):
            value = cfg.getint(self.name, 'server_limit')
            self._logger.debug('    server_limit: %s', value)
            self.server_limit = value

    @rbac('*')
    def max_servers(self, resource_desc, load_adjusted=False):
        """
        Returns `total_cpus` * `max_load` if `resource_desc` is supported;
        otherwise, zero. The return value may be limited by `server_limit`.

        resource_desc: dict
            Description of required resources.

        load_adjusted: bool
            If True, then the returned number of servers is adjusted for
            current host loading.
        """
        retcode, info = self.check_compatibility(resource_desc)
        if retcode != 0:
            return (0, info)

        avail_cpus = self.total_cpus * self.max_load
        if load_adjusted:  # Check system load.
            try:
                loadavgs = os.getloadavg()
            # Not available on Windows.
            except AttributeError:  #pragma no cover
                pass
            else:
                self._logger.debug('loadavgs %.2f, %.2f, %.2f, max_load %.2f',
                                   loadavgs[0], loadavgs[1], loadavgs[2],
                                   self.max_load * self.total_cpus)
                avail_cpus -= int(loadavgs[0])
        avail_cpus = max(int(avail_cpus), 1)
        if self.server_limit == 0:
            avail_cpus = min(avail_cpus, self.total_cpus)
        elif self.server_limit > 0:
            avail_cpus = min(avail_cpus, self.server_limit)
        # else no special limiting.

        if 'min_cpus' in resource_desc:
            req_cpus = resource_desc['min_cpus']
            if req_cpus > avail_cpus:
                return (0, {'min_cpus': 'want %s, available %s'
                                        % (req_cpus, avail_cpus)})
            else:
                return (avail_cpus / req_cpus, {})
        else:
            return (avail_cpus, {})

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
        retcode, info = self.check_compatibility(resource_desc)
        if retcode != 0:
            return (retcode, info)

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

        self._logger.debug('loadavgs %.2f, %.2f, %.2f, max_load %.2f',
                           loadavgs[0], loadavgs[1], loadavgs[2],
                           self.max_load * self.total_cpus)
        criteria = {
            'hostnames'  : [socket.gethostname()],
            'loadavgs'   : loadavgs,
            'total_cpus' : self.total_cpus,
            'max_load'   : self.max_load
        }
        if (loadavgs[0] / self.total_cpus) < self.max_load:
            return (0, criteria)
        elif len(self._deployed_servers) == 0:
            # Ensure progress by always allowing 1 server.
            return (0, criteria)
        # Tests force max_load high to avoid other issues.
        else:  #pragma no cover
            return (-1, criteria)  # Try again later.

    def check_compatibility(self, resource_desc):
        """
        Check compatibility with resource attributes.

        resource_desc: dict
            Description of required resources.

        Returns ``(retcode, info)``. If Compatible, then `retcode` is zero
        and `info` is empty. Otherwise, `retcode` will be -2 and `info` will
        be a single-entry dictionary whose key is the incompatible key in
        `resource_desc` and whose value provides data regarding the incompatibility.
        """
        retcode, info = \
            super(LocalAllocator, self).check_compatibility(resource_desc)
        if retcode != 0:
            return (retcode, info)

        for key in info:
            value = resource_desc[key]
            if key == 'localhost':
                if not value:
                    return (-2, {key: 'requested remote host'})
            elif key == 'min_cpus':
                if value > self.total_cpus:
                    return (-2, {key: 'want %s, have %s'
                                      % (value, self.total_cpus)})
        return (0, {})

register(LocalAllocator, mp_distributing.Cluster)
register(LocalAllocator, mp_distributing.HostManager)


class RemoteAllocator(ResourceAllocator):
    """
    Allocator which delegates to a remote allocator.
    Configuration of remote allocators is not allowed.

    name: string
        Local name for allocator.

    remote: proxy
        Proxy for remote allocator.
    """

    def __init__(self, name, remote):
        super(RemoteAllocator, self).__init__(name)
        self._lock = threading.Lock()
        self._remote = remote

    @rbac('*')
    def max_servers(self, resource_desc):
        """ Return maximum number of servers for remote allocator. """
        rdesc, info = self._check_local(resource_desc)
        if rdesc is None:
            return (0, info[1])
        return self._remote.max_servers(rdesc)

    @rbac('*')
    def time_estimate(self, resource_desc):
        """ Return the time estimate from the remote allocator. """
        rdesc, info = self._check_local(resource_desc)
        if rdesc is None:
            return info
        return self._remote.time_estimate(rdesc)

    def _check_local(self, resource_desc):
        """ Check locally-relevant resources. """
        rdesc = resource_desc.copy()
        for key in ('localhost', 'allocator'):
            if key not in rdesc:
                continue
            value = rdesc[key]
            if key == 'localhost':
                if value:
                    return None, (-2, {key: 'requested local host'})
            elif key == 'allocator':
                if value != self.name:
                    return None, (-2, {key: 'wrong allocator'})
            del rdesc[key]
        return (rdesc, None)

    @rbac('*')
    def deploy(self, name, resource_desc, criteria):
        """ Deploy on the remote allocator. """
        return self._remote.deploy(name, resource_desc, criteria)

    @rbac(('owner', 'user'))
    def release(self, server):
        """ Release a remotely allocated server. """
        with self._lock:  # Proxies are not thread-safe.
            self._remote.release(server)


# Cluster allocation requires ssh configuration and multiple hosts.
class ClusterAllocator(ResourceAllocator):  #pragma no cover
    """
    Cluster-based resource allocator.  This allocator manages a collection
    of :class:`LocalAllocator`, one for each machine in the cluster.

    name: string
        Name of allocator; used in log messages, etc.

    machines: list(:class:`ClusterHost`)
        Hosts to allocate from.

    authkey: string
        Authorization key to be passed-on to remote servers.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Use with caution!

    method: string
        Must be one of ``load-average``, ``greedy``, or ``round-robin``.

    ``load-average`` uses the load averages reported by each machine's
    local allocator to determine the least-loaded machine(s) and allocates
    on those. We assume that machines in the cluster are similar enough that
    ranking by load average is reasonable.

    ``greedy`` allocates N1 servers from the first machine, then
    N2 from the second machine, etc. where N is the number of cpus on
    a machine.

    ``round-robin`` allocates one server from the first machine,
    then 1 server from the second machine, etc. Note that this can result in
    an unbalanced load if the machines have different numbers of cpus.

    The ``greedy`` and ``round-robin`` methods have lower overhead but may
    allocate a server on a mchine that is already overloaded based on other
    user's activity. They do however avoid problems where load averages don't
    reflect loads added by previous allocations quickly enough.
    """

    _methods = {}  # Selection methods.

    def __init__(self, name, machines=None, authkey=None, allow_shell=False,
                 method='load-average'):
        if method not in self._methods:
            raise ValueError('method argument %r not one of %s'
                             % (method, self._methods.keys()))

        super(ClusterAllocator, self).__init__(name)

        if authkey is None:
            authkey = multiprocessing.current_process().authkey
            if authkey is None:
                authkey = 'PublicKey'
                multiprocessing.current_process().authkey = authkey

        self.cluster = None
        self._authkey = authkey
        self._allow_shell = allow_shell
        self._method = method
        self._lock = threading.Lock()
        self._last_deployed = None
        self._reply_q = Queue.Queue()
        self._deployed_servers = {}

        if machines is not None:
            self._initialize(machines)

    def _initialize(self, machines):
        """ Setup allocators on the given machines. """
        hostnames = set()
        hosts = []
        for i, host in enumerate(machines):
            if not isinstance(host, ClusterHost):
                raise TypeError('Expecting ClusterHost for machine %s, got %r'
                                % (i, host))
            if host.hostname in hostnames:
                self._logger.warning('Ignoring duplicate hostname %r',
                                     host.hostname)
                continue
            hostnames.add(host.hostname)
            host.register(LocalAllocator)
            hosts.append(host)

        self.cluster = mp_distributing.Cluster(hosts, authkey=self._authkey,
                                               allow_shell=self._allow_shell)
        self.cluster.start()
        self._logger.debug('server listening on %r', (self.cluster.address,))

        la_names = set()
        for host in self.cluster:
            manager = host.manager
            la_name = host.netname
            for char in ('-', '.'):
                la_name = la_name.replace(char, '_')

            if la_name in la_names:
                self._logger.warning('skipping duplicate %s', la_name)
            else:
                allocator = \
                    manager.openmdao_main_resource_LocalAllocator(name=la_name,
                                                  allow_shell=self._allow_shell)
                host.allocator = allocator
                self._logger.debug('allocator %r pid %s', la_name, allocator.pid)

    def __getitem__(self, i):
        return self.cluster[i]

    def __iter__(self):
        return iter(self.cluster)

    def __len__(self):
        return len(self.cluster)

    def configure(self, cfg):
        """
        Configure a cluster consisting of hosts with node-numbered hostnames
        all using the same Python executable. Hostnames are generated from
        `origin` to `nhosts`+`origin` from `format` (`origin` defaults to 0).
        The Python executable is specified by the `python` option. It defaults
        to the currently executing Python.

        Resource configuration file entry for a cluster named `HX` consisting
        of 19 hosts with the first host named `hx00` and using the current
        OpenMDAO Python::

            [HX]
            classname: openmdao.main.resource.ClusterAllocator
            nhosts: 19
            origin: 0
            format: hx%02d
            authkey: PublicKey
            allow_shell: True
            method: load-average
            tunnel_incoming: False
            tunnel_outgoing: False
            identity_filename: ~/.ssh/example.pem

        An example entry for an 'ad-hoc' cluster where the names don't have a
        particular pattern or the configuration of hosts is not uniform::

            [Ad_hoc_cluster]
            classname: openmdao.main.resource.ClusterAllocator
            authkey: PublicKey
            allow_shell: True
            method: load-average
            hosts: havoc.grc.nasa.gov viper.grc.nasa.gov

            [havoc.grc.nasa.gov]
            python: /OpenMDAO/dev/setowns1/OpenMDAO-Framework/devenv/bin/python

            [viper.grc.nasa.gov]
            python: OpenMDAO-Framework/devenv/bin/python
            tunnel_incoming: True
            tunnel_outgoing: True

        """
        if cfg.has_option(self.name, 'authkey'):
            self._authkey = cfg.get(self.name, 'authkey')
            self._logger.debug('    authkey: %s', self._authkey)

        if cfg.has_option(self.name, 'allow_shell'):
            self._allow_shell = cfg.getboolean(self.name, 'allow_shell')
            self._logger.debug('    allow_shell: %s', self._allow_shell)

        if cfg.has_option(self.name, 'method'):
            method = cfg.get(self.name, 'method')
            if method not in self._methods:
                self._logger.error('method specification %r not one of %s',
                                   method, self._methods.keys())
            else:
                self._method = method
            self._logger.debug('    method: %s', self._method)

        # ClusterHost arguments.

        if cfg.has_option(self.name, 'python'):
            python = cfg.get(self.name, 'python')
        else:
            python = sys.executable
        self._logger.debug('    python: %s', python)

        if cfg.has_option(self.name, 'tunnel_incoming'):
            tunnel_incoming = cfg.getboolean(self.name, 'tunnel_incoming')
            self._logger.debug('    tunnel_incoming: %s', tunnel_incoming)
        else:
            tunnel_incoming = False

        if cfg.has_option(self.name, 'tunnel_outgoing'):
            tunnel_outgoing = cfg.getboolean(self.name, 'tunnel_outgoing')
            self._logger.debug('    tunnel_outgoing: %s', tunnel_outgoing)
        else:
            tunnel_outgoing = False

        if cfg.has_option(self.name, 'identity_filename'):
            identity_filename = cfg.get(self.name, 'identity_filename')
            identity_filename = os.path.expanduser(identity_filename)
            if os.path.exists(identity_filename):
                self._logger.debug('    identity_filename: %s',
                                   identity_filename)
            else:
                self._logger.error('identity_filename %r not found',
                                   identity_filename)
        else:
            identity_filename = None

        machines = []
        if cfg.has_option(self.name, 'hosts'):
            hostnames = cfg.get(self.name, 'hosts').split()
            for hostname in hostnames:
                _python = python
                _incoming = tunnel_incoming
                _outgoing = tunnel_outgoing
                _identity = identity_filename

                if cfg.has_section(hostname):
                    self._logger.debug('hostname %s:', hostname)

                    if cfg.has_option(hostname, 'python'):
                        _python = cfg.get(hostname, 'python')
                        self._logger.debug('    python: %s', _python)

                    if cfg.has_option(hostname, 'tunnel_incoming'):
                        _incoming = cfg.getboolean(hostname, 'tunnel_incoming')
                        self._logger.debug('    tunnel_incoming: %s', _incoming)

                    if cfg.has_option(hostname, 'tunnel_outgoing'):
                        _outgoing = cfg.getboolean(hostname, 'tunnel_outgoing')
                        self._logger.debug('    tunnel_outgoing: %s', _outgoing)

                    if cfg.has_option(hostname, 'identity_filename'):
                        _identity = cfg.get(hostname, 'identity_filename')
                        _identity = os.path.expanduser(_identity)
                        if os.path.exists(_identity):
                            self._logger.debug('    identity_filename: %s',
                                               _identity)
                        else:
                            self._logger.error('identity_filename %r not found',
                                               _identity)

                machines.append(ClusterHost(hostname=hostname, python=_python,
                                            tunnel_incoming=_incoming,
                                            tunnel_outgoing=_outgoing,
                                            identity_filename=_identity))
        else:
            nhosts = cfg.getint(self.name, 'nhosts')
            self._logger.debug('    nhosts: %s', nhosts)

            if cfg.has_option(self.name, 'origin'):
                origin = cfg.getint(self.name, 'origin')
            else:
                origin = 0
            self._logger.debug('    origin: %s', origin)

            pattern = cfg.get(self.name, 'format')
            self._logger.debug('    format: %s', pattern)

            for i in range(origin, nhosts+origin):
                hostname = pattern % i
                machines.append(ClusterHost(hostname=hostname, python=python,
                                            tunnel_incoming=tunnel_incoming,
                                            tunnel_outgoing=tunnel_outgoing,
                                            identity_filename=identity_filename))
        self._initialize(machines)

    def max_servers(self, resource_desc):
        """
        Returns the total of :meth:`max_servers` across all
        :class:`LocalAllocator` in the cluster.

        resource_desc: dict
            Description of required resources.
        """
        credentials = get_credentials()

        rdesc, info = self._check_local(resource_desc)
        if rdesc is None:
            return (0, info[1])

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
            for i, host in enumerate(self.cluster):
                allocator = host.allocator
                if i < max_workers:
                    worker_q = WorkerPool.get()
                    worker_q.put((self._get_count,
                                  (allocator, rdesc, credentials),
                                  {}, self._reply_q))
                else:
                    todo.append(allocator)

            # Process counts.
            total = 0
            for i in range(len(self.cluster)):
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
                                  (next_allocator, rdesc, credentials),
                                  {}, self._reply_q))
                count = retval
                if count:
                    total += count

            if 'min_cpus' in resource_desc:
                req_cpus = resource_desc['min_cpus']
                if req_cpus > total:
                    return (0, {'min_cpus': 'want %s, total %s'
                                            % (req_cpus, total)})
                else:
                    return (total / req_cpus, {})
            else:
                return (total, {})

    def _get_count(self, allocator, resource_desc, credentials):
        """ Get `max_servers` from an allocator. """
        set_credentials(credentials)
        count = 0
        adjusted = (self._method == 'load-average')
        try:
            count, criteria = allocator.max_servers(resource_desc,
                                                    load_adjusted=adjusted)
        except Exception:
            msg = traceback.format_exc()
            self._logger.error('%r max_servers() caught exception %s',
                               allocator.name, msg)
        else:
            if count < 1:
                self._logger.debug('%s incompatible: %s',
                                   allocator.name, criteria)
        return max(count, 0)

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
        rdesc, info = self._check_local(resource_desc)
        if rdesc is None:
            return info

        return self._methods[self._method](self, rdesc)

    def _check_local(self, resource_desc):
        """ Check locally-relevant resources. """
        rdesc = resource_desc.copy()
        for key in ('localhost', 'allocator'):
            if key not in rdesc:
                continue
            value = rdesc[key]
            if key == 'localhost':
                if value:
                    return None, (-2, {key: 'requested local host'})
            elif key == 'allocator':
                if value != self.name:
                    return None, (-2, {key: 'wrong allocator'})
            del rdesc[key]
        return (rdesc, None)

    def _load_average(self, rdesc):
        """ Time estimate using load averages. """
        credentials = get_credentials()

        min_cpus = rdesc.get('min_cpus', 0)
        if min_cpus:
            # Spread across LocalAllocators.
            rdesc['min_cpus'] = 1

        avail_cpus = 0
        with self._lock:
            best_host = None
            best_estimate = -2
            best_criteria = {'': 'No LocalAllocator results'}

            # Prefer not to repeat use of just-used allocator.
            prev_host = self._last_deployed
            prev_estimate = -2
            prev_criteria = None
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
            for i, host in enumerate(self.cluster):
                if i < max_workers:
                    worker_q = WorkerPool.get()
                    worker_q.put((self._get_estimate,
                                  (host, rdesc, credentials),
                                  {}, self._reply_q))
                else:
                    todo.append(host)

            # Process estimates.
            host_loads = []  # Sorted list of (load, criteria)
            for i in range(len(self.cluster)):
                worker_q, retval, exc, trace = self._reply_q.get()
                if exc:
                    self._logger.error(trace)
                    retval = None

                try:
                    next_host = todo.pop(0)
                except IndexError:
                    WorkerPool.release(worker_q)
                else:
                    worker_q.put((self._get_estimate,
                                  (next_host, rdesc, credentials),
                                  {}, self._reply_q))

                if retval is None:
                    continue
                host, estimate, criteria = retval
                if estimate is None or estimate < -1:
                    continue

                # Accumulate available cpus in cluster.
                avail_cpus += criteria['total_cpus']

                # CPU-adjusted load (if available).
                if 'loadavgs' in criteria:
                    load = criteria['loadavgs'][0] / criteria['total_cpus']
                else:  # Windows
                    load = 0.

                # Insertion sort of host_loads.
                if estimate >= 0 and min_cpus:
                    new_info = (load, criteria)
                    if host_loads:
                        for i, info in enumerate(host_loads):
                            if load < info[0]:
                                host_loads.insert(i, new_info)
                                break
                        else:
                            host_loads.append(new_info)
                    else:
                        host_loads.append(new_info)

                # Update best estimate.
                if host is prev_host:
                    prev_estimate = estimate
                    prev_criteria = criteria
                elif (best_estimate <= 0 and estimate > best_estimate) or \
                     (best_estimate >  0 and estimate < best_estimate):
                    best_host = host
                    best_estimate = estimate
                    best_criteria = criteria
                elif best_estimate == 0 and estimate == 0:
                    if 'loadavgs' in best_criteria:
                        best_load = best_criteria['loadavgs'][0]
                        if load < best_load:
                            best_host = host
                            best_estimate = estimate
                            best_criteria = criteria
                    else:
                        if 'loadavgs' in criteria:  # Prefer non-Windows.
                            best_host = host
                            best_estimate = estimate
                            best_criteria = criteria

            # If no alternative, repeat use of previous host.
            if best_estimate < 0 and prev_estimate >= 0:
                best_host = prev_host
                best_estimate = prev_estimate
                best_criteria = prev_criteria

            if avail_cpus < min_cpus:
                return (-2, {'min_cpus': 'want %d, available %d' \
                                         % (min_cpus, avail_cpus)})

            # Save best host in criteria in case we're asked to deploy.
            if best_host is not None:
                best_criteria['host'] = best_host
                if min_cpus:
                    # Save min_cpus hostnames in criteria.
                    hostnames = []
                    for load, criteria in host_loads:
                        hostname = criteria['hostnames'][0]
                        hostnames.append(hostname)
                        if len(hostnames) >= min_cpus:
                            break
                        total_cpus = criteria['total_cpus']
                        max_load = criteria.get('max_load', 1)
                        load *= total_cpus  # Restore from cpu-adjusted value.
                        max_load *= total_cpus
                        load += 1
                        while load < max_load and len(hostnames) < min_cpus:
                            hostnames.append(hostname)
                            load += 1
                        if len(hostnames) >= min_cpus:
                            break
                    if len(hostnames) < min_cpus:
                        return (-1, {'min_cpus': 'want %d, idle %d' \
                                                 % (min_cpus, len(hostnames))})
                    best_criteria['hostnames'] = hostnames

            return (best_estimate, best_criteria)

    _methods['load-average'] = _load_average

    def _get_estimate(self, host, resource_desc, credentials):
        """ Get (estimate, criteria) from an allocator. """
        set_credentials(credentials)
        try:
            estimate, criteria = host.allocator.time_estimate(resource_desc)
        except Exception:
            msg = traceback.format_exc()
            self._logger.error('%r time_estimate() caught exception %s',
                               host.allocator.name, msg)
            estimate = None
            criteria = None
        else:
            if estimate == 0:
                if 'loadavgs' in criteria:
                    load = '%g' % criteria['loadavgs'][0]
                else:
                    load = 'None'
                self._logger.debug('%r returned %g (%s)', host.allocator.name,
                                   estimate, load)
            else:
                self._logger.debug('%r returned %g', host.allocator.name, estimate)

        return (host, estimate, criteria)

    def _greedy(self, rdesc):
        """ 'time estimate' using greedy selection. """
        # Get total_cpus from each allocator.
        for host in self.cluster:
            if host.total_cpus <= 0:
                count, criteria = host.allocator.max_servers({})
                host.total_cpus = count

        # Select first machine found with an (assumed) idle cpu.
        for host in self.cluster:
            if host.allocated_cpus < host.total_cpus:
                allocation_host = host
                break
        else:  # No idle cpus.
            return (-1, {'min_cpus': 'no idle cpus'})

        # Generate hostnames starting at selected machine.
        hostnames = []
        min_cpus = rdesc.get('min_cpus', 1)
        required = min_cpus
        for host in self.cluster:
            if host.allocated_cpus < host.total_cpus:
                count = host.total_cpus - host.allocated_cpus
                count = min(count, required)
                for i in range(count):
                    hostnames.append(host.netname)
                required -= count
                if required <= 0:
                    break
        else:  # Insufficient idle cpus.
            return (-1, {'min_cpus': 'want %d, idle %d' \
                                     % (min_cpus, len(hostnames))})

        return (0, {'host': allocation_host, 'hostnames': hostnames})

    _methods['greedy'] = _greedy

    def _round_robin(self, rdesc):
        """ 'time estimate' using round-robin selection. """
        # Get total_cpus from each allocator.
        for host in self.cluster:
            if host.total_cpus <= 0:
                count, criteria = host.allocator.max_servers({})
                host.total_cpus = count

        # Select next machine in sequence.
        if self._last_deployed:
            for i, host in enumerate(self.cluster):
                if host is self._last_deployed:
                    i = (i + 1) % len(self.cluster)
                    break
        else:
            i = 0

        for j in range(len(self.cluster)):
            host = self.cluster[i]
            if host.allocated_cpus < host.total_cpus:
                allocation_host = host
                break
            i = (i + 1) % len(self.cluster)
        else:  # No idle cpus.
            return (-1, {'min_cpus': 'no idle cpus'})

        # Generate hostnames starting at selected machine.
        hostnames = []
        min_cpus = rdesc.get('min_cpus', 1)
        required = min_cpus
        host_added = True
        while host_added and required > 0:
            host_added = False
            for j in range(len(self.cluster)):
                if host.allocated_cpus < host.total_cpus:
                    hostnames.append(host.netname)
                    host_added = True
                    required -= 1
                    if required <= 0:
                        break
                i = (i + 1) % len(self.cluster)
                host = self.cluster[i]

        if required > 0:  # Insufficient idle cpus.
            return (-1, {'min_cpus': 'want %d, idle %d' \
                                     % (min_cpus, len(hostnames))})

        return (0, {'host': allocation_host, 'hostnames': hostnames})

    _methods['round-robin'] = _round_robin

    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Uses the host saved in `criteria`.
        Returns a proxy to the deployed server.

        name: string
            Name for server.

        resource_desc: dict
            Description of required resources.

        criteria: dict
            The dictionary returned by :meth:`time_estimate`.
        """
        with self._lock:
            host = criteria['host']
            host.allocated_cpus += 1
            self._last_deployed = host
            del criteria['host']  # Don't pass a proxy without a server!
        self._logger.debug('deploying on %r as %r', host.allocator.name, name)
        try:
            server = host.allocator.deploy(name, resource_desc, criteria)
        except Exception as exc:
            self._logger.error('%r deploy() failed for %s: %r',
                               host.allocator.name, name, exc)
            return None

        if server is None:
            self._logger.error('%r deployment failed for %s',
                               host.allocator.name, name)
        else:
            self._deployed_servers[id(server)] = (host, server)
        return server

    def release(self, server):
        """
        Release a server (proxy).

        server: :class:`OpenMDAO_Proxy`
            Server to be released.
        """
        with self._lock:
            try:
                host = self._deployed_servers[id(server)][0]
            except KeyError:
                self._logger.error('server %r not found', server)
                return
            del self._deployed_servers[id(server)]

        host.allocated_cpus -= 1
        try:
            host.allocator.release(server)
        except Exception as exc:
            self._logger.error("Can't release %r: %r", server, exc)
        server._close.cancel()

    def shutdown(self):
        """ Shutdown, releasing resources. """
        if self.cluster is not None:
            self.cluster.shutdown()
            self.cluster = None


# Cluster allocation requires ssh configuration and multiple hosts.
class ClusterHost(mp_distributing.Host):  #pragma no cover
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
        super(ClusterHost, self).__init__(hostname, python, tunnel_incoming,
                                          tunnel_outgoing, identity_filename)
        self.netname = self.hostname.split('@')[1]
        self.allocator = None
        self.total_cpus = 0
        self.allocated_cpus = 0

