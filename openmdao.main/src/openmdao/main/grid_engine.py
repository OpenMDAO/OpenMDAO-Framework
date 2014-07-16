"""
.. _`grid_engine.py`:

GridEngine resource allocator and object server.

By adding the allocator to the resource allocation manager, resource requests
will interrogate the allocator to see if it could be used. This would typically
be done by :class:`ExternalCode` to execute a compute-intensive or parallel
application.
"""

import fnmatch
import os.path
import string

from openmdao.main.mp_support import OpenMDAO_Manager, register
from openmdao.main.objserverfactory import ObjServer
from openmdao.main.rbac import rbac
from openmdao.main.resource import FactoryAllocator, JOB_CATEGORIES, \
                                   HOME_DIRECTORY, WORKING_DIRECTORY

from openmdao.util.shellproc import ShellProc, STDOUT, PIPE, DEV_NULL

# Translate illegal job name characters.
_XLATE = string.maketrans(' \n\t\r/:@\\*?', '__________')


class GridEngineAllocator(FactoryAllocator):
    """
    Knows about GridEngine cluster resources (via `qhost`).
    Uses :class:`GridEngineServer` instead of :class:`ObjServer` when deploying.

    name: string
        Name of allocator; used in log messages, etc.

    pattern: string
        :mod:`fnmatch`-style pattern used to select hosts from `qhost` output.

    authkey: string
        Authorization key for this allocator and any deployed servers.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Since :meth:`execute_command` is required, this
        is defaulted to be True.

    .. warning::

        There is a security risk with `allow_shell` True. Be careful to limit
        factory servers to the intended set of users!

    Resource configuration file entry equivalent to defaults::

        [GridEngine]
        classname: openmdao.main.grid_engine.GridEngineAllocator
        pattern: *
        authkey: PublicKey
        allow_shell: True
        MPICH2: mpich
        OpenMPI: ompi

    The last two entries provide a mapping between DRMAA job category names
    and the configured GridEngine parallel environment names.  Additional
    categories may be configured, and the above configuration is site-specific.
    """

    _QHOST = ['qhost']  # Replaced with path to fake for testing.

    def __init__(self, name='GridEngine', pattern='*', authkey=None,
                 allow_shell=True):
        super(GridEngineAllocator, self).__init__(name, authkey, allow_shell)
        self.factory.manager_class = _ServerManager
        self.factory.server_classname = \
            'grid_engine_grid_engine_GridEngineServer'
        self.pattern = pattern
        self.category_map = {}

    def configure(self, cfg):
        """
        Configure allocator from :class:`ConfigParser` instance.
        Normally only called during manager initialization.

        cfg: :class:`ConfigParser`
            Configuration data is located under the section matching
            this allocator's `name`.

        Allows modifying factory options, `pattern` and the job
        category map.
        """
        super(GridEngineAllocator, self).configure(cfg)
        if cfg.has_option(self.name, 'pattern'):
            self.pattern = cfg.get(self.name, 'pattern')
            self._logger.debug('    pattern: %s', self.pattern)
        for category in JOB_CATEGORIES:
            if cfg.has_option(self.name, category):
                parallel_environment = cfg.get(self.name, category)
                self.category_map[category] = parallel_environment
                self._logger.debug('    %s: %s', category, parallel_environment)

    @rbac('*')
    def max_servers(self, resource_desc):
        """
        Return the maximum number of servers which could be deployed for
        `resource_desc`.  The value needn't be exact, but performance may
        suffer if it overestimates.  The value is used to limit the number
        of concurrent evaluations.

        resource_desc: dict
            Description of required resources.
        """
        retcode, info = self.check_compatibility(resource_desc)
        if retcode != 0:
            return (0, info)
        avail_cpus = len(self._get_hosts())
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
        retcode, info = self.check_compatibility(resource_desc)
        if retcode != 0:
            return (retcode, info)

        hostnames = self._get_hosts()
        if not hostnames:
            return (-2, {'hostnames': 'no hosts available'})

        if 'min_cpus' in resource_desc:
            value = resource_desc['min_cpus']
            if len(hostnames) < value:
                return (-2, {'min_cpus': 'want %s, have %s'
                                          % (value, len(hostnames))})
        criteria = {
            'hostnames'  : hostnames,
            'total_cpus' : len(hostnames),
        }
        return (0, criteria)

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
            super(GridEngineAllocator, self).check_compatibility(resource_desc)
        if retcode != 0:
            return (retcode, info)

        for key in info:
            value = resource_desc[key]
            if key == 'localhost':
                if value:
                    return (-2, {key: 'requested local host'})
            elif key == 'min_cpus' or key == 'max_cpus':
                pass  # Handle in upper layer.
            else:
                return (-2, {key: 'unrecognized key'})
        return (0, {})

    def _get_hosts(self):
        """ Return list of hostnames sorted by load. """
        # Get host load information.
        try:
            proc = ShellProc(self._QHOST, stdout=PIPE)
        except Exception as exc:
            self._logger.error('%r failed: %s' % (self._QHOST, exc))
            return []
        lines = proc.stdout.readlines()

        # Reduce to hosts we're interested in and sort by CPU-adjusted load.
        loads = []
        for line in lines:
            if line.startswith(('HOSTNAME', '-')):
                continue
            hostname, arch, ncpu, load, \
                memtot, memuse, swapto, swapus = line.split()
            if self.pattern:
                if not fnmatch.fnmatchcase(hostname, self.pattern):
                    continue
            try:
                load = float(load)
                ncpu = int(ncpu)
            except ValueError:
                continue
            loads.append((hostname, load / ncpu, ncpu))
        loads = sorted(loads, key=lambda item: item[1])

        # Return list of hostnames.
        hosts = []
        for hostname, load, ncpu in loads:
            for i in range(ncpu):
                hosts.append(hostname)
        return hosts

    @rbac('*')
    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Returns a proxy to the deployed server.
        Overrides superclass to pass `category_map` to server.

        name: string
            Name for server.

        resource_desc: dict
            Description of required resources.

        criteria: dict
            The dictionary returned by :meth:`time_estimate`.
        """
        server = super(GridEngineAllocator, self).deploy(name, resource_desc,
                                                         criteria)
        if server is not None:
            server.configure(self.category_map)
        return server


class GridEngineServer(ObjServer):
    """ Knows about executing a command via `qsub`. """

    _QSUB = ['qsub']  # Replaced with path to fake for testing.

    @rbac('owner')
    def configure(self, category_map):
        """
        Configure parallel environment category map.

        category_map: dict
            Maps from 'job_category' to parallel environment name.
        """
        self.category_map = category_map

    @rbac('owner')
    def execute_command(self, resource_desc):
        """
        Submit command based on `resource_desc`.

        resource_desc: dict
            Description of command and required resources.

        The '-V' `qsub` option is always used to export the current environment
        to the job. This environment is first updated with any 'job_environment'
        data. The '-sync yes' `qsub` option is used to wait for job completion.

        Other job resource keys are processed as follows:

        ========================= =========================
        Resource Key              Translation
        ========================= =========================
        ``submit_as_hold``        -h
        ------------------------- -------------------------
        ``rerunnable``            -r yes|no
        ------------------------- -------------------------
        ``working_directory``     Ignored
        ------------------------- -------------------------
        ``job_category``          Sets parallel environment
        ------------------------- -------------------------
        ``min_cpus``              Sets parallel environment
        ------------------------- -------------------------
        ``max_cpus``              Sets parallel environment
        ------------------------- -------------------------
        ``min_phys_memory``       Ignored
        ------------------------- -------------------------
        ``email``                 -M `value`
        ------------------------- -------------------------
        ``email_on_started``      -m b
        ------------------------- -------------------------
        ``email_on_terminated``   -m e
        ------------------------- -------------------------
        ``job_name``              -N `value`
        ------------------------- -------------------------
        ``input_path``            -i `value`
        ------------------------- -------------------------
        ``output_path``           -o `value`
        ------------------------- -------------------------
        ``error_path``            -e `value`
        ------------------------- -------------------------
        ``join_files``            -j yes|no
        ------------------------- -------------------------
        ``reservation_id``        -ar `value`
        ------------------------- -------------------------
        ``queue_name``            -q `value`
        ------------------------- -------------------------
        ``priority``              -p `value`
        ------------------------- -------------------------
        ``start_time``            -a `value`
        ------------------------- -------------------------
        ``deadline_time``         Ignored
        ------------------------- -------------------------
        ``accounting_id``         -A `value`
        ========================= =========================

        Where `value` is the corresponding resource value.

        The 'working_directory' key is ignored since the server has been
        started in this directory.  ``-cwd`` is used in the `qsub` command.

        If 'input_path' is not specified, add ``-i /dev/null``.
        If 'output_path' is not specified, add ``-o <remote_command>.stdout``.
        If 'error_path' is not specified, add ``-j yes``.

        If 'native_specification' is specified, it is added to the `qsub`
        command just before 'remote_command' and 'args'.

        If specified, 'job_category' is used to index into the category
        map set up during allocator configuration.  The mapped category
        name as well as the 'min_cpus' and 'max_cpus' values are used
        with the ``-pe`` qsub option.

        Some resource limits are also handled:

        ==================== =========================
        Resource Key         Translation
        ==================== =========================
        ``core_file_size``   Ignored
        -------------------- -------------------------
        ``data_seg_size``    Ignored
        -------------------- -------------------------
        ``file_size``        Ignored
        -------------------- -------------------------
        ``open_files``       Ignored
        -------------------- -------------------------
        ``stack_size``       Ignored
        -------------------- -------------------------
        ``virtual_memory``   Ignored
        -------------------- -------------------------
        ``cpu_time``         -l h_cpu= `value`
        -------------------- -------------------------
        ``wallclock_time``   -l h_rt= `value`
        ==================== =========================

        Output from `qsub` itself is routed to ``qsub.out``.
        """
        self.home_dir = os.path.expanduser('~')
        self.work_dir = os.getcwd()  # Server started in working directory.

        cmd = list(self._QSUB)
        cmd.extend(('-V', '-sync', 'yes', '-b', 'yes', '-cwd'))
        env = None
        inp, out, err = None, None, None

        # Process description in fixed, repeatable order.
        keys = ('submit_as_hold',
                'rerunnable',
                'job_environment',
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
                'accounting_id')

        email_events = ''
        for key in keys:
            try:
                value = resource_desc[key]
            except KeyError:
                continue

            if key == 'submit_as_hold':
                if value:
                    cmd.append('-h')
            elif key == 'rerunnable':
                cmd.extend(('-r', 'yes' if value else 'no'))
            elif key == 'job_environment':
                env = value
            elif key == 'email':
                cmd.extend(('-M', ','.join(value)))
            elif key == 'email_on_started':
                email_events += 'b'
            elif key == 'email_on_terminated':
                email_events += 'e'
            elif key == 'job_name':
                if value:
                    cmd.extend(('-N', self._jobname(value)))
            elif key == 'input_path':
                cmd.extend(('-i', self._fix_path(value)))
                inp = value
            elif key == 'output_path':
                cmd.extend(('-o', self._fix_path(value)))
                out = value
            elif key == 'error_path':
                cmd.extend(('-e', self._fix_path(value)))
                err = value
            elif key == 'join_files':
                cmd.extend(('-j', 'yes' if value else 'no'))
                if value:
                    err = 'yes'
            elif key == 'reservation_id':
                cmd.extend(('-ar', value))
            elif key == 'queue_name':
                cmd.extend(('-q', value))
            elif key == 'priority':
                cmd.extend(('-p', str(value)))
            elif key == 'start_time':
                cmd.extend(('-a', value.strftime('%Y%m%d%H%M.%S')))
            elif key == 'accounting_id':
                cmd.extend(('-A', value))

        if email_events:
            cmd.extend(('-m', email_events))

        # Setup parallel environment.
        if 'job_category' in resource_desc:
            job_category = resource_desc['job_category']
            try:
                parallel_environment = self.category_map[job_category]
            except KeyError:
                msg = 'No mapping for job_category %r' % job_category
                self._logger.error(msg)
                raise ValueError(msg)
            min_cpus = resource_desc.get('min_cpus', 1)
            max_cpus = resource_desc.get('max_cpus', min_cpus)
            cmd.extend(('-pe', parallel_environment,
                        '%d-%d' % (min_cpus, max_cpus)))

        # Set resource limits.
        if 'resource_limits' in resource_desc:
            limits = resource_desc['resource_limits']
            if 'cpu_time' in limits:
                cpu_time = limits['cpu_time']
                cmd.extend(('-l', 'h_cpu=%s' % self._timelimit(cpu_time)))
            if 'wallclock_time' in limits:
                wall_time = limits['wallclock_time']
                cmd.extend(('-l', 'h_rt=%s' % self._timelimit(wall_time)))

        # Set default command configuration.
        if inp is None:
            cmd.extend(('-i', DEV_NULL))
        if out is None:
            base = os.path.basename(resource_desc['remote_command'])
            cmd.extend(('-o', '%s.stdout' % base))
        if err is None:
            cmd.extend(('-j', 'yes'))

        # Add 'escape' clause.
        if 'native_specification' in resource_desc:
            cmd.extend(resource_desc['native_specification'])

        cmd.append(self._fix_path(resource_desc['remote_command']))

        if 'args' in resource_desc:
            for arg in resource_desc['args']:
                cmd.append(self._fix_path(arg))

        self._logger.info('%r', ' '.join(cmd))
        try:
            process = ShellProc(cmd, DEV_NULL, 'qsub.out', STDOUT, env)
        except Exception as exc:
            self._logger.error('exception creating process: %s', exc)
            raise

        self._logger.debug('    PID = %d', process.pid)
        return_code, error_msg = process.wait(1)
        self._logger.debug('    returning %s', (return_code, error_msg))
        return (return_code, error_msg)

    def _fix_path(self, path):
        """ Translates special prefixes. """
        if path.startswith(HOME_DIRECTORY):
            path = os.path.join(self.home_dir, path[len(HOME_DIRECTORY):])
        elif path.startswith(WORKING_DIRECTORY):
            path = os.path.join(self.work_dir, path[len(WORKING_DIRECTORY):])
        return path

    @staticmethod
    def _jobname(name):
        """ Create legal job name from `name`. """
        return name.translate(_XLATE)

    @staticmethod
    def _timelimit(seconds):
        """ Make time string from `seconds`. """
        seconds = float(seconds)
        hours = int(seconds / (60*60))
        seconds -= hours * 60*60
        minutes = int(seconds / 60)
        seconds -= minutes * 60
        seconds = int(seconds)
        return '%d:%d:%d' % (hours, minutes, seconds)


class _ServerManager(OpenMDAO_Manager):
    """
    A :class:`multiprocessing.Manager` which manages :class:`GridEngineServer`.
    """
    pass

register(GridEngineServer, _ServerManager, 'grid_engine.grid_engine')

