"""
GridEngine resource allocator and object server.
"""

import fnmatch
import os.path

from openmdao.main.mp_support import OpenMDAO_Manager, register
from openmdao.main.objserverfactory import ObjServer
from openmdao.main.rbac import rbac, get_credentials
from openmdao.main.resource import ResourceAllocator, \
                                   HOME_DIRECTORY, WORKING_DIRECTORY

from openmdao.util.shellproc import ShellProc, STDOUT, PIPE


class GridEngineAllocator(ResourceAllocator):
    """
    Knows about GridEngine cluster resources (via `qhost`).
    Uses :class:`GridEngineServer` instead of :class:`ObjServer` when deploying.

    name: string
        Name of allocator, used in log messages, etc.

    pattern: string
        :mod:`fnmatch`-style pattern used to select hosts from `qhost` output,

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
        classname: grid_engine.GridEngineAllocator
        authkey: PublicKey
        allow_shell: True
        pattern: *

    """

    _QHOST = 'qhost'  # Replaced with path to fake for testing.

    def __init__(self, name='GridEngine', pattern='*', authkey=None,
                 allow_shell=True):
        super(GridEngineAllocator, self).__init__(name, authkey, allow_shell)
        self.pattern = pattern
        self.manager_class = _ServerManager
        self.server_classname = \
            'openmdao_contrib_grid_engine_grid_engine_GridEngineServer'

    @rbac('*')
    def configure(self, cfg):
        """
        Configure allocator from :class:`ConfigParser` instance.
        Normally only called during manager initialization.

        cfg: :class:`ConfigParser`
            Configuration data is located under the section matching
            this allocator's `name`.

        Allows modifying `pattern`.
        """
        if cfg.has_option(self.name, 'pattern'):
            self.pattern = cfg.get(self.name, 'pattern')

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
            return 0
        return len(self._get_hosts())

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

        if 'n_cpus' in resource_desc:
            value = resource_desc['n_cpus']
            if len(hostnames) < value:
                return (-2, {'ncpus': (value, len(hostnames))})

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
        and `info` is empty. Otherwise `retcode` will be -2 and `info` will
        be a single-entry dictionary whose key is the incompatible key in
        `resource_desc` and value provides data regarding the incompatibility.
        """
        retcode, info = \
            super(GridEngineAllocator, self).check_compatibility(resource_desc)
        if retcode != 0:
            return (retcode, info)

        for key in info:
            value = resource_desc[key]
            if key == 'localhost':
                if value:
                    return (-2, {key : value})
            elif key == 'n_cpus':
                pass  # Handle in upper layer.
            else:
                return (-2, {key : (value, 'unrecognized key')})
        return (0, {})

    def _get_hosts(self):
        """ Return list of hostnames sorted by load. """
        # Get host load information.
        try:
            proc = ShellProc([self._QHOST], stdout=PIPE)
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


class GridEngineServer(ObjServer):
    """ Knows about executing a command via `qsub`. """

    _QSUB = 'qsub'  # Replaced with path to fake for testing.

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

        ========================= ====================
        Resource Key              Translation
        ========================= ====================
        job_name                  -N `value`
        ------------------------- --------------------
        working_directory         -wd `value`
        ------------------------- --------------------
        parallel_environment      -pe `value` `n_cpus`
        ------------------------- --------------------
        input_path                -i `value`
        ------------------------- --------------------
        output_path               -o `value`
        ------------------------- --------------------
        error_path                -e `value`
        ------------------------- --------------------
        join_files                -j yes|no
        ------------------------- --------------------
        email                     -M `value`
        ------------------------- --------------------
        block_email               -m n
        ------------------------- --------------------
        email_events              -m `value`
        ------------------------- --------------------
        start_time                -a `value`
        ------------------------- --------------------
        deadline_time             Not supported
        ------------------------- --------------------
        hard_wallclock_time_limit -l h_rt= `value`
        ------------------------- --------------------
        soft_wallclock_time_limit -l s_rt= `value`
        ------------------------- --------------------
        hard_run_duration_limit   -l h_cpu= `value`
        ------------------------- --------------------
        soft_run_duration_limit   -l s_cpu= `value`
        ------------------------- --------------------
        job_category              Not supported
        ========================= ====================

        Where `value` is the corresponding resource value and
        `n_cpus` is the value of the 'n_cpus' resource, or 1.

        If 'working_directory' is not specified, add ``-cwd``.
        If 'input_path' is not specified, add ``-i /dev/null``.
        If 'output_path' is not specified, add ``-o <remote_command>.stdout``.
        If 'error_path' is not specified, add ``-j yes``.

        If 'native_specification' is specified, it is added to the `qsub`
        command just before 'remote_command' and 'args'.

        Output from `qsub` itself is routed to ``qsub.out``.
        """
        self.home_dir = os.environ['HOME']
        self.work_dir = ''

        cmd = [self._QSUB, '-V', '-sync', 'yes']
        env = None
        inp, out, err = None, None, None

        # Set working directory now, for possible path fixing.
        try:
            value = resource_desc['working_directory']
        except KeyError:
            pass
        else:
            self.work_dir = self._fix_path(value)
            cmd.append('-wd')
            cmd.append(value)

        # Process description in fixed, repeatable order.
        keys = ('job_name',
                'job_environment',
                'parallel_environment',
                'input_path',
                'output_path',
                'error_path',
                'join_files',
                'email',
                'block_email',
                'email_events',
                'start_time',
                'hard_wallclock_time_limit',
                'soft_wallclock_time_limit',
                'hard_run_duration_limit',
                'soft_run_duration_limit')

        for key in keys:
            try:
                value = resource_desc[key]
            except KeyError:
                continue

            if key == 'job_name':
                cmd.append('-N')
                cmd.append(value)
            elif key == 'job_environment':
                env = value
            elif key == 'parallel_environment':
                n_cpus = resource_desc.get('n_cpus', 1)
                cmd.append('-pe')
                cmd.append(value)
                cmd.append(str(n_cpus))
            elif key == 'input_path':
                cmd.append('-i')
                cmd.append(self._fix_path(value))
                inp = value
            elif key == 'output_path':
                cmd.append('-o')
                cmd.append(self._fix_path(value))
                out = value
            elif key == 'error_path':
                cmd.append('-e')
                cmd.append(self._fix_path(value))
                err = value
            elif key == 'join_files':
                cmd.append('-j')
                cmd.append('yes' if value else 'no')
                if value:
                    err = 'yes'
            elif key == 'email':
                cmd.append('-M')
                cmd.append(','.join(value))
            elif key == 'block_email':
                if value:
                    cmd.append('-m')
                    cmd.append('n')
            elif key == 'email_events':
                cmd.append('-m')
                cmd.append(value)
            elif key == 'start_time':
                cmd.append('-a')
                cmd.append(value)  # May need to translate
            elif key == 'hard_wallclock_time_limit':
                cmd.append('-l')
                cmd.append('h_rt=%s' % self._make_time(value))
            elif key == 'soft_wallclock_time_limit':
                cmd.append('-l')
                cmd.append('s_rt=%s' % self._make_time(value))
            elif key == 'hard_run_duration_limit':
                cmd.append('-l')
                cmd.append('h_cpu=%s' % self._make_time(value))
            elif key == 'soft_run_duration_limit':
                cmd.append('-l')
                cmd.append('s_cpu=%s' % self._make_time(value))

        if not self.work_dir:
            cmd.append('-cwd')

        if inp is None:
            cmd.append('-i')
            cmd.append('/dev/null')
        if out is None:
            cmd.append('-o')
            cmd.append('%s.stdout'
                       % os.path.basename(resource_desc['remote_command']))
        if err is None:
            cmd.append('-j')
            cmd.append('yes')

        if 'native_specification' in resource_desc:
            cmd.extend(resource_desc['native_specification'])

        cmd.append(self._fix_path(resource_desc['remote_command']))

        if 'args' in resource_desc:
            for arg in resource_desc['args']:
                cmd.append(self._fix_path(arg))

        self._logger.critical('%r', ' '.join(cmd))
        try:
            process = ShellProc(cmd, '/dev/null', 'qsub.out', STDOUT, env)
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
    def _make_time(seconds):
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

register(GridEngineServer, _ServerManager,
         'openmdao.contrib.grid_engine.grid_engine')

