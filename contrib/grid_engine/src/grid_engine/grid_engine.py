"""
GridEngine resource allocator and object server.
"""

import fnmatch

from openmdao.main.mp_support import OpenMDAO_Manager, register
from openmdao.main.objserverfactory import ObjServer
from openmdao.main.rbac import rbac, get_credentials
from openmdao.main.resource import ResourceAllocator

from openmdao.util.shellproc import ShellProc, STDOUT, PIPE

HOME_DIRECTORY = '$drmaa_hd_ph$'
WORKING_DIRECTORY = '$drmaa_wd_ph$'


class GridEngineAllocator(ResourceAllocator):
    """
    Knows about GridEngine cluster resources (via 'qhost').
    Uses :class:`GridEngineServer` instead of :class:`ObjServer`.

    name: string
        Name of allocator, used in log messages, etc.

    pattern: string
        :mod:`glob`-style pattern used to select hosts from 'qhost' output,

    authkey: string
        Authorization key for this allocator and any deployed servers.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Use with caution!

    Resource configuration file entry equivalent to defaults::

        [GridEngine]
        classname: grid_engine.grid_engine.GridEngineAllocator
        authkey: PublicKey
        allow_shell: True
        pattern: *

    """

    _QHOST = 'qhost'  # Replaced with path to fake for testing.

    def __init__(self, name='GridEngine', pattern='*', authkey=None,
                 allow_shell=False):
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
    """ Knows about executing a command via 'qsub' """

    _QSUB = 'qsub'  # Replaced with path to fake for testing.

    @rbac('owner')
    def execute_command(self, resource_desc):
        """
        Submit command based on `resource_desc`.

        resource_desc: dict
            Description of command and required resources.
        """
        cmd = [self._QSUB, '-V', '-sync']
        env = None
        cwd = None
        inp, out, err = None, None, None

        for key, value in resource_desc.items():
            if key == 'job_name':
                cmd.append('-N')
                cmd.append(value)
            elif key == 'working_directory':
                cmd.append('-wd')
                cmd.append(value)
                cwd = value
            elif key == 'job_environment':
                env = value
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
                cmd.append(value)       # May need to translate
            elif key == 'deadline_time':
                cmd.append('-dl')
                cmd.append(value)       # May need to translate
            elif key == 'hard_wallclock_time_limit':
                cmd.append('-l')
                cmd.append('h_rt')
                cmd.append(str(value))  # May need to translate
            elif key == 'soft_wallclock_time_limit':
                cmd.append('-soft')
                cmd.append('-l')
                cmd.append('s_rt')
                cmd.append(str(value))  # May need to translate
                cmd.append('-hard')
            elif key == 'hard_run_duration_limit':
                cmd.append('-l')
                cmd.append('h_cpu')
                cmd.append(str(value))  # May need to translate
            elif key == 'soft_run_duration_limit':
                cmd.append('-soft')
                cmd.append('-l')
                cmd.append('s_cpu')
                cmd.append(str(value))  # May need to translate
                cmd.append('-hard')

        if cwd is None:
            cmd.append('-cwd')

        if inp is None:
            cmd.append('-i')
            cmd.append('/dev/null')
        if out is None:
            cmd.append('-o')
            cmd.append('%s.stdout' % resource_desc['remote_command'])
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

    @staticmethod
    def _fix_path(path):
        """ Translates special prefixes. """
        if path.startswith(HOME_DIRECTORY):
            path = '$HOME' + path[len(HOME_DIRECTORY):]
        elif path.startswith(WORKING_DIRECTORY):
            path = path[len(WORKING_DIRECTORY):]  # Valid?
        return path


class _ServerManager(OpenMDAO_Manager):
    """
    A :class:`multiprocessing.Manager` which manages :class:`GridEngineServer`.
    """
    pass

register(GridEngineServer, _ServerManager,
         'openmdao.contrib.grid_engine.grid_engine')


