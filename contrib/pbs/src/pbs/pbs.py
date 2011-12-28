"""
PBS resource allocator and object server.

By adding the allocator to the resource allocation manager, resource requests
will interrogate the allocator to see if it could be used. This would typically
be done by :class:`ExternalCode` to execute a compute-intensive or parallel
application.
"""

import fnmatch
import os.path
import sys

from openmdao.main.mp_support import OpenMDAO_Manager, register
from openmdao.main.objserverfactory import ObjServer
from openmdao.main.rbac import rbac
from openmdao.main.resource import FactoryAllocator, \
                                   HOME_DIRECTORY, WORKING_DIRECTORY

from openmdao.util.shellproc import ShellProc, STDOUT, PIPE


class PBS_Allocator(FactoryAllocator):
    """
    Knows about PBS cluster resources (via `qhost`).
    Uses :class:`PBS_Server` instead of :class:`ObjServer` when deploying.

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

        [PBS]
        classname: pbs.PBS_Allocator
        pattern: *
        authkey: PublicKey
        allow_shell: True

    """

    _QHOST = ['qhost']  # Replaced with path to fake for testing.

    def __init__(self, name='PBS', pattern='*', authkey=None,
                 allow_shell=True):
        super(PBS_Allocator, self).__init__(name, authkey, allow_shell)
        self.factory.manager_class = _ServerManager
        self.factory.server_classname = 'pbs_pbs_PBS_Server'
        self.pattern = pattern

    def configure(self, cfg):
        """
        Configure allocator from :class:`ConfigParser` instance.
        Normally only called during manager initialization.

        cfg: :class:`ConfigParser`
            Configuration data is located under the section matching
            this allocator's `name`.

        Allows modifying factory options and `pattern`.
        """
        super(PBS_Allocator, self).configure(cfg)
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
            super(PBS_Allocator, self).check_compatibility(resource_desc)
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


class PBS_Server(ObjServer):
    """ Knows about executing a command via `qsub`. """

    _QSUB = ['qsub']  # Replaced with path to fake for testing.

    @rbac('owner')
    def execute_command(self, resource_desc):
        """
        Submit command based on `resource_desc`.

        resource_desc: dict
            Description of command and required resources.

        The '-V' `qsub` option is always used to export the current environment
        to the job. This environment is first updated with any 'job_environment'
        data. The '-W block=true' `qsub` option is used to wait for job
        completion.

        Other job resource keys are processed as follows:

        ========================= ===========================
        Resource Key              Translation
        ========================= ===========================
        job_name                  -N `value`
        ------------------------- ---------------------------
        working_directory         -W sandbox= `value`
        ------------------------- ---------------------------
        parallel_environment      Ignored
        ------------------------- ---------------------------
        n_cpus                    -l select= `value` :ncpus=1
        ------------------------- ---------------------------
        input_path                -i `value`
        ------------------------- ---------------------------
        output_path               -o `value`
        ------------------------- ---------------------------
        error_path                -e `value`
        ------------------------- ---------------------------
        join_files                -j oe
        ------------------------- ---------------------------
        email                     -M `value`
        ------------------------- ---------------------------
        block_email               -m n
        ------------------------- ---------------------------
        email_events              -m `value`
        ------------------------- ---------------------------
        start_time                -a `value`
        ------------------------- ---------------------------
        deadline_time             Ignored
        ------------------------- ---------------------------
        hard_wallclock_time_limit -l walltime= `value`
        ------------------------- ---------------------------
        soft_wallclock_time_limit -l walltime= `value`
        ------------------------- ---------------------------
        hard_run_duration_limit   -l walltime= `value`
        ------------------------- ---------------------------
        soft_run_duration_limit   -l walltime= `value`
        ------------------------- ---------------------------
        job_category              Ignored
        ========================= ===========================

        Where `value` is the corresponding resource value.

        If 'working_directory' is not specified, add ``-W sandbox=<cwd>``.
        If 'input_path' is not specified, add ``-i /dev/null``.
        If 'output_path' is not specified, add ``-o <remote_command>.stdout``.
        If 'error_path' is not specified, add ``-j oe``.

        If 'native_specification' is specified, it is added to the `qsub`
        command just before 'remote_command' and 'args'.

        Output from `qsub` itself is routed to ``qsub.out``.
        """
        self.home_dir = os.path.expanduser('~')
        self.work_dir = ''
        dev_null = 'nul:' if sys.platform == 'win32' else '/dev/null'

        cmd = list(self._QSUB)
        cmd.extend(('-V', '-W', 'block=true'))
        env = None
        inp, out, err = None, None, None

        # Set working directory now, for possible path fixing.
# FIXME: sandbox is HOME, not set, or PRIVATE
        try:
            value = resource_desc['working_directory']
        except KeyError:
            pass
        else:
            self.work_dir = self._fix_path(value)
            cmd.extend(('-W', 'sandbox=%s' % value))

        # Process description in fixed, repeatable order.
        keys = ('job_name',
                'job_environment',
                'n_cpus',
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
                cmd.extend(('-N', value))
            elif key == 'job_environment':
                env = value
            elif key == 'n_cpus':
                cmd.extend(('-l', 'select=%d:ncpus=1' % value))
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
            elif key == 'email':
                cmd.extend(('-M', ','.join(value)))
            elif key == 'block_email':
                if value:
                    cmd.extend(('-m', 'n'))
            elif key == 'email_events':
                cmd.extend(('-m', value.replace('s', '')))  # No suspend event
            elif key == 'start_time':
                cmd.extend(('-a', translate1(value)))
            elif key == 'hard_wallclock_time_limit':
                cmd.extend(('-l', 'walltime=%s' % translate2(value)))
            elif key == 'soft_wallclock_time_limit':
                cmd.extend(('-l', 'walltime=%s' % translate2(value)))
            elif key == 'hard_run_duration_limit':
                cmd.extend(('-l', 'walltime=%s' % translate2(value)))
            elif key == 'soft_run_duration_limit':
                cmd.extend(('-l', 'walltime=%s' % translate2(value)))

        if not self.work_dir:
            cmd.extend(('-W', 'sandbox=%s' % os.getcwd()))

        if inp is None:
            cmd.extend(('-i', dev_null))
        if out is None:
            base = os.path.basename(resource_desc['remote_command'])
            cmd.extend(('-o', '%s.stdout' % base))
        if err is None:
            cmd.extend(('-j', 'oe'))

        if 'native_specification' in resource_desc:
            cmd.extend(resource_desc['native_specification'])

        cmd.append(self._fix_path(resource_desc['remote_command']))

        if 'args' in resource_desc:
            for arg in resource_desc['args']:
                cmd.append(self._fix_path(arg))

        self._logger.info('%r', ' '.join(cmd))
        try:
            process = ShellProc(cmd, dev_null, 'qsub.out', STDOUT, env)
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
# FIXME: this is called for multiple different things!
# FIXME: not handling > 99 hours correctly.
        seconds = float(seconds)
        hours = int(seconds / (60*60))
        seconds -= hours * 60*60
        minutes = int(seconds / 60)
        seconds -= minutes * 60
        seconds = int(seconds)
        return '%02d%02d.%02d' % (hours, minutes, seconds)


class _ServerManager(OpenMDAO_Manager):
    """
    A :class:`multiprocessing.Manager` which manages :class:`PBS_Server`.
    """
    pass

register(PBS_Server, _ServerManager, 'pbs.pbs')

