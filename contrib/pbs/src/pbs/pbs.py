"""
PBS resource allocator and object server.

By adding the allocator to the resource allocation manager, resource requests
will interrogate the allocator to see if it could be used. This would typically
be done by :class:`ExternalCode` to execute a compute-intensive or parallel
application.
"""

import os.path
import sys

from openmdao.main.mp_support import OpenMDAO_Manager, register
from openmdao.main.objserverfactory import ObjServer
from openmdao.main.rbac import rbac
from openmdao.main.resource import FactoryAllocator, \
                                   HOME_DIRECTORY, WORKING_DIRECTORY

from openmdao.util.shellproc import ShellProc, STDOUT


class PBS_Allocator(FactoryAllocator):
    """
    Uses :class:`PBS_Server` instead of :class:`ObjServer` when deploying.

    name: string
        Name of allocator, used in log messages, etc.

    account_id: string
        Default value for the ``account_id`` resource key.

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
        account_id: no-default-set
        authkey: PublicKey
        allow_shell: True

    """

    def __init__(self, name='PBS', account_id='no-default-set',
                 authkey=None, allow_shell=True):
        super(PBS_Allocator, self).__init__(name, authkey, allow_shell)
        os.environ['OPENMDAO_PBS_ACCOUNTID'] = account_id
        self.factory.manager_class = _ServerManager
        self.factory.server_classname = 'pbs_pbs_PBS_Server'
#FIXME: need to somehow determine available cpus.
        self.n_cpus = 10000

    def configure(self, cfg):
        """
        Configure allocator from :class:`ConfigParser` instance.
        Normally only called during manager initialization.

        cfg: :class:`ConfigParser`
            Configuration data is located under the section matching
            this allocator's `name`.

        Allows modifying `account_id` and factory options.
        """
        super(PBS_Allocator, self).configure(cfg)
        if cfg.has_option(self.name, 'account_id'):
            account_id = cfg.get(self.name, 'account_id')
            self._logger.debug('    account_id: %s', account_id)
            os.environ['OPENMDAO_PBS_ACCOUNTID'] = account_id

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
        return self.n_cpus

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

        if 'n_cpus' in resource_desc:
            value = resource_desc['n_cpus']
            if self.n_cpus < value:
                return (-2, {'n_cpus': 'want %s, have %s'
                                       % (value, self.n_cpus)})
        criteria = {
            'total_cpus': self.n_cpus,
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
                    return (-2, {key: 'requested local host'})
            elif key == 'n_cpus':
                pass  # Handle in upper layer.
            else:
                return (-2, {key: 'unrecognized key'})
        return (0, {})


class PBS_Server(ObjServer):
    """ Knows about executing a command via `qsub`. """

    _QSUB = ['qsub']  # Replaced with fake command for testing.

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
        account_id                -W group_list= `value`
        ------------------------- ---------------------------
        queue                     -q `value`
        ------------------------- ---------------------------
        job_name                  -N `value`
        ------------------------- ---------------------------
        working_directory         Handled in generated script
        ------------------------- ---------------------------
        parallel_environment      Ignored
        ------------------------- ---------------------------
        n_cpus                    -l select= `value` :ncpus=1
        ------------------------- ---------------------------
        input_path                Handled in generated script
        ------------------------- ---------------------------
        output_path               Handled in generated script
        ------------------------- ---------------------------
        error_path                Handled in generated script
        ------------------------- ---------------------------
        join_files                Handled in generated script
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

        In order to support a working directory other than HOME or a
        PBS-generated scratch directory, a short script is written with
        PBS directives in the header. The script will change to the working
        directory and then run the command.
        
        If 'working_directory' is not specified, use current server directory.
        If 'input_path' is not specified, use ``/dev/null``.
        If 'output_path' is not specified, use ``<remote_command>.stdout``.
        If 'error_path' is not specified, use stdout.

        If 'native_specification' is specified, it is added to the `qsub`
        command just before the name of the generated script.

        Output from `qsub` itself is routed to ``qsub.out``.
        """
        self.home_dir = os.path.expanduser('~')
        self.work_dir = ''
        dev_null = 'nul:' if sys.platform == 'win32' else '/dev/null'

        cmd = list(self._QSUB)
        cmd.extend(('-V', '-W', 'block=true', '-j', 'oe'))
        if sys.platform != 'win32':
            cmd.extend(('-S', '/bin/sh'))
        env = None
        inp, out, err = None, None, None
        join_files = False

        # Set working directory now, for possible path fixing.
        try:
            value = resource_desc['working_directory']
        except KeyError:
            pass
        else:
            self.work_dir = self._fix_path(value)

        # Write script to be submitted rather than putting everything on
        # 'qsub' command line. We have to do this since otherwise there's
        # no way to set an execution directory or input path.
        if 'job_name' in resource_desc:
            base = resource_desc['job_name']
        else:
            base = os.path.basename(resource_desc['remote_command'])
        script_name = '%s.qsub' % base

        with open(script_name, 'w') as script:
            script.write('#!/bin/sh\n')

            if 'account_id' in resource_desc:
                account_id = resource_desc['account_id']
            else:
                account_id = os.environ.get('OPENMDAO_PBS_ACCOUNTID', None)
            script.write('#PBS -W group_list=%s\n' % account_id.strip())

            # Process description in fixed, repeatable order.
            keys = ('queue',
                    'job_name',
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

                if key == 'queue':
                    script.write('#PBS -q %s\n' % value)
                elif key == 'job_name':
                    script.write('#PBS -N %s\n' % value)
                elif key == 'job_environment':
                    env = value
                elif key == 'n_cpus':
                    script.write('#PBS -l select=%d:ncpus=1\n' % value)
                elif key == 'input_path':
                    inp = value
                elif key == 'output_path':
                    out = value
                elif key == 'error_path':
                    err = value
                elif key == 'join_files':
                    join_files = value
                elif key == 'email':
                    script.write('#PBS -M %s\n' % ','.join(value))
                elif key == 'block_email':
                    if value:
                        script.write('#PBS -m n\n')
                elif key == 'email_events':
                    value = value.replace('s', '')  # No suspend event
                    if value:
                        script.write('#PBS -m %s\n' % value)
                elif key == 'start_time':
                    script.write('#PBS -a %s\n' % value)
                elif key == 'hard_wallclock_time_limit':
                    script.write('#PBS -l walltime=%s\n' % self._timelimit(value))
                elif key == 'soft_wallclock_time_limit':
                    script.write('#PBS -l walltime=%s\n' % self._timelimit(value))
                elif key == 'hard_run_duration_limit':
                    script.write('#PBS -l walltime=%s\n' % self._timelimit(value))
                elif key == 'soft_run_duration_limit':
                    script.write('#PBS -l walltime=%s\n' % self._timelimit(value))
    
            if 'native_specification' in resource_desc:
                cmd.extend(resource_desc['native_specification'])

            # Have script move to work directory relative to
            # home directory on execution host.
            home = os.path.realpath(os.path.expanduser('~'))
            work = os.path.realpath(self.work_dir or os.getcwd())
            if work.startswith(home):
                work = work[len(home)+1:]
                if sys.platform == 'win32':
                    script.write('cd %%HOMEDRIVE%%HOMEPATH%%\n')
                else:
                    script.write('cd $HOME\n')
            else:
                # This can potentially cause problems...
                self._logger.warning('work %r not a descendant of home %r'
                                     % (work, home))
            script.write('cd %s\n' % work)

            script.write(self._fix_path(resource_desc['remote_command']))

            if 'args' in resource_desc:
                for arg in resource_desc['args']:
                    script.write(' %s' % self._fix_path(arg))

            script.write(' <%s' % (inp or dev_null))
            script.write(' >%s' % (out or '%s.stdout' % base))
            if join_files or err is None:
                script.write(' 2>&1')
            else:
                script.write(' 2>%s' % err)
            script.write('\n')
            if sys.platform != 'win32':
                os.chmod(script_name, 0700)

        cmd.append(os.path.join('.', script_name))
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
    def _timelimit(seconds):
        """ Make time limit string from `seconds`. """
        hours = int(seconds / (60*60))
        seconds -= hours * 60*60
        minutes = int(seconds / 60)
        seconds -= minutes * 60
        seconds = int(seconds)
        return '%d:%02d:%02d' % (hours, minutes, seconds)


class _ServerManager(OpenMDAO_Manager):
    """
    A :class:`multiprocessing.Manager` which manages :class:`PBS_Server`.
    """
    pass

register(PBS_Server, _ServerManager, 'pbs.pbs')

