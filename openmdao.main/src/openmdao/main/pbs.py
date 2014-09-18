"""
.. _`pbs.py`:

PBS resource allocator and object server.

By adding the allocator to the resource allocation manager, resource requests
will interrogate the allocator to see if it could be used. This would typically
be done by :class:`ExternalCode` to execute a compute-intensive or parallel
application.
"""

import os.path
import string
import sys

from openmdao.main.mp_support import OpenMDAO_Manager, register
from openmdao.main.objserverfactory import ObjServer
from openmdao.main.rbac import rbac
from openmdao.main.resource import FactoryAllocator, \
                                   HOME_DIRECTORY, WORKING_DIRECTORY

from openmdao.util.shellproc import ShellProc, STDOUT, DEV_NULL

# Translate illegal job name characters.
# (Job name may be used as script filename, so we're more restrictive than PBS)
_XLATE = string.maketrans(' \n\r\t/\\:;*?.[]%$', '_______________')


class PBS_Allocator(FactoryAllocator):
    """
    Uses :class:`PBS_Server` instead of :class:`ObjServer` when deploying.

    name: string
        Name of allocator; used in log messages, etc.

    accounting_id: string
        Default value for the ``accounting_id`` resource key.

    authkey: string
        Authorization key for this allocator and any deployed servers.

    allow_shell: bool
        If True, :meth:`execute_command` and :meth:`load_model` are allowed
        in created servers. Since :meth:`execute_command` is required, this
        defaults to be True.

    .. warning::

        There is a security risk with `allow_shell` True. Be careful to limit
        factory servers to the intended set of users!

    Resource configuration file entry equivalent to defaults::

        [PBS]
        classname: openmdao.main.pbs.PBS_Allocator
        accounting_id: no-default-set
        authkey: PublicKey
        allow_shell: True

    """

    def __init__(self, name='PBS', accounting_id='no-default-set',
                 authkey=None, allow_shell=True):
        super(PBS_Allocator, self).__init__(name, authkey, allow_shell)
        self.accounting_id = accounting_id
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

        Allows modifying `accounting_id` and factory options.
        """
        super(PBS_Allocator, self).configure(cfg)
        if cfg.has_option(self.name, 'accounting_id'):
            self.accounting_id = cfg.get(self.name, 'accounting_id')
            self._logger.debug('    accounting_id: %s', self.accounting_id)

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
        elif 'min_cpus' in resource_desc:
            return (self.n_cpus / resource_desc['min_cpus'], {})
        else:
            return (self.n_cpus, {})

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
        and `info` is empty. Otherwise, `retcode` will be -2 and `info` will
        be a single-entry dictionary whose key is the incompatible key in
        `resource_desc` and whose value provides data regarding the incompatibility.
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
            elif key == 'min_cpus':
                if self.n_cpus < value:
                    return (-2, {'min_cpus': 'want %s, have %s'
                                             % (value, self.n_cpus)})
            elif key == 'max_cpus':
                pass
            else:
                return (-2, {key: 'unrecognized key'})
        return (0, {})

    @rbac('*')
    def deploy(self, name, resource_desc, criteria):
        """
        Deploy a server suitable for `resource_desc`.
        Returns a proxy to the deployed server.
        Overrides superclass to pass `accounting_id` to server.

        name: string
            Name for server.

        resource_desc: dict
            Description of required resources.

        criteria: dict
            The dictionary returned by :meth:`time_estimate`.
        """
        server = super(PBS_Allocator, self).deploy(name, resource_desc,
                                                   criteria)
        if server is not None:
            server.configure(self.accounting_id)
        return server


class PBS_Server(ObjServer):
    """ Knows about executing a command via `qsub`. """

    _QSUB = ['qsub']  # Replaced with fake command for testing.

    @rbac('owner')
    def configure(self, accounting_id):
        """
        Configure default accounting id.

        accounting_id: string
            Used as default ``accounting_id`` value.
        """
        self.accounting_id = accounting_id

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
        ``submit_as_hold``        -h
        ------------------------- ---------------------------
        ``rerunnable``            -r y|n
        ------------------------- ---------------------------
        ``working_directory``     Handled in generated script
        ------------------------- ---------------------------
        ``job_category``          Ignored
        ------------------------- ---------------------------
        ``min_cpus``              -l select= `value` :ncpus=1
        ------------------------- ---------------------------
        ``max_cpus``              Ignored
        ------------------------- ---------------------------
        ``min_phys_memory``       Ignored
        ------------------------- ---------------------------
        ``email``                 -M `value`
        ------------------------- ---------------------------
        ``email_on_started``      -m b
        ------------------------- ---------------------------
        ``email_on_terminated``   -m e
        ------------------------- ---------------------------
        ``job_name``              -N `value`
        ------------------------- ---------------------------
        ``input_path``            Handled in generated script
        ------------------------- ---------------------------
        ``output_path``           Handled in generated script
        ------------------------- ---------------------------
        ``error_path``            Handled in generated script
        ------------------------- ---------------------------
        ``join_files``            Handled in generated script
        ------------------------- ---------------------------
        ``reservation_id``        Ignored
        ------------------------- ---------------------------
        ``queue_name``            -q `value`
        ------------------------- ---------------------------
        ``priority``              -p `value`
        ------------------------- ---------------------------
        ``start_time``            -a `value`
        ------------------------- ---------------------------
        ``deadline_time``         Ignored
        ------------------------- ---------------------------
        ``accounting_id``         -W group_list= `value`
        ========================= ===========================

        Where `value` is the corresponding resource value.

        To support a working directory other than HOME or a
        PBS-generated scratch directory, a short script is written with
        PBS directives in the header. The script will change to the working
        directory of the server and then run the command.
        
        If 'input_path' is not specified, use ``/dev/null``.
        If 'output_path' is not specified, use ``<remote_command>.stdout``.
        If 'error_path' is not specified, use stdout.

        If 'native_specification' is specified, it is added to the `qsub`
        command just before the name of the generated script. If it contains
        a ``select`` clause, then that will prevent generation of a ``select``
        clause related to 'min_cpus'.

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
        ``cpu_time``         Ignored
        -------------------- -------------------------
        ``wallclock_time``   -l walltime= `value`
        ==================== =========================

        Output from `qsub` itself is routed to ``qsub.out``.
        If the job reports an error, ``qsub.out`` will be appended to either
        `error_path`, or if that was not specified, stdout.
        """
        self.home_dir = os.path.expanduser('~')
        self.work_dir = os.getcwd()  # Server started in working directory.

        cmd = list(self._QSUB)
        cmd.extend(('-V', '-W', 'block=true', '-j', 'oe'))
        if sys.platform == 'win32':  # pragma no cover
            prefix = 'REM PBS'
            cmd.extend(('-C', '"%s"' % prefix))
            suffix = '-qsub.bat'
        else:
            prefix = '#PBS'
            cmd.extend(('-S', '/bin/sh'))
            suffix = '.qsub'
        env = None
        inp, out, err = None, None, None
        join_files = False

        # Write script to be submitted rather than putting everything on
        # 'qsub' command line. We have to do this since otherwise there's
        # no way to set an execution directory or input path.
        base = None
        if 'job_name' in resource_desc:
            base = self._jobname(resource_desc['job_name'])
        if not base:
            base = os.path.basename(resource_desc['remote_command'])
        script_name = '%s%s' % (base, suffix)

        native_specification = resource_desc.get('native_specification', [])

        with open(script_name, 'w') as script:
            if sys.platform == 'win32':  # pragma no cover
                script.write('@echo off\n')
            else:
                script.write('#!/bin/sh\n')

            # PBS (at least at NAS) requires 'group_list' be set.
            if 'accounting_id' in resource_desc:
                accounting_id = resource_desc['accounting_id']
            else:
                accounting_id = self.accounting_id
            script.write('%s -W group_list=%s\n'
                         % (prefix, accounting_id.strip()))

            # Process description in fixed, repeatable order.
            keys = ('submit_as_hold',
                    'rerunnable',
                    'job_environment',
                    'min_cpus',
                    'email',
                    'email_on_started',
                    'email_on_terminated',
                    'job_name',
                    'input_path',
                    'output_path',
                    'error_path',
                    'join_files',
                    'queue_name',
                    'priority',
                    'start_time')

            email_events = ''
            for key in keys:
                try:
                    value = resource_desc[key]
                except KeyError:
                    continue

                if key == 'submit_as_hold':
                    if value:
                        script.write('%s -h\n' % prefix)
                elif key == 'rerunnable':
                    script.write('%s -r %s\n' % (prefix, 'y' if value else 'n'))
                elif key == 'job_environment':
                    env = value
                elif key == 'min_cpus':
                    # Only write select clause if not in 'native_specification'.
                    for arg in native_specification:
                        if 'select' in arg:
                            break
                    else:
                        script.write('%s -l select=%d:ncpus=1\n' % (prefix, value))
                elif key == 'email':
                    script.write('%s -M %s\n' % (prefix, ','.join(value)))
                elif key == 'email_on_started':
                    email_events += 'b'
                elif key == 'email_on_terminated':
                    email_events += 'e'
                elif key == 'job_name':
                    value = value or base
                    script.write('%s -N %s\n' % (prefix, self._jobname(value)))
                elif key == 'input_path':
                    inp = value
                elif key == 'output_path':
                    out = value
                elif key == 'error_path':
                    err = value
                elif key == 'join_files':
                    join_files = value
                elif key == 'queue_name':
                    script.write('%s -q %s\n' % (prefix, value))
                elif key == 'priority':
                    script.write('%s -p %d\n' % (prefix, value))
                elif key == 'start_time':
                    script.write('%s -a %s\n'
                                 % (prefix, value.strftime('%Y%m%d%H%M.%S')))

            if email_events:
                script.write('%s -m %s\n' % (prefix, email_events))

            # Set resource limits.
            if 'resource_limits' in resource_desc:
                limits = resource_desc['resource_limits']
                if 'wallclock_time' in limits:
                    wall_time = limits['wallclock_time']
                    script.write('%s -l walltime=%s\n'
                                 % (prefix, self._timelimit(wall_time)))

            # Have script move to work directory.
            home = os.path.realpath(os.path.expanduser('~'))
            work = os.path.realpath(self.work_dir)
            if work.startswith(home):
                work = work[len(home)+1:]
                if sys.platform == 'win32':  # pragma no cover
                    script.write('cd %HOMEDRIVE%%HOMEPATH%\n')
                else:
                    script.write('cd $HOME\n')
            if ' ' in work:
                work = '"%s"' % work
            script.write('cd %s\n' % work)

            script.write(self._fix_path(resource_desc['remote_command']))

            if 'args' in resource_desc:
                for arg in resource_desc['args']:
                    arg = self._fix_path(arg)
                    if ' ' in arg and arg[0] not in ('"', "'"):
                        arg = '"%s"' % arg
                    script.write(' %s' % arg)

            script.write(' <%s' % (inp or DEV_NULL))
            script.write(' >%s' % (out or '%s.stdout' % base))
            if join_files or err is None:
                script.write(' 2>&1')
            else:
                script.write(' 2>%s' % err)
            script.write('\n')

        if sys.platform != 'win32':
            os.chmod(script_name, 0700)

        # Add 'escape' clause.
        cmd.extend(native_specification)

        with open(script_name, 'rU') as inp:
            self._logger.debug('%s:', script_name)
            for line in inp:
                self._logger.debug('    %s', line.rstrip())

        # Submit job.
        cmd.append(os.path.join('.', script_name))
        self._logger.info('%r', ' '.join(cmd))
        try:
            process = ShellProc(cmd, DEV_NULL, 'qsub.out', STDOUT, env)
        except Exception as exc:
            self._logger.error('exception creating process: %s', exc)
            if os.path.exists('qsub.out'):
                with open('qsub.out', 'rU') as inp:
                    self._logger.error('qsub.out:')
                    for line in inp:
                        self._logger.error('    %s', line.rstrip())
            raise

        # Submitted, wait for completion.
        self._logger.debug('    PID = %d', process.pid)
        return_code, error_msg = process.wait(1)
        self._logger.debug('    returning %s', (return_code, error_msg))
        if return_code and os.path.exists('qsub.out'):
            if join_files or err is None:
                qsub_echo = out or '%s.stdout' % base
            else:
                qsub_echo = err
            with open('qsub.out', 'rU') as inp:
                with open(qsub_echo, 'a+') as out:
                    out.write('===== error_msg =====\n')
                    out.write('%r\n' % error_msg)
                    out.write('===== qsub.out =====\n')
                    self._logger.error('qsub.out:')
                    for line in inp:
                        self._logger.error('    %s', line.rstrip())
                        out.write(line)
        return (return_code, error_msg)

    def _fix_path(self, path):
        """ Translates special prefixes. """
        if path.startswith(HOME_DIRECTORY):
            path = os.path.join(self.home_dir, path[len(HOME_DIRECTORY):])
            if ' ' in path:
                path = '"%s"' % path
        elif path.startswith(WORKING_DIRECTORY):
            path = os.path.join(self.work_dir, path[len(WORKING_DIRECTORY):])
            if ' ' in path:
                path = '"%s"' % path
        return path

    @staticmethod
    def _jobname(name):
        """ Create legal job name from `name`. """
        name = name.strip()[:15]  # 15 characters max.
        name = name.translate(_XLATE)
        if name and not name[0].isalpha():
            name = 'Z%s' % name[1:]
        return name

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

