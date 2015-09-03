"""
.. _`external_code.py`:
"""

import glob
import logging
import os.path
import shutil
import stat
import sys
import time

# pylint: disable-msg=E0611,F0401
from openmdao.main.datatypes.api import Bool, Dict, Str, FileRef, Float, Int, List

from openmdao.main.api import Component
from openmdao.main.exceptions import RunInterrupted, RunStopped
from openmdao.main.rbac import AccessController, RoleError, rbac, remote_access
from openmdao.main.resource import ResourceAllocationManager as RAM

from openmdao.util.filexfer import filexfer, pack_zipfile, unpack_zipfile
from openmdao.util import shellproc

from numpy.distutils.exec_command import find_executable


class ExternalCode(Component):
    """
    Run an external code as a component. The component can be configured to
    run the code on a remote server. See :meth:`execute`.

    Default stdin is the 'null' device, default stdout is the console, and
    default stderr is ``error.out``.
    """

    STDOUT   = shellproc.STDOUT
    DEV_NULL = shellproc.DEV_NULL

    # pylint: disable-msg=E1101
    command = List(Str, desc='The command to be executed.')
    env_vars = Dict({}, iotype='in',
                    desc='Environment variables required by the command.')
    resources = Dict({}, iotype='in',
                     desc='Resources required to run this component.')
    poll_delay = Float(0., low=0., units='s', iotype='in',
                       desc='Delay between polling for command completion.'
                            ' A value of zero will use an internally computed'
                            ' default.')
    timeout = Float(0., low=0., iotype='in', units='s',
                    desc='Maximum time to wait for command completion.'
                         ' A value of zero implies an infinite wait.')
    timed_out = Bool(False, iotype='out', desc='True if the command timed-out.')
    return_code = Int(0, iotype='out', desc='Return code from the command.')

    def __init__(self):
        super(ExternalCode, self).__init__()
        self.check_external_outputs=True

        self.stdin  = self.DEV_NULL
        self.stdout = None
        self.stderr = "error.out"

        self._process = None
        self._server = None

    # This gets used by remote server.
    def get_access_controller(self):  #pragma no cover
        """ Return :class:`AccessController` for this object. """
        return _AccessController()

    @rbac(('owner', 'user'))
    def set(self, path, value):
        """
        Don't allow setting of 'command' or 'resources' by a remote client.
        """
        if path in ('command', 'resources', 'get_access_controller') \
           and remote_access():
            self.raise_exception('%r may not be set() remotely' % path,
                                 RuntimeError)
        return super(ExternalCode, self).set(path, value)

    def execute(self):
        """
        Runs the specified command.

            1. Checks that all external input files exist.
            2. Runs the command.
            3. Checks that all external output files exist.

        If a subclass generates outputs (such as postprocessing results),
        then it should set attribute ``check_external_outputs`` False and call
        :meth:`check_files` itself.

        If `resources` have been specified, an appropriate server
        is allocated and the command is run on that server.
        Otherwise the command is run locally.

        When running remotely, the following resources are set:

        ================ =====================================
        Key              Value
        ================ =====================================
        job_name         self.get_pathname()
        ---------------- -------------------------------------
        remote_command   self.command (first item)
        ---------------- -------------------------------------
        args             self.command (2nd through last items)
        ---------------- -------------------------------------
        job_environment  self.env_vars
        ---------------- -------------------------------------
        input_path       self.stdin
        ---------------- -------------------------------------
        output_path      self.stdout
        ---------------- -------------------------------------
        error_path       self.stderr (if != STDOUT)
        ---------------- -------------------------------------
        join_files       If self.stderr == STDOUT
        ---------------- -------------------------------------
        wallclock_time   self.timeout (if non-zero)
        ================ =====================================

        .. note::

            Input files to be sent to the remote server are defined by
            :class:`FileMetadata` entries in the `external_files` list
            with `input` True.  Similarly, output files to be retrieved
            from the remote server are defined by entries with `output`
            True.

        .. warning::

            Any file **not** labeled with `binary` True will undergo
            newline translation if the local and remote machines have
            different newline representations. Newline translation will
            corrupt a file which is binary but hasn't been labeled as
            such.

        """
        self.return_code = -12345678
        self.timed_out = False

        if not self.command:
            self.raise_exception('Empty command list', ValueError)

        self.check_files(inputs=True)

        return_code = None
        error_msg = ''
        try:
            if self.resources:
                return_code, error_msg = self._execute_remote()
            else:
                return_code, error_msg = self._execute_local()

            if return_code is None:
                if self._stop:
                    self.raise_exception('Run stopped', RunStopped)
                else:
                    self.timed_out = True
                    self.raise_exception('Timed out', RunInterrupted)

            elif return_code:
                if isinstance(self.stderr, str):
                    if os.path.exists(self.stderr):
                        stderrfile = open(self.stderr, 'r')
                        error_desc = stderrfile.read()
                        stderrfile.close()
                        err_fragment = "\nError Output:\n%s" % error_desc
                    else:
                        err_fragment = "\n[stderr %r missing]" % self.stderr
                else:
                    err_fragment = error_msg
                    
                self.raise_exception('return_code = %d%s' \
                    % (return_code, err_fragment), RuntimeError)

            if self.check_external_outputs:
                self.check_files(inputs=False)
        finally:
            self.return_code = -999999 if return_code is None else return_code

    def check_files(self, inputs):
        """
        Check that all 'specific' input or output external files exist.
        If an external file path specifies a pattern, it is *not* checked.

        inputs: bool
            If True, check inputs; otherwise outputs.
        """
        # External files.
        for metadata in self.external_files:
            path = metadata.path
            for ch in ('*?['):
                if ch in path:
                    break
            else:
                if inputs:
                    if not metadata.get('input', False):
                        continue
                else:
                    if not metadata.get('output', False):
                        continue
                if not os.path.exists(path):
                    iotype = 'input' if inputs else 'output'
                    self.raise_exception('missing %s file %r' % (iotype, path),
                                         RuntimeError)
        # Stdin, stdout, stderr.
        if inputs and self.stdin and self.stdin != self.DEV_NULL:
            if not os.path.exists(self.stdin):
                self.raise_exception('missing stdin file %r' % self.stdin,
                                     RuntimeError)

        if not inputs and self.stdout and self.stdout != self.DEV_NULL:
            if not os.path.exists(self.stdout):
                self.raise_exception('missing stdout file %r' % self.stdout,
                                     RuntimeError)

        if not inputs and self.stderr \
                      and self.stderr != self.DEV_NULL \
                      and self.stderr != self.STDOUT \
                      and (not self.resources or \
                           not self.resources.get('join_files')):
            if not os.path.exists(self.stderr):
                self.raise_exception('missing stderr file %r' % self.stderr,
                                     RuntimeError)
        # File variables.
        if inputs:
            for pathname, obj in self.items(iotype='in', recurse=True):
                if isinstance(obj, FileRef):
                    path = self.get_metadata(pathname, 'local_path')
                    if path and not os.path.exists(path):
                        self.raise_exception("missing 'in' file %r" % path,
                                             RuntimeError)
        else:
            for pathname, obj in self.items(iotype='out', recurse=True):
                if isinstance(obj, FileRef):
                    if not os.path.exists(obj.path):
                        self.raise_exception("missing 'out' file %r" % obj.path,
                                             RuntimeError)

    def _execute_local(self):
        """ Run command. """
        self._logger.info('executing %s...', self.command)
        start_time = time.time()

        # check to make sure command exists
        if isinstance(self.command, basestring):
            program_to_execute = self.command
        else:
            program_to_execute = self.command[0]
        command_full_path = find_executable( program_to_execute )

        if not command_full_path:
            self.raise_exception("The command to be executed, '%s', cannot be found" % program_to_execute,
                                 ValueError)

        command_for_shell_proc = self.command
        if sys.platform == 'win32':
            command_for_shell_proc = ['cmd.exe', '/c' ] + command_for_shell_proc

        self._process = \
            shellproc.ShellProc(command_for_shell_proc, self.stdin,
                                self.stdout, self.stderr, self.env_vars)
        self._logger.debug('PID = %d', self._process.pid)

        try:
            return_code, error_msg = \
                self._process.wait(self.poll_delay, self.timeout)
        finally:
            self._process.close_files()
            self._process = None

        et = time.time() - start_time
        if et >= 60:  #pragma no cover
            self._logger.info('elapsed time: %.1f sec.', et)

        return (return_code, error_msg)

    def _execute_remote(self):
        """
        Allocate a server based on required resources, send inputs,
        run command, and retrieve results.
        """
        rdesc = self.resources.copy()

        # Allocate server.
        self._server, server_info = RAM.allocate(rdesc)
        if self._server is None:
            self.raise_exception('Server allocation failed :-(', RuntimeError)

        if self._logger.level == logging.NOTSET:
            # By default avoid lots of protocol messages.
            self._server.set_log_level(logging.DEBUG)
        else:
            self._server.set_log_level(self._logger.level)

        return_code = -88888888
        error_msg = ''
        try:
            # Create resource description for command.
            rdesc['job_name'] = self.get_pathname() or self.__class__.__name__
            rdesc['remote_command'] = self.command[0]
            if len(self.command) > 1:
                rdesc['args'] = self.command[1:]
            if self.env_vars:
                rdesc['job_environment'] = self.env_vars
            if not self.stdin:
                self.raise_exception('Remote execution requires stdin of'
                                     ' DEV_NULL or filename, got %r'
                                     % self.stdin, ValueError)
            if self.stdin != self.DEV_NULL:
                rdesc['input_path'] = self.stdin
            if self.stdout:
                rdesc['output_path'] = self.stdout
            else:
                rdesc['output_path'] = '%s.stdout' % self.command[0]
            if self.stderr:
                if self.stderr == self.STDOUT:
                    rdesc['join_files'] = True
                else:
                    rdesc['error_path'] = self.stderr
            else:
                rdesc['error_path'] = '%s.stderr' % self.command[0]
            if self.timeout:
                if 'resource_limits' in rdesc:
                    limits = rdesc['resource_limits'].copy()
                else:
                    limits = {}
                limits['wallclock_time'] = self.timeout
                rdesc['resource_limits'] = limits

            # Send inputs.
            patterns = []
            textfiles = []
            for metadata in self.external_files:
                if metadata.get('input', False):
                    patterns.append(metadata.path)
                    if not metadata.binary:
                        textfiles.append(metadata.path)
            for pathname, obj in self.items(iotype='in', recurse=True):
                if isinstance(obj, FileRef):
                    local_path = self.get_metadata(pathname, 'local_path')
                    if local_path:
                        patterns.append(local_path)
                        if not obj.binary:
                            textfiles.append(local_path)
            if self.stdin and self.stdin != self.DEV_NULL:
                patterns.append(self.stdin)
                textfiles.append(self.stdin)
            if patterns:
                self._send_inputs(patterns, textfiles)
            else:
                self._logger.debug('No input files')

            # Run command.
            self._logger.info('executing %s...', self.command)
            start_time = time.time()
            return_code, error_msg = \
                self._server.execute_command(rdesc)
            et = time.time() - start_time
            if et >= 60:  #pragma no cover
                self._logger.info('elapsed time: %.1f sec.', et)

            # Retrieve results.
            patterns = []
            textfiles = []
            for metadata in self.external_files:
                if metadata.get('output', False):
                    patterns.append(metadata.path)
                    if not metadata.binary:
                        textfiles.append(metadata.path)
            for pathname, obj in self.items(iotype='out', recurse=True):
                if isinstance(obj, FileRef):
                    patterns.append(obj.path)
                    if not obj.binary:
                        textfiles.append(obj.path)
            patterns.append(rdesc['output_path'])
            textfiles.append(rdesc['output_path'])
            if self.stderr != self.STDOUT:
                patterns.append(rdesc['error_path'])
                textfiles.append(rdesc['error_path'])
            self._retrieve_results(patterns, textfiles)

            # Echo stdout if not redirected.
            if not self.stdout:
                name = rdesc['output_path']
                if os.path.exists(name):
                    with open(name, 'rU') as inp:
                        sys.stdout.write(inp.read())
                    os.remove(name)
                else:
                    sys.stdout.write('\n[No stdout available]\n')

            # Echo stderr if not redirected.
            if not self.stderr:
                name = rdesc['error_path']
                if os.path.exists(name):
                    with open(name, 'rU') as inp:
                        sys.stderr.write(inp.read())
                    os.remove(name)
                else:
                    sys.stdout.write('\n[No stderr available]\n')
        finally:
            RAM.release(self._server)
            self._server = None

        return (return_code, error_msg)

    def _send_inputs(self, patterns, textfiles):
        """ Sends input files matching `patterns`. """
        self._logger.info('sending inputs...')
        start_time = time.time()

        filename = 'inputs.zip'
        pfiles, pbytes = pack_zipfile(patterns, filename, self._logger)
        try:
            filexfer(None, filename, self._server, filename, 'b', False)
            ufiles, ubytes = self._server.unpack_zipfile(filename,
                                                         textfiles=textfiles)
        finally:
            os.remove(filename)
            self._server.remove(filename)

        # Difficult to force file transfer error.
        if ufiles != pfiles or ubytes != pbytes:  #pragma no cover
            msg = 'Inputs xfer error: %d:%d vs. %d:%d' \
                  % (ufiles, ubytes, pfiles, pbytes)
            self.raise_exception(msg, RuntimeError)

        et = time.time() - start_time
        if et >= 60:  #pragma no cover
            self._logger.info('elapsed time: %f sec.', et)

    def _retrieve_results(self, patterns, textfiles):
        """ Retrieves result files matching `patterns`. """
        self._logger.info('retrieving results...')
        start_time = time.time()

        filename = 'outputs.zip'
        pfiles, pbytes = self._server.pack_zipfile(patterns, filename)
        filexfer(self._server, filename, None, filename, 'b', False)

        # Valid, but empty, file causes unpack_zipfile() problems.
        try:
            if os.path.getsize(filename) > 0:
                ufiles, ubytes = unpack_zipfile(filename, logger=self._logger,
                                                textfiles=textfiles)
            else:
                ufiles, ubytes = 0, 0
        finally:
            os.remove(filename)
            self._server.remove(filename)

        # Difficult to force file transfer error.
        if ufiles != pfiles or ubytes != pbytes:  #pragma no cover
            msg = 'Results xfer error: %d:%d vs. %d:%d' \
                  % (ufiles, ubytes, pfiles, pbytes)
            self.raise_exception(msg, RuntimeError)

        et = time.time() - start_time
        if et >= 60:  #pragma no cover
            self._logger.info('elapsed time: %f sec.', et)

    def stop(self):
        """ Stop the external code. """
        self._stop = True
        if self._process:
            self._process.terminate()

    def copy_inputs(self, inputs_dir, patterns):
        """
        Copy inputs from `inputs_dir` that match `patterns`.

        inputs_dir: string
            Directory to copy files from. Relative paths are evaluated from
            the component's execution directory.

        patterns: list or string
            One or more :mod:`glob` patterns to match against.

        This can be useful for resetting problem state.
        """
        self._logger.info('copying initial inputs from %s...', inputs_dir)
        with self.dir_context:
            if not os.path.exists(inputs_dir):
                self.raise_exception("inputs_dir '%s' does not exist" \
                                     % inputs_dir, RuntimeError)
            self._copy(inputs_dir, patterns)

    def copy_results(self, results_dir, patterns):
        """
        Copy files from `results_dir` that match `patterns`.

        results_dir: string
            Directory to copy files from. Relative paths are evaluated from
            the component's execution directory.

        patterns: list or string
            One or more :mod:`glob` patterns to match against.

        This can be useful for workflow debugging when the external
        code takes a long time to execute.
        """
        self._logger.info('copying precomputed results from %s...', results_dir)
        with self.dir_context:
            if not os.path.exists(results_dir):
                self.raise_exception("results_dir '%s' does not exist" \
                                     % results_dir, RuntimeError)
            self._copy(results_dir, patterns)

    def _copy(self, directory, patterns):
        """
        Copy files from `directory` that match `patterns`
        to the current directory and ensure they are writable.

        directory: string
            Directory to copy files from.

        patterns: list or string
            One or more :mod:`glob` patterns to match against.
        """
        if isinstance(patterns, basestring):
            patterns = [patterns]

        for pattern in patterns:
            pattern = os.path.join(directory, pattern)
            for src_path in sorted(glob.glob(pattern)):
                dst_path = os.path.basename(src_path)
                self._logger.debug('    %s', src_path)
                shutil.copy(src_path, dst_path)
                # Ensure writable.
                mode = os.stat(dst_path).st_mode
                mode |= stat.S_IWUSR
                os.chmod(dst_path, mode)


# This gets used by remote server.
class _AccessController(AccessController):  #pragma no cover
    """ Don't allow setting of 'command' by remote client. """

    def check_access(self, role, methodname, obj, attr):
        """ Raise :class:`RoleError` if invalid access. """
        if attr in ('command', 'get_access_controller') and \
           methodname == '__setattr__':
            raise RoleError('No %s access to %r' % (methodname, attr))

