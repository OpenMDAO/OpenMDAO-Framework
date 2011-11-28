""" Base class for an external application that needs to be executed. """

import glob
import os.path
import shlex
import shutil
import subprocess
import stat
import time

# pylint: disable-msg=E0611,F0401
from openmdao.lib.datatypes.api import Bool, Dict, Str, Float, Int

from openmdao.main.api import ComponentWithDerivatives
from openmdao.main.exceptions import RunInterrupted, RunStopped
from openmdao.main.rbac import AccessController, RoleError, rbac, remote_access
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.util.filexfer import filexfer, pack_zipfile, unpack_zipfile
from openmdao.util.shellproc import ShellProc



class ExternalCode(ComponentWithDerivatives):
    """ Run an external code as a component. """

    PIPE   = subprocess.PIPE
    STDOUT = subprocess.STDOUT

    # pylint: disable-msg=E1101
    command = Str('', 
                  desc='The command to be executed.')
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
    timed_out = Bool(False, iotype='out',
                     desc='True if the command timed-out.')
    return_code = Int(0, iotype='out',
                      desc='Return code from the command.')

    def __init__(self, *args, **kwargs):
        super(ExternalCode, self).__init__(*args, **kwargs)

        self.stdin  = None
        self.stdout = None
        self.stderr = "error.out"

        self._process = None
        self._server = None

    # This gets used by remote server.
    def get_access_controller(self):  #pragma no cover
        """ Return :class:`AccessController` for this object. """
        return _AccessController()

    @rbac(('owner', 'user'))
    def set(self, path, value, index=None, src=None, force=False):
        """ Don't allow setting of 'command' by remote client. """
        if path in ('command', 'get_access_controller') and remote_access():
            self.raise_exception('%r may not be set() remotely' % path,
                                 RuntimeError)
        return super(ExternalCode, self).set(path, value, index, src, force)

    def execute(self):
        """
        Runs the specified command.

        First removes existing output (but not in/out) files.
        Then if `resources` have been specified, an appropriate server
        is allocated and the command is run on that server.
        Otherwise the command is run locally.

        When running remotely, the following resources are set:

        ======================= =====================================
        Key                     Value
        ======================= =====================================
        job_name                self.get_pathname()
        ----------------------- -------------------------------------
        remote_command          self.command (first item)
        ----------------------- -------------------------------------
        args                    self.command (2nd through last items)
        ----------------------- -------------------------------------
        job_environment         self.env_vars
        ----------------------- -------------------------------------
        input_path              self.stdin
        ----------------------- -------------------------------------
        output_path             self.stdout
        ----------------------- -------------------------------------
        error_path              self.stderr (if != STDOUT)
        ----------------------- -------------------------------------
        join_files              If self.stderr == STDOUT
        ----------------------- -------------------------------------
        hard_run_duration_limit self.timeout (if non-zero)
        ======================= =====================================

        """
        self.return_code = -12345678
        self.timed_out = False

        for metadata in self.external_files:
            if metadata.get('output', False) and \
               not metadata.get('input', False):
                for path in glob.glob(metadata.path):
                    if os.path.exists(path):
                        os.remove(path)

        if not self.command:
            self.raise_exception('Null command line', ValueError)

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
                    stderrfile = open(self.stderr, 'r')
                    error_desc = stderrfile.read()
                    stderrfile.close()
                    
                    err_fragment = "\nError Output:\n%s" % error_desc
                else:
                    err_fragment = error_msg
                    
                self.raise_exception('return_code = %d%s' \
                    % (return_code, err_fragment), RuntimeError)
        finally:
            self.return_code = -999999 if return_code is None else return_code

    def _execute_local(self):
        """ Run command. """
        self._logger.info("executing '%s'...", self.command)
        start_time = time.time()

        self._process = \
            ShellProc(self.command, self.stdin, self.stdout, self.stderr,
                      self.env_vars)
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
        # Allocate server.
        self._server, server_info = RAM.allocate(self.resources)
        if self._server is None:
            self.raise_exception('Server allocation failed :-(', RuntimeError)

        return_code = -88888888
        error_msg = ''
        try:
            # Create resource description for command.
            rdesc = self.resources.copy()
            rdesc['job_name'] = self.get_pathname()
            items = shlex.split(self.command)
            rdesc['remote_command'] = items[0]
            if len(items) > 1:
                rdesc['args'] = items[1:]
            if self.env_vars:
                rdesc['job_environment'] = self.env_vars
            if self.stdin:
                rdesc['input_path'] = self.stdin
            if self.stdout:
                rdesc['output_path'] = self.stdout
            if self.stderr:
                if self.stderr == self.STDOUT:
                    rdesc['join_files'] = True
                else:
                    rdesc['error_path'] = self.stderr
            if self.timeout:
                rdesc['hard_run_duration_limit'] = self.timeout

            # Send inputs.
            patterns = []
            for metadata in self.external_files:
                if metadata.get('input', False):
                    patterns.append(metadata.path)
            if patterns:
                self._send_inputs(patterns)
            else:
                self._logger.debug("No input metadata paths")

            # Run command.
            self._logger.info("executing '%s'...", self.command)
            start_time = time.time()
            return_code, error_msg = \
                self._server.execute_command(rdesc)
            et = time.time() - start_time
            if et >= 60:  #pragma no cover
                self._logger.info('elapsed time: %f sec.', et)

            # Retrieve results.
            patterns = []
            for metadata in self.external_files:
                if metadata.get('output', False):
                    patterns.append(metadata.path)
            if patterns:
                self._retrieve_results(patterns)
            else:
                self._logger.debug("No output metadata paths")

        finally:
            RAM.release(self._server)
            self._server = None

        return (return_code, error_msg)

    def _send_inputs(self, patterns):
        """ Sends input files matching `patterns`. """
        self._logger.info('sending inputs...')
        start_time = time.time()

        filename = 'inputs.zip'
        pfiles, pbytes = pack_zipfile(patterns, filename, self._logger)
        try:
            filexfer(None, filename, self._server, filename, 'b')
            ufiles, ubytes = self._server.unpack_zipfile(filename)
        finally:
            os.remove(filename)

        # Difficult to force file transfer error.
        if ufiles != pfiles or ubytes != pbytes:  #pragma no cover
            msg = 'Inputs xfer error: %d:%d vs. %d:%d' \
                  % (ufiles, ubytes, pfiles, pbytes)
            self.raise_exception(msg, RuntimeError)

        et = time.time() - start_time
        if et >= 60:  #pragma no cover
            self._logger.info('elapsed time: %f sec.', et)

    def _retrieve_results(self, patterns):
        """ Retrieves result files matching `patterns`. """
        self._logger.info('retrieving results...')
        start_time = time.time()

        filename = 'outputs.zip'
        pfiles, pbytes = self._server.pack_zipfile(tuple(patterns), filename)
        filexfer(self._server, filename, None, filename, 'b')

        # Valid, but empty, file causes unpack_zipfile() problems.
        try:
            if os.path.getsize(filename) > 0:
                ufiles, ubytes = unpack_zipfile(filename, self._logger)
            else:
                ufiles, ubytes = 0, 0
        finally:
            os.remove(filename)

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
        if attr in ('command', 'get_access_controller') and \
           methodname == '__setattr__':
            raise RoleError('No %s access to %r' % (methodname, attr))

