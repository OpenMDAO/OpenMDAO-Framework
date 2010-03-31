import glob
import os.path
import shutil
import subprocess
import stat
import time

from enthought.traits.api import Dict

from openmdao.main.api import Component
from openmdao.main.exceptions import RunInterrupted, RunStopped
from openmdao.main.resource import ResourceAllocationManager as RAM
from openmdao.lib.api import Bool, Float, Int, Str
from openmdao.util.filexfer import filexfer, pack_zipfile, unpack_zipfile
from openmdao.util.shellproc import ShellProc


class ExternalCode(Component):
    """
    Run an external code as a component.

    - `command` is the command to be executed.
    - `env_vars` is a dictionary of environment variables for `command`.
    - `resources` is a dictionary of resources required to run this component.
    - `poll_delay` is the delay between polling for command completion \
      (seconds). A value <= zero will use an internally computed default.
    - `timeout` is the maximum time to wait for command completion (seconds). \
      A value <= zero implies an infinite wait.
    - `return_code` is the value returned by the command.
    - `timed_out` is set True if the command timed-out.
    """
    PIPE   = subprocess.PIPE
    STDOUT = subprocess.STDOUT

    command = Str('', iotype='in',
                  desc='Command to be executed.')
    env_vars = Dict({}, iotype='in',
                    desc='Environment variables.')
    resources = Dict({}, iotype='in',
                     desc='Resources required to run this component.')
    poll_delay = Float(0., units='s', io_type='in',
                       desc='Delay between polling for command completion.')
    timeout = Float(0., low=0., iotype='in', units='s',
                    desc='Max time to wait for command completion.')
    timed_out = Bool(False, iotype='out',
                     desc='True if command timed-out.')
    return_code = Int(0, iotype='out',
                      desc='Return code from command.')

    def __init__(self, *args, **kwargs):
        super(ExternalCode, self).__init__(*args, **kwargs)

        self.stdin  = None
        self.stdout = None
        self.stderr = None

        self._process = None
        self._server = None

    def execute(self):
        """
        Removes existing output (but not in/out) files,
        If `resources` have been specified, then an appropriate server
        is allocated and the command is run on that server.
        Otherwise runs the command locally.
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
        except Exception as exc:
            self.raise_exception('%s' % exc, type(exc))
        else:
            if return_code is None:
                if self._stop:
                    self.raise_exception('Run stopped', RunStopped)
                else:
                    self.timed_out = True
                    self.raise_exception('Timed out', RunInterrupted)
            elif return_code:
                self.raise_exception('return_code = %d%s' \
                                     % (return_code, error_msg), RuntimeError)
        finally:
            self.return_code = -999999 if return_code is None else return_code

    def _execute_local(self):
        """ Run command. """
        self.info("executing '%s'...", self.command)
        start_time = time.time()

        self._process = \
            ShellProc(self.command, self.stdin, self.stdout, self.stderr,
                      self.env_vars)
        self.debug('PID = %d', self._process.pid)

        try:
            return_code, error_msg = \
                self._process.wait(self.poll_delay, self.timeout)
        finally:
            self._process.close_files()
            self._process = None

        et = time.time() - start_time
        if et >= 60:
            self.info('elapsed time: %f sec.', et)

        return (return_code, error_msg)

    def _execute_remote(self):
        """
        Allocate a server based on required resources, send inputs,
        run command, and retrieve results.
        """
        # Allocate server.
        self._server, server_info = RAM.allocate(self.resources)
        if self._server is None:
            self.raise_exception('Server allocation failed :-(')

        return_code = -88888888
        error_msg = ''
        try:
            # Send inputs.
            patterns = []
            for metadata in self.external_files:
                if metadata.get('input', False):
                    patterns.append(metadata.path)
            if patterns:
                self._send_inputs(patterns)
            else:
                self.debug("No input metadata paths")

            # Run command.
            self.info("executing '%s'...", self.command)
            start_time = time.time()
            return_code, error_msg = \
                self._server.execute_command(self.command, self.stdin,
                                             self.stdout, self.stderr,
                                             self.env_vars, self.poll_delay,
                                             self.timeout)
            et = time.time() - start_time
            if et >= 60:
                self.info('elapsed time: %f sec.', et)

            # Retrieve results.
            patterns = []
            for metadata in self.external_files:
                if metadata.get('output', False):
                    patterns.append(metadata.path)
            if patterns:
                self._retrieve_results(patterns)
            else:
                self.debug("No output metadata paths")

        finally:
            RAM.release(self._server)
            self._server = None

        return (return_code, error_msg)

    def _send_inputs(self, patterns):
        """ Sends input files matching `patterns`. """
        self.info('sending inputs...')
        start_time = time.time()

        filename = 'inputs.zip'
        pfiles, pbytes = pack_zipfile(patterns, filename, self._logger)
        try:
            filexfer(None, filename, self._server, filename, 'b')
            ufiles, ubytes = self._server.unpack_zipfile(filename)
        finally:
            os.remove(filename)

        if ufiles != pfiles or ubytes != pbytes:
            msg = 'Results xfer error: %d:%d vs. %d:%d' \
                  % (ufiles, ubytes, pfiles, pbytes)
            self.raise_exception(msg, RuntimeError)

        et = time.time() - start_time
        if et >= 60:
            self.info('elapsed time: %f sec.', et)

    def _retrieve_results(self, patterns):
        """ Retrieves result files matching `patterns`. """
        self.info('retrieving results...')
        start_time = time.time()

        filename = 'outputs.zip'
        pfiles, pbytes = self._server.pack_zipfile(tuple(patterns), filename)
        try:
            filexfer(self._server, filename, None, filename, 'b')
            ufiles, ubytes = unpack_zipfile(filename, self._logger)
        finally:
            os.remove(filename)

        if ufiles != pfiles or ubytes != pbytes:
            msg = 'Results xfer error: %d:%d vs. %d:%d' \
                  % (ufiles, ubytes, pfiles, pbytes)
            self.raise_exception(msg, RuntimeError)

        et = time.time() - start_time
        if et >= 60:
            self.info('elapsed time: %f sec.', et)

    def stop(self):
        """ Stop the external code. """
        self._stop = True
        if self._process:
            self._process.terminate()

    def copy_inputs(self, inputs_dir, patterns):
        """
        Copy inputs from `inputs_dir` that match `patterns`.
        This can be useful for resetting problem state.
        """
        self.info('copying initial inputs from %s...', inputs_dir)
        self.push_dir()
        try:
            if not os.path.exists(inputs_dir):
                self.raise_exception("inputs_dir '%s' does not exist" \
                                     % inputs_dir, RuntimeError)
            self.copy_files(inputs_dir, patterns)
        finally:
            self.pop_dir()

    def copy_results(self, results_dir, patterns):
        """
        Copy files from `results_dir` that match `patterns`.
        This can be useful for workflow debugging when the external
        code takes a long time to execute.
        """
        self.info('copying precomputed results from %s...', results_dir)
        self.push_dir()
        try:
            if not os.path.exists(results_dir):
                self.raise_exception("results_dir '%s' does not exist" \
                                     % results_dir, RuntimeError)
            self.copy_files(results_dir, patterns)
        finally:
            self.pop_dir()

    def copy_files(self, directory, patterns):
        """
        Copies files from `directory` that match `patterns`
        to the current directory and ensures they are writable.
        """
        for pattern in patterns:
            pattern = os.path.join(directory, pattern)
            for src_path in sorted(glob.glob(pattern)):
                dst_path = os.path.basename(src_path)
                self.debug('copy %s -> %s', src_path, dst_path)
                shutil.copy(src_path, dst_path)
                # Ensure writable.
                mode = os.stat(dst_path).st_mode
                mode |= stat.S_IWUSR
                os.chmod(dst_path, mode)

