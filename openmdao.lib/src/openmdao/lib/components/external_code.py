import glob
import os.path
import shutil
import signal
import stat
import subprocess
import sys
import time

from openmdao.main.api import Component
from openmdao.main.exceptions import RunInterrupted, RunStopped
from openmdao.lib.api import Bool, Float, Int, Str

__all__ = ('ExternalCode',)


class ExternalCode(Component):
    """
    Run an external code as a component.

    - `command` is the command to be executed.
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
    poll_delay = Float(0., units='s', io_type='in',
                       desc='Delay between polling for command completion.')
    timeout = Float(0., low=0., iotype='in', units='s',
                    desc='Max time to wait for command completion.')
    timed_out = Bool(False, iotype='out',
                     desc='True if command timed-out.')
    return_code = Int(0, iotype='out',
                      desc='Return code from command.')

    def __init__(self, doc=None, directory=''):
        super(ExternalCode, self).__init__(doc, directory)

        self.stdin   = None
        self.stdout  = None
        self.stderr  = None

        self._env     = None
        self._process = None

    def execute(self):
        """
        Removes existing output (but not in/out) files,
        expands `command` user and environment variables,
        and then launches a process with the resulting command line.
        Polls for command completion or timeout.
        """
        for metadata in self.external_files:
            if metadata.get('output', False) and \
               not metadata.get('input', False):
                for path in glob.glob(metadata.path):
                    if os.path.exists(path):
                        os.remove(path)

        cmd = self.command
        cmd = os.path.expanduser(cmd)
        cmd = os.path.expandvars(cmd)
        if not cmd:
            self.raise_exception('Null command line', ValueError)

        # If using a shell, args should be the command line.
        # Otherwise, args needs to be the split up arguments.
        shell = True
        if shell:
            args = cmd
        else:
            args = cmd.split()

        env = self._env
        executable = None

        # Directory setting is Component push_dir/pop_dir.
        cwd = None

        # If streams are strings, open corresponding files.
        if isinstance(self.stdin, basestring):
            stdin = open(self.stdin, 'r')
        else:
            stdin = self.stdin

        if isinstance(self.stdout, basestring):
            stdout = open(self.stdout, 'w')
        else:
            stdout = self.stdout

        if isinstance(self.stderr, basestring):
            stderr = open(self.stderr, 'w')
        else:
            stderr = self.stderr

        return_code = None
        try:
            bufsize = 0
            close_fds = False
            universal_newlines = False

            preexec_fn = None

            startup_info = None  # For Windows.
            creation_flags = 0   # For Windows.

            self.timed_out = False
            self._process = None

            try:
                self.debug("executing '%s'...", cmd)
                self._process = subprocess.Popen(args, bufsize, executable,
                                                 stdin, stdout, stderr,
                                                 preexec_fn, close_fds,
                                                 shell, cwd, env,
                                                 universal_newlines,
                                                 startup_info, creation_flags)
            except Exception, exc:
                self.raise_exception('Exception creating process: %s' % exc,
                                     RuntimeError)

            start_time = time.time()
            self.debug('PID = %d', self._process.pid)

            if self.poll_delay <= 0:
                poll_delay = max(0.1, self.timeout/100.)
                poll_delay = min(10., poll_delay)
            else:
                poll_delay = self.poll_delay
            npolls = int(self.timeout / poll_delay) + 1

            time.sleep(poll_delay)
            return_code = self._process.poll()
            while return_code is None:
                npolls -= 1
                if (self.timeout > 0) and (npolls < 0):
                    self.timed_out = True
                    return_code = None
                    self._process.terminate()
                    self.raise_exception('Timed out', RunInterrupted)
                time.sleep(poll_delay)
                try:
                    return_code = self._process.poll()
                except AttributeError:
                    break  # self._process became None, maybe due to control-C

            et = time.time() - start_time
            if et >= 60:
                self.debug('elapsed time: %f sec.', et)

            if self._stop:
                self.raise_exception('Run stopped', RunStopped)

            if return_code:
                reason = ''
                if return_code > 0:
                    reason = ': %s' % os.strerror(return_code)
                elif return_code < 0:
                    sig = -return_code
                    if sig < signal.NSIG:
                        for item in signal.__dict__.keys():
                            if item.startswith('SIG'):
                                if getattr(signal, item) == sig:
                                    reason = ': %s' % item
                                    break
                self.raise_exception('return_code = %d%s '% \
                                     (return_code, reason), RuntimeError)
        finally:
            self._process = None
            self.return_code = -999999 if return_code is None else return_code

            # If streams are strings, close corresponding files.
            if isinstance(self.stdin, basestring):
                stdin.close()
            if isinstance(self.stdout, basestring):
                stdout.close()
            if isinstance(self.stderr, basestring):
                stderr.close()

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

