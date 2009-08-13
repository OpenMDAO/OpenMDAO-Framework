
__all__ = ('ExternalCode',)


import os.path
import signal
import subprocess
import sys
import time

if sys.platform == 'win32':
    import win32api

from enthought.traits.api import Float, Bool, Int, Str, Any

from openmdao.main.api import Component, UnitsFloat
from openmdao.main.exceptions import RunInterrupted, RunStopped


# TODO: better process kill implementation (2.6 terminate, process tree).
def kill_proc(proc, sig):
    """ Kill process with given signal. """
    if sys.platform == 'win32':
        win32api.TerminateProcess(int(proc._handle), -1)
    else:
        os.kill(proc.pid, sig)


class ExternalCode(Component):
    """
    Run an external code as a component.

    - `command` is the command to be executed.
    - `timeout` is the maximum time to wait for command completion (seconds). \
      A value <= zero implies an infinite wait.
    - `return_code` is the value returned by the command.
    - `timed_out` is set True if the command timed-out.
    """
    PIPE   = subprocess.PIPE
    STDOUT = subprocess.STDOUT

    command = Str('', iostatus='in',
                  desc='Command to be executed.')

    timeout = UnitsFloat(0., low=0., iostatus='in', units='s',
                    desc='Max time to wait for command completion.')

    timed_out = Bool(False, iostatus='out',
                     desc='True if command timed-out.')

    return_code = Any(0, iostatus='out',
                      desc='Return code from command.')

    def __init__(self, doc=None, directory=''):
        super(ExternalCode, self).__init__(doc, directory)

        self.stdin   = None
        self.stdout  = None
        self.stderr  = None

        self._env     = None
        self._process = None

# pylint: disable-msg=E1101

    def execute(self):
        """ Run command. """
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

        try:
            bufsize = 0
            close_fds = False
            universal_newlines = False

            preexec_fn = None

            startup_info = None  # For Windows.
            creation_flags = 0   # For Windows.

            self.timed_out = False
            self.return_code = None
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

            self.debug('PID = %d', self._process.pid)

            poll_delay = max(0.1, self.timeout/100.)
            poll_delay = min(10., poll_delay)
            npolls = int(self.timeout / poll_delay) + 1

            time.sleep(poll_delay)
            self.return_code = self._process.poll()
            while self.return_code is None:
                npolls -= 1
                if (self.timeout > 0) and (npolls < 0):
                    self.timed_out = True
                    self.return_code = None
                    kill_proc(self._process, signal.SIGKILL)
                    self.raise_exception('Timed out', RunInterrupted)
                time.sleep(poll_delay)
                try:
                    self.return_code = self._process.poll()
                except AttributeError:
                    break  # self._process became None, maybe due to control-C

            if self._stop:
                self.raise_exception('Run stopped', RunStopped)

            if self.return_code:
                reason = ''
                if self.return_code > 0:
                    reason = ': %s' % os.strerror(self.return_code)
                elif self.return_code < 0:
                    sig = -self.return_code
                    if sig < signal.NSIG:
                        for item in signal.__dict__.keys():
                            if item.startswith('SIG'):
                                if getattr(signal, item) == sig:
                                    reason = ': %s' % item
                                    break
                self.raise_exception('return_code = %d%s '% \
                                     (self.return_code, reason), RuntimeError)
        finally:
            self._process = None
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
            kill_proc(self._process, signal.SIGTERM)

