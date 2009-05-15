
__all__ = ('ExternalCode',)
__version__ = '0.1'

import os.path
import signal
import subprocess
import time

from openmdao.main import Component, Bool, Float, Int, String
from openmdao.main.exceptions import RunInterrupted, RunStopped
from openmdao.main.variable import INPUT, OUTPUT


class ExternalCode(Component):
    """ Run an external code as a component. """

    PIPE   = subprocess.PIPE
    STDOUT = subprocess.STDOUT

    def __init__(self, name='ExternalCode', parent=None, doc=None,
                 directory=''):
        super(ExternalCode, self).__init__(name, parent, doc, directory)

        String('command', self, INPUT, default='',
               doc='Command to be executed.')

        Float('timeout', self, INPUT, default=0, min_limit=0, units='s',
              doc='Max time to wait for command completion.')

        Bool('timed_out', self, OUTPUT, default=False,
             doc='True if command timed-out.')

        Int('return_code', self, OUTPUT, default=0,
            doc='Return code from command.')

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
# TODO: better process kill implementation (2.6 terminate, process tree).
                    os.kill(self._process.pid, signal.SIGKILL)
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
            os.kill(self._process.pid, signal.SIGTERM)
            # new in version 2.6
            # self._process.terminate()

