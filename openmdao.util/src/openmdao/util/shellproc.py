import os.path
import signal
import subprocess
import sys
import time

PIPE = subprocess.PIPE
STDOUT = subprocess.STDOUT


class CalledProcessError(subprocess.CalledProcessError):
    """ :class:`subprocess.CalledProcessError` plus `errormsg` attribute. """

    def __init__(self, returncode, cmd, errormsg):
        super(CalledProcessError, self).__init__(returncode, cmd)
        self.errormsg = errormsg

    def __str__(self):
        return 'Command %r returned non-zero exit status %d: %s' \
                % (self.cmd, self.returncode, self.errormsg)


class ShellProc(subprocess.Popen):
    """
    A slight modification to :class:`subprocess.Popen`.
    If `args` is a string then the ``shell`` argument is set True.
    Updates a copy of ``os.environ`` with `env`, and opens files for any
    stream which is a :class:`basestring`.

    args: string or list
        If a string, then this is the command line to execute and the
        :class:`subprocess.Popen` ``shell`` argument is set True.
        Otherwise this is a list of arguments, the first is the command
        to execute.

    stdin, stdout, stderr: string, file, or int
        Specify handling of corresponding stream. If a string, a file
        of that name is opened. Otherwise see the :mod:`subprocess`
        documentation.

    env: dict
        Environment variables for the command.
    """

    def __init__(self, args, stdin=None, stdout=None, stderr=None, env=None):
        environ = os.environ.copy()
        if env:
            environ.update(env)

        self._stdin_arg  = stdin
        self._stdout_arg = stdout
        self._stderr_arg = stderr

        if isinstance(stdin, basestring):
            self._inp = open(stdin, 'r')
        else:
            self._inp = stdin
    
        if isinstance(stdout, basestring):
            self._out = open(stdout, 'w')
        else:
            self._out = stdout

        if isinstance(stderr, basestring):
            self._err = open(stderr, 'w')
        else:
            self._err = stderr

        shell = isinstance(args, basestring)

        try:
            subprocess.Popen.__init__(self, args, stdin=self._inp,
                                      stdout=self._out, stderr=self._err,
                                      shell=shell, env=environ)
        except Exception:
            self.close_files()
            raise

    def close_files(self):
        """ Closes files that were implicitly opened. """
        if isinstance(self._stdin_arg, basestring):
            self._inp.close()
        if isinstance(self._stdout_arg, basestring):
            self._out.close()
        if isinstance(self._stderr_arg, basestring):
            self._err.close()

    def terminate(self, timeout=None):
        """
        Stop child process. If `timeout` is specified then :meth:`wait` will
        be called to wait for the process to terminate.

        timeout: float (seconds)
            Maximum time to wait for the process to stop.
            A value of zero implies an infinite maximum wait.

        """
        super(ShellProc, self).terminate()
        if timeout is not None:
            self.wait(timeout=timeout)

    def wait(self, poll_delay=0., timeout=0.):
        """
        Polls for command completion or timeout.
        Closes any files implicitly opened.
        Returns ``(return_code, error_msg)``.

        poll_delay: float (seconds)
            Time to delay between polling for command completion.
            A value of zero uses an internal default.

        timeout: float (seconds)
            Maximum time to wait for command completion.
            A value of zero implies an infinite maximum wait.
        """
        return_code = None
        try:
            if poll_delay <= 0:
                poll_delay = max(0.1, timeout/100.)
                poll_delay = min(10., poll_delay)
            npolls = int(timeout / poll_delay) + 1

            time.sleep(poll_delay)
            return_code = self.poll()
            while return_code is None:
                npolls -= 1
                if (timeout > 0) and (npolls < 0):
                    self.terminate()
                    break
                time.sleep(poll_delay)
                return_code = self.poll()
        finally:
            self.close_files()

        # self.returncode set by self.poll().
        if return_code is not None:
            self.errormsg = self.error_message(return_code)
        else:
            self.errormsg = 'Timed out'
        return (return_code, self.errormsg)

    def error_message(self, return_code):
        """
        Return error message for `return_code`.
        The error messages are derived from the operating system definitions,
        some programs don't necessarily return exit codes conforming to these
        definitions.

        return_code: int
            Return code from :meth:`poll`.
        """
        error_msg = ''
        if return_code:
            if return_code > 0:
                error_msg = ': %s' % os.strerror(return_code)
            elif sys.platform != 'win32':
                sig = -return_code
                if sig < signal.NSIG:
                    for item in signal.__dict__.keys():
                        if item.startswith('SIG'):
                            if getattr(signal, item) == sig:
                                error_msg = ': %s' % item
                                break
        return error_msg


def call(args, stdin=None, stdout=None, stderr=None, env=None,
         poll_delay=0., timeout=0.):
    """
    Run command with arguments.
    Returns ``(return_code, error_msg)``.

    args: string or list
        If a string, then this is the command line to execute and the
        :class:`subprocess.Popen` ``shell`` argument is set True.
        Otherwise this is a list of arguments, the first is the command
        to execute.

    stdin, stdout, stderr: string, file, or int
        Specify handling of corresponding stream. If a string, a file
        of that name is opened. Otherwise see the :mod:`subprocess`
        documentation.

    env: dict
        Environment variables for the command.

    poll_delay: float (seconds)
        Time to delay between polling for command completion.
        A value of zero uses an internal default.

    timeout: float (seconds)
        Maximum time to wait for command completion.
        A value of zero implies an infinite maximum wait.
    """
    process = ShellProc(args, stdin, stdout, stderr, env)
    try:
        return process.wait(poll_delay, timeout)
    finally:
        process.close_files()


def check_call(args, stdin=None, stdout=None, stderr=None, env=None,
               poll_delay=0., timeout=0.):
    """
    Run command with arguments.
    If non-zero `return_code`, raises :class:`CalledProcessError`.

    args: string or list
        If a string, then this is the command line to execute and the
        :class:`subprocess.Popen` ``shell`` argument is set True.
        Otherwise this is a list of arguments, the first is the command
        to execute.

    stdin, stdout, stderr: string, file, or int
        Specify handling of corresponding stream. If a string, a file
        of that name is opened. Otherwise see the :mod:`subprocess`
        documentation.

    env: dict
        Environment variables for the command.

    poll_delay: float (seconds)
        Time to delay between polling for command completion.
        A value of zero uses an internal default.

    timeout: float (seconds)
        Maximum time to wait for command completion.
        A value of zero implies an infinite maximum wait.
    """
    process = ShellProc(args, stdin, stdout, stderr, env)
    try:
        return_code, error_msg = process.wait(poll_delay, timeout)
    finally:
        process.close_files()

    if return_code:
        raise CalledProcessError(return_code, args, error_msg)

