import os.path
import signal
import subprocess
import time

PIPE = subprocess.PIPE
STDOUT = subprocess.STDOUT


class CalledProcessError(subprocess.CalledProcessError):
    """ :class:`subprocess.CalledProcessError` plus `errormsg` attribute. """
    pass


class ShellProc(subprocess.Popen):
    """
    A slight modification to :class:`subprocess.Popen`.
    Sets the ``shell`` argument True, updates a copy of ``os.environ`` with
    `env`, and opens files for any stream which is a :class:`basestring`.
    """

    def __init__(self, args, stdin=None, stdout=None, stderr=None, env=None):
        if env:
            environ = os.environ.copy()
            environ.update(env)
        else:
            environ = None

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

        subprocess.Popen.__init__(self, args, stdin=self._inp, stdout=self._out,
                                  stderr=self._err, shell=True, env=environ)

    def close_files(self):
        """ Closes files that were implicitly opened. """
        if isinstance(self._stdin_arg, basestring):
            self._inp.close()
        if isinstance(self._stdout_arg, basestring):
            self._out.close()
        if isinstance(self._stderr_arg, basestring):
            self._err.close()

    def wait(self, poll_delay=0., timeout=0.):
        """
        Polls for command completion or timeout.
        Closes any files implicitly opened.
        Returns ``(return_code, error_msg)``.
        """
        return_code = None
        error_msg = ''
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
                    error_msg = 'Timed out'
                    break
                time.sleep(poll_delay)
                return_code = self.poll()

            if return_code:
                if return_code > 0:
                    error_msg = ': %s' % os.strerror(return_code)
                else:
                    sig = -return_code
                    if sig < signal.NSIG:
                        for item in signal.__dict__.keys():
                            if item.startswith('SIG'):
                                if getattr(signal, item) == sig:
                                    error_msg = ': %s' % item
                                    break
        finally:
            self.close_files()

        self.errormsg = error_msg  # returncode set by self.poll().
        return (return_code, error_msg)


def call(args, stdin=None, stdout=None, stderr=None, env=None,
         poll_delay=0., timeout=0.):
    """ Run command with arguments. Returns ``(return_code, error_msg)``. """
    process = ShellProc(args, stdin, stdout, stderr, env)
    try:
        return_code, error_msg = process.wait(poll_delay, timeout)
    finally:
        process.close_files()
    return (return_code, error_msg)


def check_call(args, stdin=None, stdout=None, stderr=None, env=None,
               poll_delay=0., timeout=0.):
    """
    Run command with arguments.
    If non-zero `return_code`, raises :class:`CalledProcessError`.
    """
    process = ShellProc(args, stdin, stdout, stderr, env)
    try:
        return_code, error_msg = process.wait(poll_delay, timeout)
    finally:
        process.close_files()

    if return_code:
        exc = CalledProcessError()
        exc.returncode = return_code
        exc.error_msg = error_msg
        raise exc

